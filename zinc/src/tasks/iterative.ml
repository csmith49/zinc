type benchmark = {
    name : string;
    goal : Dtype.t;
    examples : (Vterm.t list * Vterm.t) list;
    signature : Primitive.t list;
}

(* converting benchmark to initial synthesis node *)
let to_node : benchmark -> Node.t = fun bm -> 
    let open Constraint.Alt in let open Make in let open Name.Alt in {
    Node.root = Name.of_string "start";
    recursion = None;
    obligation = (zero <= k);
    solution = begin
        let context = Context.Empty in
        let root = Name.of_string bm.name in
        let w = root <+ "wild" in
            Vterm.Wild (context, bm.goal, Vterm.abstract_one w (Vterm.Var (Vterm.Free w)))
    end;
    }

(* utility for applying function to list of inputs *)
let rec app_list : Vterm.t -> Vterm.t list -> Vterm.t = fun f -> fun ts ->
    app_list' f (CCList.rev ts)
and app_list' : Vterm.t -> Vterm.t list -> Vterm.t = fun f -> function
    | [] -> f
    | x :: [] -> Vterm.App (f, x)
    | x :: xs -> Vterm.App (app_list' f xs, x)

(* checking if a solution satisfies a benchmark *)
let check (solution : Vterm.t) (bm : benchmark) : bool =
    let _ = print_endline ("  Evaluating: " ^ (Vterm.format solution)) in
    let check_ex (inputs, output) =
        let i = app_list solution inputs |> Vterm.eval in
        let o = output |> Vterm.eval in
        let _ = print_endline ("    Input: " ^ Vterm.Evaluation.to_string i) in
        let _ = print_endline ("    Output: " ^ Vterm.Evaluation.to_string o) in
        let ans = i = o in
        let _ = print_endline ("    Result: " ^ string_of_bool ans) in
            ans
    in CCList.for_all check_ex bm.examples

(* writing types is muuuuch easier if we just import make *)
open Make

(* K MEANS *)

(* for k-means - we operate over points *)
let point = pair (real, real)
(* and have a precise sensitivity constraint *)
let k_sens = Sensitivity.Mult (eps, n)

(* primitives for kmeans *)
let k_step = {
    Primitive.name = "k_step";
    dtype = sbind (k,
        p_list (point, k) => (modal (eps, bag point) -* prob (p_list (point, k)))
    );
    source = let process_point tm = match tm with
        | Vterm.Pair (Vterm.Real l, Vterm.Real r) ->
            let l = 2.0 *. l in
            let r = 2.0 *. r in
                Vterm.Value (Vterm.Pair (Vterm.Real l, Vterm.Real r))
        | _ -> Vterm.Diverge in
    Vterm.Function ("k_step", fun centers ->
        Vterm.Value (Vterm.Function ("", fun data ->
            match Vterm.eval centers with
                | Vterm.Value ls ->
                    Vterm.Evaluation.list_map process_point ls
                | _ -> Vterm.Diverge
    )))
}

(* example input construction *)
let p : float * float -> Vterm.t = fun p ->
    Vterm.Alt.pair (Vterm.Alt.real (fst p)) (Vterm.Alt.real (snd p))

let bag_1 = [
    (1.0, 1.0);
    (2.0, 2.0);
    (3.0, 3.0);
] |> CCList.map p |> Vterm.Alt.bag

let centers_1 = [
    (1.0, 2.0);
    (2.0, 1.0);
    (2.5, 2.5);
] |> CCList.map p |> Vterm.Alt.conslist

let rec_1 = Vterm.App (Vterm.App (k_step.source, centers_1), bag_1)
let rec_2 = Vterm.App (Vterm.App (k_step.source, rec_1), bag_1)

let kmeans = {
    name = "kmeans";
    goal = 
    (* sbind (k, sbind (n,  *)
        (p_int n) => (
            (p_list (point, k)) => (modal (k_sens, bag point) -* prob (p_list (point, k)))
        );
        (* )); *)
    examples = [
        ([Vterm.Alt.nat 0 ; centers_1 ; bag_1], centers_1);
        (
            [Vterm.Alt.nat 1 ; centers_1 ; bag_1],
            rec_1 |> Vterm.eval |> Vterm.Evaluation.get_exn
        );
        (
            [Vterm.Alt.nat 2 ; centers_1 ; bag_1],
            rec_2 |> Vterm.eval |> Vterm.Evaluation.get_exn
        );
    ];
    signature = [k_step];
}

(* IDC *)

(* operate over queries *)
let query = Dtype.Base "query"
(* and approx_dbs *)
let approx_db = Dtype.Base "approx_db"
(* with base dbs *)
let db = Dtype.Base "db"

(* sensitivity *)
let idc_sens = let open Sensitivity.Alt in
    two *! n *! eps

(* utilities for implementation *)
let list_to_bag : float list -> Vterm.t = fun fs -> fs
    |> CCList.map Vterm.Alt.real |> Vterm.Alt.bag
let bag_to_list : Vterm.t -> float list option = function
    | Vterm.Bag ts -> ts |> CCList.map Vterm.eval |> CCList.map Vterm.Evaluation.to_real |> CCOpt.sequence_l
    | _ -> None

let rec cross_product : float list -> float list -> float = fun ls -> fun rs -> match ls, rs with
    | l :: ls, r :: rs ->
        (l *. r) +. (cross_product ls rs)
    | [], [] -> 0.0
    | _ -> failwith "no"

(* three primitives to build off of *)

(* evaluating linear queries - they're one-sensitive, so easy type *)
(* query -> db -o_1 real *)
let eval_q = {
    Primitive.name = "eval_q";
    dtype = query => (modal (one, db) -* real);
    source = Vterm.Function ("eval_q", fun q ->
        Vterm.Value (Vterm.Function ("eval_q q", fun db ->
            match Vterm.eval q, Vterm.eval db with
                | Vterm.Value q, Vterm.Value db -> begin match bag_to_list q, bag_to_list db with
                    | Some q, Some db -> Vterm.Value (cross_product q db |> Vterm.Alt.real)
                    | _ -> Vterm.Diverge
                end
                | _ -> Vterm.Diverge
        )));
}

(* privacy distinguisher *)
(* query bag -> approx_db -> db -o_e prob query *)
let pa = {
    Primitive.name = "pa";
    dtype = (bag query) => (approx_db => (modal (eps, db) -* (prob query)));
    source = Vterm.Function ("pa", fun queries ->
        Vterm.Value (Vterm.Function ("pa qs", fun approx ->
            Vterm.Value (Vterm.Function ("pa qs a", fun db ->
                Vterm.Diverge
            )))));
}

(* database update algorithm *)
(* approx_db -> query -> real -> approx_db *)
let dua = {
    Primitive.name = "dua";
    dtype = approx_db => (query => (real => approx_db));
    source = Vterm.Function ("dua", fun approx ->
        Vterm.Value (Vterm.Function ("dua a", fun q ->
            Vterm.Value (Vterm.Function ("dua a q", fun result ->
                Vterm.Diverge
            )))));
}

(* and a mechanism for adding noise *)
(* real -*_1 prob real *)
let add_noise = {
    Primitive.name = "add_noise";
    dtype = modal (eps, real) -* real;
    source = Vterm.Function ("add_noise", fun v -> Vterm.Value v);
}

(* constant for representing our initial approximation *)
let init_approx = {
    Primitive.name = "init_approx";
    dtype = approx_db;
    source = Vterm.Nat Vterm.Zero;
}

let idc = {
    name = "idc";
    goal = (p_int n) => (bag query => (modal (idc_sens, db) -* (prob approx_db)));
    examples = [];
    signature = [pa ; dua ; eval_q ; add_noise ; init_approx];
}

(* CUMULATIVE DENSITY FUNCTION *)

(* primitives for computing cdf *)

(* size of bags - databases, here *)
let bag_size = {
    Primitive.name = "bag_size";
    dtype = modal (one, bag real) -* real;
    source = Vterm.Function ("bag_size", fun b -> match b with
        | Vterm.Bag ts -> ts
            |> CCList.length
            |> float_of_int
            |> Vterm.Evaluation.of_real
        | _ -> Vterm.Diverge        
    )
}

(* filter bag into parts via a predicate *)
let bag_split = {
    Primitive.name = "bag_split";
    dtype = (real => bool) => (modal (one, bag real) -* pair (bag real, bag real));
    source = Vterm.Function ("bag_split", fun pred ->
        Vterm.Value (Vterm.Function ("bag_split p", fun b -> match b with
            | Vterm.Bag ts -> let rec split ts = begin match ts with
                | [] -> Vterm.Value (Vterm.Pair (Vterm.Bag [], Vterm.Bag []))
                | x :: xs -> begin match split xs with
                    | Vterm.Value (Vterm.Pair (Vterm.Bag ls, Vterm.Bag rs)) ->
                        if Vterm.App (pred, x) |> Vterm.eval |> Vterm.Evaluation.is_true then
                            Vterm.Value (Vterm.Pair (Vterm.Bag (x :: ls), Vterm.Bag rs))
                        else
                            Vterm.Value (Vterm.Pair (Vterm.Bag ls, Vterm.Bag (x :: rs)))
                    | _ -> Vterm.Diverge
                    end
                end in split ts
            | _ -> Vterm.Diverge
        )))
}
(* constructor for the relevant pred *)
let split_pred = {
    Primitive.name = "split_pred";
    dtype = real => (real => bool);
    source = Vterm.Function ("split_pred", fun r ->
        Vterm.Value (Vterm.Function ("split_pred r", fun x ->
            Vterm.Evaluation.real_gt (Vterm.eval x) (Vterm.eval r) |> Vterm.Evaluation.of_bool
        )))
}

let bag_split_lt = {
    Primitive.name = "bag_split_lt";
    dtype = real => (modal (one, bag real) -* pair (bag real, bag real));
    source = Vterm.Function ("bag_split_lt", fun r ->
        Vterm.Value (Vterm.Function ("bag_split_lt r", fun b ->
            Vterm.App (bag_split.source, Vterm.App (split_pred.source, r)) |> Vterm.eval
        )))
}

(* pair manipulation stuff *)
let p_fst = {
    Primitive.name = "fst";
    dtype = tbind (
        a, modal (one, pair (a, a)) -* a
    );
    source = Vterm.Function ("fst", fun p -> match p with
        | Vterm.Pair (tm, _) -> Vterm.Value tm
        | _ -> Vterm.Diverge);
}
(* pair manipulation stuff *)
let p_snd = {
    Primitive.name = "snd";
    dtype = tbind (
        a, modal (one, pair (a, a)) -* a
    );
    source = Vterm.Function ("snd", fun p -> match p with
        | Vterm.Pair (_, tm) -> Vterm.Value tm
        | _ -> Vterm.Diverge);
}

(* list manipulation *)
let nil = {
    Primitive.name = "nil";
    dtype = p_list (real, zero);
    source = Vterm.ConsList (Vterm.Nil);
}
let cons = {
    Primitive.name = "cons";
    dtype = sbind (k, 
        modal (one, real) -* (modal (one, p_list (real, k)) -* (p_list (real, Sensitivity.Plus (k, one))))
    );
    source = Vterm.Function ("cons", fun hd ->
        Vterm.Value (Vterm.Function ("cons hd", fun tl ->
            Vterm.Value (Vterm.ConsList (Vterm.Cons (hd, tl)))
        )));
}

(* overall sensitivity *)
let cdf_sens = let open Sensitivity.Alt in
    k *! eps

(* example construction *)
let db_1 = [
    0.5 ; 1.5; 1.5; 2.5; 3.5 ;
] |> CCList.map Vterm.Alt.real |> Vterm.Alt.bag

let bucket_0 = Vterm.Alt.conslist []
let bucket_1 = [
    2.0
] |> CCList.map Vterm.Alt.real |> Vterm.Alt.conslist
let bucket_2 = [
    1.0 ; 2.0
] |> CCList.map Vterm.Alt.real |> Vterm.Alt.conslist

let result_0 = Vterm.Alt.conslist []
let result_1 = [
    3.0
] |> CCList.map Vterm.Alt.real |> Vterm.Alt.conslist
let result_2 = [
    1.0 ; 2.0
] |> CCList.map Vterm.Alt.real |> Vterm.Alt.conslist

let cdf = {
    name = "cdf";
    goal = p_list (real, k) => (modal (cdf_sens, bag real) -* prob (p_list (real, k)));
    examples = [
        ( [bucket_0 ; db_1], result_0 );
        ( [bucket_1 ; db_1], result_1 );
        ( [bucket_2 ; db_1], result_2 );
    ];
    signature = [bag_size ; bag_split_lt ; p_fst ; p_snd ; nil ; cons ; add_noise];
}

let all = [kmeans ; idc ; cdf]