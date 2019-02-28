type hole = Zipper.branch * Vterm.t * Vterm.t

type benchmark = {
    name : string;
    goal : Dtype.t;
    examples : (Vterm.t list * Vterm.t) list;
    signature : Primitive.t list;
    template : (Vterm.t list) -> Vterm.t;
    holes : hole list;
    recursion : Termination.Filter.t;
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

let rec choose_holes : int -> hole list -> (Zipper.branch list * Vterm.t list) = fun bits -> function
    | (branch, wildcard, default) :: rest ->
        let (branches, terms) = choose_holes (bits / 2) rest in
            if bits mod 2 = 1 then (* when lowest bit is set use default *)
                (branches, default :: terms)
            else (* when lowest bit is not set set up wilcard *)
                (branch :: branches, wildcard :: terms)
    | [] -> ([], [])

let template_to_node : int -> benchmark -> Node.t = fun bits -> fun bm ->
    let branches, terms = choose_holes bits bm.holes in
    let zipper = (bm.template terms, branches) in
    let tm = Zipper.to_term zipper in
    let open Constraint.Alt in let open Make in let open Name.Alt in {
        Node.root = Name.of_string "start";
        recursion = Some bm.recursion;
        obligation = (zero <= k);
        solution = tm;
    }

let make_hole : string -> Dtype.t -> Vterm.t -> hole = fun id -> fun dt -> fun default ->
    let name = Name.of_string id in
    let wild = Vterm.Var (Vterm.Free (Name.extend name "wild")) in
    let context = Context.Symbolic (Name.extend name "context") in
        (
            Zipper.ZWild (context, dt, Name.extend name "wild"), wild, default
        )

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
module KMeans = struct
    (* for k-means - we operate over points *)
    let point = pair (real, real)
    (* and have a precise sensitivity constraint *)
    let sens = Sensitivity.Mult (eps, n)

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

    (* the desired solution *)
    module Solution = struct
        open Vterm.Alt

        let f = var "f"
        let f_dt = (p_int n) => ((p_list (point, k)) => (modal (sens, Make.bag point) -* prob (p_list (point, k))))

        let x = var "x"
        let x_dt = (p_int n)

        let y = var "y"
        let y_dt = p_list (point, k)

        let z = var "z"
        let z_dt = Make.bag point 

        let n = var "n"
        let n_sens = Sensitivity.Free (Name.of_string "n_sens")

        let d = var "d"
        let d_dt = p_list (point, k)

        let zero = return y
        let recurse = f <!> n <!> y <!> z
        let step = k_step.source <!> d <!> z

        let template args = match args with
            | zero_e :: recurse_e :: step_e :: _ ->
                fix f f_dt (abs x x_dt (abs y y_dt (abs z  z_dt (
                    match_nat x zero_e (n_sens, n, sample d d_dt recurse_e step_e)
                ))))
            | _ -> failwith "not enough arguments"

        let holes = [
            make_hole "zero" (prob (p_list (point, k))) zero;
            make_hole "recurse" (prob (p_list (point, k))) recurse;
            make_hole "step" (prob (p_list (point, k))) step;
        ]
    end

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

    let benchmark = {
        name = "kmeans";
        goal = 
        (* sbind (k, sbind (n,  *)
            (p_int n) => (
                (p_list (point, k)) => (modal (sens, bag point) -* prob (p_list (point, k)))
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
        template = Solution.template;
        holes = Solution.holes;
        recursion = {
            Termination.Filter.base = [
                Name.of_string "f" ; Name.of_string "x" ; Name.of_string "y" ; Name.of_string "z"
            ];
            order = [
                Termination.LT (Name.of_string "n", Name.of_string "x")
            ]
        }
    }
end


(* IDC *)
module IDC = struct
    (* operate over queries *)
    let query = Dtype.Base "query"
    (* and approx_dbs *)
    let approx_db = Dtype.Base "approx_db"
    (* with base dbs *)
    let db = Dtype.Base "db"

    (* sensitivity *)
    let sens = let open Sensitivity.Alt in
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

    let rec select_query : (float list) list -> (float list) -> (float list) -> float list = fun qs -> fun adb -> fun db ->
        match qs with
            | [] -> failwith "nah"
            | q :: [] -> q
            | q :: qs ->
                let rest = select_query qs adb db in
                let q_perf = Float.abs (
                    (cross_product q adb) -. (cross_product q db)
                ) in
                let r_perf = Float.abs (
                    (cross_product rest adb) -. (cross_product rest db)
                ) in if q_perf > r_perf then q else rest

    let rec update_adb : (float list) -> (float list) -> float -> float list = fun adb -> fun query -> fun ans ->
        let a_ans = cross_product adb query in update_adb' adb query a_ans ans
    and update_adb' adb query a_ans ans =
        match adb, query with
            | x :: xs, q :: qs ->
                let rest = update_adb' xs qs a_ans ans in
                let x' = x *. Float.exp(q *. (ans -. a_ans) /. 2.0) in
                    x' :: rest
            | [], [] -> []
            | _ -> failwith "ill-formed"

    (* three primitives to build off of *)

    (* evaluating linear queries - they're one-sensitive, so easy type *)
    (* query -> db -o_1 real *)
    let eval_q = {
        Primitive.name = "eval_q";
        dtype = query => (modal (one, db) -* real);
        source = Vterm.Function ("eval_q", fun q ->
            Vterm.Value (Vterm.Function ("eval_q q", fun db ->
                match bag_to_list q, bag_to_list db with
                    | Some q, Some db -> Vterm.Value (cross_product q db |> Vterm.Alt.real)
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
                    match Vterm.eval queries with
                        | Vterm.Value (Vterm.Bag queries) -> begin
                            match queries |> CCList.map bag_to_list |> CCOpt.sequence_l with
                                | Some queries -> begin match bag_to_list approx, bag_to_list db with
                                    | Some adb, Some db ->
                                        Vterm.Value (select_query queries adb db |> list_to_bag)
                                    | _ -> Vterm.Diverge
                                end
                                | _ -> Vterm.Diverge
                            end
                        | _ -> Vterm.Diverge
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
                    match bag_to_list approx, bag_to_list q, Vterm.eval result with
                        | Some adb, Some q, Vterm.Value (Vterm.Real r) ->
                            Vterm.Value (update_adb adb q r |> list_to_bag)
                        | _ -> Vterm.Diverge
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
        source = list_to_bag [1.0; 1.0; 1.0;];
    }

    module Solution = struct
        open Vterm.Alt

        let f = var "f"
        let f_dt = (p_int n) => (Make.bag query => (modal (sens, db) -* (prob approx_db)))

        let iter = var "iter"
        let iter_dt = p_int n
        
        let qs = var "qs"
        let qs_dt = Make.bag query

        let data = var "data"
        let data_dt = db

        let n = var "n"
        let n_sens = Sensitivity.Free (Name.of_string "n_sens")

        let approx = var "approx"
        let approx_dt = approx_db
        
        let q = var "q"
        let q_dt = query
        
        let actual = var "actual"
        let actual_dt = Make.real

        let zero = return init_approx.source
        let draw_approx = f <!> n <!> qs <!> data
        let draw_q = pa.source <!> qs <!> approx <!> data
        let draw_actual = add_noise.source <!> approx <!> q <!> data
        let result = return (dua.source <!> approx <!> q <!> actual)

        let template args = match args with
            | zero_e :: draw_approx_e :: draw_q_e :: draw_actual_e :: result_e :: _ ->
                fix f f_dt (abs iter iter_dt (abs qs qs_dt (abs data data_dt (
                    match_nat iter zero_e (n_sens, n,
                        sample approx approx_dt draw_approx_e (
                            sample q q_dt draw_q_e (
                                sample actual actual_dt draw_actual_e (
                                    result_e
                                ))))))))
            | _ -> failwith "not enough args"

        let holes = [
            make_hole "zero" (prob approx_db) zero;
            make_hole "d_approx" (prob approx_db) draw_approx;
            make_hole "d_q" (prob query) draw_q;
            make_hole "d_actual" (prob Make.real) draw_actual;
            make_hole "result" (prob approx_db) result;
        ]
    end

    let q_1 = list_to_bag [1.0; 0.0; 0.0;]
    let q_2 = list_to_bag [0.0; 1.0; 0.0;]
    let q_3 = list_to_bag [0.0; 0.0; 1.0;]
    let queries = Vterm.Bag [
        q_1;
        q_2;
        q_3;
    ]

    let db_1 = list_to_bag [2.0; 1.0; 3.0]

    let result_0 = init_approx.source
    let result_1 = update_adb [2.0; 1.0; 3.0] [0.0; 0.0; 1.0] 3.0
    let result_2 = let q = select_query [
        [1.0; 0.0; 0.0;] ; [0.0; 1.0; 0.0;] ; [0.0; 0.0; 1.0;]
    ] result_1 [2.0; 1.0; 3.0] in update_adb result_1 q (cross_product result_1 [2.0; 1.0; 3.0])

    let benchmark = {
        name = "idc";
        goal = (p_int n) => (bag query => (modal (sens, db) -* (prob approx_db)));
        examples = [
            ([Vterm.Alt.nat 0 ; queries ; db_1], result_0);
            ([Vterm.Alt.nat 1 ; queries ; db_1], result_1 |> list_to_bag);
        ];
        signature = [pa ; dua ; eval_q ; add_noise ; init_approx];
        template = Solution.template;
        holes = Solution.holes;
        recursion = {
            Termination.Filter.base = [
                Name.of_string "f" ; Name.of_string "x" ; Name.of_string "y" ; Name.of_string "z"
            ];
            order = [
                Termination.LT (Name.of_string "n", Name.of_string "x")
            ]
        }
    }
end

(* CUMULATIVE DENSITY FUNCTION *)
module CDF = struct
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

    (* let benchmark = {
        name = "cdf";
        goal = p_list (real, k) => (modal (cdf_sens, bag real) -* prob (p_list (real, k)));
        examples = [
            ( [bucket_0 ; db_1], result_0 );
            ( [bucket_1 ; db_1], result_1 );
            ( [bucket_2 ; db_1], result_2 );
        ];
        signature = [bag_size ; bag_split_lt ; p_fst ; p_snd ; nil ; cons ; IDC.add_noise];
    } *)
end

let all = [KMeans.benchmark ; IDC.benchmark]