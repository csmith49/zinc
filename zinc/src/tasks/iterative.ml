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
}

let all = [kmeans]