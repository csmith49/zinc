(* rationals are just treated as pairs of numbers, unless they're infinite *)
type t =
    | Infinity
    | Q of int * int

(* we don't actually have a way to encode infinity, so we do this instead *)
let max_q = Q (128, 1)
let zero_q = Q (0, 1)

(* comparison probably doesn't need to be done, but we'll do it anyways *)
let compare (a : t) (b : t) : int = match a, b with
    | Infinity, Infinity -> 0
    | Infinity, _ -> 1
    | _, Infinity -> -1
    | _ -> Pervasives.compare a b

(* we need ways to convert qs to various types, and from various types *)
let of_int (i : int) : t = Q (i, 1)
let to_string : t -> string = function
    | Infinity -> "INFTY"
    | Q (a, b) -> (string_of_int a) ^ "/" ^ (string_of_int b)

(* and some basic arithmetic *)
let add (a : t) (b : t) : t = match a, b with
    | Infinity, _ -> Infinity
    | _, Infinity -> Infinity
    | Q (x, y), Q (n, d) -> Q (x * d + n * y, y * d)
let mult (a : t) (b : t) : t = match a, b with
    | Infinity, _ -> Infinity
    | _, Infinity -> Infinity
    | Q (x, y), Q (n, d) -> Q (x * n, y * d)

(* finally, we have to convert to z3 objects *)
let rec to_z3 : t -> Solver.expr = function
    | Infinity -> to_z3 max_q
    | Q (n, d) -> Solver.make_rational n d

(* TODO *)
let enforce_bounds (x : Variable.t) : Solver.expr =
    let xs = Solver.make_variable x in
        Solver.make_and (Solver.make_leq xs (to_z3 max_q)) (Solver.make_geq xs (to_z3 zero_q))
