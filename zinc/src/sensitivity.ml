module SubMap = CCMap.Make(String)
type substitution = string SubMap.t

module SizeKind = struct
    (* sometimes it makes sense to keep the sizes separate from the sensitivity *)
    type t =
        | Value of Rational.t
        | Variable of string
        | Plus of t * Rational.t
    (* but we still want to print things, so... *)
    let rec to_string : t -> string = function
        | Value q -> Rational.to_string q
        | Variable x -> x
        | Plus (s, q) -> (to_string s) ^ " + " ^ (Rational.to_string q)
    (* and convert to z3 formulas... *)
    let rec to_z3 : t -> Solver.expr = function
        | Value q -> Rational.to_z3 q
        | Variable x -> Solver.make_variable x
        | Plus (s, q) -> Solver.make_add (to_z3 s) (Rational.to_z3 q)
    (* and possibly move variables around and whatnot *)
    let rec variables : t -> string list = function
        | Value _ -> []
        | Variable x -> [x]
        | Plus (s, _) -> variables s
    let rec apply_sub (sub : substitution) (e : t) : t = match e with
        | Value _ -> e
        | Variable x -> Variable (SubMap.get_or x sub ~default:x)
        | Plus (s, q) -> Plus (apply_sub sub s, q)
end


(* but the type we really care about is just sensitivity *)
type t =
    | Value of Rational.t
    | Variable of string
    | Plus of t * t
    | Times of t * t
    | Size of SizeKind.t

(* with all the usual operations *)
let rec to_string : t -> string = function
    | Value q -> Rational.to_string q
    | Variable x -> x
    | Plus (l, r) -> (to_string l) ^ " + " ^ (to_string r)
    | Times (l, r) -> (to_string l) ^ " * " ^ (to_string r)
    | Size s -> SizeKind.to_string s
let rec to_z3 : t -> Solver.expr = function
    | Value q -> Rational.to_z3 q
    | Variable x -> Solver.make_variable x
    | Plus (l, r) -> Solver.make_add (to_z3 l) (to_z3 r)
    | Times (l, r) -> Solver.make_times (to_z3 l) (to_z3 r)
    | Size s -> SizeKind.to_z3 s
let rec variables : t -> string list = function
    | Value _ -> []
    | Variable x -> [x]
    | Plus (l, r) -> (variables l) @ (variables r)
    | Times (l, r) -> (variables l) @ (variables r)
    | Size s -> SizeKind.variables s
let rec apply_sub (sub : substitution) (e : t) : t = match e with
    | Value _ -> e
    | Variable x -> Variable (SubMap.get_or x sub ~default:x)
    | Plus (l, r) -> Plus (apply_sub sub l, apply_sub sub r)
    | Times (l, r) -> Times (apply_sub sub l, apply_sub sub r)
    | Size s -> Size (SizeKind.apply_sub sub s)
