(* the type we really care about is just sensitivity *)
type t =
    | Value of Rational.t
    | Variable of Variable.t
    | Plus of t * t
    | Times of t * t

(* subs map variables to other expressions *)
module SubMap = CCMap.Make(Variable)
type substitution = t SubMap.t

(* with all the usual operations *)
let rec to_string : t -> string = function
    | Value q -> Rational.to_string q
    | Variable x -> Variable.to_string x
    | Plus (l, r) -> (to_string l) ^ " + " ^ (to_string r)
    | Times (l, r) -> (to_string l) ^ " * " ^ (to_string r)
let rec to_z3 : t -> Solver.expr = function
    | Value q -> Rational.to_z3 q
    | Variable x -> Solver.make_variable x
    | Plus (l, r) -> Solver.make_add (to_z3 l) (to_z3 r)
    | Times (l, r) -> Solver.make_times (to_z3 l) (to_z3 r)
let rec variables : t -> Variable.t list = function
    | Value _ -> []
    | Variable x -> [x]
    | Plus (l, r) -> (variables l) @ (variables r)
    | Times (l, r) -> (variables l) @ (variables r)
let rec apply_sub (sub : substitution) (e : t) : t = match e with
    | Value _ -> e
    | Variable x -> SubMap.get_or x sub ~default:(Variable x)
    | Plus (l, r) -> Plus (apply_sub sub l, apply_sub sub r)
    | Times (l, r) -> Times (apply_sub sub l, apply_sub sub r)

(* and importantly, some arithmetic operations which are really just wrappers *)
let add (l : t) (r : t) : t = Plus (l, r)
let mult (l : t) (r : t) : t = Times (l, r)
let rational_mult (l : Rational.t) (r : t) : t = Times (Value l, r)

(* TODO *)
(* ensure that we only bound variables once *)
(* we're lifting this all the way up *)
let equality_to_z3 (l : t) (r : t) : Solver.expr =
    let eq = Solver.make_eq (to_z3 l) (to_z3 r) in
    let vars = CCList.sort_uniq ((variables l) @ (variables r)) in
    let bounds = CCList.map Rational.enforce_bounds vars in
        Solver.make_and_list (eq :: bounds)
