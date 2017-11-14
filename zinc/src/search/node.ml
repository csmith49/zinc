(* what we maintain in our search *)
type t = {
  root : Name.t;
  obligation : Constraint.t;
  solution : Fterm.t;
}

type vector = {
  size : int;
  constant : int;
  unconstrained : int;
  infinity : int;
}
type weight = int * int * int * int

let to_vector : t -> vector = fun n ->
  let c = n.obligation |> Constraint.flatten |> Simplify.assignment_simplify in
  let sensitivities = Simplify.extract_sensitivities (c) in
  {
    size = Fterm.size n.solution;
    constant = CCList.length (CCList.filter Simplify.is_constant sensitivities);
    unconstrained = CCList.length (CCList.filter Simplify.is_unconstrained sensitivities);
    infinity = CCList.length (CCList.filter Simplify.is_infinity sensitivities);
  }

(* mostly just for testing *)
let to_string (n : t) : string = Fterm.to_string n.solution

(* we do comparisons through this priority module *)
module Priority = struct
  type t = int * Fterm.t
  let compare (l : t) (r : t) : int = match l, r with
    | (li, lt), (ri, rt) -> match Pervasives.compare li ri with
      | 0 -> Pervasives.compare lt rt
      | (_ as c) -> c
end

let int_of_vector : weight -> vector -> int = fun w -> fun v ->
  let a, b, c, d = w in
  a * v.size + b * v.constant + c * v.unconstrained + d * v.infinity

(* size, constants, variables, infinity *)
let default_weight = (1, 0, 0, 0)
let good_weight = (1, 0, 3, 0)
let aws_weight = (1, 0, 10, 0)

(* we might as well make this easy *)
let to_priority : t -> Priority.t = fun n -> 
  (* (CCList.length ( n.obligation |> Constraint.flatten |> Simplify.simplify) , n.solution) *)
 (* (Fterm.size n.solution, n.solution) *)
  (* (n.obligation |> Constraint.flatten |> Simplify.sensitivity_weight, n.solution) *)
  (n |> to_vector |> (int_of_vector aws_weight), n.solution)

(* and hijack it for comparisons *)
let compare (l : t) (r : t) : int = Priority.compare (to_priority l) (to_priority r)