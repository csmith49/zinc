module Vector = struct
  type t  = {
    size : int;
    constant : int;
    unconstrained : int;
    infinity : int;
  }
  
  type weight = int * int * int * int
  
  let to_int : weight -> t -> int = fun w -> fun v ->
    let a, b, c, d = w in
    a * v.size + b * v.constant + c * v.unconstrained + d * v.infinity
end

type t = {
  root : Name.t;
  obligation : Constraint.t;
  solution : Fterm.t;
}

(* mostly just for testing *)
let to_string (n : t) : string = Fterm.to_string n.solution

module Priority = struct
  type t = {
    root : Name.t;
    obligation : Constraint.t;
    solution : Fterm.t;
    priority : int;
  }

  let compare (l : t) (r : t) : int =
    let ans = compare l.priority r.priority in
    match ans with
      | 0 -> compare l.solution r.solution
      | _ -> ans
  
  let leq (l : t) (r : t) : bool = (compare l r) < 1
end

let to_vector : t -> Vector.t = fun n ->
let c = n.obligation |> Constraint.flatten |> Simplify.assignment_simplify in
(* let c = n.obligation |> Constraint.flatten in *)
let sensitivities = Simplify.extract_sensitivities (c) in  
  {
    Vector.size = Fterm.size n.solution;
    constant = CCList.length (CCList.filter Simplify.is_constant sensitivities);
    unconstrained = CCList.length (CCList.filter Simplify.is_unconstrained sensitivities);
    infinity = CCList.length (CCList.filter Simplify.is_infinity sensitivities);
  }

let of_priority : Priority.t -> t = fun p ->
  {
    root = p.Priority.root;
    obligation = p.Priority.obligation;
    solution  = p.Priority.solution;
  }

let to_priority : Vector.weight -> t -> Priority.t = fun w -> fun n ->
  {
    Priority.root = n.root;
    obligation = n.obligation;
    solution = n.solution;
    priority = n |> to_vector |> (Vector.to_int w);
  }