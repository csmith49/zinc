(* what we maintain in our search *)
type t = {
  root : Name.t;
  obligation : Constraint.t;
  solution : Fterm.t;
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

(* we might as well make this easy *)
let to_priority : t -> Priority.t = fun n -> (Fterm.size n.solution, n.solution)

(* and hijack it for comparisons *)
let compare (l : t) (r : t) : int = Priority.compare (to_priority l) (to_priority r)