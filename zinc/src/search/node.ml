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

(* okay, what kind of metric makes sense here *)

(* step 1: look at converting sensitivity relations to ssa form *)
module SSA = struct
  (* A (n, s) := n <- s *)
  type t = A of Name.t * Sensitivity.t

  (* root is always the k we care about *)
  let root = Name.of_string "k"

  (* printing is simple, should really only be used for debugging *)
  let to_string : t -> string = function
    | A (n, s) ->
      let n' = Name.to_string n in
      let s' = Sensitivity.to_string s in
        n' ^ " <- " ^ s'

  (* weird accessor for later *)
  let assigns_to (n : Name.t) (ssa : t) : bool = match ssa with
    | A (n', s) -> n' = n

  (* mostly we should be able to do this - unclear when we'll get none *)
  let of_sens_rel : Sensitivity.Relation.t -> t option = fun srs ->
    let _ = print_endline "GENERATING:" in
    let _ = print_endline (Sensitivity.Relation.to_string srs) in 
    let l, r = match srs with
      | Sensitivity.Relation.Eq (l, r) -> l, r
      | Sensitivity.Relation.LEq (l, r) -> l, r
    in let _ = print_endline ("\tLEFT: " ^ (Sensitivity.to_string l)) in
    let _ = print_endline ("\tRIGHT: " ^ (Sensitivity.to_string r)) in
    let _ = if (l = r) then print_endline ("\tAGREE") else () in 
      if l = r then None else match l, r with
      | Sensitivity.Free n, _ -> Some (A (n, r))
      | _, Sensitivity.Free n -> Some (A (n, l))
      | _ -> None

  (* the only reason we want the above is to do this stuff *)
  let of_constraint : Constraint.t -> t list = fun c ->
    let sens_rels = Constraint.flatten c in
    CCList.filter_map of_sens_rel sens_rels

  (* for making the graph, we need to know what each assignment depends on *)
  let ssa_depends_on : t -> Name.t list = function
    | A (n, s) -> Sensitivity.free_vars s

  let depends_on (prog : t list) (n : Name.t) : Name.t list =
    CCList.flat_map ssa_depends_on (CCList.filter (assigns_to n) prog)

  (* now we can make a graph *)
  type vertex = Name.t
  type dependency_graph = (vertex, unit) CCGraph.t

  let to_graph (prog : t list) : dependency_graph =
    CCGraph.of_fun (depends_on prog)

  let is_acyclic (dg : dependency_graph) : bool = 
    let root_graph = CCGraph.Seq.return root in
      CCGraph.is_dag dg root_graph
end
