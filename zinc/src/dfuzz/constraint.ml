open Context.Alt

(* constraints have two forms - equality, and inequality *)
type relation =
  | Eq of Sensitivity.t * Sensitivity.t
  | LEq of Sensitivity.t * Sensitivity.t

(* but we maintain a list of these relations as we abduce *)
type t = relation list

(* an important conversion is context relations to constraints *)
let relation_of_context_relation (n : Name.t) (rel : Context.relation) : relation = match rel with
  | Context.Eq (l, r) -> Eq (n <$ l, n <$ r)

let of_context_relation (rel : Context.relation) : t =
  let f n = relation_of_context_relation n rel in CCList.map f (vars rel)

(* and at some point we need to convert each to a z3 formula *)
(*
let rec to_z3 : t -> z3 = function
  | Empty -> z3.true
  | Cons (r, rs) ->
    let r' = relation_to_z3 r in
    z3.and r' (to_z3 rs)
*)
