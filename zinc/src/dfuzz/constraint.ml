(* constraints only have two forms - equality amongst size variables and relations amonst sensitivities *)
type relation =
  | EQ of Size.t * Size.t
  | LEQ of Sensitivity.t * Sensitivity.t

(* but we maintain a list of these relations as we abduce *)
type t = relation Stack.t

(* and at some point we need to convert each to a z3 formula *)
(*
let rec to_z3 : t -> z3 = function
  | Empty -> z3.true
  | Cons (r, rs) ->
    let r' = relation_to_z3 r in
    z3.and r' (to_z3 rs)
*)

(* this lets us define the abduction procedure *)
let rec abduction (l : Dtype.t) (r : Dtype.t) : t option =
