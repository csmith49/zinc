open Stack.Alt

(* constraints have two forms - equality, and inequality *)
type relation =
  | Eq of Sensitivity.t * Sensitivity.t
  | LEq of Sensitivity.t * Sensitivity.t

(* but we maintain a list of these relations as we abduce *)
type t = relation Stack.t

(* an important conversion is contexts to constraints *)

(* and at some point we need to convert each to a z3 formula *)
(*
let rec to_z3 : t -> z3 = function
  | Empty -> z3.true
  | Cons (r, rs) ->
    let r' = relation_to_z3 r in
    z3.and r' (to_z3 rs)
*)
