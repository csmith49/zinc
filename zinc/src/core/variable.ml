(* a minor extension of name.t *)
(* we distinguish between free and bound using the strategy of mcbride and mckinna *)
type t =
  | Free of Name.t
  | Bound of int

(* simple printing *)
let to_string : t -> string = function
  | Free n -> Name.to_string n
  | Bound i -> string_of_int i
