type t =
  | Int of int
  | List of t
  | Bag of t
  | Real of float
  | F of abstraction
and abstraction = t -> t

(* printing *)
let to_string : t -> string = function
  | Int i -> string_of_int i
  | Real r -> string_of_float r
  | F _ -> "<FUN>"
  | _ -> "unimplemented"
