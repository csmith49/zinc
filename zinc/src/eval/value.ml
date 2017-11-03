type t =
  | Int of int
  | Bool of bool
  | Real of float  
  | Bag of t list
  | F of abstraction
and abstraction = t -> t

(* printing *)
let rec to_string : t -> string = function
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Real r -> string_of_float r
  | F _ -> "<FUN>"
  | Bag ts -> "[" ^ (CCString.concat ", " (CCList.map to_string ts)) ^ "]"
  | _ -> "unimplemented"
