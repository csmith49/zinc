module StringMap = CCMap.Make(String)

type t =
  | Int of int
  | Bool of bool
  | Real of float  
  | Bag of t list
  | F of abstraction
  | Row of row
  | Discrete of string
and abstraction = t -> t
and row = t StringMap.t

(* printing *)
let rec to_string : t -> string = function
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Real r -> string_of_float r
  | F _ -> "<FUN>"
  | Bag ts -> "[" ^ (CCString.concat ", " (CCList.map to_string ts)) ^ "]"
  | Row r -> "ROW"
  | Discrete d -> d