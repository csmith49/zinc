module StringMap = CCMap.Make(String)

type t =
  | Int of int
  | Bool of bool
  | Real of float  
  | Bag of t list
  | F of abstraction
  | Row of row
  | Discrete of string
  | Pair of t * t
and abstraction = t -> t
and row = t StringMap.t

(* printing *)
let rec to_string : t -> string = function
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Real r -> string_of_float r
  | F _ -> "<FUN>"
  | Bag ts -> "[" ^ (String.concat ", " (CCList.map to_string ts)) ^ "]"
  | Row r -> "ROW"
  | Discrete d -> d
  | Pair (l, r) -> "(" ^ (to_string l) ^ ", " ^ (to_string r) ^ ")"

(* making simple values *)
let rec row_of_list : (string * t) list -> t = function
  | [] -> Row StringMap.empty
  | (s, v) :: vs -> match row_of_list vs with
    | Row row -> Row (StringMap.add s v row)
    | _ -> failwith "shouldn't happen"

(* we need comparisons *)
let rec compare (l : t) (r : t) : int = match l, r with
  | Real l, Real r -> Pervasives.compare l r
  | Bag ls, Bag rs -> Pervasives.compare (CCList.sort compare ls) (CCList.sort compare rs)
  | _ -> Pervasives.compare l r

let geq (l : t) (r : t) : bool = (compare l r) = 1