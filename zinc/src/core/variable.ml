(* a minor extension of name.t *)
(* we distinguish between free and bound using the strategy of mcbride and mckinna *)
type t =
  | Free of Name.t
  | Bound of int

(* simple printing *)
let to_string : t -> string = function
  | Free n -> Name.to_string n
  | Bound i -> string_of_int i

(* helpers for mcbride and mckinna *)
let name_to_db (name : Name.t) (depth : int) (v : t) : t = match v with
  | Free n' -> if n' == name then Bound depth else v
  | _ -> v

let db_to_name (depth : int) (img : Name.t) (v : t) : t = match v with
  | Bound i -> if i == depth then Free img else v
  | _ -> v
