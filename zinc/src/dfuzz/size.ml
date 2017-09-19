(* representation of size terms *)
type t =
  | Var of Variable.t
  | Zero
  | Succ of t

(* simple printing *)
let rec to_string : t -> string = function
  | Var v -> Variable.to_string v
  | Zero -> "0"
  | Succ s -> let s' = to_string s in
    s' ^ "++"

(* mcbride and mckinna abstraction and instantiation *)
(* because this data type has no binders, we will only ever call these helper functions *)
let rec name_to_db (name : Name.t) (depth : int) (s : t) : t = match s with
  | Succ s' -> Succ (name_to_db name depth s')
  | Zero -> Zero
  | Var v -> Var (Variable.name_to_db name depth v)

let rec db_to_name (depth : int) (img : Name.t) (s : t) : t = match s with
  | Succ s' -> Succ (db_to_name depth img s')
  | Zero -> Zero
  | Var v -> Var (Variable.db_to_name depth img v)
