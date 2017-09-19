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

let rec replace (img : t) (depth : int) (s : t) : t = match s with
  | Succ s' -> Succ (replace img depth s')
  | Zero -> Zero
  | Var v -> if Variable.matches v depth then img else s

let rec replace_variable (img : Variable.t) (depth : int) (s : t) : t = match s with
  | Succ s' -> Succ (replace_variable img depth s')
  | Zero -> Zero
  | Var v -> Var (Variable.replace img depth v)
