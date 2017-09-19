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
