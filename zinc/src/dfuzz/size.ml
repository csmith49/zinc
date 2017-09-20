(* representation of size terms *)
type t =
  | Free of Name.t
  | Bound of int
  | Zero
  | Succ of t

(* mcbride and mckinna abstraction and instantiation *)
(* because this data type has no binders, we will only ever call these helper functions *)
let rec name_to (n : Name.t) (db : int) (s : t) : t = match s with
  | Free n' -> if n == n' then (Bound db) else s
  | Succ s' -> Succ (name_to n db s')
  | _ -> s

let rec replace (img : t) (db : int) (s : t) : t = match s with
  | Bound i -> if i == db then img else s
  | Succ s' -> Succ (replace img db s')
  | _ -> s
