(* representation of sensitivity terms *)
type t =
  | Free of Name.t
  | Bound of int
  | Const of Rational.t (* note - this covers infinite case too *)
  | Size of Size.t
  | Plus of t * t
  | Mult of t * t

(* mcbride and mckinna abstraction and instantiation *)
(* because this data type has no binders, we will only ever call these helper functions *)
let rec name_to (n : Name.t) (db : int) (s : t) : t = match s with
  | Free n' -> if n == n' then (Bound db) else s
  | Plus (l, r) -> Plus (name_to n db l, name_to n db r)
  | Mult (l, r) -> Mult (name_to n db l, name_to n db r)
  | Size s' -> Size (Size.name_to n db s')
  | _ -> s

let rec replace (img : t) (db : int) (s : t) : t = match s with
  | Bound i -> if i == db then img else s
  | Plus (l, r) -> Plus (replace img db l, replace img db r)
  | Mult (l, r) -> Mult (replace img db l, replace img db r)
  | Size s' -> begin match img with
      | Size img' -> Size (Size.replace img' db s')
      | _ -> s
  end
  | _ -> s
