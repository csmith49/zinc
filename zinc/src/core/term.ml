type ('a, 'b) t =
  | Free of Name.t
  | Bound of int
  | App of ('a, 'b) t * ('a, 'b) t
  | Abs of 'b * ('a, 'b) scope
  | Const of 'a
  | Wild of 'b
and ('a, 'b) scope = Sc of ('a, 'b) t

let rec name_to (n : Name.t) (db : int) (tm : ('a, 'b) t) : ('a, 'b) t = match tm with
  | Free n' -> if n == n' then (Bound db) else tm
  | App (f, args) -> App (name_to n db f, name_to n db args)
  | Abs (dom, Sc body) -> Abs (dom, Sc (name_to n (db + 1) body))
  | _ -> tm

let abstract (n : Name.t) (tm : ('a, 'b) t) : ('a, 'b) scope = Sc (name_to n 0 tm)

let rec replace (img : ('a, 'b) t) (db : int) (body : ('a, 'b) t) : ('a, 'b) t = match body with
  | Bound i -> if i == db then img else body
  | App (f, args) -> App (replace img db f, replace img db args)
  | Abs (dom, Sc body) -> Abs (dom, Sc (replace img (db + 1) body))
  | _ -> body

let instantiate (img : ('a, 'b) t) (s : ('a, 'b) scope) : ('a, 'b) t = match s with
  | Sc body -> replace img 0 body
