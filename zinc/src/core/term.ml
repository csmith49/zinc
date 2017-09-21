type ('a, 'b) t =
  | Free of Name.t
  | Bound of int
  | App of ('a, 'b) t * ('a, 'b) t
  | Abs of 'b * ('a, 'b) scope
  | Const of 'a
  | Wild of 'b * ('a, 'b) scope
and ('a, 'b) scope = Sc of ('a, 'b) t

(* mcbride and mckinna abstraction and instantiation *)
let rec name_to (n : Name.t) (db : int) (tm : ('a, 'b) t) : ('a, 'b) t = match tm with
  | Free n' -> if n = n' then (Bound db) else tm
  | App (f, args) -> App (name_to n db f, name_to n db args)
  | Abs (dom, Sc body) -> Abs (dom, Sc (name_to n (db + 1) body))
  | Wild (dom, Sc body) -> Wild (dom, Sc (name_to n (db + 1) body))
  | _ -> tm

let abstract (n : Name.t) (tm : ('a, 'b) t) : ('a, 'b) scope = Sc (name_to n 0 tm)

let rec replace (img : ('a, 'b) t) (db : int) (body : ('a, 'b) t) : ('a, 'b) t = match body with
  | Bound i -> if i = db then img else body
  | App (f, args) -> App (replace img db f, replace img db args)
  | Abs (dom, Sc body) -> Abs (dom, Sc (replace img (db + 1) body))
  | Wild (dom, Sc body) -> Wild (dom, Sc (replace img (db + 1) body))
  | _ -> body

let instantiate (img : ('a, 'b) t) (s : ('a, 'b) scope) : ('a, 'b) t = match s with
  | Sc body -> replace img 0 body

(* our submodules want to refer to the type while maintaining their own copy of t *)
type ('a, 'b) term = ('a, 'b) t

(* prefixes help us maintain binding levels and whatnot *)
module Prefix = struct
  (* we have two different kinds of bindings *)
  type binder =
    | PAbs
    | PWild
  (* and bindings maintain a name with the correct domain and binder *)
  type 'b binding = Name.t * 'b * binder
  (* so a prefix maintains a stack of bindings *)
  type 'b t = ('b binding) Stack.t
  (* infix binding applications/inverses *)
  let (@>) (b : 'b binding) (tm : ('a, 'b) term) : ('a, 'b) term = match b with
    | (n, dom, bndr) -> match bndr with
      | PAbs -> Abs (dom, abstract n tm)
      | PWild -> Wild (dom, abstract n tm)
  let (<@) (n : Name.t) (tm : ('a, 'b) term) : ('b binding * ('a, 'b) term) option = match tm with
    | Abs (dom, body) -> Some ((n, dom, PAbs), instantiate (Free n) body)
    | Wild (dom, body) -> Some ((n, dom, PWild), instantiate (Free n) body)
    | _ -> None
  (* which we lift to binding over prefixes *)
  let rec bind (prefix : 'b t) (tm : ('a, 'b) term) : ('a, 'b) term = match prefix with
    | Stack.Empty -> tm
    | Stack.Cons (b, ps) -> bind ps (b @> tm)
end
