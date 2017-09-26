type  t =
  | Free of Name.t
  | Bound of int
  | App of t * t
  | Abs of Dtype.t * scope
  | Const of Value.t
  | Wild of Dtype.t * scope
and scope = Sc of t

(* mcbride and mckinna abstraction and instantiation *)
let rec name_to (n : Name.t) (db : int) (tm : t) : t = match tm with
  | Free n' -> if n = n' then (Bound db) else tm
  | App (f, args) -> App (name_to n db f, name_to n db args)
  | Abs (dom, Sc body) -> Abs (dom, Sc (name_to n (db + 1) body))
  | Wild (dom, Sc body) -> Wild (dom, Sc (name_to n (db + 1) body))
  | _ -> tm

let abstract (n : Name.t) (tm : t) : scope = Sc (name_to n 0 tm)

let rec replace (img : t) (db : int) (body : t) : t = match body with
  | Bound i -> if i = db then img else body
  | App (f, args) -> App (replace img db f, replace img db args)
  | Abs (dom, Sc body) -> Abs (dom, Sc (replace img (db + 1) body))
  | Wild (dom, Sc body) -> Wild (dom, Sc (replace img (db + 1) body))
  | _ -> body

let instantiate (img : t) (s : scope) : t = match s with
  | Sc body -> replace img 0 body

(* our submodules want to refer to the type while maintaining their own copy of t *)
type term = t

(* prefixes help us maintain binding levels and whatnot *)
module Prefix = struct
  (* we have two different kinds of bindings *)
  type binder =
    | PAbs
    | PWild
  (* and bindings maintain a name with the correct domain and binder *)
  type binding = Name.t * Dtype.t * binder
  (* so a prefix maintains a stack of bindings *)
  type t = binding Stack.t
  (* infix binding applications/inverses *)
  let (@>) (b : binding) (tm : term) : term = match b with
    | (n, dom, bndr) -> match bndr with
      | PAbs -> Abs (dom, abstract n tm)
      | PWild -> Wild (dom, abstract n tm)
  let (<@) (n : Name.t) (tm : term) : (binding * term) option = match tm with
    | Abs (dom, body) -> Some ((n, dom, PAbs), instantiate (Free n) body)
    | Wild (dom, body) -> Some ((n, dom, PWild), instantiate (Free n) body)
    | _ -> None
  (* which we lift to binding over prefixes *)
  let rec bind (prefix : t) (tm : term) : term = match prefix with
    | Stack.Empty -> tm
    | Stack.Cons (b, ps) -> bind ps (b @> tm)
end
