type t =
  | Free of Name.t
  | Bound of int
  | App of t * t
  | Abs of Dtype.t * scope
  | Const of Value.t
  | Prim of string * Value.t
  | Wild of Name.t * Dtype.t * scope
and scope = Sc of t

(* mcbride and mckinna abstraction and instantiation *)
let rec name_to (n : Name.t) (db : int) (tm : t) : t = match tm with
  | Free n' -> if n = n' then (Bound db) else tm
  | App (f, args) -> App (name_to n db f, name_to n db args)
  | Abs (dom, Sc body) -> Abs (dom, Sc (name_to n (db + 1) body))
  | Wild (context, dom, Sc body) -> Wild (context, dom, Sc (name_to n (db + 1) body))
  | _ -> tm

let abstract (n : Name.t) (tm : t) : scope = Sc (name_to n 0 tm)

let rec replace (img : t) (db : int) (body : t) : t = match body with
  | Bound i -> if i = db then img else body
  | App (f, args) -> App (replace img db f, replace img db args)
  | Abs (dom, Sc body) -> Abs (dom, Sc (replace img (db + 1) body))
  | Wild (context, dom, Sc body) -> Wild (context, dom, Sc (replace img (db + 1) body))
  | _ -> body

let instantiate (img : t) (s : scope) : t = match s with
  | Sc body -> replace img 0 body

(* our submodules want to refer to the type while maintaining their own copy of t *)
type term = t

(* prefixes help us maintain binding levels and whatnot *)
module Prefix = struct
  open Stack.Alt
  (* we have two different kinds of bindings *)
  (* and bindings maintain a name with the correct domain and binder *)
  type binding =
    | PAbs of Name.t * Dtype.t
    | PWild of Name.t * Name.t * Dtype.t
  (* so a prefix maintains a stack of bindings *)
  type t = binding Stack.t
  (* infix binding applications/inverses *)
  module Alt = struct
    let (@>) (b : binding) (tm : term) : term = match b with
      | PAbs (n, dom) -> Abs (dom, abstract n tm)
      | PWild (context, n, dom) -> Wild (context, dom, abstract n tm)
    let (<@) (n : Name.t) (tm : term) : (binding * term) option = match tm with
      | Abs (dom, body) -> Some (PAbs (n, dom), instantiate (Free n) body)
      | Wild (context, dom, body) -> Some (PWild (context, n, dom), instantiate (Free n) body)
      | _ -> None
  end
  open Alt
  (* which we lift to binding over prefixes *)
  let rec bind (prefix : t) (tm : term) : term = match prefix with
    | Stack.Empty -> tm
    | Stack.Cons (b, ps) -> bind ps (b @> tm)
  let rec unbind (root : Name.t) (var : string) (tm : term) : (t * term) =
    unbind' root var 1 (Stack.Empty, tm)
  and unbind' (root : Name.t) (var : string) (index : int) (p : t * term) : (t * term) = match p with
    | (prefix, tm) -> match (root <+ Name.Id (var, index)) <@ tm with
      | Some (b, tm') -> unbind' root var (index + 1) (prefix <+ b, tm')
      | None -> (prefix, tm)
end

(* we use Huet style zippers for unfolding/refolding terms as we expand *)
module Zipper = struct
  open Stack.Alt
  open Prefix.Alt
  (* our type is somehow a stack of derivatives of terms *)
  type branch =
    | BAppLeft of term
    | BAppRight of term
    | BBind
  (* we maintain a current view, the bindings needed to reconstruct the term, and the branches *)
  type t = term * Prefix.t * branch Stack.t
  (* basic navigation *)
  let up : t -> t option = function
    | (tm, prefix, Stack.Cons (branch, branches)) -> begin match branch with
        | BAppLeft tm' -> Some (App (tm, tm'), prefix, branches)
        | BAppRight tm' -> Some (App (tm', tm), prefix, branches)
        | BBind -> begin match prefix with
            | Stack.Cons (binding, bindings) -> Some (binding @> tm, bindings, branches)
            | _ -> None
          end
      end
    | _ -> None
  let right : t -> t option = function
    | (tm, prefix, Stack.Cons (BAppLeft tm', branches)) -> Some (tm', prefix, branches <+ (BAppRight tm))
    | _ -> None
  (* because we're going down past binders, we'll have to name variables *)
  let down (root : Name.t) (var : string) (z : t) : t option = match z with
    | (tm, prefix, branches) -> let n = root <+ Name.Id (var, Stack.size prefix) in match tm with
      | App (l, r) -> Some (l, prefix, branches <+ (BAppLeft r))
      | Abs _ | Wild _ -> begin match n <@ tm with
          | Some (binding, body) -> Some (body, prefix <+ binding, branches <+ BBind)
          | None -> None
        end
      | _ -> None
  (* simple getter/setters *)
  let get : t -> term = function
    | (tm, _, _) -> tm
  let set (tm : term) : t -> t = function
    | (_, prefix, branches) -> (tm, prefix, branches)
  (* more advanced iteration *)
  open CCOpt.Infix
  open CCFun
  (* we encode preorder traversal *)
  let rec next (z : t) : t option = (right z) <+> (up z) >>= next
  let preorder (root : Name.t) (var : string) (z : t) : t option = (down root var z) <+> (next z)
  let rec preorder_until (root : Name.t) (var : string) (predicate : term -> bool) (z : t) : t option =
    (CCOpt.if_ (predicate % get) z) <+> (preorder root var z) >>= (preorder_until root var predicate)
  (* convert to/from terms *)
  let of_term : term -> t = fun tm -> (tm, Stack.Empty, Stack.Empty)
  let rec to_term : t -> term = fun z -> match up z with
    | Some z' -> to_term z'
    | _ -> get z
end
