(* updated versions - we use applicative style to model our terms, with debruijn indices *)
type 'a t =
  | Abs of 'a t
  | App of 'a t * 'a t
  | Symbol of 'a
type 'a term = 'a t

(* our terms are instances of functors *)
let rec fmap (f : 'a -> 'b) : 'a t -> 'b t = function
  | Abs ts -> Abs (fmap f ts)
  | App (ls, rs) -> App (fmap f ls, fmap f rs)
  | Symbol s -> Symbol (f s)
let (<$>) = fmap

(* and sometimes we want to extract our symbols, but don't know if we've got a symbol *)
let extract : 'a term -> 'a option = function
  | Symbol x -> Some x
  | _ -> None

(* to support iteration, here's a zuper zipper *)
module Zipper = struct
  open CCOpt.Infix
  open CCFun
  (* zipper type is a little convoluted, so we sep branches and values *)
  type 'a branch =
    | A
    | L of 'a term
    | R of 'a term
  type 'a t = 'a term * 'a branch list
  (* some getters and setters for the current tree *)
  let get : 'a t -> 'a term = fst
  let set (v : 'a term) : 'a t -> 'a t =
    CCPair.map1 (fun c -> v)
  let of_term (v : 'a term) : 'a t = (v, [])
  let rec to_term (z : 'a t) : 'a term =
    let current = fst z in match snd z with
      | [] -> fst z
      | A :: bs -> to_term (Abs current, bs)
      | (L other) :: bs -> to_term (App (current, other), bs)
      | (R other) :: bs -> to_term (App (other, current), bs)
  (* and now we worry about movement *)
  let up (z : 'a t) : 'a t option =
    let current = fst z in match snd z with
      | [] -> None
      | A :: bs -> Some (Abs current, bs)
      | (L other) :: bs -> Some (App (current, other), bs)
      | (R other) :: bs -> Some (App (other, current), bs)
  let down_left (z : 'a t) : 'a t option = match z with
    | (p, zs) -> match p with
      | Abs ts -> Some (ts, A :: zs)
      | App (l, r) -> Some (l, (L r) :: zs)
      | Symbol s -> None
  let down_right (z : 'a t) : 'a t option = match z with
    | (p, zs) -> match p with
      | Abs ts -> Some (ts, A :: zs)
      | App (l, r) -> Some (r, (R l) :: zs)
      | Symbol s -> None
  let left (z : 'a t) : 'a t option = match z with
    | (p, (R other) :: zs) -> Some (other, (L p) :: zs)
    | _ -> None
  let right (z : 'a t) : 'a t option = match z with
    | (p, (L other) :: zs) -> Some (other, (R p) :: zs)
    | _ -> None
  (* and some more useful iteration tools *)
  let rec next (z : 'a t) : 'a t option = (right z) <+> (up z) >>= next
  let preorder (z : 'a t) : 'a t option = (down_left z) <+> (next z)
  let rec preorder_until (p : 'a term -> bool) (z : 'a t) : 'a t option =
    (CCOpt.if_ (p % get) z ) <+> (preorder z) >>= (preorder_until p)
end
