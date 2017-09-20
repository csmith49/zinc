(* useful syntax and types *)
open Stack
open Stack.Infix

(* we have 3 different ways of binding variables in our types *)
type binder =
  | Bind
  | Exists
  | ForAll

(* so a binding just maintains which quantifier and the name *)
type binding = (Name.t * binder)

(* and a prefix is a stack of bindings *)
type t = binding Stack.t

(* we have useful binding constructors and possible destructors *)
let abstract (b : binding) (dt : Dtype.t) : Dtype.t = match b with
  | (n, b') -> match b' with
    | Bind -> Dtype.Bind (Dtype.abstract n dt)
    | Exists -> Dtype.Quant (Dtype.Quantifier.Exists, Dtype.abstract n dt)
    | ForAll -> Dtype.Quant (Dtype.Quantifier.ForAll, Dtype.abstract n dt)

let instantiate (n : Name.t) (dt : Dtype.t) : (binding * Dtype.t) option = match dt with
  | Dtype.Quant (Dtype.Quantifier.Exists, s) -> Some ((n, Exists), Dtype.instantiate n s)
  | Dtype.Quant (Dtype.Quantifier.ForAll, s) -> Some ((n, ForAll), Dtype.instantiate n s)
  | Dtype.Bind s -> Some ((n, Bind), Dtype.instantiate n s)
  | _ -> None

(* some prefix versions of the very same *)
let (@>) (b : binding) (dt : Dtype.t) : Dtype.t = abstract b dt
let (<@) (n : Name.t) (dt : Dtype.t) : (binding * Dtype.t) option = instantiate n dt

(* now the prefix-level manipulation *)
let rec bind (prefix : t) (dt : Dtype.t) : Dtype.t = match prefix with
  | Empty -> dt
  | Cons (b, ps) -> bind ps (b @> dt)

let rec unbind (root : Name.t) (var : string) (dt : Dtype.t) : (t * Dtype.t) =
  unbind' root var 1 (Empty, dt)
and unbind' (root : Name.t) (var : string) (i : int) (pair : t * Dtype.t) : t * Dtype.t = match pair with
  | (prefix, dt) -> let n = (root <+ Name.Id (var, i)) in match n <@ dt with
    | Some (b, dt') -> unbind' root var (i + 1) (prefix <+ b, dt')
    | None -> (prefix, dt)
