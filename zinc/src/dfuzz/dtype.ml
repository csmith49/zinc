(* base type *)
type t =
  | Free of Name.t
  | Bound of int
  | Precise of precise
  | Quant of quantifier * kind * scope
  | Func of modal * t
  | Tensor of t * t
  | Base of base
and precise =
  | N of Sensitivity.t
  | M of Sensitivity.t * t
  | R of Sensitivity.t
and quantifier =
  | Exists
  | ForAll
and kind =
  | KSens
  | KType
and scope = Sc of t
and modal = Modal of Sensitivity.t * t
and base =
  | Real
  | Integer
  | Bool
  | String
  | Database

(* mcbride and mckinna *)
let rec name_to (n : Name.t) (db : int) (dt : t) : t = match dt with
  | Free n' -> if n = n' then (Bound db) else dt
  | Precise p -> begin match p with
      | N s -> Precise (N (Sensitivity.name_to n db s))
      | M (s, dt') -> Precise (M (Sensitivity.name_to n db s, name_to n db dt'))
      | R s -> Precise (R (Sensitivity.name_to n db s))
  end
  | Quant (q, k, Sc body) -> Quant (q, k, Sc (name_to n (db + 1) body))
  | Func (m, codom) -> begin match m with
      | Modal (s, dom) -> Func (Modal (Sensitivity.name_to n db s, name_to n db dom), name_to n db codom)
  end
  | Tensor (l, r) -> Tensor (name_to n db l, name_to n db r)
  | _ -> dt (* catches Base and Bound case *)

let abstract (n : Name.t) (dt : t) : scope = Sc (name_to n 0 dt)

let rec replace (img : t) (db : int) (dt : t) : t = match dt with
  | Bound i -> if i = db then img else dt
  | Precise p -> begin match p with
      | M (s, dt') -> Precise (M (s, replace img db dt'))
      | _ -> Precise p
    end
  | Quant (q, k, Sc body) -> Quant (q, k, Sc (replace img (db + 1) body))
  | Func (m, codom) -> begin match m with
      | Modal (s, dom) -> Func (Modal (s, replace img db dom), replace img db codom)
    end
  | Tensor (l, r) -> Tensor (replace img db l, replace img db r)
  | _ -> dt

let rec replace_sensitivity (img : Sensitivity.t) (db : int) (dt : t) : t = match dt with
  | Precise p -> begin match p with
      | N s -> Precise (N (Sensitivity.replace img db s))
      | M (s, dt') -> Precise (M (Sensitivity.replace img db s, dt'))
      | R s -> Precise (R (Sensitivity.replace img db s))
    end
  | Quant (q, k, Sc body) -> Quant (q, k, Sc (replace_sensitivity img (db + 1) body))
  | Func (m, codom) -> begin match m with
      | Modal (s, dom) ->
        let dom' = replace_sensitivity img db dom in
        let codom' = replace_sensitivity img db codom in
        Func (Modal (Sensitivity.replace img db s, dom'), codom')
    end
  | Tensor (l, r) -> Tensor (replace_sensitivity img db l, replace_sensitivity img db r)
  | _ -> dt

let instantiate (img : t) (s : scope) : t = match s with
  | Sc body -> replace img 0 body

let instantiate_sensitivity (img : Sensitivity.t) (s : scope) : t = match s with
  | Sc body -> replace_sensitivity img 0 body

(* submodules will want to refer to this type *)
type dtype = t

(* prefixs help us maintain binding levels and whatnot *)
module Prefix = struct
  (* bindings maintain all the information necessary to reconstruct the binder *)
  type binding = Name.t * quantifier * kind
  (* so a prefix maintains a stack of bindings *)
  type t = binding Stack.t
  (* infix binding applications/inverses *)
  let (@>) (b : binding) (dt : dtype) : dtype = match b with
    | (n, q, k) -> Quant (q, k, abstract n dt)
  let (<@) (n : Name.t) (dt : dtype) : (binding * dtype) option = match dt with
    | Quant (q, k, body) -> begin match k with
        | KSens -> Some ((n, q, k), instantiate_sensitivity (Sensitivity.Free n) body)
        | KType -> Some ((n, q, k), instantiate (Free n) body)
      end
    | _ -> None
  (* which we'll lift to binding over prefixes *)
  let rec bind (prefix : t) (dt : dtype) : dtype = match prefix with
    | Stack.Empty -> dt
    | Stack.Cons (b, ps) -> bind ps (b @> dt)
end

(* the real reason we care about alternative syntax is to make it easier to write these types in the benchmarks *)
module Alt = struct
  (* non-sensitive function application *)
  let (=>) (d : t) (cd : t) : t =
    Func (Modal (Sensitivity.Const Rational.Infinity, d), cd)

  (* regular function application *)
  let (-*) (m : modal) (cd : t) : t = Func (m, cd)

  (* tensor construction *)
  let ( * ) (l : t) (r : t) : t = Tensor (l, r)

  (* some simple base type constructors *)
  let real : t = Base (Real)
  let nat : t = Base (Integer)

  (* sensitivity constructor *)
  let si (p : (int * t)) : modal = match p with
    | (i, dt) -> Modal (Sensitivity.Const (Rational.of_int i), dt)
  let s (p : (Sensitivity.t * t)) : modal = match p with
    | (s, dt) -> Modal (s, dt)

  (* we have a default sens variable *)
  let k : Sensitivity.t = Sensitivity.Free (Name.of_string "k")
  (* and a default constructor for binding them *)
  let exists (p : Sensitivity.t * t) : t = match p with
    | (Sensitivity.Free name, dt) -> Quant (Exists, KSens, abstract name dt)
    | _ -> failwith "can't bind over a non-variable"
end
