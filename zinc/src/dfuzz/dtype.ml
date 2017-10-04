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
let rec abstract (n : Name.t) (dt : t) : scope = Sc (abstract' n 0 dt)
and abstract' (n : Name.t) (db : int) (dt : t) : t = match dt with
  | Free n' -> if n = n' then (Bound db) else dt
  | Precise p -> begin match p with
      | N s -> Precise (N (Sensitivity.abstract' n db s))
      | M (s, dt') -> Precise (M (Sensitivity.abstract' n db s, abstract' n db dt'))
      | R s -> Precise (R (Sensitivity.abstract' n db s))
    end
  | Quant (q, k, Sc body) -> Quant (q, k, Sc (abstract' n (db + 1) body))
  | Func (m, codom) -> begin match m with
      | Modal (s, dom) -> Func (Modal (Sensitivity.abstract' n db s, abstract' n db dom), abstract' n db codom)
    end
  | Tensor (l, r) -> Tensor (abstract' n db l, abstract' n db r)
  | _ -> dt (* catches Base and Bound case *)

let rec instantiate (img : t) (s : scope) : t = match s with
  | Sc body -> instantiate' img 0 body
and instantiate' (img : t) (db : int) (dt : t) : t = match dt with
  | Bound i -> if i = db then img else dt
  | Precise p -> begin match p with
      | M (s, dt') -> Precise (M (s, instantiate' img db dt'))
      | _ -> Precise p
    end
  | Quant (q, k, Sc body) -> Quant (q, k, Sc (instantiate' img (db + 1) body))
  | Func (m, codom) -> begin match m with
      | Modal (s, dom) -> Func (Modal (s, instantiate' img db dom), instantiate' img db codom)
    end
  | Tensor (l, r) -> Tensor (instantiate' img db l, instantiate' img db r)
  | _ -> dt

let rec instantiate_sens (img : Sensitivity.t) (s : scope) : t = match s with
  | Sc body -> instantiate_sens' img 0 body
and instantiate_sens' (img : Sensitivity.t) (db : int) (dt : t) : t = match dt with
  | Precise p -> begin match p with
      | N s -> Precise (N (Sensitivity.instantiate' img db s))
      | M (s, dt') -> Precise (M (Sensitivity.instantiate' img db s, dt'))
      | R s -> Precise (R (Sensitivity.instantiate' img db s))
    end
  | Quant (q, k, Sc body) -> Quant (q, k, Sc (instantiate_sens' img (db + 1) body))
  | Func (m, codom) -> begin match m with
      | Modal (s, dom) ->
        let dom' = instantiate_sens' img db dom in
        let codom' = instantiate_sens' img db codom in
        Func (Modal (Sensitivity.instantiate' img db s, dom'), codom')
    end
  | Tensor (l, r) -> Tensor (instantiate_sens' img db l, instantiate_sens' img db r)
  | _ -> dt


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
        | KSens -> Some ((n, q, k), instantiate_sens (Sensitivity.Free n) body)
        | KType -> Some ((n, q, k), instantiate (Free n) body)
      end
    | _ -> None
  (* which we'll lift to binding over prefixes *)
  let rec bind (prefix : t) (dt : dtype) : dtype = match prefix with
    | Stack.Empty -> dt
    | Stack.Cons (b, ps) -> bind ps (b @> dt)
end

(* printing - this is a real piece of work *)
let rec to_string : t -> string = fun dt -> to_string' Name.Cycle.dtype Name.Cycle.sensitivity dt
and to_string' (dts : Name.Cycle.t) (sens : Name.Cycle.t) : t -> string = function
  | Free n -> Name.to_string n
  | Bound i -> string_of_int i
  | Precise p -> begin match p with
      | N s ->
        let s' = Sensitivity.to_string s in
        "N[" ^ s' ^ "]"
      | M (s, dt) ->
        let s' = Sensitivity.to_string s in
        let dt' = to_string' dts sens dt in
        "M[" ^ s' ^ ", " ^ dt' ^ "]"
      | R s ->
        let s' = Sensitivity.to_string s in
        "R[" ^ s' ^ "]"
    end
  | Quant (q, k, body) -> begin match k with
      | KSens ->
        let s = Name.Cycle.current sens in
        let sens' = Name.Cycle.rotate sens in
        let body' = instantiate_sens (Sensitivity.Free s) body in
        let q' = quantifier_to_string q in
        q' ^ (Name.to_string s) ^ "." ^ (to_string' dts sens' body')
      | KType ->
        let dt = Name.Cycle.current dts in
        let dts' = Name.Cycle.rotate dts in
        let body' = instantiate (Free dt) body in
        let q' = quantifier_to_string q in
        q' ^ (Name.to_string dt) ^ "." ^ (to_string' dts' sens body')
    end
  | Func (m, dt) -> begin match m with
      | Modal (s, dt') ->
        let dom = to_string' dts sens dt' in
        let codom = to_string' dts sens dt in
        let s' = Sensitivity.to_string s in
        dom ^ " -*[" ^ s' ^ "] " ^ codom
    end
  | Tensor (l, r) ->
    let l' = to_string' dts sens l in
    let r' = to_string' dts sens r in
    "(" ^ l' ^ ", " ^ r' ^ ")"
  | Base b -> begin match b with
      | Real -> "R"
      | Integer -> "N"
      | Bool -> "Bool"
      | String -> "Str"
      | Database -> "DB"
    end
and quantifier_to_string : quantifier -> string = function
  | Exists -> "E"
  | ForAll -> "A"

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
