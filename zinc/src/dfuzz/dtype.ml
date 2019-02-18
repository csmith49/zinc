(* base type *)
type t =
  | Free of Name.t
  | Bound of int
  | Precise of precise
  | Quant of quantifier * kind * scope
  | Func of modal * t
  | Tensor of t * t
  | Base of base
  | Bounded of bounded
  | Monad of t
and precise =
  | Natural of Sensitivity.t
  | Real of Sensitivity.t
  | List of Sensitivity.t * t
and quantifier =
  | Exists
  | ForAll
and kind =
  | KSens
  | KType
and scope = Sc of t
and modal = Modal of Sensitivity.t * t
and base = string
and bounded =
  | Interval of Sensitivity.t
  | MSet of Sensitivity.t * t

(* mcbride and mckinna *)
let rec abstract (n : Name.t) (dt : t) : scope = Sc (abstract' n 0 dt)
and abstract' (n : Name.t) (db : int) (dt : t) : t = match dt with
  | Free n' -> if n = n' then (Bound db) else dt
  | Precise p -> begin match p with
      | Natural s -> Precise (Natural (Sensitivity.abstract' n db s))
      | List (s, dt') -> Precise (List (Sensitivity.abstract' n db s, abstract' n db dt'))
      | Real s -> Precise (Real (Sensitivity.abstract' n db s))
    end
  | Quant (q, k, Sc body) -> Quant (q, k, Sc (abstract' n (db + 1) body))
  | Func (m, codom) -> begin match m with
      | Modal (s, dom) -> Func (Modal (Sensitivity.abstract' n db s, abstract' n db dom), abstract' n db codom)
    end
  | Tensor (l, r) -> Tensor (abstract' n db l, abstract' n db r)
  | Bounded b -> begin match b with
      | Interval s -> Bounded (Interval (Sensitivity.abstract' n db s))
      | MSet (s, dt') -> Bounded (MSet (Sensitivity.abstract' n db s, abstract' n db dt'))
    end
  | Monad dt -> Monad (abstract' n db dt)
  | _ -> dt (* catches Base and Bound case *)

let rec instantiate (img : t) (s : scope) : t = match s with
  | Sc body -> instantiate' img 0 body
and instantiate' (img : t) (db : int) (dt : t) : t = match dt with
  | Bound i -> if i = db then img else dt
  | Precise p -> begin match p with
      | List (s, dt') -> Precise (List (s, instantiate' img db dt'))
      | _ -> Precise p
    end
  | Bounded p -> begin match p with
      | MSet (s, dt') -> Bounded (MSet (s, instantiate' img db dt'))
      | _ -> Bounded p
    end
  | Quant (q, k, Sc body) -> Quant (q, k, Sc (instantiate' img (db + 1) body))
  | Func (m, codom) -> begin match m with
      | Modal (s, dom) -> Func (Modal (s, instantiate' img db dom), instantiate' img db codom)
    end
  | Tensor (l, r) -> Tensor (instantiate' img db l, instantiate' img db r)
  | Monad dt -> Monad (instantiate' img db dt)
  | _ -> dt

let rec instantiate_sens (img : Sensitivity.t) (s : scope) : t = match s with
  | Sc body -> instantiate_sens' img 0 body
and instantiate_sens' (img : Sensitivity.t) (db : int) (dt : t) : t = match dt with
  | Precise p -> begin match p with
      | Natural s -> Precise (Natural (Sensitivity.instantiate' img db s))
      | List (s, dt') -> Precise (List (Sensitivity.instantiate' img db s, instantiate_sens' img db dt'))
      | Real s -> Precise (Real (Sensitivity.instantiate' img db s))
    end
  | Quant (q, k, Sc body) -> Quant (q, k, Sc (instantiate_sens' img (db + 1) body))
  | Func (m, codom) -> begin match m with
      | Modal (s, dom) ->
        let dom' = instantiate_sens' img db dom in
        let codom' = instantiate_sens' img db codom in
        Func (Modal (Sensitivity.instantiate' img db s, dom'), codom')
    end
  | Tensor (l, r) -> Tensor (instantiate_sens' img db l, instantiate_sens' img db r)
  | Bounded b -> begin match b with
      | Interval s -> Bounded (Interval (Sensitivity.instantiate' img db s))
      | MSet (s, dt') -> Bounded (MSet (Sensitivity.instantiate' img db s, instantiate_sens' img db dt'))
    end
  | Monad dt -> Monad (instantiate_sens' img db dt)
  | _ -> dt

(* submodules will want to refer to this type *)
type dtype = t

(* prefixs help us maintain binding levels and whatnot *)
module Prefix = struct
  (* bindings maintain all the information necessary to reconstruct the binder *)
  type binding = Name.t * quantifier * kind
  (* so a prefix maintains a Rlist of bindings *)
  type t = binding list
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
    | [] -> dt
    | b :: ps -> bind ps (b @> dt)
end

(* printing - this is a real piece of work *)
let rec to_string : t -> string =
  fun dt ->
    let stream = Name.Stream.of_root (Name.of_string "") in
    fst (to_string' dt stream)
and to_string' (dt : t) (s : Name.Stream.t) : string * Name.Stream.t = match dt with
  | Free n -> (Name.to_string n, s)
  | Bound i -> (string_of_int i, s)
  | Precise p -> begin match p with
      | Natural sens ->
        let sens' = Sensitivity.to_string sens in
        ("N[" ^ sens' ^ "]", s)
      | List (sens, dt') ->
        let sens' = Sensitivity.to_string sens in
        let dt'', s' = to_string' dt' s in
        ("L(" ^ dt'' ^ ")[" ^ sens' ^ "]", s')
      | Real sens ->
        let sens' = Sensitivity.to_string sens in
        ("R[" ^ sens' ^ "]", s)
    end
  | Quant (q, kind, body) -> begin match kind with
      | KSens ->
        let k, s' = Name.Stream.draw_sens s in
        let body', s'' = to_string' (instantiate_sens (Sensitivity.Free k) body) s' in
        ((quantifier_to_string q) ^ (Name.to_string k) ^ "." ^ body', s'')
      | KType ->
        let a, s' = Name.Stream.draw_dt s in
        let body', s'' = to_string' (instantiate (Free a) body) s' in
        ((quantifier_to_string q) ^ (Name.to_string a) ^ "." ^ body', s'')
    end
  | Func (m, codom) -> begin match m with
      | Modal (sens, dom) ->
        let sens' = Sensitivity.to_string sens in
        let dom', s' = to_string' dom s in
        let codom', s'' = to_string' codom s' in
        ("(" ^ dom' ^ " -o[" ^ sens' ^ "] " ^ codom' ^ ")", s'')
    end
  | Tensor (l, r) ->
    let l', s' = to_string' l s in
    let r', s'' = to_string' r s' in
    ("(" ^ l' ^ ", " ^ r' ^ ")", s'')
  | Base b -> (b, s) 
  | Bounded b -> begin match b with
      | Interval b -> ("Interval[" ^ (Sensitivity.to_string b) ^ "]", s)
      | MSet (sens, dt') ->
        let sens' = Sensitivity.to_string sens in
        let dt'', s' = to_string' dt' s in
        ("MSet(" ^ dt'' ^ ")[" ^ sens' ^ "]", s')
    end
  | Monad p ->
    let p', s' = to_string' p s in
    ("O " ^ p', s')

and quantifier_to_string : quantifier -> string = function
  | Exists -> "∃"
  | ForAll -> "∀"

(* utility functions for various purposes *)
let rec free_vars : t -> Name.t list = function
  | Free n -> [n]
  | Precise p -> begin match p with
      | Natural s -> Sensitivity.free_vars s
      | List (s, dt) -> (free_vars dt) @ (Sensitivity.free_vars s)
      | Real s -> Sensitivity.free_vars s
    end
  | Quant (_, _, Sc body) -> free_vars body
  | Func (Modal (s, d), c) -> (free_vars d) @ (free_vars c) @ (Sensitivity.free_vars s)
  | Tensor (l, r) -> (free_vars l) @ (free_vars r)
  | Bounded b -> begin match b with
      | Interval s -> Sensitivity.free_vars s
      | MSet (s, dt) -> (free_vars dt) @ (Sensitivity.free_vars s)
    end
  | Monad p -> free_vars p
  | _ -> []

(* the real reason we care about alternative syntax is to make it easier to write these types in the benchmarks *)
module Alt = struct
  (* non-sensitive function application *)
  let (=>) (d : t) (cd : t) : t =
    Func (Modal (Sensitivity.Const Rational.Infinity, d), cd)

  (* regular function application *)
  let (-*) (m : modal) (cd : t) : t = Func (m, cd)

  (* tensor construction *)
  let ( * ) (l : t) (r : t) : t = Tensor (l, r)

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
