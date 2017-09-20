module Quantifier = struct
  type t =
    | Exists
    | ForAll
  let to_string : t -> string = function
    | Exists -> "Exists"
    | ForAll -> "ForAll"
end

module Base = struct
  type t =
    | Real
    | Integer
    | Bool
    | String
    | Database
  let to_string : t -> string = function
    | Real -> "Real"
    | Integer -> "Integer"
    | Bool -> "Bool"
    | String -> "String"
    | Database -> "DB"
end

type t =
  | Var of Variable.t
  | Base of Base.t
  | Precise of precise
  | Quant of Quantifier.t * scope
  | Func of modal * t
  | Tensor of t * t
  | Bind of scope
and precise =
  | N of Size.t
  | M of Size.t * t
  | R of Sensitivity.t
and scope = Scope of t
and modal = Modal of Sensitivity.t * t

(* mcbride and mckinna name conversion through abstraction and instantiation *)
let rec name_to_db (name : Name.t) (depth : int) (dt : t) : t = match dt with
  | Bind (Scope dt') -> Bind (Scope (name_to_db name (depth + 1) dt'))
  | Tensor (l, r) -> Tensor (name_to_db name depth l, name_to_db name depth r)
  | Func (m, dt') -> Func (modal_name_to_db name depth m, name_to_db name depth dt')
  | Quant (q, Scope dt') -> Quant (q, Scope (name_to_db name (depth + 1) dt'))
  | Precise p -> Precise (precise_name_to_db name depth p)
  | Base _ -> dt
  | Var v -> Var (Variable.name_to_db name depth v)
and precise_name_to_db (name : Name.t) (depth : int) (p : precise) : precise = match p with
  | N s -> N (Size.name_to_db name depth s)
  | M (s, dt) -> M (Size.name_to_db name depth s, name_to_db name depth dt)
  | R s -> R (Sensitivity.name_to_db name depth s)
and modal_name_to_db (name : Name.t) (depth : int) (m : modal) : modal = match m with
  | Modal (s, dt) -> Modal (Sensitivity.name_to_db name depth s, name_to_db name depth dt)

let rec db_to_name (depth : int) (name : Name.t) (dt : t) : t = match dt with
  | Bind (Scope dt') -> Bind (Scope (db_to_name (depth + 1) name dt'))
  | Tensor (l, r) -> Tensor (db_to_name depth name l, db_to_name depth name r)
  | Func (m, dt') -> Func (modal_db_to_name depth name m, db_to_name depth name dt')
  | Quant (q, Scope dt') -> Quant (q, Scope (db_to_name (depth + 1) name dt'))
  | Precise p -> Precise (precise_db_to_name depth name p)
  | Base _ -> dt
  | Var v -> Var (Variable.db_to_name depth name v)
and precise_db_to_name (depth : int) (name : Name.t) (p : precise) : precise = match p with
  | N s -> N (Size.db_to_name depth name s)
  | M (s, dt) -> M (Size.db_to_name depth name s, db_to_name depth name dt)
  | R s -> R (Sensitivity.db_to_name depth name s)
and modal_db_to_name (depth : int) (name : Name.t) (m : modal) : modal = match m with
  | Modal (s, dt) -> Modal (Sensitivity.db_to_name depth name s, db_to_name depth name dt)

(* note that instantiation here is weaker - all we can do is rename a variable, not replace it arbitrarily *)
let abstract (name : Name.t) (dt : t) : scope = Scope (name_to_db name 0 dt)
let instantiate (name : Name.t) (s : scope) : t = match s with
    Scope dt -> db_to_name 0 name dt

(* let's be honest, the above is not easy to deal with when making new types *)
(* if we define enough extra functions (including infix), maybe we can cobble together a reasonable meta language *)
(* it'd be really nice to have modular implicits here, so that we could overload constructors based on strings vs ints *)
module Make = struct
  (* non-sensitive function application *)
  let (=>) (d : t) (cd : t) : t =
    Func (Modal (Sensitivity.Const Rational.Infinity, d), cd)

  (* regular function application *)
  let (-*) (m : modal) (cd : t) : t = Func (m, cd)

  (* tensor construction *)
  let ( * ) (l : t) (r : t) : t = Tensor (l, r)

  (* existential quantifier *)
  let exists : (string * t) -> t = function
    (s, dt) -> Quant (Quantifier.Exists, abstract (Name.of_string s) dt)

  (* universial quantifier *)
  let forall : (string * t) -> t = function
    (s, dt) -> Quant (Quantifier.ForAll, abstract (Name.of_string s) dt)
end
