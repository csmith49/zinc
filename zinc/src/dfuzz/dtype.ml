type t = Dtype

(* we want to be able to say what kind of variables we're storing and where *)
module P = struct
  type dtype = DType
  type size = Size
  type sens = Sens
end

type _ expr =
  | Free : 'a * Name.t -> 'a expr
  | Bound : 'a * int -> 'a expr
  (* size constructors *)
  | Zero : P.size expr
  | Succ : P.size expr -> P.size expr
  (* sensitivity constructors *)
  | Const : Rational.t -> P.sens expr
  | Size : P.size expr -> P.sens expr
  | Plus : P.sens expr * P.sens expr -> P.sens expr
  | Mult : P.sens expr * P.sens expr -> P.sens expr
  (* dtype constructors *)
  | Precise : precise -> P.dtype expr
  | Quant : quantifier * scope -> P.dtype expr
  | Tensor : P.dtype expr * P.dtype expr -> P.dtype expr
  | Base : base -> P.dtype expr
and precise =
  | N : P.size expr -> precise
  | M : P.size expr * P.dtype expr -> precise
  | R : P.sens expr -> precise
and base =
  | Real
  | Int
  | Bool
  | Str
  | DB
and scope =
  | Sc : P.dtype expr -> scope
and quantifier =
  | Exists
  | ForAll

(* mcbride and mckinna abstraction/instantiation *)
let rec name_to : type a. Name.t -> int -> a expr -> a expr =
  fun n db e -> match e with
    (* variables *)
    | Free (p, n') -> if n == n' then (Bound (p, db)) else e
    | Bound _ -> e
    (* size *)
    | Zero -> e
    | Succ s -> Succ (name_to n db s)
    (* sensitivity *)
    | Const _ -> e
    | Size s -> Size (name_to n db s)
    | Plus (l, r) -> Plus (name_to n db l, name_to n db r)
    | Mult (l, r) -> Mult (name_to n db l, name_to n db r)
    (* dtype *)
    | Precise p -> begin match p with
        | N s -> Precise (N (name_to n db s))
        | M (s, dt) -> Precise (M (name_to n db s, name_to n db dt))
        | R s -> Precise (R (name_to n db s))
      end
    | Quant (q, body) -> begin match body with
        | Sc dt -> Quant (q, Sc (name_to n (db + 1) dt))
      end
    | Tensor (l, r) -> Tensor (name_to n db l, name_to n db r)
    | Base _ -> e

let rec replace_size : type a. P.size expr -> int -> a expr -> a expr =
  fun img db e -> match e with
    | Bound (P.Size, i) -> if i == db then img else body
    | Succ s -> Succ (replace_size img db s)
    | Plus (l, r) -> Plus (replace_size img db l, replace_size img db r)
    | Mult (l, r) -> Mult (replace_size img db l, replace_size img db r)
    | Precise p -> begin match p with
        | N s -> Precise (N (replace_size img db s))
        | M (s, dt) -> Precise (M (replace_size img db s, replace_size img db dt))
        | R s -> Precise (R (replace_size img db s))
      end
    | Quant (q, body) -> begin match body with
        | Sc dt -> Quant (q, Sc (replace_size img (db + 1) dt))
      end
    | Tensor (l, r) -> Tensor (replace_size img db l, replace_size img db r)
    | _ -> e




(*
let rec replace : type a b. a expr -> int -> b expr -> eexpr =
  fun img db e -> match e with
    (* variables *)
    | Free _ -> E e
    | Bound i -> if i == db then E img else E e
    (* size *)
    | Zero -> E e
    | Succ s -> E (Succ (replace img db e))
    (* sensitivity *)
    | Const _ -> E e
    | Size s -> E (Size (replace img db e))
    | Plus (l, r) -> E (Plus (replace img db l, replace img db r))
    | Mult (l, r) -> E (Mult (replace img db l, replace img db r))
    (* dtype *)
    | Precise p -> begin match p with
        | N s -> E (Precise (N (replace img db s)))
        | M (s, dt) -> E (Precise (M (replace img db s, replace img db dt)))
        | R s -> E (Precise (R (replace img db s)))
      end
    | Quant (q, body) -> begin match body with
        | Sc dt -> E (Quant( q, Sc (replace img (db + 1) dt)))
      end
    | Tensor (l, r) -> E (Tensor (replace img db l, replace img db r))
    | Base _ -> E (e) *)
