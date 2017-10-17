open Sensitivity.Alt
open Name.Alt

(* we define extended modal types to support failure *)
module EModal = struct
  (* step 1: define lattice w/ glb to add top type *)
  type edtype =
    | Concrete of Dtype.t
    | Top
  (* the only operator we care about in the lattice: glb *)
  let glb (l : edtype) (r : edtype) : edtype = match l, r with
    | Concrete l', Concrete r' ->
      if l' = r' then l else failwith "types not equal"
    | Top, _ -> r
    | _, Top -> l
  (* lift modal type constructor to edtypes *)
  type t = S of Sensitivity.t * edtype
  (* with the appropriate arithmetic operations lifted from sensitivities *)
  let add (l : t) (r : t) : t = match l, r with
    | S (ls, ledt), S (rs, redt) ->
      S (ls +! rs, glb ledt redt )
  let mult (s : Sensitivity.t) (r : t) : t = match r with
    | S (s', edt) -> S (s *! s', edt)
  (* a destructor *)
  let to_sensitivity : t -> Sensitivity.t = function
    | S (s, _) -> s
  (* and a few constructors *)
  let empty = S (Sensitivity.Const (Rational.Q (0, 1)), Top)
  let of_dtype (dt : Dtype.t) : t = S (Sensitivity.Const (Rational.Q (1, 1)), Concrete dt)
  let of_name (n : Name.t) : t = S (Sensitivity.Free n, Top)
  (* alternative syntax, for later *)
  module Alt = struct
    let ( =? ) (l : edtype) (r : edtype) : edtype = glb l r
    let ( *? ) (s : Sensitivity.t) (r : t) : t = mult s r
    let ( +? ) (l : t) (r : t) : t = add l r
    let zero = empty
    let e (n : Name.t) : t = of_name n
  end
  (* sometimes we'll need to print these *)
  let rec to_string : t -> string = function
    | S (s, edt) -> match edt with
      | Concrete dt -> "![" ^ (Sensitivity.to_string s) ^ "] " ^ (Dtype.to_string dt)
      | Top -> "T"
end

(* contexts are represented symbolically as much as possible *)
type t =
  | Concrete of Name.t * EModal.t
  | Symbolic of Name.t
  | Plus of t * t
  | Times of Sensitivity.t * t
  | Empty

open EModal.Alt

let concrete_of_var (n : Name.t) (dt : Dtype.t) : t =
  Concrete (n, EModal.S (Sensitivity.Const (Rational.of_int 1), EModal.Concrete dt))

(* what names are actually bound in here? *)
let rec support : t -> Name.t list = function
  | Concrete (n, em) -> [n]
  | Symbolic _ -> []
  | Plus (l, r) -> (support l) @ (support r)
  | Times (_, context) -> support context
  | Empty -> []

(* given a name, we need the type from the context *)
let rec extract_type (n : Name.t) (c : t) : EModal.t = match c with
  | Concrete (n', em) -> if n = n' then em else zero
  | Symbolic n' -> e (n' ++ n)
  | Plus (l, r) -> (extract_type n l) +? (extract_type n r)
  | Times (s, c') -> s *? (extract_type n c')
  | Empty -> zero

(* sure, this is just a simple composition, but still *)
let extract_sensitivity (n : Name.t) (c : t) : Sensitivity.t = EModal.to_sensitivity (extract_type n c)

(* so that we can keep things straight *)
type relation = Eq of t * t

(* we care about the concrete support of a relation *)
let relation_support : relation -> Name.t list = function
  | Eq (l, r) -> (support l) @ (support r)

(* because we love that alternative syntax *)
module Alt = struct
  let (<$) (n : Name.t) (c : t) : Sensitivity.t = extract_sensitivity n c
  let vars : relation -> Name.t list = relation_support
  let (=$) (l : t) (r : t) : relation = Eq (l, r)
end

(* printing *)
let rec to_string : t -> string = function
  | Concrete (n, em) ->
    let n' = Name.to_string n in
    let em' = EModal.to_string em in
    "{" ^ n' ^ " : " ^ em' ^ "}"
  | Symbolic n -> Name.to_string n
  | Plus (l, r) ->
    let l' = to_string l in
    let r' = to_string r in
    l' ^ " + " ^ r'
  | Times (s, c) ->
    let s' = Sensitivity.to_string s in
    let c' = to_string c in
    s' ^ " * " ^ c'
  | Empty -> "{}"
