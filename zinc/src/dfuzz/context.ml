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
end

(* contexts are represented symbolically as much as possible *)
type t =
  | Concrete of Name.t * EModal.t
  | Symbolic of Name.t
  | Plus of t * t
  | Times of Sensitivity.t * t
  | Empty

open EModal.Alt

(* what names are actually bound in here? *)
let rec support : t -> Name.t list = function
  | Concrete (n, em) -> [n]
  | Symbolic _ -> []
  | Plus (l, r) -> (support l) @ (support r)
  | Times (_, context) -> support context
  | Empty -> []

let rec extract_type (n : Name.t) (c : t) : EModal.t = match c with
  | Concrete (n', em) -> if n = n' then em else zero
  | Symbolic n' -> e (n' ++ n)
  | Plus (l, r) -> (extract_type n l) +? (extract_type n r)
  | Times (s, c') -> s *? (extract_type n c')
  | Empty -> zero

let extract_sensitivity (n : Name.t) (c : t) : Sensitivity.t = EModal.to_sensitivity (extract_type n c)
