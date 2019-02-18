(* contexts maintain maps between type variables and sensitivities *)
(* they DO NOT maintain type requirements - synthesis and explicit annotations handle that *)

type t =
  | Concrete of Name.t * Sensitivity.t
  | Symbolic of Name.t
  | Linear of t * Sensitivity.t * t
  | Join of t * t
  | Substitution of t * Sensitivity.Sub.t
  | Empty

(* simplest construction creates concrete contexts from names (assuming usage 1) *)
let concrete_of_var (n : Name.t) : t = Concrete (n, Sensitivity.Const (Rational.of_int 1))

(* find names concretely bound *)
let rec support : t -> Name.t list = function
  | Concrete (x, _) -> [x]
  | Symbolic _ -> []
  | Linear (l, _, r) -> (support l) @ (support r)
  | Join (l, r) -> (support l) @ (support r)
  | Substitution (c, _) -> support c
  | Empty -> []

(* makes the following arithmetic easier *)
open Sensitivity.Alt

(* extract sensitivity expression for a given name *)
let rec extract_sensitivity (n : Name.t) (c : t) : Sensitivity.t = match c with
  | Concrete (x, s) -> if Name.eq x n then s else zero
  | Symbolic x -> Sensitivity.Free (Name.extend_by_name x n)
  | Linear (l, s, r) ->
    let l' = extract_sensitivity n l in
    let r' = extract_sensitivity n r in
      l' +! (s *! r')
  | Join (l, r) ->
    let l' = extract_sensitivity n l in
    let r' = extract_sensitivity n r in
      l' +! r'
  | Substitution (c, sub) ->
    let s = extract_sensitivity n c in
      Sensitivity.Sub.apply s sub
  | Empty -> zero

(* printing *)
let rec to_string : t -> string = function
  | Concrete (x, s) ->
    let x' = Name.to_string x in
    let s' = Sensitivity.to_string s in
      "{" ^ x' ^ " : " ^ s' ^ "}"
  | Symbolic x -> Name.to_string x
  | Linear (l, s, r) ->
    let l' = to_string l in
    let r' = to_string r in
    let s' = Sensitivity.to_string s in
      l' ^ " + " ^ s' ^ " * " ^ r'
  | Join (l, r) ->
    let l' = to_string l in
    let r' = to_string r in
      l' ^ " # " ^ r'
  | Substitution (c, sub) ->
    let c' = to_string c in
    let sub' = Sensitivity.Sub.to_string sub in
      c' ^ sub'
  | Empty -> "∅"

(* so that we can keep things straight *)
type context = t

module Relation = struct
  type t = 
    | Eq of context * context
    | LEq of context * context

  let to_string : t -> string = function
    | Eq (l, r) ->
      let l' = to_string l in
      let r' = to_string r in
        l' ^ " == " ^ r'
    | LEq (l, r) ->
      let l' = to_string l in
      let r' = to_string r in
        l' ^ " <= " ^ r'
  
  let support : t -> Name.t list = function
    | Eq (l, r) -> (support l) @ (support r)
    | LEq (l, r) -> (support l) @ (support r)
end

(* because we love that alternative syntax *)
module Alt = struct
  let (<$) (n : Name.t) (c : t) : Sensitivity.t = extract_sensitivity n c
  let vars : Relation.t -> Name.t list = Relation.support

  (* construction *)
  let ( ==. ) (l : t) (r : t) : Relation.t = Relation.Eq (l, r)
  let ( <=. ) (l : t) (r : t) : Relation.t = Relation.LEq (l, r)

  let ( +. ) (l : t) (r : t) : context = Linear (l, one, r)
  let ( *. ) (s : Sensitivity.t) (r : t) : context = Linear (Empty, s, r)
  (* let concrete (n : Name.t) (s : Sensitivity.t) (dt : Dtype.t) = Concrete (n, EModal.S (s, EModal.Concrete dt)) *)
end