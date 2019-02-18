open Context.Alt

module Relation = struct
  type t = 
    | S of Sensitivity.Relation.t
    | C of Context.Relation.t
  
  let to_string : t -> string = function
    | S sr -> Sensitivity.Relation.to_string sr
    | C cr -> Context.Relation.to_string cr
end

type t =
  | Unsat
  | Top
  | Conjunction of Relation.t * t

let rec join (l : t) (r : t) : t option = match l, r with
  | Unsat, _ | _, Unsat -> None
  | Top, (_ as c) -> Some c
  | (_ as c), Top -> Some c
  | Conjunction (l, ls), Conjunction (r, rs) -> match join ls rs with
    | Some cs -> Some (Conjunction (l, Conjunction (r, cs)))
    | _ -> None

let is_unsat : t -> bool = function
  | Unsat -> true
  | _ -> false

let rec to_string : t -> string = function
  | Unsat -> "⊥"
  | Top -> "⊤"
  | Conjunction (r, Top) -> Relation.to_string r
  | Conjunction (r, rs) -> (Relation.to_string r) ^ " ∧ " ^ (to_string rs)

(* alternative construction syntax *)
module Alt = struct
  let top : t =  Top
  let (==) (s : Sensitivity.t) (s' : Sensitivity.t) : t = 
    Conjunction (Relation.S (Sensitivity.Relation.Eq (s, s')), top)
  let (<=) (s : Sensitivity.t) (s' : Sensitivity.t) : t =
    Conjunction (Relation.S (Sensitivity.Relation.LEq (s, s')), top)
  let rec (&) (l : t) (r : t) : t = match l, r with
    | Top, (_ as r) -> r
    | (_ as r), Top -> r
    | Conjunction (l, ls), (Conjunction _ as right) -> Conjunction (l, ls & right)
    | _ -> Unsat
  let unsat : t = Unsat
  let num : int -> Sensitivity.t = fun n -> Sensitivity.Const (Rational.of_int n)

  let c_rel : Context.Relation.t -> t = fun cr -> Conjunction (Relation.C cr, Top)
  let s_rel : Sensitivity.Relation.t -> t = fun sr -> Conjunction (Relation.S sr, Top)
end

(* simple to-list conversion *)
let rec to_list : t -> Relation.t list = function
  | Unsat -> []
  | Top -> []
  | Conjunction (r, rs) -> r :: (to_list rs)

(* pull out the sensitivity relations and context relations *)
let rec separate : Relation.t list -> (Sensitivity.Relation.t list * Context.Relation.t list) = function
  |[] -> ([], [])
  | r :: rs -> match separate rs with
    | (sens, contexts) -> match r with
      | Relation.S sr -> (sr :: sens, contexts)
      | Relation.C cr -> (sens, cr :: contexts)

(* find out which variables actually get bound *)
let concrete_support : Context.Relation.t list -> Name.t list = CCList.flat_map Context.Relation.support

(* once concrete variables are known, it's easy to convert context relations to a set of sensitivity relations *)
let context_rel_to_sens_rel_list (vars : Name.t list) (cr : Context.Relation.t) : Sensitivity.Relation.t list =
  let f = fun v -> 
    match cr with 
      | Context.Relation.Eq (l, r) -> Sensitivity.Relation.Eq (v <$ l, v <$ r)
      | Context.Relation.LEq (l, r) -> Sensitivity.Relation.LEq (v <$ l, v <$ r)
  in CCList.map f vars

(* flattening *)
let flatten : t -> Sensitivity.Relation.t list = fun c ->
  let (srs, crs) = separate (to_list c) in
  let variables = concrete_support crs in
    CCList.sort_uniq ~cmp:Pervasives.compare (srs @ (CCList.flat_map (context_rel_to_sens_rel_list variables) crs))

(* let's define an interpretation over a sensitivitity relation list *)
(* first we need a notion of a model *)
module Model = struct
  module NameMap = CCMap.Make(Name)
  (* we're putting this in its own module, but it's effectively a wrapper around NameMap *)
  type t = Rational.t NameMap.t
  let get (n : Name.t) (m : t) : Rational.t = NameMap.find n m
  (* some simple generation helpers *)
  let of_list : (Name.t * Rational.t) list -> t = NameMap.of_list
  let add : Name.t -> Rational.t -> t -> t = NameMap.add
end

(* we can evaluate terms first *)
let rec interpret_term (s : Sensitivity.t) (m : Model.t) : Rational.t = match s with
  | Sensitivity.Free n -> Model.get n m
  | Sensitivity.Bound i -> failwith "can't interpret a bound variable"
  | Sensitivity.Plus (l, r) ->
    let l' = interpret_term l m in
    let r' = interpret_term r m in
      Rational.add l' r'
  | Sensitivity.Mult (l, r) ->
    let l' = interpret_term l m in
    let r' = interpret_term r m in
      Rational.mult l' r'
  | Sensitivity.Zero -> Rational.of_int 0
  | Sensitivity.Succ s' -> Rational.add (Rational.of_int 1) (interpret_term s' m)
  | Sensitivity.Const c -> c

(* and then lift that to relations *)
let interpret_relation (sr : Sensitivity.Relation.t) (m : Model.t) : bool = match sr with
  | Sensitivity.Relation.Eq (l, r) ->
    let l' = interpret_term l m in
    let r' = interpret_term r m in
      (Rational.compare l' r') = 0
  | Sensitivity.Relation.LEq (l, r) ->
    let l' = interpret_term l m in
    let r' = interpret_term r m in
      (Rational.compare l' r') < 1

(* and finally to closed formulas *)
let interpret (srs : Sensitivity.Relation.t list) (m : Model.t) : bool = 
  CCList.for_all (fun sr -> interpret_relation sr m) srs

(* utilities for picking up variables *)
let variables : Sensitivity.Relation.t -> Name.t list = function
  | Sensitivity.Relation.Eq (l, r) -> (Sensitivity.free_vars l) @ (Sensitivity.free_vars r)
  | Sensitivity.Relation.LEq (l, r) -> (Sensitivity.free_vars l) @ (Sensitivity.free_vars r)