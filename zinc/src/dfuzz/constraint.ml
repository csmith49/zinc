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
  in CCList.map f vars

(* flattening *)
let flatten : t -> Sensitivity.Relation.t list = fun c ->
  let (srs, crs) = separate (to_list c) in
  let variables = concrete_support crs in
    CCList.sort_uniq (srs @ (CCList.flat_map (context_rel_to_sens_rel_list variables) crs))