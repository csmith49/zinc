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
    | Conjunction (l, ls), (Conjunction _ as right) ->
      ls & Conjunction (l, right)
    | _ -> Unsat
  let unsat : t = Unsat
  let num : int -> Sensitivity.t = fun n -> Sensitivity.Const (Rational.of_int n)

  let c_rel : Context.Relation.t -> t = fun cr -> Conjunction (Relation.C cr, Top)
  let s_rel : Sensitivity.Relation.t -> t = fun sr -> Conjunction (Relation.S sr, Top)
end
