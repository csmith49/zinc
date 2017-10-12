open CCFun
open Z3

(* alias to simplify some type expressions *)
type expr = Expr.expr

(* what arguments might we put here? *)
let context = mk_context []

(* some arguments - like the rational values - must be bounded *)
(* this is how we code infinity *)
module RationalConstants = struct
  let infinity : Rational.t = Rational.Q (128, 1)
  let zero : Rational.t = Rational.Q (0, 1)
  let one : Rational.t = Rational.Q (1, 1)
end

(* we have a lot of constructors, put them here *)
module Make = struct
  (* constants and variables *)
  let rec rational : Rational.t -> expr = function
    | Rational.Q (n, d) -> Arithmetic.Real.mk_numeral_nd context n d
    | Rational.Infinity -> rational RationalConstants.infinity
  let variable : Name.t -> expr = fun n -> Arithmetic.Real.mk_const_s context (Name.to_string n)

  (* arithmetic *)
  let plus (l : expr) (r : expr) : expr = Arithmetic.mk_add context [l; r]
  let mult (l : expr) (r : expr) : expr = Arithmetic.mk_mul context [l; r]

  (* constraint construction *)
  let conjoin (l : expr) (r : expr) : expr = Boolean.mk_and context [l; r]
  let conjoin_list (ls : expr list) : expr = Boolean.mk_and context ls

  (* comparisons *)
  let leq (l : expr) (r : expr) : expr = Arithmetic.mk_le context l r
  let geq (l : expr) (r : expr) : expr = Arithmetic.mk_ge context l r
  let eq (l : expr) (r : expr) : expr = conjoin (leq l r) (geq l r)

  (* an empty expression - just true *)
  let empty : expr = Boolean.mk_true context

  (* and a failure *)
  let failure : expr = Boolean.mk_false context
end

(* conversion between our different data types *)
let rec expr_of_sensitivity : Sensitivity.t -> expr = function
  | Sensitivity.Free n -> Make.variable n
  | Sensitivity.Const c -> Make.rational c
  | Sensitivity.Plus (l, r) -> Make.plus (expr_of_sensitivity l) (expr_of_sensitivity r)
  | Sensitivity.Mult (l, r) -> Make.mult (expr_of_sensitivity l) (expr_of_sensitivity r)
  | Sensitivity.Zero -> Make.rational RationalConstants.zero
  | Sensitivity.Succ s -> Make.plus (expr_of_sensitivity s) (Make.rational RationalConstants.one)
  | _ -> failwith "can't convert to a z3 formula"

let expr_of_relation : Constraint.relation -> expr = function
  | Constraint.LEq (l, r) -> Make.leq (expr_of_sensitivity l) (expr_of_sensitivity r)
  | Constraint.Eq (l, r) -> Make.eq (expr_of_sensitivity l) (expr_of_sensitivity r)

let expr_of_constraint : Constraint.t -> expr = function
  | Constraint.Conjunction cs -> Make.conjoin_list ( Make.empty :: (CCList.map expr_of_relation cs))
  | Constraint.Unsat -> Make.failure

(* TODO : make sure that all rational variables are bounded by the constants of infinity and zero *)

(* holds solver construction and whatnot *)
module Check = struct
  let solver = Z3.Solver.mk_solver context None
  let is_sat : expr -> bool = fun c -> match Z3.Solver.check solver [c] with
    | Z3.Solver.SATISFIABLE -> true
    | _ -> false
end
