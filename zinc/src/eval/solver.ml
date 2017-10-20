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

(* signature parameterizing a strategy for converting relations to z3 expressions *)
module type STRATEGY = sig
  type t
  val of_constraint : Constraint.t -> t
  val to_expr_list : t -> expr list
end

(* converts an instance of STRATEGY to a module for checking wrt z3 *)
module Strategy = functor (S : STRATEGY) -> struct
  let solver = Z3.Solver.mk_solver context None

  let propositional_representation : expr list -> expr * expr = fun exprs ->
    let p = Boolean.mk_const_s context "p" in
    let propositional = Boolean.mk_eq context p (Make.conjoin_list exprs) in
      (p, propositional)

  (* the reason we want this - given a strat, wrap it in a call to z3 *)
  let check : Constraint.t -> bool = fun c ->
    let exprs = S.to_expr_list (S.of_constraint c) in
    let (p, propositional) = propositional_representation exprs in
    let _ = Z3.Solver.reset solver in
    let _ = Z3.Solver.add solver [propositional] in
    match Z3.Solver.check solver [p] with
      | Z3.Solver.SATISFIABLE -> true
      | _ -> false
end

(* here we describe strategies *)
module Basic = struct
  type t = Sensitivity.Relation.t list

  (* conversion from constraints *)
  let of_constraint : Constraint.t -> t = Constraint.flatten

  (* we convert each expression independently *)
  let rec expr_of_sens : Sensitivity.t -> expr = function
    | Sensitivity.Free n -> Make.variable n
    | Sensitivity.Const c -> Make.rational c
    | Sensitivity.Plus (l, r) -> Make.plus (expr_of_sens l) (expr_of_sens r)
    | Sensitivity.Mult (l, r) -> Make.mult (expr_of_sens l) (expr_of_sens r)
    | Sensitivity.Zero -> Make.rational RationalConstants.zero
    | Sensitivity.Succ s -> Make.plus (expr_of_sens s) (Make.rational RationalConstants.one)
    | _ -> failwith "can't convert to a z3 formula"

  (* lift to constraining relations *)
  let expr_of_sens_rel : Sensitivity.Relation.t -> expr = function
    | Sensitivity.Relation.Eq (l, r) -> Make.eq (expr_of_sens l) (expr_of_sens r)
    | Sensitivity.Relation.LEq (l, r) -> Make.leq (expr_of_sens l) (expr_of_sens r)

  (* but we also constrain any variables that we have *)
  let constrain_variable : Name.t -> expr = fun n ->
    let x = Make.variable n in
    let zero = Make.rational RationalConstants.zero in
    let infinity = Make.rational RationalConstants.infinity in
      Make.conjoin (Make.leq x infinity) (Make.leq zero x)

  (* lift free var computation to relations *)
  let variables : Sensitivity.Relation.t -> Name.t list = function
    | Sensitivity.Relation.Eq (l, r) -> (Sensitivity.free_vars l) @ (Sensitivity.free_vars r)
    | Sensitivity.Relation.LEq (l, r) -> (Sensitivity.free_vars l) @ (Sensitivity.free_vars r)

  (* we find all variables so we can constrain appropriately *)
  let variables : t -> Name.t list = CCList.flat_map variables

  (* the final conversion *)
  let to_expr_list : t -> expr list = function
    | [] -> [Make.failure]
    | (_ as srs) ->
      let var_constraints = CCList.map constrain_variable (variables srs) in
      let sens_constraints = CCList.map expr_of_sens_rel srs in
      var_constraints @ sens_constraints
end