open Z3

(* just an alias to simplify type expressions *)
type expr = Expr.expr

(* we create a single context - honestly not sure what might need to be in the list *)
let context = mk_context []

(* conversion functions *)
let make_rational (n : int) (d : int) : expr = Arithmetic.Real.mk_numeral_nd context n d
let make_variable (x : Variable.t) : expr = Arithmetic.Real.mk_const_s context (Variable.to_string x)

(* and syntax functions *)
let make_and (l : expr) (r : expr) : expr = Boolean.mk_and context [l; r]
let make_and_list (ls: expr list) : expr = Boolean.mk_and context ls

let make_add (l : expr) (r : expr) : expr = Arithmetic.mk_add context [l; r]
let make_times (l : expr) (r : expr) : expr = Arithmetic.mk_mul context [l; r]
