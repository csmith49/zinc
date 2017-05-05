(* the context that we'll use throughout zinc *)
let context = Z3.mk_context []

(* a type alias so we don't have to write so much *)
type expr = Z3.Expr.expr

(* some helper functions for constructing values *)
let make_int : int -> expr = Z3.Arithmetic.Integer.mk_numeral_i context
let make_int_const : string -> expr = Z3.Arithmetic.Integer.mk_const_s context

(* and for arithmetic *)
let add (l : expr) (r : expr) : expr = Z3.Arithmetic.mk_add context [l;r]
