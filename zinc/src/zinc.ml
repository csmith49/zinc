open Containers
open Term
open Dtype
open Sensitivity

let f = Symbol "f"
let i = Rational.Infinity
let _ = print_endline (Rational.to_string i)
let _ = Rational.to_z3 i
