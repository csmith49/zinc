open Containers
open Sensitivity
open Context
open Signature

let i = Rational.Infinity
let _ = print_endline (Rational.to_string i)
let _ = Rational.to_z3 i
