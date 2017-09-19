(* representation of sensitivity terms *)
type t =
  | Var of Variable.t
  | Const of Rational.t (* note - this covers infinite case too *)
  | Size of Size.t
  | Plus of t * t
  | Mult of t * t

(* simple printing *)
let rec to_string : t -> string = function
  | Var v -> Variable.to_string v
  | Const q -> Rational.to_string q
  | Size s -> Size.to_string s
  | Plus (l, r) ->
    let l' = to_string l in
    let r' = to_string r in
      l' ^ " + " ^ r'
  | Mult (l, r) ->
    let l' = to_string l in
    let r' = to_string r in
      l' ^ " * " ^ r'
