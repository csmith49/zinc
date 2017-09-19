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

(* mcbride and mckinna abstraction and instantiation *)
(* because this data type has no binders, we will only ever call these helper functions *)
let rec name_to_db (name : Name.t) (depth : int) (s : t) : t = match s with
  | Mult (l, r) -> Mult (name_to_db name depth l, name_to_db name depth r)
  | Plus (l, r) -> Plus (name_to_db name depth l, name_to_db name depth r)
  | Size s' -> Size (Size.name_to_db name depth s')
  | Const _ -> s
  | Var v -> match v with
    | Variable.Free n' -> if n' == name then Var (Variable.Bound depth) else s
    | _ -> s

let rec replace (img : t) (depth : int) (s : t) : t = match s with
  | Mult (l, r) -> Mult (replace img depth l, replace img depth r)
  | Plus (l, r) -> Plus (replace img depth l, replace img depth r)
  | Size s' -> s
  | Const _ -> s
  | Var v -> if Variable.matches v depth then img else s
