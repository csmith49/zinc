open CCFun

(* the information we actually care about passing around *)
type node =
    | Wild of Dtype.t * Dtype.Context.t
    | Function of label
    | Variable of label
and label = string

(* some helpers for iteration and whatnot *)
let is_wild : node -> bool = function
    | Wild _ -> true
    | _ -> false
let is_function : node -> bool = function
    | Function _ -> true
    | _ -> false
let is_variable : node -> bool = function
    | Variable _ -> true
    | _ -> false

(* and our programs *)
type t = node Term.t
type context = node Term.Zipper.t

(* for printings sake *)
let rec to_string : t -> string = function
    | Term.Abs e -> "\\." ^ (to_string e)
    | Term.App (l, r) -> "(" ^ (to_string l) ^ " " ^ (to_string r) ^ ")"
    | Term.Symbol s -> match s with
        | Wild _ -> "?"
        | Function f -> f
        | Variable v -> v
