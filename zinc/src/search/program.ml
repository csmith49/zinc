open CCFun
open CCOpt.Infix

(* the information we actually care about passing around *)
type node =
    (* | Wild of Dtype.t * Dtype.Context.t *)
    | Wild of Dtype.t * Context.t
    | Function of string
    | Variable of Variable.t

(* some helpers for iteration and whatnot *)
let node_is_wild : node -> bool = function
    | Wild _ -> true
    | _ -> false

(* and our programs *)
type t = node Term.t
type context = node Term.Zipper.t

(* with some more useful accessor functions and whatnot *)
let get_node : context -> node option =
    Term.extract % Term.Zipper.get

let is_wild (p : t) : bool =
    let n = Term.extract p in
        CCOpt.get_or ~default:false (node_is_wild <$> n)

let rec variables_in_scope (c : context) : Variables.t list =
    let xs = match Term.Zipper.get c with
        | Term.Abs (x, _) -> [x]
        | _ -> []
    in match Term.Zipper.up c with
        | Some c' -> xs @ (variables_in_scope c')
        | _ -> xs

(* for printings sake *)
let rec to_string : t -> string = function
    | Term.Abs (x, e) -> "\\" ^ (Variable.to_string x) ^ "." ^ (to_string e)
    | Term.App (l, r) -> "(" ^ (to_string l) ^ " " ^ (to_string r) ^ ")"
    | Term.Symbol s -> match s with
        | Wild _ -> "?"
        | Function f -> f
        | Variable v -> Variable.to_string v