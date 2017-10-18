open Context.Alt

(* constraints have two forms - equality, and inequality *)
type relation =
  | Eq of Sensitivity.t * Sensitivity.t
  | LEq of Sensitivity.t * Sensitivity.t

(* but we maintain a list of these relations as we abduce *)
type t =
  | Conjunction of relation list
  | Unsat

(* an important conversion is context relations to constraints *)
let relation_of_context_relation (n : Name.t) (rel : Context.relation) : relation = match rel with
  | Context.Eq (l, r) -> Eq (n <$ l, n <$ r)

let of_context_relation (rel : Context.relation) : t =
  let _ = print_endline ("\nTESTING CONVERSION") in
  let _ = print_endline (Context.relation_to_string rel) in
  let v = vars rel in
  let _ = CCList.iter (fun x -> print_endline ("\t" ^ (Name.to_string x))) v in
  let f = fun n -> relation_of_context_relation n rel in Conjunction (CCList.map f (vars rel))

let is_unsat : t -> bool = function
  | Unsat -> true
  | _ -> false

let rec to_string : t -> string = function
  | Unsat -> "UNSAT"
  | Conjunction [] -> "T"
  | Conjunction (r :: []) -> relation_to_string r
  | Conjunction (r :: rs) -> (relation_to_string r) ^ " & " ^ (to_string (Conjunction rs))
and relation_to_string : relation -> string = function
  | LEq (l, r) -> 
    let l' = Sensitivity.to_string l in
    let r' = Sensitivity.to_string r in
      l' ^ " <= " ^ r'
  | Eq (l, r) ->
    let l' = Sensitivity.to_string l in
    let r' = Sensitivity.to_string r in
      l' ^ " = " ^ r'

(* alternative construction syntax *)
module Alt = struct
  let top : t =  Conjunction []
  let (==) (s : Sensitivity.t) (s' : Sensitivity.t) : t = Conjunction [Eq (s, s')]
  let (<=) (s : Sensitivity.t) (s' : Sensitivity.t) : t = Conjunction [LEq (s, s')]
  let (&) (l : t) (r : t) : t = match l, r with
    | Conjunction ls, Conjunction rs -> Conjunction (ls @ rs)
    | _ -> Unsat
  let unsat : t = Unsat
  let num : int -> Sensitivity.t = fun n -> Sensitivity.Const (Rational.of_int n)
end
