open Context.Alt

(* constraints have two forms - equality, and inequality *)
type relation =
  | Eq of Sensitivity.t * Sensitivity.t
  | LEq of Sensitivity.t * Sensitivity.t
  | Empty

(* but we maintain a list of these relations as we abduce *)
type t = relation list option

(* an important conversion is context relations to constraints *)
let relation_of_context_relation (n : Name.t) (rel : Context.relation) : relation = match rel with
  | Context.Eq (l, r) -> Eq (n <$ l, n <$ r)

let of_context_relation (rel : Context.relation) : t =
  let f n = relation_of_context_relation n rel in Some (CCList.map f (vars rel))

(* alternative construction syntax *)
module Alt = struct
  let top : t =  Some [Empty]
  let (==) (s : Sensitivity.t) (s' : Sensitivity.t) : t = Some [Eq (s, s')]
  let (<=) (s : Sensitivity.t) (s' : Sensitivity.t) : t = Some [LEq (s, s')]
  let (&) (l : t) (r : t) : t = CCOpt.map2 (@) l r
end
