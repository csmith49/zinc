open CCFun
open Stack.Infix

(* string provides readability, int provides uniqueness *)
type id = Id of string * int

(* useful deconstructor *)
let fst : id -> string = function
  Id (s, _) -> s

(* a name is a history of ids (of agents, contexts, and variables) *)
type t = id Stack.t

(* when we only have a single entity, the number isn't as important *)
let add_string (n : t) (s : string) : t =
  n <+ Id (s, 0)

(* we can make names simply from strings using above *)
let of_string (s : string) : t = add_string Stack.Empty s

(* there's many ways to write these out - we go for a non-unique, readable presentation *)
let to_string : t -> string = fst % Stack.hd

(* our comparisons deal with prefixes *)
let rec is_prefix (short : t) (long : t) : bool =
  if short == long then
    true
  else match long with
    | Stack.Empty -> false
    | Stack.Cons (_, ns) -> is_prefix short ns
