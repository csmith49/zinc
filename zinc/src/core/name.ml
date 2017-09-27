open CCFun
open Stack.Alt

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

(* a type for denoting when we need to rename things *)
type 'a agency = t -> 'a

(* a type indicating a list of names *)
type prefix = t Stack.t

(* pair up a list of things with names for each *)
let rec name_list (root : t) (var : string) (ls : 'a list) : (t * 'a) list =
  name_list' root var 1 ls
and name_list' (root : t) (var : string) (index : int) (ls : 'a list) : (t * 'a) list = match ls with
  | x :: xs -> (root <+ Id (var, index), x) :: (name_list' root var (index + 1) xs)
  | [] -> []

(* simpler syntax *)
module Alt = struct
  let ( ++ ) (n : t) (n' : t) : t = n <++ n'
  let n (s : string) : t = of_string s
  let ( <+ ) (n : t) (s : string) = add_string n s
end
