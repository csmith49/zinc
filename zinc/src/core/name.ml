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

(* there's many ways to write these out - we go for a readable presentation *)
let to_string : t -> string = fun n -> match Stack.hd n with
  | Id (s, i) ->
    if i < 5 then
      s ^ (CCString.repeat "\'" i)
    else
      s ^ "_" ^ (string_of_int i)

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

(* for submodules *)
type name = t
let compare = Pervasives.compare

(* naming cycles *)
module Cycle = struct
  type t = Cycle of string list * int * int
  (* creating new from list *)
  let of_list : string list -> t = fun ns -> Cycle (ns, 0, 0)
  (* getting the current *)
  let current : t -> name = function
    | Cycle (ns, loop, index) -> Stack.Empty <+ Id (CCList.nth ns index, loop)
  (* and moving *)
  let rotate : t -> t = function
    | Cycle (ns, loop, index) ->
      let index' = index + 1 in
      if index' >= (CCList.length ns) then
        Cycle (ns, loop + 1, 0)
      else
        Cycle (ns, loop, index')
  (* some defaults we like *)
  let sensitivity = of_list ["k";"s";"t";"p";"q"]
  let abstraction = of_list ["x";"y";"z";"f";"g"]
  let dtype = of_list ["a";"b";"c";"d";"e"]
  let wildcard = of_list ["w"]
end
