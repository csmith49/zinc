open CCFun
open Rlist.Alt

(* string provides readability, int provides uniqueness *)
type id = Id of string * int

(* useful deconstructor *)
let fst : id -> string = function
  Id (s, _) -> s

(* a name is a history of ids (of agents, contexts, and variables) *)
type t = id Rlist.t

(* when we only have a single entity, the number isn't as important *)
let add_string (n : t) (s : string) : t =
  n <+ Id (s, 0)

(* we can make names simply from strings using above *)
let of_string (s : string) : t = add_string Rlist.Empty s

(* we'll use this for printing unique strings *)
let rec hash : t -> CCHash.hash = function
  | Rlist.Empty -> 1
  | Rlist.Cons (i, nm) -> (CCHash.pair id_hash hash) (i, nm)
and id_hash : id -> CCHash.hash = function
  | Id (s, i) -> (CCHash.pair CCHash.string CCHash.int) (s, i)

(* there's many ways to write these out - we go for a readable presentation *)
let rec to_string : t -> string = fun n -> match n with
  | Rlist.Empty -> "EMPTY"
  | Rlist.Cons (i, nm) -> let i' = match i with
    | Id (s, i) ->
      if i < 5 then
        s ^ (CCString.repeat "\'" i)
      else
        s ^ "_" ^ (string_of_int i)
    in let uniq = match hash nm with
      | 1 -> ""
      | (_ as u) -> "{" ^ (string_of_int u) ^ "}"
    in i' ^ uniq

(* our comparisons deal with prefixes *)
let rec is_prefix (short : t) (long : t) : bool =
  if short == long then
    true
  else match long with
    | Rlist.Empty -> false
    | Rlist.Cons (_, ns) -> is_prefix short ns

(* a type indicating a list of names *)
type prefix = t Rlist.t

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
module Stream = struct
  (* cycles maintain a list of variable symbols, and warp around whenever we use too many *)
  module Cycle = struct
    type t = {
      symbols : string list;
      index : int;
      loop_counter : int;
    }
    (* pulling a new symbol just means we have to update the pointers *)
    let draw : t -> id * t = fun c ->
      let wrapped = c.index >= (CCList.length c.symbols) in
      let i = if wrapped then 0 else c.index in
      let loop = if wrapped then c.loop_counter + 1 else c.loop_counter in
      (Id (CCList.nth c.symbols i, loop) , {c with index = i; loop_counter = loop})
    (* base construction just takes a symbol list *)
    let of_list : string list -> t = fun ss -> {symbols = ss; index = 0; loop_counter = 0}
  end
  (* a stream maintains a separate cycle for each of the kinds of variables we have floating around *)
  type t = {
    root : name;
    abs_symbols : Cycle.t;
    wild_symbols : Cycle.t;
    sens_symbols : Cycle.t;
    dt_symbols : Cycle.t;
  }
  (* drawing a different kind of symbol needs a different kind of function *)
  let draw_abs : t -> name * t = fun s ->
    let (n, c') = Cycle.draw s.abs_symbols in
    (s.root <+ n, {s with abs_symbols = c'})
  let draw_wild : t -> name * t = fun s ->
    let (n, c') = Cycle.draw s.wild_symbols in
    (s.root <+ n, {s with wild_symbols = c'})
  let draw_sens : t -> name * t = fun s ->
    let (n, c') = Cycle.draw s.sens_symbols in
    (s.root <+ n, {s with sens_symbols = c'})
  let draw_dt : t -> name * t = fun s ->
    let (n, c') = Cycle.draw s.dt_symbols in
    (s.root <+ n, {s with dt_symbols = c'})
(* simplest constructor just takes in a root *)
  let of_root : name -> t = fun r -> {
      root = r;
      abs_symbols = Cycle.of_list ["x";"y";"z"];
      wild_symbols = Cycle.of_list ["w"];
      sens_symbols = Cycle.of_list ["s";"k";"t"];
      dt_symbols = Cycle.of_list ["a";"b";"c"];
  }
end
