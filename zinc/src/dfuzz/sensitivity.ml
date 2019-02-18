(* representation of sensitivity terms *)
type t =
  | Free of Name.t
  | Bound of int
  | Const of Rational.t (* note - this covers infinite case too *)
  | Plus of t * t
  | Mult of t * t
  (* size embedding *)
  | Zero
  | Succ of t

(* mcbride and mckinna abstraction and instantiation *)
(* because this data type has no binders, we will only ever call these helper functions *)
let rec abstract' (n : Name.t) (db : int) (s : t) : t = match s with
  | Free n' -> if n = n' then (Bound db) else s
  | Plus (l, r) -> Plus (abstract' n db l, abstract' n db r)
  | Mult (l, r) -> Mult (abstract' n db l, abstract' n db r)
  | Succ s -> Succ (abstract' n db s)
  | _ -> s

let rec instantiate' (img : t) (db : int) (s : t) : t = match s with
  | Bound i -> if i = db then img else s
  | Plus (l, r) -> Plus (instantiate' img db l, instantiate' img db r)
  | Mult (l, r) -> Mult (instantiate' img db l, instantiate' img db r)
  | Succ s -> Succ (instantiate' img db s)
  | _ -> s

(* pick up free variables *)
let rec free_vars : t -> Name.t list = function
  | Free n -> [n]
  | Plus (l, r) -> (free_vars l) @ (free_vars r)
  | Mult (l, r) -> (free_vars l) @ (free_vars r)
  | Succ s -> free_vars s
  | _ -> []

(* a utility module for simplifying some constructors elsewhere *)
module Alt = struct
  (* arithmetic *)
  let ( +! ) (l : t) (r : t) : t = Plus (l, r)
  let ( *! ) (l : t) (r : t) : t = Mult (l, r)
  (* constants *)
  let zero = Const (Rational.of_int 0)
  let one = Const (Rational.of_int 1)
  let s (n : string) : t = Free (Name.of_string n)
end

(* printing *)
let rec to_string : t -> string = function
  | Free n -> Name.to_string n
  | Bound i -> "BOUND: " ^ (string_of_int i)
  | Const q -> Rational.to_string q
  | Plus (l, r) ->
    let l' = to_string l in
    let r' = to_string r in
    l' ^ " + " ^ r'
  | Mult (l, r) ->
    let l' = to_string l in
    let r' = to_string r in
    l' ^ " * " ^ r'
  | Zero -> "0"
  | Succ s -> (to_string s) ^ " + 1"

(* and utilities for substituting outside the context of binders *)
type sens = t

module Sub = struct
  module NameMap = CCMap.Make(Name)
  type t = sens NameMap.t

  let rec apply (s : sens) (sub : t) : sens = match s with
    | Free n -> NameMap.get_or ~default:s n sub
    | Plus (l, r) -> Plus (apply l sub, apply r sub)
    | Mult (l, r) -> Mult (apply l sub, apply r sub)
    | Succ r -> Succ (apply r sub)
    | _ -> s

  let empty = NameMap.empty

  let add : Name.t -> sens -> t -> t = NameMap.add

  let of_list : (Name.t * sens) list -> t = NameMap.of_list

  let to_string (sub : t) : string =
    let f = fun (k, v) -> (Name.to_string k) ^ "/" ^ (to_string v) in
    let sub' = sub
      |> NameMap.to_list
      |> CCList.map f
      |> CCString.concat ", "
    in "[" ^ sub' ^ "]"
end


module Relation = struct
  type t =
    | Eq of sens * sens
    | LEq of sens * sens
  
  let to_string : t -> string = function
    | Eq (l, r) ->
      let l' = to_string l in
      let r' = to_string r in
        l' ^ " = " ^ r'
    | LEq (l, r) ->
      let l' = to_string l in
      let r' = to_string r in
        l' ^ " <= " ^ r'

  let fmap (f : sens -> sens) : t -> t = fun sr -> match sr with
    | Eq (l, r) -> Eq (f l, f r)
    | LEq (l, r) -> LEq (f l, f r)

  let rhs : t -> sens = function
    | Eq (_, r) -> r
    | LEq (_, r) -> r
end