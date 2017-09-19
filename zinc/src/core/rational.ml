(* rationals are just treated as pairs of numbers, unless they're infinite *)
type t =
    | Infinity
    | Q of int * int

(* comparison probably doesn't need to be done, but we'll do it anyways *)
let compare (a : t) (b : t) : int = match a, b with
    | Infinity, Infinity -> 0
    | Infinity, _ -> 1
    | _, Infinity -> -1
    | _ -> Pervasives.compare a b

(* we need ways to convert qs to various types, and from various types *)
let of_int (i : int) : t = Q (i, 1)
let to_string : t -> string = function
    | Infinity -> "INFTY"
    | Q (a, b) -> (string_of_int a) ^ "/" ^ (string_of_int b)

(* and some basic arithmetic *)
let add (a : t) (b : t) : t = match a, b with
    | Infinity, _ -> Infinity
    | _, Infinity -> Infinity
    | Q (x, y), Q (n, d) -> Q (x * d + n * y, y * d)
let mult (a : t) (b : t) : t = match a, b with
    | Infinity, _ -> Infinity
    | _, Infinity -> Infinity
    | Q (x, y), Q (n, d) -> Q (x * n, y * d)

(* some simple utility functions for helping out *)
let rec gcd (x : int) (y : int) : int =
  if y == 0 then x else gcd y (x mod y)

(* we _might_ want to reduce down to a simpler form every once in a while *)
let reduce : t -> t = function
  | Infinity -> Infinity
  | Q (n, d) -> if d == 0 then Infinity else
    let g = gcd n d in Q (n / g, d / g)

(* wrapping math operators as infix *)
module Infix = struct
  let ( +% ) (a : t) (b : t) : t = add a b
  let ( *% ) (a : t) (b : t) : t = mult a b
end
