type t =
  | Int of int
  | List of t
  | Bag of t
  | Real of float
  | F of abstraction
and abstraction = t -> t
