type t =
  | Int
  | List of t
  | Bag of t
  | F of abstraction
and abstraction = t -> t
