type _ hoas =
  | Lit : 'a -> 'a hoas
  | Abs : ('a -> 'b) -> ('a -> 'b) hoas
  | App : ('a -> 'b) * 'a hoas -> 'b hoas

let rec eval : 'a hoas -> 'a = function
  | Lit v -> v
  | Abs f -> f
  | App (f, v) -> f (eval v)
