type ('gamma, 'a) index =
  | Zero : ('gamma * 'a, 'a) index
  | Shift : ('gamma, 'a) index -> ('gamma * 'b, 'a) index

type ('gamma, 'a) term =
  | Var : ('gamma, 'a) index -> ('gamma, 'a) term
  | App : ('gamma, 'a -> 'b) term * ('gamma, 'a) term -> ('gamma, 'b) term
  | Lam : ('gamma * 'a, 'b) term -> ('gamma, 'a -> 'b) term

let rec lookup : type g a. (g, a) index -> g -> a = fun i env ->
  match i with
    | Zero -> snd env
    | Shift i -> lookup i (fst env)

let rec eval : type g a. (g, a) term -> g -> a = fun t env ->
  match t with
  | Var i -> lookup i env
  | App (l, r) -> (eval l env) (eval r env)
  | Lam t -> fun x -> eval t (x, env)
