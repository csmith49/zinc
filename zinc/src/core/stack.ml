(* effectively a simple list, separated for type safety *)
type 'a t =
  | Empty
  | Cons of 'a * 'a t

(* the only destructor we might want outside of pattern matching *)
let hd : 'a t -> 'a = function
  | Empty -> failwith "can't take hd of empty stack"
  | Cons (s, _) -> s

(* infix functions separated for clarity in importing modules *)
module Infix = struct
  let (<+) (ss : 'a t) (s : 'a) : 'a t = Cons (s, ss)
end
