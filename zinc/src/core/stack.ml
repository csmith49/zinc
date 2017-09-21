(* effectively a simple list, separated for type safety *)
type 'a t =
  | Empty
  | Cons of 'a * 'a t

(* the only destructor we might want outside of pattern matching *)
let hd : 'a t -> 'a = function
  | Empty -> failwith "can't take hd of empty stack"
  | Cons (s, _) -> s

(* combining stacks in the obvious way *)
let rec concat (l : 'a t) (r : 'a t) : 'a t = match r with
  | Empty -> l
  | Cons (s, ss) -> Cons (s, concat l ss)

(* infix functions separated for clarity in importing modules *)
module Alt = struct
  let (<+) (ss : 'a t) (s : 'a) : 'a t = Cons (s, ss)
  let (<++) (ss : 'a t) (ss' : 'a t) : 'a t = concat ss ss'
end
