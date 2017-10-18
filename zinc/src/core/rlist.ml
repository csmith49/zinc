(* effectively a simple list, separated for type safety *)
type 'a t =
  | Empty
  | Cons of 'a * 'a t

(* the only destructor we might want outside of pattern matching *)
let hd : 'a t -> 'a = function
  | Empty -> failwith "can't take hd of empty Rlist"
  | Cons (s, _) -> s

(* combining Rlists in the obvious way *)
let rec concat (l : 'a t) (r : 'a t) : 'a t = match r with
  | Empty -> l
  | Cons (s, ss) -> Cons (s, concat l ss)

let rec size : 'a t -> int = function
  | Empty -> 0
  | Cons (_, ss) -> 1 + (size ss)

let rec of_list : 'a list -> 'a t = function
  | [] -> Empty
  | x :: xs -> Cons (x, of_list xs)

let rec to_list : 'a t -> 'a list = function
  | Empty -> []
  | Cons (x, xs) -> x :: (to_list xs)

(* infix functions separated for clarity in importing modules *)
module Alt = struct
  let (<+) (ss : 'a t) (s : 'a) : 'a t = Cons (s, ss)
  let (<++) (ss : 'a t) (ss' : 'a t) : 'a t = concat ss ss'
end
