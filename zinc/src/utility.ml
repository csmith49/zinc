(* split a list into the elements before, at and after an index *)
let split_at (pos : int) (ls : 'a list) : ('a list * 'a * 'a list) option = None

(* split a list into the n - 1 elements and the last element *)
let rec split_last (ls : 'a list) : ('a list * 'a) option = match ls with
    | [] -> None
    | x :: xs -> match split_last xs with
        | None -> Some ([], x)
        | Some (xs', l) -> Some (x :: xs', l)
(* and the reverse *)
let split_first (ls : 'a list) : ('a * 'a list) option = match ls with
    | [] -> None
    | x :: xs -> Some (x, xs)
