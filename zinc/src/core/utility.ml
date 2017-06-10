open CCOpt.Infix

(* a common module, seen all over *)
module StringMap = CCMap.Make(String)

(* split a list into the elements before, at and after an index *)
let rec split_at (pos : int) (ls : 'a list) : ('a list * 'a * 'a list) option = match ls with
    | [] -> None
    | x :: xs ->
        if pos = 0 then
            Some ([], x, xs)
        else
            let f = fun (l, p, r) -> x :: l, p, r in
                f <$> (split_at (pos - 1) xs)

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

(* a simpler fold, that doesn't apply when there's only one element in the list *)
let rec fold (f : 'a -> 'a -> 'a) (ls : 'a list) : 'a = match ls with
    | [] -> failwith "nothing to merge"
    | x :: [] -> x
    | x :: xs -> f x (fold f xs)
