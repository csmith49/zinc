type priority = int

type 'a t = Empty | Node of priority * 'a * 'a t * 'a t

let empty = Empty

(* TODO: node case looks broken *)
let rec push p e queue = match queue with
    | Empty -> Node (p, e, Empty, Empty)
    | Node (pc, ec, left, right) ->
        if p <= pc
            then Node (p, e, push pc ec right, left)
            else Node (pc, ec, push p e right, left)

exception Frontier_is_empty

let rec remove_top = function
    | Empty -> raise Frontier_is_empty
    | Node (p, e, left, Empty) -> left
    | Node (p, e, Empty, right) -> right
    | Node (p, e, (Node (lp, le, _, _) as left), (Node (rp, re, _, _) as right)) ->
        if lp <= rp
            then Node (lp, le, remove_top left, right)
            else Node (rp, re, left, remove_top right)

let pop = function
    | Empty -> raise Frontier_is_empty
    | Node (p, e, _, _) as queue -> (p, e, remove_top queue)
