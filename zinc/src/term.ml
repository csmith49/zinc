open CCOpt.Infix

(* simple rose trees represent everything we might need - parameterized by node value *)
type 'a tree =
    | Leaf of 'a
    | Node of 'a * 'a tree list
(* maybe we encapsulate constructors -- really all that matters is the number of children *)
let make (v : 'a) (ts : 'a tree list) : 'a tree =
    if CCList.is_empty ts then
        Leaf v
    else
        Node (v, ts)

module Zipper = struct
    (* zipper type for rose trees is actually rather complex, but we'll manage somehow *)
    type 'a t = Z of 'a * 'a tree list * ('a * ('a tree list) * ('a tree list)) list
    (* some setters and getters and whatnot *)
    let get : 'a t -> 'a = function
        | Z (v, _, _) -> v
    let set (v : 'a) : 'a t -> 'a t = function
        | Z (_, ts, zs) -> Z (v, ts, zs)
    (* fancier ones *)
    let get_tree : 'a t -> 'a tree = function
        | Z (v, ts, _) -> make v ts
    let set_tree (t : 'a tree) : 'a t -> 'a t = function
        | Z (_, _, zs) -> match t with
            | Leaf v -> Z (v, [], zs)
            | Node (v, ts) -> Z (v, ts, zs)
    (* constructors and destructors, always from trees *)
    let of_tree : 'a tree -> 'a t = function
        | Leaf v -> Z (v, [], [])
        | Node (v, ts) -> Z (v, ts, [])
    let rec to_tree (z : 'a t) : 'a tree =
        let t = get_tree z in match z with
            | Z (_, _, []) -> t
            | Z (_, _, (f, ls, rs) :: zs) ->
                let z' = Z (f, ls @ [t] @ rs, zs) in
                    to_tree z'
    (* of course, we need ways to move around the zipper *)
    let up : 'a t -> 'a t option = function
        | Z (_, _, []) -> None
        | Z (v, ts, (f, ls, rs) :: zs) ->
            let t = make v ts in
                Some (Z (f, ls @ [t] @ rs, zs))
    let down_at (pos : int) : 'a t -> 'a t option = function
        | Z (v, ts, zs) -> if CCList.is_empty ts then None else
            let f = fun (ls, t, rs) -> match t with
                | Leaf v' -> Some (Z (v', [], (v, ls, rs) :: zs))
                | Node (v', ts') -> Some (Z (v', ts', (v, ls, rs) :: zs))
            in (Utility.split_at pos ts) >>= f
    let down (z : 'a t) : 'a t option = down_at 0 z
end
