module type PRIORITY = sig
  type t
  val compare : t -> t -> int
end

(* this specializes the standard p queue operations for our sig *)
functor Make (P : PRIORITY) -> struct
  type priority = P.t
  type 'a t =
    | Empty
    | Node of priority * 'a * 'a t * 'a t
    (* a default constructor - we make new queues by pushing *)
  let empty = Empty
  (* pushing maintains the invariant that smallest priority is on top *)
  let rec push (p : priority) (e : 'a) (queue : 'a t) : 'a t = match queue with
    | Empty -> Node (p, e, Empty, Empty)
    | Node (p', c', left, right) ->
      if (P.compare p p') <= 0
      then Node (p, e, push right p' c', left)
      else Node (p', c', push right p e, left)
  (* it is possible to fail, but it shouldn't happen often *)
  exception Empty_structure
  (* to get the minimal element, we have to remove the root node *)
  let rec remove_top : 'a t -> 'a t = function
    | Empty -> raise Empty_structure
    | Node (p, e, left, Empty) -> left
    | Node (p, e, Empty, right) -> right
    | Node (p, e, (Node (lp, le, _, _) as left), (Node (rp, re, _, _) as right)) ->
      if (P.compare lp rp) <= 0
      then Node (lp, le, remove_top left, right)
      else Node (rp, re, left, remove_top right)
  (* and the expected interface *)
  let pop : 'a t -> 'a * P.t * 'a t = function
    | Empty -> raise Empty_structure
    | Node (p, e, _, _) as queue -> (p, e, remove_top queue)
end
