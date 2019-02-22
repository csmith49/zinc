module F = CCHeap.Make_from_compare(Node.Priority)

type t = F.t

let push : 'a Node.Vector.coefficients -> t -> Node.t -> t = fun weights -> fun f -> fun n ->
  let to_w = fun n -> n |> Node.Vector.of_node |> Node.Vector.to_int weights in
  let p = Node.Priority.of_node to_w n in 
    F.add f p

let empty : t = F.empty

let pop : t -> t * Node.t = fun f -> match F.take_exn f with
  | (f, p) -> (f, Node.Priority.to_node p)
