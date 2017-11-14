module F = CCHeap.Make(Node.Priority)

type t = F.t

let push :  t -> Node.Vector.weight -> Node.t -> t = fun f -> fun w -> fun n ->
  let p = Node.to_priority w n in F.add f p

let empty : t = F.empty

let pop : t -> t * Node.t = fun f -> match F.take_exn f with
  | (f, p) -> (f, Node.of_priority p)
