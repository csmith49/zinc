(* currently just syntactic objects *)
type component = Component of string * Dtype.t list * Dtype.t
(* and a signature just maintains a list *)
type t = component list

(* reindex type variables so they're all fresh *)
(* TODO *)
let freshen (c : component) : component = c
