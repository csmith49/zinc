(* primitives represent the functions we're actually synthesizing over *)
type t = {
  name : string;
  dtype : Dtype.t;
  source : Value.t;
}

(* step one is being able to convert primitives to proposals *)
let to_proposal : t -> Search.Proposal.t = fun p -> {
    Search.Proposal.solution = Fterm.Prim (p.name, p.source);
    Search.Proposal.dtype = p.dtype;
  }
