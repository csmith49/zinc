(* primitives represent the functions we're actually synthesizing over *)
type t = {
  name : string;
  dtype : Dtype.t;
  source : Value.t;
}

(* step one is being able to convert primitives to proposals *)
let to_proposal : t -> Proposal.t = fun p -> {
    Proposal.solution = Fterm.Prim (p.name, p.source);
    Proposal.dtype = p.dtype;
    Proposal.wildcards = Stack.Empty;
    Proposal.context = Context.Empty;
    Proposal.variables = [];
  }
