(* primitives represent the funtions we're actually synthesizing over *)
type primitive = {
  name : string;
  dtype : Dtype.t;
  source : Value.t;
}

(* decompositions are all the ways we can break down the types of primitives *)
module Decomposition = struct
  type t = {
    inputs : Dtype.modal list;
    output : Dtype.t;
  }
  (* essential to our construction of decompositions *)
  let constant_decomposition : Dtype.t -> t = fun dtype -> {inputs = []; output = dtype}
  (* we can sometimes make a decomp from others *)
  let shift : t -> t option = fun decomp -> match decomp.output with
    | Dtype.Func (m, codom) -> Some ({inputs = decomp.inputs @ [m]; output = codom})
    | _ -> None
  (* we really want all decompositions, or maybe just the last one *)
  let rec all (dtype : Dtype.t) : t list = all' (constant_decomposition dtype)
  and all' (decomp : t) : t list = match shift decomp with
    | Some decomp' -> decomp :: (all' decomp')
    | _ -> [decomp]
  let rec last (dtype : Dtype.t) : t = last' (constant_decomposition dtype)
  and last' (decomp : t) : t = match shift decomp with
    | Some decomp' -> last' decomp'
    | _ -> decomp
end
