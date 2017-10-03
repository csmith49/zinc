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
  (* this is the canonical decomposition *)
  let rec last (dtype : Dtype.t) : t = last' (constant_decomposition dtype)
  and last' (decomp : t) : t = match shift decomp with
    | Some decomp' -> last' decomp'
    | _ -> decomp
  (* and some accessors, for elsewhere *)
  let output_type (d : t) : Dtype.t = d.output
  let input_types (d : t) : Dtype.modal list = d.inputs
end

module Stub = struct
  type t = Term.Prefix.t * Term.t
end

(* primitives represent the funtions we're actually synthesizing over *)
type primitive = {
  name : string;
  dtype : Dtype.t;
  source : Value.t;
}

let to_term (prim : primitive) : Term.t = Term.Prim (prim.name, prim.source)

open Name.Alt

(* generating stubs from the provided synthesis info *)
let gen_stub (root : Name.t) (c : Context.t) (decomp : Decomposition.t) (prim : primitive) : Stub.t * (Context.relation option) =
  (* let's get some naming going on *)
  let inputs = Name.name_list root "input" decomp.Decomposition.inputs in
  (* wildcard construction is straightforward, just project and wrap as a Free *)
  let wildcards = CCList.map (fun ni -> Term.Free ((fst ni) <+ "variable")) inputs in
  (* bindings require contexts *)
  let bindings = CCList.map (fun ni -> match ni with
      | (n, Dtype.Modal (s, dt)) ->
        let context = Context.Symbolic (n <+ "context") in
        Term.Prefix.PWild (context, n <+ "variable", dt))
      inputs in
  let prefix = Stack.of_list bindings in
  (* constructor wrapper for the cata to come *)
  let mk_app l r = Term.App (l, r) in
  (* our base node, constructed from a primitive *)
  let f = to_term prim in
  (* term constructor *)
  let term = CCList.fold_left mk_app f wildcards in
  (* now we hvae to construct the context relation *)
  let scaled_contexts = CCList.map (fun ni -> match ni with
      | (n, Dtype.Modal (s, dt)) ->
        Context.Times (s, Context.Symbolic (n <+ "context")))
      inputs in
  let mk_add l r = Context.Plus (l, r) in
  let relation = match scaled_contexts with
    | [] -> None
    | ls -> Some (Context.Eq (c, CCList.fold_left mk_add (CCList.hd ls) (CCList.tl ls))) in
  ((prefix, term), relation)
