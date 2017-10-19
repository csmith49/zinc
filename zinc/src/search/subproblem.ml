type t = {
  root : Name.t;
  obligation : Constraint.t;
  hole : Fterm.Zipper.t;
  context : Context.t;
  goal : Dtype.t;
}

(* utility syntax stuff *)
open Name.Alt

(* nodes can be instantiated into several subproblems, we just pick the first bound *)
let of_node (root : Name.t) (n : Node.t) : t = match n.Node.solution with
  | Fterm.Wild (context, dom, body) ->
    let w = root <+ "w" in
    let wildcard = Fterm.Free w in
    let body' = Fterm.instantiate wildcard body in
    let zipper = Fterm.Zipper.preorder_until root "x" (fun t -> t = wildcard) (Fterm.Zipper.of_term body') in
    begin match zipper with
      | Some z -> {
        root = root <+ "node";
        obligation = n.Node.obligation;
        hole = z;
        context = context;
        goal = dom;
      }
      | _ -> failwith "can't construct subproblem without bound wildcard"
    end
  | _ -> failwith "can't construct subproblem from node without a wildcard"

(* depending on the variables in scope, we have a list of proposals *)
let rec variable_proposals (sp : t) : Proposal.t list =
  let f = fun (n, dt) -> {
    Proposal.variables = [];
    Proposal.solution = Fterm.Free n;
    Proposal.dtype = dt;
    Proposal.wildcards = Rlist.Empty;
    Proposal.context = Context.concrete_of_var n dt;
  } in CCList.map f (variables sp)
and variables (sp : t) : (Name.t * Dtype.t) list = match sp.hole with
  | (_, prefix, _) ->
    let bindings = Rlist.to_list prefix in
    let f = fun b -> match b with
      | Fterm.Prefix.BAbs (n, dt) -> Some (n, dt)
      | _ -> None
    in CCList.filter_map f bindings

(* and of course, if we want a function we can certainly make one *)
let lambda_proposal (sp : t) : Proposal.t option = match sp.goal with
  | Dtype.Func (Dtype.Modal (s, dom), codom) ->
    let w = sp.root <+ "w" in
    let context = Context.Symbolic (sp.root <+ "c") in
    let wild_bindings = Rlist.Cons (Fterm.Prefix.BWild (w, context, codom), Rlist.Empty) in
    Some {
      Proposal.variables = [];
      Proposal.solution = Fterm.Abs (dom, Fterm.Sc (Fterm.Free w));
      Proposal.dtype = sp.goal;
      Proposal.wildcards = wild_bindings;
      Proposal.context = Context.Times (s, context);
    }
  | _ -> None

open Constraint.Alt

(* plugging a proposal back in to get a node *)
(* last third of the ~> relation *)
let insert_proposal (p : Proposal.t) (sp : t) : Node.t option =
  let phi, sub = Inference.subtype_unify sp.root p.Proposal.variables sp.goal p.Proposal.dtype in
  if not (Constraint.is_unsat phi) && not (Inference.Sub.is_impossible sub) then
  let context_obligations = 
    Constraint.Conjunction (Constraint.Relation.C (Context.Relation.Eq (p.Proposal.context, sp.context)), Constraint.Top) in
  Some {
    Node.root = sp.root;
    Node.obligation = phi & context_obligations & sp.obligation;
    Node.solution =
      let tm = Inference.Sub.apply_fterm sub p.Proposal.solution in
      let z = Fterm.Zipper.set tm sp.hole in
      Fterm.Prefix.bind p.Proposal.wildcards (Fterm.Zipper.to_term z);
  }
  else None

open Fterm.Prefix.Alt
open Context.Alt
open Name.Alt

let rec specialize (root : Name.t) (prop : Proposal.t) : Proposal.t list =
  let recurse = match prop.Proposal.dtype with
    | Dtype.Func (Dtype.Modal (s, dom), codom) ->
      let c = Context.Symbolic (root <+ "c") in
      let w = root <+ "wild" in
      let binding = Fterm.Prefix.BWild (w, c, dom) in
      let f = prop.Proposal.solution in
      (* construct recursive call *)
      let p = {
        Proposal.variables = [];
        Proposal.solution = Fterm.App (f, Fterm.Free w);
        Proposal.dtype = codom;
        Proposal.wildcards = Rlist.Cons (binding, prop.Proposal.wildcards);
        Proposal.context = Context.Plus (prop.Proposal.context, Context.Times (s, c));
      } in specialize (root <+ "step") p
    | Dtype.Quant (q, k, body) when q = Dtype.ForAll && k = Dtype.KType ->
      let a = root <+ "type" in
      let f = prop.Proposal.solution in
      let p = {
        Proposal.variables = a :: prop.Proposal.variables;
        Proposal.solution = Fterm.TyApp (f, Dtype.Free a);
        Proposal.dtype = Dtype.instantiate (Dtype.Free a) body;
        Proposal.wildcards = prop.Proposal.wildcards;
        Proposal.context = prop.Proposal.context;
      } in specialize (root <+ "step") p
    | Dtype.Quant (q, k, body) when q = Dtype.ForAll && k = Dtype.KSens ->
      let s = root <+ "sens" in
      let f = prop.Proposal.solution in
      let p = {
        Proposal.variables = prop.Proposal.variables;
        Proposal.solution = Fterm.SensApp (f, Sensitivity.Free s);
        Proposal.dtype = Dtype.instantiate_sens (Sensitivity.Free s) body;
        Proposal.wildcards = prop.Proposal.wildcards;
        Proposal.context = prop.Proposal.context;
      } in specialize (root <+ "step") p
    | _ -> []
  in prop :: recurse