type t = {
  root : Name.t;
  obligation : Constraint.t;
  hole : Fterm.Zipper.t;
  context : Context.t;
  goal : Dtype.t;
}

(* utility syntax stuff *)
open Name.Alt
open Fterm.Prefix.Alt
open Context.Alt
open Constraint.Alt

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
  let f = fun (n, dt) -> 
    {
      Proposal.solution = Fterm.Free n;
      Proposal.dtype = dt;
      Proposal.wildcards = [];
      Proposal.context = Context.concrete_of_var n dt;
      Proposal.obligation = Constraint.Top;
    } in CCList.map f (variables sp)
and variables (sp : t) : (Name.t * Dtype.t) list = match sp.hole with
  | (_, prefix, _) ->
    let f = fun b -> match b with
      | Fterm.Prefix.BAbs (tag, dt) -> Some (tag, dt)
      | _ -> None
    in CCList.filter_map f prefix

(* and of course, if we want a function we can certainly make one *)
let lambda_proposal (sp : t) : Proposal.t option = match sp.goal with
  | Dtype.Func (Dtype.Modal (s, dom), codom) ->
    let w = sp.root <+ "w" in
    let tag = sp.root <+ "x" in
    let context = Context.Symbolic (sp.root <+ "c") in
    let wild_bindings = [Fterm.Prefix.BWild (w, context, codom)] in
    Some {
      Proposal.solution = Fterm.Abs (tag, dom, Fterm.Sc (Fterm.Free w));
      Proposal.dtype = sp.goal;
      Proposal.wildcards = wild_bindings;
      Proposal.context = context;
      Proposal.obligation = c_rel (context =. (sp.context +. (concrete tag s dom)));
    }
  | _ -> None

open Constraint.Alt

(* plugging a proposal back in to get a node *)
(* last third of the ~> relation *)
let insert_proposal (p : Proposal.t) (sp : t) : Node.t option =
  match Inference.subtype_unify sp.root sp.goal p.Proposal.dtype with
    | Some (phi, sub) ->  
      Some {
        Node.root = sp.root;
        Node.obligation = phi & p.Proposal.obligation & sp.obligation;
        Node.solution =
          Inference.Sub.apply_fterm 
            sub 
            (Fterm.Prefix.bind 
              p.Proposal.wildcards 
              (Fterm.Zipper.to_term (Fterm.Zipper.set p.Proposal.solution sp.hole)));
      }
    | None -> None

let rec specialize (root : Name.t) (prop : Proposal.t) (context : Context.t) : Proposal.t list =
  let recurse = match prop.Proposal.dtype with
    | Dtype.Func (Dtype.Modal (s, dom), codom) ->
      let root = root <+ "step_abs" in
      let c = Context.Symbolic (root <+ "c") in
      let c_wild = Context.Symbolic (root <+ "c_wild") in
      let w = root <+ "wild" in
      let binding = Fterm.Prefix.BWild (w, c_wild, dom) in
      let f = prop.Proposal.solution in
      (* construct recursive call *)
      let p = {
        Proposal.solution = Fterm.App (f, Fterm.Free w);
        Proposal.dtype = codom;
        Proposal.wildcards = binding :: prop.Proposal.wildcards;
        Proposal.context = c;
        Proposal.obligation = 
          prop.Proposal.obligation & c_rel (c =. (prop.Proposal.context +. (s *. c_wild)));
      } in specialize root p context
    | Dtype.Quant (q, k, body) when q = Dtype.ForAll && k = Dtype.KType ->
      let root = root <+ "step_tyabs" in
      let c = Context.Symbolic (root <+ "c") in
      let a = root <+ "type" in
      let f = prop.Proposal.solution in
      let p = {
        Proposal.solution = Fterm.TyApp (f, Dtype.Free a);
        Proposal.dtype = Dtype.instantiate (Dtype.Free a) body;
        Proposal.wildcards = prop.Proposal.wildcards;
        Proposal.context = c;
        Proposal.obligation = c_rel (c =. prop.Proposal.context);
      } in specialize root p context
    | Dtype.Quant (q, k, body) when q = Dtype.ForAll && k = Dtype.KSens ->
      let root = root <+ "step_sensabs" in
      let c = Context.Symbolic (root <+ "c") in
      let s = root <+ "sens" in
      let f = prop.Proposal.solution in
      let p = {
        Proposal.solution = Fterm.SensApp (f, Sensitivity.Free s);
        Proposal.dtype = Dtype.instantiate_sens (Sensitivity.Free s) body;
        Proposal.wildcards = prop.Proposal.wildcards;
        Proposal.context = c;
        Proposal.obligation = c_rel (c =. prop.Proposal.context);
      } in specialize root p context
    | _ -> []
  in {prop with Proposal.obligation = prop.Proposal.obligation & (c_rel (context =. prop.Proposal.context));} :: recurse