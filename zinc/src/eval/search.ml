let signature : (string * Value.t * Dtype.t) list ref = ref [];

module Stub = struct
  type t = Fterm.Prefix.t * Fterm.t
  (* simple constructor *)
  let of_primitive (p : Signature.primitive) : t =
    let name = p.Signature.name in
    let source = p.Signature.source in
    (Stack.Empty, Fterm.Prim (name, source))
end

module Problem = struct
  open Name.Alt
  (* all the things we need to maintain to find type-safe substitutions *)
  type t = {
    root : Name.t;
    hole : Fterm.Zipper.t;
    context : Context.t;
    goal : Dtype.t;
  }
  (* our primary conversions is from terms *)
  let of_term (root : Name.t) : Fterm.t -> t = function
    | Fterm.Wild (context, dom, body) ->
      let wildcard = root <+ "wild" in
      let body' = Fterm.instantiate (Fterm.Free wildcard) body in
      let zipper = Fterm.Zipper.preorder_until root "x" (fun t -> t = (Fterm.Free wildcard)) (Fterm.Zipper.of_term body') in
      begin match zipper with
        | Some z ->
          {
            root = root;
            hole = z;
            context = context;
            goal = dom;
          }
        | _ -> failwith "can't construct a problem without the bound wildcard"
      end
    | _ -> failwith "can't construct a problem from anything without a wildcard"
  (* to get back to terms we insert stubs *)
  let insert_stub (stub : Stub.t) (problem : t) : Fterm.t = match stub with
    | (prefix, tm) ->
      let tm' = Fterm.Zipper.to_term (Fterm.Zipper.set tm problem.hole) in
      Fterm.Prefix.bind prefix tm'
  (* one of the more important bits - how do we solve a problem *)
  (* we're looking for stubs that, when inserted, yield a type-safe term *)
  let find_stubs : t -> (Stub.t * Constraint.t) list = fun p -> []
end

module Node = struct
  type t = {
    obligation : Constraint.t;
    solution : Fterm.t;
  }
end

(* slightly updated *)
module Subproblem = struct
  open Name.Alt
  (* basically, the signature for the ~> relation *)
  type t = {
    root : Name.t;
    obligation : Constraint.t;
    hole : Fterm.Zipper.t;
    context : Context.t;
    goal : Dtype.t;
  }
  (* construction from a search term *)
  let of_node (root : Name.t) (n : Node.t) : t = match n.Node.solution with
    | Fterm.Wild (context, dom, body) ->
      let w = root <+ "wild" in
      let wildcard = Fterm.Free w in
      let body' = Fterm.instantiate wildcard body in
      let zipper = Fterm.Zipper.preorder_until root "x" (fun t -> t = wildcard) (Fterm.Zipper.of_term body') in
      begin match zipper with
        | Some z -> {
            root = root;
            obligation = n.Node.obligation;
            hole = z;
            context = context;
            goal = dom;
          }
        | _ -> failwith "can't construct subproblem without the bound wildcard"
      end
    | _ -> failwith "can't construct subproblem from node without a wildcard"
end

(* to make things a little easier *)
module Proposal = struct
  type t = {
    solution : Fterm.t;
    dtype : Dtype.t;
  }
end

open Constraint.Alt
open Fterm.Prefix.Alt
open Context.Alt
open Name.Alt

(* the latter two-thirds of the ~> relation *)
let rec specialize
    (root : Name.t)
    (context : Context.t)
    (proposal : Proposal.t)
    (vars : Name.t list)
    (sp : Subproblem.t) : Node.t option =
  (* base case - are we done? *)
  let c, sub = Inference.subtype_unify root vars sp.Subproblem.goal proposal.Proposal.dtype in
  if not (Constraint.is_unsat c) && (Inference.Sub.is_impossible sub) then
    Some ({obligation = c & sp.Subproblem.obligation; solution = Inference.Sub.apply_fterm sub proposal.Proposal.solution})
  else match proposal.Proposal.dtype with
    | Dtype.Func (Dtype.Modal (s, dom), codom) ->
      (* base name creation and whatnot *)
      let c = Context.Symbolic (root <+ "context") in
      let w = root <+ "wild" in
      let binding = Fterm.Prefix.BWild (w, c, dom) in
      let f = proposal.Proposal.solution in
      (* construct the new proposal *)
      let prop = {
        Proposal.solution = binding @> Fterm.App (f, Free w);
        Proposal.dtype = codom;
      } in
      (* recurse *)
      specialize (root <+ "step") (Context.Plus (context, Context.Times (s, c))) prop vars sp
    | Dtype.Quant (q, k, body) when q = Dtype.ForAll && k = Dtype.KType ->
      let a = root <+ "type" in
      let f = proposal.Proposal.solution in
      (*  *)
      let prop = {
        Proposal.solution = Fterm.TyApp (f, Dtype.Free a);
        Proposal.dtype = Dtype.instantiate (Dtype.Free a) body;
      } in
      specialize (root <+ "step") context prop (a :: vars) sp
    | Dtype.Quant (q, k, body) when q = Dtype.ForAll && k = Dtype.KSens ->
      let s = root <+ "sens" in
      let f = proposal.Proposal.solution in
      (*  *)
      let prop = {
        Proposal.solution = Fterm.SensApp (f, Sensitivity.Free s);
        Proposal.dtype = Dtype.instantiate_sens (Sensitivity.Free s) body;
      } in
      specialize (root <+ "step") context prop vars sp
    | _ -> None
