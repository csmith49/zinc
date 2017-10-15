(* what we're searching over *)
module Node = struct
  (* mostly we just maintain a partial solution (w/ wildcards) and the proof obligation to satisfty *)
  type t = {
    obligation : Constraint.t;
    solution : Fterm.t;
  }
  (* we'll need some heuristics to guide the search *)
  let size : t -> int = fun n -> Fterm.size n.solution
  (* we do comparisons through priority *)
  module Priority = struct
    (* we basically just tuple on size / value *)
    type t = int * Fterm.t
    (* comparison is size first *)
    let compare (l : t) (r : t) : int = match l, r with
      | (li, lt), (ri, rt) -> match Pervasives.compare li ri with
        | 0 -> Pervasives.compare lt rt
        | (_ as c) -> c
  end
  (* conversion to priority *)
  let to_priority : t -> Priority.t = fun n -> (size n, n.solution)
  (* comparison through priority *)
  let compare (l : t) (r : t) : int = Priority.compare (to_priority l) (to_priority r)
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

(* these make the specialization def a little easier to grok *)
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
    Some ({
        Node.obligation = c & sp.Subproblem.obligation;
        Node.solution = Inference.Sub.apply_fterm sub proposal.Proposal.solution
      })
  else match proposal.Proposal.dtype with
    | Dtype.Func (Dtype.Modal (s, dom), codom) ->
      (* base name creation and whatnot *)
      let c = Context.Symbolic (root <+ "context") in
      let w = root <+ "wild" in
      let binding = Fterm.Prefix.BWild (w, c, dom) in
      let f = proposal.Proposal.solution in
      (* construct the new proposal *)
      let prop = {
        Proposal.solution = binding @> Fterm.App (f, Fterm.Free w);
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
