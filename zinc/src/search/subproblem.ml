(* subproblems maintain all the necessary info to construct a node *)
(* via replacing a particular wildcard that exists at t.hole *)
type t = {
    root : Name.t;
    obligation : Constraint.t;
    hole : Zipper.t;
    context : Context.t;
    goal : Dtype.t;
}

type subproblem = t

open Name.Alt

let of_node (root : Name.t) (n : Node.t) : t = match n.Node.solution with
  | Vterm.Wild (context, dt, body) ->
    let wildcard = Vterm.Var (Vterm.Free (root <+ "HOLE")) in
    let body = Vterm.instantiate_one wildcard body in
    let _ = print_endline (Vterm.format body) in
    let zipper_pred tm = match tm with
      | Vterm.Var (Vterm.Free n) -> Name.eq n (root <+ "HOLE")
      | _ -> false in
    let zipper = Zipper.preorder_until root "x" zipper_pred (Zipper.of_term body) in
    begin match zipper with
      | Some z -> {
        root = root <+ "node";
        obligation = n.Node.obligation;
        hole = z;
        context = context;
        goal = dt;
      }
      | _ -> failwith "can't find bound wildcard"
      end
    | _ -> failwith "no toplevel wildcard"

module Proposal = struct
  type t = {
    solution : Vterm.t;
    dtype : Dtype.t;
    wildcards : Zipper.branch list;
    context : Context.t;
    obligation : Constraint.t;
  }

  let to_string (p : t) : string =
    let tm = Vterm.to_string p.solution in
    let dt = Dtype.to_string p.dtype in
      tm ^ " : " ^ dt

  open Context.Alt
  open Constraint.Alt

  let variables : subproblem -> t list = fun sp ->
    let f = fun (n, dt) -> {
      solution = Vterm.Var (Vterm.Free n);
      dtype = dt;
      wildcards = [];
      context = Context.concrete_of_var n;
      obligation = Constraint.Top;
    } in
    let vars = Zipper.scope sp.hole in
      CCList.map f vars

  let lambda : subproblem -> t option = fun sp -> match sp.goal with
    | Dtype.Func (Dtype.Modal (s, dom), codom) ->
      let w = sp.root <+ "w" in
      let wildcard = Vterm.Var (Vterm.Free w) in
      let tag = sp.root <+ "x" in
      let context = Context.Symbolic (sp.root <+ "c") in
      let bindings = [Zipper.ZWild (context, codom, w)] in Some {
        solution = Vterm.Abs (tag, dom, Vterm.Scope wildcard);
        dtype = sp.goal;
        wildcards = bindings;
        context = context;
        obligation = c_rel (context ==. (sp.context +. (Context.Concrete (tag, s))));
      }
    | _ -> None

  let rec specialize (root : Name.t) (prop : t) (context : Context.t) : t list =
    let recurse = match prop.dtype with
      | Dtype.Func (Dtype.Modal (s, dom), codom) ->
        let root = root <+ "step_abs" in
        let c = Context.Symbolic (root <+ "c") in
        let c_wild = Context.Symbolic (root <+ "c_wild") in
        let w = root <+ "wild" in
        let binding = Zipper.ZWild (c_wild, dom, w) in
        let f = prop.solution in
        (* construct recursive call *)
        let p = {
          solution = Vterm.App (f, Vterm.Var (Vterm.Free w));
          dtype = codom;
          wildcards = binding :: prop.wildcards;
          context = c;
          obligation = 
            prop.obligation & c_rel (c ==. (Context.Linear (prop.context, s, c_wild)));
        } in specialize root p context
      | Dtype.Quant (q, k, body) when q = Dtype.ForAll && k = Dtype.KType ->
        let root = root <+ "step_tyabs" in
        let c = Context.Symbolic (root <+ "c") in
        let a = root <+ "type" in
        let f = prop.solution in
        let p = {
          solution = Vterm.TypeApp (f, Dtype.Free a);
          dtype = Dtype.instantiate (Dtype.Free a) body;
          wildcards = prop.wildcards;
          context = c;
          obligation = c_rel (c ==. prop.context);
        } in specialize root p context
      | Dtype.Quant (q, k, body) when q = Dtype.ForAll && k = Dtype.KSens ->
        let root = root <+ "step_sensabs" in
        let c = Context.Symbolic (root <+ "c") in
        let s = root <+ "sens" in
        let f = prop.solution in
        let p = {
          solution = Vterm.SensApp (f, Sensitivity.Free s);
          dtype = Dtype.instantiate_sens (Sensitivity.Free s) body;
          wildcards = prop.wildcards;
          context = c;
          obligation = c_rel (c ==. prop.context);
        } in specialize root p context
      | _ -> []
    in {prop with obligation = prop.obligation & (c_rel (context ==. prop.context));} :: recurse
end

let insert_proposal (p : Proposal.t) (subprob : t) : Node.t option = let open Constraint.Alt in
  match Inference.subtype_unify subprob.root subprob.goal p.Proposal.dtype with
    | Some (phi, sub) -> Some {
      Node.root = subprob.root;
      obligation = phi & p.Proposal.obligation & subprob.obligation;
      solution =
        let body = Zipper.set p.solution subprob.hole |> Zipper.to_term in
        let tm = Zipper.to_term (body, p.wildcards) in
          Inference.Sub.apply_vterm sub tm
    }
    | _ -> None