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
      let tag = {
        Vterm.a_var = sp.root <+ "x";
        a_dt = dom;
      } in
      let context = Context.Symbolic (sp.root <+ "c") in
      let bindings = [Zipper.ZWild (context, codom, w)] in Some {
        solution = Vterm.Abs (tag, Vterm.Scope wildcard);
        dtype = sp.goal;
        wildcards = bindings;
        context = context;
        obligation = c_rel (context ==. (sp.context +. (Context.Concrete (tag.a_var, s))));
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

  let prob_do : subproblem -> t option = fun sp -> match sp.goal with
    | Dtype.Monad _ -> None
    | (_ as dt) ->
      let w = sp.root <+ "w_do" in
      let binding = Zipper.ZWild (sp.context, Dtype.Monad dt, w) in Some {
        solution = Vterm.Do (Vterm.Var (Vterm.Free w));
        dtype = dt;
        wildcards = [binding];
        context = sp.context;
        obligation = Constraint.Top;
      }
  let prob_return : subproblem -> t option = fun sp -> match sp.goal with
    | Dtype.Monad dt ->
      let w = sp.root <+ "w_ret" in
      let c = Context.Symbolic (sp.root <+ "c_ret") in
      let binding = Zipper.ZWild (c, dt, w) in Some {
        solution = Vterm.Return (Vterm.Var (Vterm.Free w));
        dtype = sp.goal;
        wildcards = [binding];
        context = c;
        obligation = c_rel (sp.context ==. (Sensitivity.Const (Rational.Infinity)) *. c);
      }
    | _ -> None
  let prob_draw : subproblem -> t option = fun sp -> match sp.goal with
    | Dtype.Monad dt ->
      (* what are we going to call the sampled value *)
      let x = sp.root <+ "x" in
      (* making wildcards *)
      let w_dist = sp.root <+ "w_dist" in
      let w_usage = sp.root <+ "w_usage" in
      (* the fresh type constants *)
      let tau = Dtype.Free (sp.root <+ "tau") in
      (* and the relevant bindings *)
      let c_dist = Context.Symbolic (sp.root <+ "c_dist") in
      let c_usage = Context.Symbolic (sp.root <+ "c_usage") in
      let c_usage' = Context.Symbolic (sp.root <+ "c_usage'") in
      let dist_binding = Zipper.ZWild (c_dist, Dtype.Monad tau, w_dist) in
      let usage_binding = Zipper.ZWild (c_usage, sp.goal, w_usage) in
      (* making tag *)
      let tag = {
        Vterm.d_var = x;
        d_dt = tau;
      } in Some {
        solution = Vterm.LetDraw (tag, Vterm.Var (Vterm.Free w_dist), Vterm.Scope (Vterm.Var (Vterm.Free w_usage)));
        dtype = sp.goal;
        wildcards = [dist_binding ; usage_binding];
        context = c_dist +. c_usage';
        obligation = c_rel (c_usage ==. Context.Join (
          c_usage', Sensitivity.Const (Rational.Infinity) *. (Context.concrete_of_var x)
        ))
      }
    | _ -> None

  (* pattern matching stuff *)
  let match_nat : subproblem -> t list = fun sp ->
    let from_var (n, dt) = begin match dt with
      | Dtype.Precise (Dtype.Natural (Sensitivity.Free s)) ->
        (* names for matched variables and whatnot *)
        let root = Name.extend_by_name sp.root n in
        let x = root <+ "nat" in
        let i = Sensitivity.Free (root <+ "i") in
        let r = Sensitivity.Free (root <+ "r") in
        (* constructing the tag *)
        let tag = {
          Vterm.n_var = x; 
          n_sens = i;
        } in
        (* construct the wildcards *)
        let w_zero = root <+ "w_zero" in
        let w_succ = root <+ "w_succ" in
        (* and the bindings *)
        let c_zero = Context.Symbolic (root <+ "c_zero") in
        let c_succ = Context.Symbolic (root <+ "c_succ") in
        let c_plain = Context.Symbolic (root <+ "c_nat_plain") in
        let dt_zero = sp.goal
          |> Dtype.abstract s
          |> Dtype.instantiate_sens (Sensitivity.Alt.zero) in
        let dt_succ = sp.goal
          |> Dtype.abstract s
          |> Dtype.instantiate_sens (Sensitivity.Plus (i, Sensitivity.Alt.one)) in
        let zero_binding = Zipper.ZWild (c_zero, dt_zero, w_zero) in
        let succ_binding = Zipper.ZWild (c_succ, dt_succ, w_succ) in Some {
          solution = Vterm.MatchNat (tag, 
            Vterm.Var (Vterm.Free n), 
            Vterm.Var (Vterm.Free w_zero),
            Vterm.Scope (Vterm.Var (Vterm.Free w_succ)));
          dtype = sp.goal;
          wildcards = [zero_binding ; succ_binding];
          context = sp.context;
          obligation =
            c_rel (sp.context ==. c_plain +. (r *. (Context.concrete_of_var n))) &
            c_rel (c_zero ==. Context.Substitution (c_plain, Sensitivity.Sub.of_list [(s, Sensitivity.Alt.zero)])) &
            c_rel (c_succ ==. Context.Join (
              Context.Substitution (
                c_plain, 
                Sensitivity.Sub.of_list [(s, Sensitivity.Plus (i, Sensitivity.Alt.one))]),
              r *. Context.concrete_of_var x)
              );
        }
      | _ -> None
      end in
    CCList.filter_map from_var (Zipper.scope sp.hole)

  let match_cons : subproblem -> t list = fun sp ->
    let from_var (n, dt) = begin match dt with
      | Dtype.Precise (Dtype.List (Sensitivity.Free s, tau)) ->
        (* names for matched variables and whatnot *)
        let root = Name.extend_by_name sp.root n in
        let y = root <+ "cons_hd" in
        let x = root <+ "cons_tl" in
        let i = Sensitivity.Free (root <+ "i") in
        let r = Sensitivity.Free (root <+ "r") in
        (* constructing the tag *)
        let tag = {
          Vterm.c_hd = y;
          c_tl = x;
          c_dt = tau;
          c_sens = i;
        } in
        (* construct the wildcards *)
        let w_nil = root <+ "w_nil" in
        let w_cons = root <+ "w_cons" in
        (* and the bindings *)
        let c_nil = Context.Symbolic (root <+ "c_nil") in
        let c_cons = Context.Symbolic (root <+ "c_cons") in
        let c_plain = Context.Symbolic (root <+ "c_cons_plain") in
        let dt_nil = sp.goal
          |> Dtype.abstract s
          |> Dtype.instantiate_sens (Sensitivity.Alt.zero) in
        let dt_succ = sp.goal
          |> Dtype.abstract s
          |> Dtype.instantiate_sens (Sensitivity.Plus (i, Sensitivity.Alt.one)) in
        let nil_binding = Zipper.ZWild (c_nil, dt_nil, w_nil) in
        let cons_binding = Zipper.ZWild (c_cons, dt_succ, w_cons) in Some {
          solution = Vterm.MatchCons (tag,
            Vterm.Var (Vterm.Free n),
            Vterm.Var (Vterm.Free w_nil),
            Vterm.Widen (Vterm.Scope (Vterm.Var (Vterm.Free w_cons))));
          dtype = sp.goal;
          wildcards = [nil_binding ; cons_binding];
          context = sp.context;
          obligation =
            c_rel (sp.context ==. c_plain +. (r *. (Context.concrete_of_var n))) &
            c_rel (c_nil ==. Context.Substitution (c_plain, Sensitivity.Sub.of_list [(s, Sensitivity.Alt.zero)])) &
            c_rel (c_cons ==. Context.Join (
              Context.Substitution (
                c_plain,
                Sensitivity.Sub.of_list ([s, Sensitivity.Plus (i, Sensitivity.Alt.one)])
              ), Context.Join (
                r *. (Context.concrete_of_var y),
                r *. (Context.concrete_of_var x)
              )
            ))
        }

      | _ -> None
    end in CCList.filter_map from_var (Zipper.scope sp.hole)
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