(* primitives represent the functions we're actually synthesizing over *)
type t = {
  name : string;
  dtype : Dtype.t;
  source : Vterm.t ;
}

(* step one is being able to convert primitives to proposals *)
let to_proposal : t -> Subproblem.Proposal.t = fun p -> {
    Subproblem.Proposal.solution = p.source;
    dtype = p.dtype;
    wildcards = [];
    context = Context.concrete_of_var (Name.of_string p.name);
    obligation = Constraint.Top;
  }

module Utility = struct
  open Make
  (* we'll do a lot of projection over rows *)
  let projection (n : string) (dt : Dtype.t) : t = {
    name = n;
    dtype = (row dt) => dt;
    source = Vterm.Function (n, fun v -> match v with
      | Vterm.Bag ts -> 
        let find = function
          | Vterm.Pair (Vterm.Discrete s, tm) when s = n -> Some tm
          | _ -> None in
        CCList.filter_map find ts |> CCList.head_opt |> Vterm.Evaluation.of_option
      | _ -> Diverge)
  }

  (* and conversions from a type to another *)
  let conversion (n : string) (dom : Dtype.t) (sens : int) (codom : Dtype.t) : t = {
    name = n;
    dtype = modal (Sensitivity.Const (Rational.of_int sens), dom) -* codom;
    source = Vterm.Function (n, fun v -> Vterm.Value v);
  }

  let bounded_conversion (n : string) (dom: Dtype.t) (sens : int) : t = {
    name = n;
    dtype = modal (one, dom) -* (bounded_by sens);
    source = Vterm.Function (n, fun v -> Vterm.Value v);
  }

  let discrete_check (n : string) (goal : string) (goal_type : Dtype.t) : t = {
    name = n;
    dtype = goal_type => bool;
    source = Vterm.Function (n, fun v -> match v with
      | Vterm.Discrete s when s = goal -> Vterm.Value (Vterm.Bool True)
      | Vterm.Discrete _ -> Vterm.Value (Vterm.Bool False)
      | _ -> Diverge)
  }
end