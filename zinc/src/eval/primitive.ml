(* primitives represent the functions we're synthesizing over *)
type t = {
  name : string;
  dtype : Dtype.t;
  source : Value.t;
}

(* step one is being able to convert primitives to proposals *)
let to_proposal : t -> Proposal.t = fun p -> {
    Proposal.solution = Fterm.Prim (p.name, p.source);
    Proposal.dtype = p.dtype;
    Proposal.wildcards = [];
    Proposal.context = Context.Empty;
    Proposal.obligation = Constraint.Top;
  }

module Utility = struct
  open Make
  (* we do a lot of projection over rows *)
  let projection (n : string) (dt : Dtype.t) : t = {
    name = n;
    dtype = row => dt;
    source = Value.F (fun v -> match v with
      | Value.Row r -> Value.StringMap.find n r
      | _ -> failwith "not a row")
  }

  (* and conversions from a type to another *)
  let conversion (n : string) (dom : Dtype.t) (sens : int) (codom : Dtype.t) : t = {
    name = n;
    dtype = modal (Sensitivity.Const (Rational.of_int sens), dom) -* codom;
    source = Value.F (fun v -> v);
  }

  let bounded_conversion (n : string) (dom: Dtype.t) (sens : int) : t = {
    name = n;
    dtype = modal (one, dom) -* (bounded_by sens);
    source = Value.F (fun v -> v);
  }

  let discrete_check (n : string) (goal : string) (goal_type : Dtype.t) : t = {
    name = n;
    dtype = goal_type => bool;
    source = Value.F (fun v -> match v with
      | Value.Discrete s -> Value.Bool (s = goal)
      | _ -> failwith "not a discrete type")
  }
end