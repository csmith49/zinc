type t = {
  solution : Fterm.t;
  dtype : Dtype.t;
  wildcards : Fterm.Prefix.t;
  context : Context.t;
  obligation : Constraint.t;
}

let to_string (p : t) : string =
  let tm = Fterm.to_string p.solution in
  let dt = Dtype.to_string p.dtype in
    tm ^ " : " ^ dt