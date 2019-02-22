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