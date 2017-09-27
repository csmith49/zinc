let signature : (string * Value.t * Dtype.t) list ref = ref [];

module Stub = struct
  type t = Term.Prefix.t * Term.t
end

module Problem = struct
  open Name.Alt
  (* all the things we need to maintain to find type-safe substitutions *)
  type t = {
    root : Name.t;
    hole : Term.Zipper.t;
    context : Context.t;
    goal : Dtype.t;
  }
  (* our primary conversions is from terms *)
  let of_term (root : Name.t) : Term.t -> t = function
    | Term.Wild (context, dom, body) ->
      let wildcard = root <+ "wild" in
      let body' = Term.instantiate (Term.Free wildcard) body in
      let zipper = Term.Zipper.preorder_until root "x" (Term.is_variable wildcard) (Term.Zipper.of_term body') in
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
  let insert_stub (stub : Stub.t) (problem : t) : Term.t = match stub with
    | (prefix, tm) ->
      let tm' = Term.Zipper.to_term (Term.Zipper.set tm problem.hole) in
      Term.Prefix.bind prefix tm'
  (* one of the more important bits - how do we solve a problem *)
  (* we're looking for stubs that, when inserted, yield a type-safe term *)
  let find_stubs : t -> (Stub.t * Constraint.t) list = fun p -> []
end
