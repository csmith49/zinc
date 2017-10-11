let signature : (string * Value.t * Dtype.t) list ref = ref [];

module Stub = struct
  type t = Fterm.Prefix.t * Fterm.t
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
