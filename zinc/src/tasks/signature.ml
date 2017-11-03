open Make

(* each module will maintain several lists of primitives *)
type t = Primitive.t list

(* basics first *)
module Basic = struct
  let succ = {
    Primitive.name = "succ";
    Primitive.dtype = modal (one, real) -* real;
    Primitive.source = Value.F (fun v -> match v with
        | Value.Real r -> Value.Real (r +. 1.0)
        | _ -> failwith "can't eval on this type")
  }

  let square = {
    Primitive.name = "square";
    Primitive.dtype = real => real;
    Primitive.source = Value.F (fun v -> match v with
        | Value.Real r -> Value.Real (r *. r)
        | _ -> failwith "can't eval on this type")
  }

  let double = {
    Primitive.name = "double";
    Primitive.dtype = modal (two, real) -* real;
    Primitive.source = Value.F (fun v -> match v with
        | Value.Real r -> Value.Real (r *. 2.0)
        | _ -> failwith "can't eval on this type")
  }

  let signature = [succ; square; double]
end

(* some polymorphic functions *)
module MapReduce = struct
  let filter = {
    Primitive.name = "filter";
    Primitive.dtype = 
      tbind (a, sbind (s, sbind (n,
        modal (n, modal (s, a) -* bool) -* (modal (s, mset (a, n)) -* mset (a, infinity))
      )));
    Primitive.source = Value.F (fun v -> match v with
      | Value.F p -> Value.F (fun v -> match v with
        | Value.Bag ts -> Value.Bag (CCList.filter (fun b -> (p b) = (Value.Bool true)) ts)
        | _ -> failwith "can't apply filter to a non-bag")
      | _ -> failwith "can't apply a non-function predicate");
  }

  let map = {
    Primitive.name = "map";
    Primitive.dtype =
      (* is this actually the right type? what's the sensitivity on the mset input? *)
      tbind (a, tbind (b, sbind (s, sbind (n, 
        modal (n, modal (s, a) -* b) -* (modal (one, mset (a, n)) -* mset (b, n))
      ))));
    Primitive.source = Value.F (fun v -> match v with
      | Value.F f -> Value.F (fun v -> match v with
        | Value.Bag ts -> Value.Bag (CCList.map f ts)
        | _ -> failwith "can't apply map to a non-bag")
      | _ -> failwith "can't apply a non-function mapper");
  }

  let count = {
    Primitive.name = "count";
    Primitive.dtype =
      tbind (a, sbind (n, 
        modal (one, mset (a, n)) -* int
      ));
    Primitive.source = Value.F (fun v -> match v with
      | Value.Bag ts -> Value.Int (CCList.length ts)
      | _ -> failwith "can't count a non-bag");
  }
end