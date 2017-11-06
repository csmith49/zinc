open Make

(* we have some utility functions for treating values a little more cleanly *)
let binarize (f : Value.abstraction) = fun x -> fun y -> match (f x) with
  | Value.F f' -> f' y
  | _ -> failwith "can't binarize"

let unpack_real (r : Value.t) : float = match r with
  | Value.Real f -> f
  | Value.Int i -> float_of_int i
  | _ -> failwith "not a real number"

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

  let cast_int = {
    Primitive.name = "cast";
    Primitive.dtype = modal (one, int) -* real;
    Primitive.source = Value.F (fun v -> match v with
      | Value.Int i -> Value.Real (float_of_int i)
      | _ -> failwith "can't cast a non-int");
  }

  let big = {
    Primitive.name = "big";
    Primitive.dtype = real => bool;
    Primitive.source = Value.F (fun v -> match v with
      | Value.Real r -> Value.Bool (r >= 10.0)
      | _ -> failwith "can't check if a non-real is big");
  }

  let signature = [succ; square; double; cast_int; big]
end

(* some polymorphic functions *)
module MapReduce = struct
  let filter = {
    Primitive.name = "filter";
    Primitive.dtype = 
      tbind (a, sbind (n,
        modal (n, a => bool) -* (modal (one, mset (a, n)) -* mset (a, infinity))
      ));
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

  let reduce = {
    Primitive.name = "reduce";
    Primitive.dtype = 
      sbind (s, sbind (n,
        modal (n, modal (s, real) -* (modal (s, real) -* real)) -* (modal (s, mset (bounded, n)) -* real)
      ));
    Primitive.source = Value.F (fun v -> match v with
      | Value.F f -> Value.F (fun v -> match v with
        | Value.Bag ts -> begin match ts with
          | [] -> Value.Real 0.0
          | _ -> CCList.fold_left (binarize f) (Value.Real 0.0) ts
        end
        | _ -> failwith "can't apply reduce to a non-bag")
      | _ -> failwith "can't apply a non-function reducer"
    );
  }

  let signature = [filter; map; reduce]
end

module Aggregate = struct
  let count = {
    Primitive.name = "count";
    dtype = tbind (a, sbind (n, modal (one, mset (a, n)) -* int));
    source = Value.F (fun v -> match v with
      | Value.Bag ts -> Value.Int (CCList.length ts)
      | _ -> failwith "can't count a non-bag");
  }

  let sum = {
    Primitive.name = "sum";
    dtype = sbind (n, modal (one, mset (bounded, n)) -* real);
    source = Value.F (fun v -> match v with
      | Value.Bag ts -> Value.Real (CCList.fold_left (+.) 0.0 (CCList.map unpack_real ts))
      | _ -> failwith "can't sum a non-bag");
  }

  let average = {
    Primitive.name = "average";
    dtype = sbind (n, modal (one, mset (bounded, n)) -* real);
    source = Value.F (fun v -> match v with
      | Value.Bag ts ->
        let total = CCList.fold_left (+.) 0.0 (CCList.map unpack_real ts) in
        let count = float_of_int (CCList.length ts) in
          Value.Real (total /. count)
      | _ -> failwith "can't average a non-bag");
  }

  let signature = [count; sum; average]
end