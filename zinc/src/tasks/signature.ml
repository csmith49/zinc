open Make

(* we have some utility functions for treating values a little more cleanly *)
let binarize (f : Value.abstraction) = fun x -> fun y -> match (f x) with
  | Value.F f' -> f' y
  | _ -> failwith "can't binarize"

(* and pulling out reals so we can map over a list *)
let unpack_real (r : Value.t) : float = match r with
  | Value.Real f -> f
  | Value.Int i -> float_of_int i
  | _ -> failwith "not a real number"

(* we'll do a lot of projection over rows *)
let projection (n : string) (dt : Dtype.t) : Primitive.t = {
  Primitive.name = n;
  dtype = row => dt;
  source = Value.F (fun v -> match v with
    | Value.Row r -> Value.StringMap.find n r
    | _ -> failwith "not a row")
}

let discrete_constant (n : string) (dt : Dtype.t) : Primitive.t = {
  Primitive.name = n;
  dtype = dt;
  source = Value.Discrete n;
}

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
    dtype = tbind (a, sbind (n, modal (one, mset (a, n)) -* real));
    source = Value.F (fun v -> match v with
      | Value.Bag ts -> Value.Real (float_of_int (CCList.length ts))
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

(* combinatorics for defining predicates *)
module Predicate = struct
  (* some simple helpers *)
  let value_and (l : Value.t) (r : Value.t) : Value.t = match l, r with
    | Value.Bool p, Value.Bool q -> Value.Bool (p && q)
    | _ -> failwith "can't and non-bools"
  let value_or (l : Value.t) (r : Value.t) : Value.t = match l, r with
    | Value.Bool p, Value.Bool q -> Value.Bool (p && q)
    | _ -> failwith "can't or non-bools"
  let value_not (b : Value.t) : Value.t = match b with
    | Value.Bool b -> Value.Bool (not b)
    | _ -> failwith "can't not non-bools"

  let pred_and = {
    Primitive.name = "and";
    dtype = tbind (a, 
      (a => bool) => ((a => bool) => (a => bool))
    );
    source = Value.F (fun v -> match v with
      | Value.F p1 -> Value.F (fun v -> match v with
        | Value.F p2 -> Value.F (fun a -> value_and (p1 a) (p2 a))
        | _ -> failwith "not a func")
      | _ -> failwith "not a func");
  }

  let pred_or = {
    Primitive.name = "or";
    dtype = tbind (a, 
      (a => bool) => ((a => bool) => (a => bool))
    );
    source = Value.F (fun v -> match v with
      | Value.F p1 -> Value.F (fun v -> match v with
        | Value.F p2 -> Value.F (fun a -> value_or (p1 a) (p2 a))
        | _ -> failwith "not a func")
      | _ -> failwith "not a func");
  }

  let pred_not = {
    Primitive.name = "not";
    dtype = tbind (a,
      (a => bool) => (a => bool)
    );
    source = Value.F (fun v -> match v with
      | Value.F p -> Value.F (fun v -> value_not (p v))
      | _ -> failwith "not a func");
  }

  let signature = [pred_and; pred_or; pred_not]
end

module Constants = struct
  let value_true = {
    Primitive.name = "true";
    dtype = bool;
    source = Value.Bool true
  }
  let value_false = {
    Primitive.name = "false";
    dtype = bool;
    source = Value.Bool false;
  }

  let signature = [value_true; value_false]
end

module Database = struct
  let compare = {
    Primitive.name = "compare";
    Primitive.dtype = tbind (a, row => ((row => a) => (a => bool)));
    Primitive.source = Value.F (fun v -> match v with
      | Value.Row r -> Value.F (fun v -> match v with
        | Value.F p -> Value.F (fun v -> Value.Bool (p (Value.Row r) == v))
        | _ -> failwith "not a function")
      | _ -> failwith "not a row");
  }

  let signature = [compare]
end

(* signature for ADULT benchmarks *)
module Adult = struct
  (* the dtype encodings *)
  let gender_t = constant_type "Gender"
  let race_t = constant_type "Race"
  let hours_t = constant_type "Hours"
  let profession_t = constant_type "Profession"
  let work_class_t = constant_type "Work Class"
  let education_t = constant_type "Education"

  (* all the keys in our dataset *)
  let gt_50k = projection "gt_50k" bool
  let gender = projection "gender" gender_t
  let race = projection "race" race_t
  let work_hours = projection "work_hours" hours_t
  let education_level = projection "education_level" education_t
  let profession = projection "profession" profession_t
  let work_class = projection "work_class" work_class_t
  let capital_gains = projection "capital_gains" real

  let keys = [gt_50k; race; gender; work_hours; education_level; profession; work_class; capital_gains]

  (* with simple conversions *)
  let hours_to_value = {
    Primitive.name = "hours_to_value";
    dtype = modal (Sensitivity.Const (Rational.of_int 168), hours_t) -* real;
    source = Value.F (fun v -> v);
  }
  let education_to_value = {
    Primitive.name = "edu_to_value";
    dtype = modal (Sensitivity.Const (Rational.of_int 20), education_t) -* bounded;
    source = Value.F (fun v -> v);
  }

  (* and some simple predicates for the search *)
  let gt_40_hrs = {
    Primitive.name = "gt_40_hrs";
    dtype = hours_t => bool;
    source = Value.F (fun v -> match v with
      | Value.Real r -> Value.Bool (r >= 40.0)
      | _ -> failwith "not an hour");
  }
  let is_female = {
    Primitive.name = "is_female";
    dtype = gender_t => bool;
    source = Value.F (fun v -> match v with
      | Value.Discrete s -> Value.Bool (s = "female")
      | _ -> failwith "not a gender");
  }
  let is_army = {
    Primitive.name = "is_army";
    dtype = profession_t => bool;
    source = Value.F (fun v -> match v with
      | Value.Discrete s -> Value.Bool (s = "army")
      | _ -> failwith "not a profession");
  }
  let is_trade = {
    Primitive.name = "is_trade";
    dtype = profession_t => bool;
    source = Value.F (fun v -> match v with
      | Value.Discrete s -> Value.Bool (s = "trade")
      | _ -> failwith "not a profession")
  }

  (* the total signature *)
  let signature = gt_40_hrs :: is_female ::is_army :: is_trade :: education_to_value :: keys
  
  (* and a utility for constructing examples *)
  let schema = ["gt_50k"; "gender"; "race"; "work_hours"; "education_level"; "profession"; "work_class"; "capital_gains"]
  let make (vs : Value.t list) : Value.t = Value.row_of_list (CCList.combine schema vs)
end

(* arithmetic *)
module Arithmetic = struct
  let add = {
    Primitive.name = "add";
    dtype = sbind (s, sbind (n,
      modal (one, real) -* (modal (one, real) -* real)
    ));
    source = Value.F (fun v -> match v with
      | Value.Real l -> Value.F (fun v -> match v with
        | Value.Real r -> Value.Real (l +. r)
        | _ -> failwith "not a number")
      | _ -> failwith "not a number");
  }
  let mult = {
    Primitive.name = "mult";
    dtype = sbind (s, sbind (n,
      modal (s, p_real n) -* (modal (n, p_real s) -* real)
    ));
    source = Value.F (fun v -> match v with
      | Value.Real l -> Value.F (fun v -> match v with
        | Value.Real r -> Value.Real (l *. r)
        | _ -> failwith "not a number")
      | _ -> failwith "not a number");
  }
  let bad_mult = {
    Primitive.name = "bad_mult";
    dtype = sbind (s, sbind (n,
      (p_real s) => ( (p_real n) => real)
    ));
    source = Value.F (fun v -> match v with
    | Value.Real l -> Value.F (fun v -> match v with
      | Value.Real r -> Value.Real (l *. r)
      | _ -> failwith "not a number")
    | _ -> failwith "not a number");
  }

  let signature = [add; mult]
end