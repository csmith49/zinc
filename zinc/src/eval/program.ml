type t = (Value.t, Dtype.t) Term.t

let rec eval (p : t) : Value.t = match p with
  | Term.App (f, args) -> begin match (eval f) with
      | Value.F f' -> f' (eval args)
      | _ -> failwith "can't apply a non-abstraction"
  end
  | Term.Abs (dom, body) -> let f v = eval (Term.instantiate (Term.Const v) body) in Value.F f
  | Term.Const v -> v
  | _ -> failwith "can't evaluate provided program"
