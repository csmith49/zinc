(* each module will maintain several lists of primitives *)
type t = Primitive.t list

(* basics first *)
module Basic = struct
  open Dtype.Alt

  let succ = {
    Primitive.name = "succ";
    Primitive.dtype = si (1, real) -* real;
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
    Primitive.dtype = si (2, real) -* real;
    Primitive.source = Value.F (fun v -> match v with
        | Value.Real r -> Value.Real (r *. 2.0)
        | _ -> failwith "can't eval on this type")
  }

  let signature = [succ; square; double]
end
