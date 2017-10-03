open Signature

(* each module will maintain several lists of primitives *)
type t = primitive list

(* basics first *)
module Basic = struct
  open Dtype.Alt

  let succ = {
    name = "succ";
    dtype = si (1, real) -* real;
    source = Value.F (fun v -> match v with
        | Value.Real r -> Value.Real (r +. 1.0)
        | _ -> failwith "can't eval on this type")
  }

  let square = {
    name = "square";
    dtype = real => real;
    source = Value.F (fun v -> match v with
        | Value.Real r -> Value.Real (r *. r)
        | _ -> failwith "can't eval on this type")
  }

  let double = {
    name = "double";
    dtype = si (2, real) -* real;
    source = Value.F (fun v -> match v with
        | Value.Real r -> Value.Real (r *. 2.0)
        | _ -> failwith "can't eval on this type")
  }

  let signature = [succ; square; double]
end
