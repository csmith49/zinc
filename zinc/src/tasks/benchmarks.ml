(* benchmarks maintain all the info we need to test synthesis *)
type t = {
  name : string;
  goal_type : Dtype.t;
  io_examples : (Value.t * Value.t) list;
  search_grammar : Signature.primitive list;
}

open Dtype.Alt

(* PUT BENCHMARKS HERE *)
let basic_example_01 = {
  name = "basic_01";
  goal_type = exists (k, s (k, real) -* real);
  io_examples = [(Value.Real 0.0, Value.Real 1.0); (Value.Real 2.0, Value.Real 5.0)];
  search_grammar = Primitives.Basic.signature;
}


(* PUT BENCHMARK LISTS HERE *)

let basic = [basic_example_01]
