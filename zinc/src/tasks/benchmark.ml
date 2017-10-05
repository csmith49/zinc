(* benchmarks maintain all the info we need to test synthesis *)
type t = {
  name : string;
  goal_type : Dtype.t;
  io_examples : (Value.t * Value.t) list;
  search_grammar : Signature.primitive list;
}

(* benchmarks will contain IO examples - we want to verify a solution *)
let verify (tm : Fterm.t) (io : (Value.t * Value.t) list) : bool =
  let check_pair = fun (i, o) -> let o' = Fterm.eval (Fterm.App (tm, Fterm.Const i)) in o = o' in
  CCList.for_all check_pair io

open Dtype.Alt

(* PUT BENCHMARKS HERE *)
let basic_example_01 = {
  name = "basic_01";
  goal_type = s (k, real) -* real;
  io_examples = [(Value.Real 0.0, Value.Real 1.0); (Value.Real 2.0, Value.Real 5.0)];
  search_grammar = Primitives.Basic.signature;
}


(* PUT BENCHMARK LISTS HERE *)

let basic = [basic_example_01]
