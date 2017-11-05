(* benchmarks maintain all the info we need to test synthesis *)
type t = {
  name : string;
  goal_type : Dtype.t;
  io_examples : (Value.t * Value.t) list;
  search_grammar : Primitive.t list;
}

open Constraint.Alt
open Name.Alt

open Make

(* to intialize the search, we convert benchmarks to nodes *)
let to_node : t -> Node.t = fun b -> {
    Node.root = Name.of_string "start";
    Node.obligation = (k <= (num 10));
    Node.solution =
      let context = Context.Empty in
      let dtype = b.goal_type in
      let root = Name.of_string b.name in
      let w = root <+ "wild" in
      Fterm.Wild (context, dtype, Fterm.abstract w (Fterm.Free w));
  }

(* benchmarks will contain IO examples - we want to verify a solution *)
let verify (tm : Fterm.t) (io : (Value.t * Value.t) list) : bool =
  let check_pair = fun (i, o) -> let o' = Fterm.eval (Fterm.App (tm, Fterm.Const i)) in o = o' in
  CCList.for_all check_pair io

(* PUT BENCHMARKS HERE *)
let basic_example_01 = {
  name = "basic_01";
  goal_type = modal (k, real) -* real;
  io_examples = [(Value.Real 0.0, Value.Real 1.0); (Value.Real 2.0, Value.Real 5.0)];
  search_grammar = Signature.Basic.signature;
}

let basic_example_02 = {
  name = "basic_02";
  goal_type = modal (k, mset (real, infinity)) -* int;
  io_examples = [(Value.Bag [Value.Real 0.0; Value.Real 11.0], Value.Int 1)];
  search_grammar = Signature.Basic.signature @ Signature.MapReduce.signature;
}

(* PUT BENCHMARK LISTS HERE *)

let basic = [basic_example_01; basic_example_02]
