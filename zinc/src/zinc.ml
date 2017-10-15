open Signature
open Inference
open Search
open Solver

(* references for command line arguments *)
let benchmark_name = ref "";;

(* the actual command line arguments *)
let spec_list = [
  ("-bm", Arg.Set_string benchmark_name, "Sets the benchmark to test");
];;

(* populate the references - no anonymous functions *)
let usage_message = "";;
let anon_function = fun s -> ();;
Arg.parse (Arg.align spec_list) anon_function usage_message;;

(* main loop happens here *)
let rec extract_benchmark (name : string) (bs : Benchmark.t list) : Benchmark.t = match bs with
  | b :: bs' -> if b.Benchmark.name = name then b else extract_benchmark name bs'
  | _ -> failwith "can't find provided benchmark";;

let benchmark = extract_benchmark !benchmark_name Benchmark.basic;;

let wild = Name.of_string "wild";;
let start = Fterm.Wild (
    Context.Empty,
    benchmark.Benchmark.goal_type,
    Fterm.abstract wild (Fterm.Free wild));;

print_endline (Fterm.to_string start)
