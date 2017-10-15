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

(* pull the benchmark from the arguments *)
let benchmark = extract_benchmark !benchmark_name Benchmark.basic;;

(* consruct the frontier from the benchmarks start position *)
module Frontier = Pqueue.Make(Search.Node.Priority);;

let start_node = Benchmark.to_node benchmark;;
let frontier = Frontier.push (Search.Node.to_priority start_node) (start_node) Frontier.empty;;

print_endline (Fterm.to_string start_node.Search.Node.solution)
