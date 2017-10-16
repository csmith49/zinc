open Signature
open Inference
open Search
open Solver

(* references for command line arguments *)
let benchmark_name = ref ""

(* the actual command line arguments *)
let spec_list = [
  ("-bm", Arg.Set_string benchmark_name, "Sets the benchmark to test");
]

(* populate the references - no anonymous functions *)
let usage_message = ""
let anon_function = fun s -> ()
let _ = Arg.parse (Arg.align spec_list) anon_function usage_message

(* main loop happens here *)
let rec extract_benchmark (name : string) (bs : Benchmark.t list) : Benchmark.t = match bs with
  | b :: bs' -> if b.Benchmark.name = name then b else extract_benchmark name bs'
  | _ -> failwith "can't find provided benchmark"

(* pull the benchmark from the arguments *)
let benchmark = extract_benchmark !benchmark_name Benchmark.basic

(* consruct the frontier from the benchmarks start position *)
module Frontier = Pqueue.Make(Search.Node.Priority)
let frontier = ref Frontier.empty

(* an exception we throw to exit the loop *)
exception SynthSuccess of Fterm.t

(* the infinite loop, in a function we can back out of *)
let synthesize (bm : Benchmark.t) : unit =
  (* generate start node and initialize frontier *)
  let start_node = Benchmark.to_node bm in
  let _ = frontier := Frontier.push (Search.Node.to_priority start_node) start_node !frontier in
  let primitive_proposals = CCList.map Primitive.to_proposal benchmark.Benchmark.search_grammar in
  (* repeatedly pull nodes and check for satisfiability *)
  while true do
    (* pull from frontier *)
    let node, p, frontier' = Frontier.pop !frontier in
    let tm = node.Search.Node.solution in
    (* check if tm is a solution *)
    if (Fterm.wild_closed tm) then
      if (Benchmark.verify tm bm.Benchmark.io_examples) then raise (SynthSuccess tm) else ()
    (* if not, and there's a wild binder, find all expansions *)
    else
      let subproblem = Search.Subproblem.of_node (Name.of_string "w") node in
      let proposals = primitive_proposals @ (Search.Subproblem.variable_proposals subproblem) in
      let root = Name.of_string "specialize" in
      let f = fun p -> Search.specialize root p subproblem in
      let solutions = CCList.filter_map f proposals in
        CCList.iter (fun n -> frontier := Frontier.push (Search.Node.to_priority n) n !frontier) solutions
  done;;

(* run the experiment, and catch the output *)
try synthesize benchmark with
  | SynthSuccess tm -> print_endline (Fterm.to_string tm)
