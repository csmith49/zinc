open Signature
open Inference
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
module Frontier = Pqueue.Make(Node.Priority)
let frontier = ref Frontier.empty

(* an exception we throw to exit the loop *)
exception SynthSuccess of Fterm.t

(* the infinite loop, in a function we can back out of *)
let synthesize (bm : Benchmark.t) : unit =
  (* generate start node and initialize frontier *)
  let start_node = Benchmark.to_node bm in
  let _ = frontier := Frontier.push (Node.to_priority start_node) start_node !frontier in
  let primitive_proposals = CCList.map Primitive.to_proposal benchmark.Benchmark.search_grammar in
  (* repeatedly pull nodes and check for satisfiability *)
  while true do
    (* pull from frontier *)
    let node, p, frontier' = Frontier.pop !frontier in
    let _ = frontier := frontier' in
    let tm = node.Node.solution in
    (* PRINTING *)
    let _ = print_endline ("Checking: " ^ (Fterm.to_string tm)) in
    (* check if tm is a solution *)
    if (Fterm.wild_closed tm) then
      if (Benchmark.verify tm bm.Benchmark.io_examples) then raise (SynthSuccess tm) else ()
    (* if not, and there's a wild binder, find all expansions *)
    else
      let subproblem = Subproblem.of_node (Name.of_string "w") node in
      let proposals = primitive_proposals @ (Subproblem.variable_proposals subproblem) @ (CCOpt.to_list (Subproblem.lambda_proposal subproblem)) in
      let root = Name.of_string "specialize" in
      (* PRINTING *)
      let f = fun p -> Subproblem.specialize root p in
      let solutions = CCList.flat_map f proposals in
      let steps = CCList.filter_map (fun p -> Subproblem.insert_proposal p subproblem) solutions in
        CCList.iteri (fun i -> fun n -> 
        frontier := Frontier.push (Node.to_priority n) {n with Node.root = Stack.Cons (Name.Id ("n", i), n.Node.root);} !frontier) steps
  done;;

(* run the experiment, and catch the output *)
try synthesize benchmark with
  | SynthSuccess tm -> print_endline ("Solution found: " ^ (Fterm.to_string tm))
