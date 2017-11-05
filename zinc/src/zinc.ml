open Signature
open Inference

(* references for command line arguments *)
let benchmark_name = ref ""
let verbosity = ref 1
let pause = ref false

(* functions defining the level of printing *)
let normal_print : string -> unit = fun s -> if !verbosity >= 1 then print_string s else ()
let bm_print : string -> unit = fun s -> if !verbosity >= 3 then print_string s else ()

(* the actual command line arguments *)
let spec_list = [
  ("-bm", Arg.Set_string benchmark_name, "Sets the benchmark to test");
  ("-v", Arg.Set_int verbosity, "Sets the level of verbosity (>= 3 for benchmarking output)");
  ("-p", Arg.Set pause, "Pauses for input after each check");
]

(* populate the references - no anonymous functions *)
let usage_message = ""
let anon_function = fun s -> ()
let _ = Arg.parse (Arg.align spec_list) anon_function usage_message

let _ = normal_print ("Evaluating benchmark: " ^ !benchmark_name ^ "\n")

(* main loop happens here *)
let rec extract_benchmark (name : string) (bs : Benchmark.t list) : Benchmark.t = match bs with
  | b :: bs' -> if b.Benchmark.name = name then b else extract_benchmark name bs'
  | _ -> failwith ("can't find provided benchmark: " ^ name)

(* pull the benchmark from the arguments *)
let benchmark = extract_benchmark !benchmark_name Benchmark.basic

(* construct the strategy *)
module Strategy = Solver.Strategy(Solver.Basic)

(* consruct the frontier from the benchmarks start position *)
module Frontier = Pqueue.Make(Node.Priority)
let frontier = ref Frontier.empty

(* an exception we throw to exit the loop *)
exception SynthSuccess of Fterm.t

(* we use a counter for stat-keeping purposes, and for making sure node roots are unique *)
let counter = ref 0

open Name.Alt

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
    let _ = counter := !counter + 1 in

    (* PRINTING *)
    let _ = normal_print ("Checking: " ^ (Fterm.to_string tm) ^ "\n    Obligation: " ^ (Constraint.to_string node.Node.obligation) ^ "\n") in
    
    (* check if the obligation is satisfiable *)
    let meets_obligation = Strategy.check node.Node.obligation in
    
    (* if it is, then we either check for termination or expand *)
    let _ = if meets_obligation then

      (* PRINTING *)
      let _ = normal_print ("    Satisfiable!\n") in

      (* check if tm is a solution *)
      if (Fterm.wild_closed tm) then
        let meets_examples = Benchmark.verify tm bm.Benchmark.io_examples in
        if meets_examples then raise (SynthSuccess tm) else ()
      
      (* if not, and there's a wild binder, find all expansions *)
      else
        let root = node.Node.root <+ ("spec_" ^ (string_of_int !counter)) in
        let subproblem = Subproblem.of_node (root <+ "w") node in
        let proposals = primitive_proposals @ (Subproblem.variable_proposals subproblem) in
      
        let f = fun p -> Subproblem.specialize root p subproblem.Subproblem.context in
        let solutions = CCList.flat_map f proposals in
        let steps = CCList.filter_map (fun p -> 
          let ans = Subproblem.insert_proposal p subproblem in
          let _ = normal_print ("\tEx: " ^ (Proposal.to_string p) ^ "..." ^ (Constraint.to_string p.Proposal.obligation) ^ "..." ^ (if CCOpt.is_some ans then "ok" else "no") ^ "\n") in ans)
          (solutions @ (CCOpt.to_list (Subproblem.lambda_proposal subproblem))) in
        
        CCList.iter (fun n -> 
          frontier := Frontier.push (Node.to_priority n) n !frontier) steps
    else
      normal_print ("    Unsatisfiable.\n")
    in if !pause then (let _ = read_line () in ()) else ()
  done;;

(* run the experiment, and catch the output *)
try synthesize benchmark with
  | SynthSuccess tm -> print_endline ("Solution found: " ^ (Fterm.to_string tm))
