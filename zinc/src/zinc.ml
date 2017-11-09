open Signature
open Inference

(* references for command line arguments *)
let benchmark_name = ref ""
let verbosity = ref 1
let pause = ref false
let time_it = ref false
let dont_prune = ref false
let dont_annotate = ref false
let strategy = ref "fancy"

(* functions defining the level of printing *)
let normal_print : string -> unit = fun s -> if !verbosity >= 1 then print_string s else ()
let more_print : string -> unit = fun s -> if !verbosity >= 2 then print_string s else ()
let bm_print : string -> unit = fun s -> if !verbosity >= 3 then print_string s else ()

let string_of_fterm : Fterm.t -> string = fun tm -> 
  if !dont_annotate then Fterm.to_clean_string tm else Fterm.to_string tm

(* the actual command line arguments *)
let spec_list = [
  ("-bm", Arg.Set_string benchmark_name, " Sets the benchmark to test");
  ("-v", Arg.Set_int verbosity, " Sets the level of verbosity (>= 3 for benchmarking output)");
  ("-pause", Arg.Set pause, " Pauses for input after each check");
  ("-time", Arg.Set time_it, " Enables timing");
  ("-disable", Arg.Set dont_prune, " Disables SAT-pruning");
  ("-annotations", Arg.Set dont_annotate, " Disables type annotations on term output");
  ("-strat", Arg.Set_string strategy, " Sets the SAT strategy");
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
let benchmark = extract_benchmark !benchmark_name Dataset.all

(* construct the strategy *)
module BasicStrategy = Solver.Strategy(Solver.Basic)
module FancyStrategy = Solver.Strategy(Solver.Fancy)


let check : Constraint.t -> bool = match !strategy with
  | "basic" -> BasicStrategy.check
  | "fancy" -> FancyStrategy.check
  | _ -> FancyStrategy.check

(* consruct the frontier from the benchmarks start position *)
module Frontier = Pqueue.Make(Node.Priority)
let frontier = ref Frontier.empty

(* an exception we throw to exit the loop *)
exception SynthSuccess of Fterm.t

(* we use a counter for stat-keeping purposes, and for making sure node roots are unique *)
let counter = ref 0
(* we also maintain timers, for obvious purposes *)
let sat_time = ref 0.0
let total_time = ref 0.0

open Name.Alt

(* the infinite loop, in a function we can back out of *)
let synthesize (bm : Benchmark.t) : unit =

  (* generate start node and initialize frontier *)
  let start_node = Benchmark.to_node bm in
  let _ = frontier := Frontier.push (Node.to_priority start_node) start_node !frontier in
  let primitive_proposals = CCList.map Primitive.to_proposal benchmark.Benchmark.grammar in
  
  (* repeatedly pull nodes and check for satisfiability *)
  while true do
    (* pull from frontier *)
    let node, p, frontier' = Frontier.pop !frontier in
    let _ = frontier := frontier' in
    let tm = node.Node.solution in
    let _ = counter := !counter + 1 in

    let start_time = Sys.time () in

    (* PRINTING *)
    let _ = normal_print ("Checking: " ^ (string_of_fterm tm) ^ "\n") in
    let _ = more_print ("    Obligation: " ^ (Constraint.to_string node.Node.obligation) ^ "\n") in
    let _ = if !pause then (let _ = read_line () in ()) else () in

    (* check if the obligation is satisfiable *)
    let meets_obligation = if !dont_prune then true else check node.Node.obligation in
    
    (* update the timer *)
    let sat_check_time = (Sys.time()) -. start_time in 
    let _ = sat_time := sat_check_time +. !sat_time in
    let _ = if !time_it then normal_print ("    SAT check time: " ^ (string_of_float sat_check_time) ^ "\n") else () in

    (* if it is, then we either check for termination or expand *)
    let _ = if meets_obligation then

      (* PRINTING *)
      let _ = more_print ("    Satisfiable!\n") in

      (* check if tm is a solution *)
      if (Fterm.wild_closed tm) then
        let meets_examples = 
          try Benchmark.verify tm bm 
          with _ -> false
          in
        if meets_examples then 
          let _ = total_time := ((Sys.time ()) -. start_time) +. !total_time in raise (SynthSuccess tm) 
        else ()
      
      (* if not, and there's a wild binder, find all expansions *)
      else
        let root = node.Node.root <+ ("spec_" ^ (string_of_int !counter)) in
        let subproblem = Subproblem.of_node (root <+ "w") node in
        let proposals = primitive_proposals @ (Subproblem.variable_proposals subproblem) in
      
        let _ = more_print ("    Goal: " ^ (Dtype.to_string subproblem.Subproblem.goal) ^ "\n") in

        let f = fun p -> Subproblem.specialize root p subproblem.Subproblem.context in
        let solutions = CCList.flat_map f proposals in
        let steps = CCList.filter_map (fun p -> 
          let ans = Subproblem.insert_proposal p subproblem in
          let _ = more_print ("\tExpansion: " ^ "\n\t    " ^ (Proposal.to_string p) ^ "...\n\t    " ^ (Constraint.to_string p.Proposal.obligation) ^ "...\n\t    " ^ (if CCOpt.is_some ans then "ok" else "no") ^ "\n") in ans)
          (solutions @ (CCOpt.to_list (Subproblem.lambda_proposal subproblem))) in
        
        CCList.iter (fun n -> 
          frontier := Frontier.push (Node.to_priority n) n !frontier) steps
    else
      more_print ("    Unsatisfiable.\n")
    in let _ = if !pause then (let _ = read_line () in ()) else () in

    (* update total printer *)
    let total_check_time = (Sys.time ()) -. start_time in
    let _ = total_time := total_check_time +. !total_time in
    let _ = if !time_it then (normal_print ("    Total time: " ^ (string_of_float total_check_time) ^ "\n")) in
    ()
  done;;

(* run the experiment, and catch the output *)
try synthesize benchmark with
  | SynthSuccess tm -> 
    let _ = print_endline ("Solution found: " ^ (string_of_fterm tm)) in
    let _ = bm_print ("Solutions explored: " ^ (string_of_int !counter) ^ "\n") in
    let _ = if !time_it then print_endline ("Total time: " ^ (string_of_float !total_time)) else () in
    let _ = if !time_it then print_endline ("SAT time: " ^ (string_of_float !sat_time)) else () in
    ()
