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
let obligation = ref false
let counting = ref false
let sizing = ref false

(* the references for the weights, a little overly verbose *)
let p_weight_1 = ref 1
let p_weight_2 = ref 0
let p_weight_3 = ref 10
let p_weight_4 = ref 0

(* the actual command line arguments *)
let spec_list = [
  ("-bm", Arg.Set_string benchmark_name, " Sets the benchmark to test");
  ("-v", Arg.Set_int verbosity, " Sets the level of verbosity (>= 3 for benchmarking output)");
  ("-pause", Arg.Set pause, " Pauses for input after each check");
  ("-time", Arg.Set time_it, " Enables timing");
  ("-disable", Arg.Set dont_prune, " Disables SAT-pruning");
  ("-annotations", Arg.Set dont_annotate, " Disables type annotations on term output");
  ("-strat", Arg.Set_string strategy, " Sets the SAT strategy");
  ("-obligation", Arg.Set obligation, " Displays the simplified proof obligation generated");
  ("-weights", Arg.Tuple [
      Arg.Set_int p_weight_1;
      Arg.Set_int p_weight_2;
      Arg.Set_int p_weight_3;
      Arg.Set_int p_weight_4;
    ], " Sets weights for priority construction");
  ("-count", Arg.Set counting, " Enables counting of solutions explored");
  ("-size", Arg.Set sizing, " Enables size checking for programs and simplified constraints");
]

(* populate the references - no anonymous functions *)
let usage_message = ""
let anon_function = fun s -> ()
let _ = Arg.parse (Arg.align spec_list) anon_function usage_message

(* define the print levels *)
let normal_print : bool = !verbosity >= 1
let more_print : bool = !verbosity >= 2
let bm_print : bool = !verbosity >= 3

let string_of_fterm : Fterm.t -> string = if !dont_annotate then Fterm.to_clean_string else Fterm.to_string

let weights : Node.Vector.four Node.Vector.coefficients = 
  Node.Vector.of_four !p_weight_1 !p_weight_2 !p_weight_3 !p_weight_4

let _ = if normal_print then print_endline ("Evaluating benchmark: " ^ !benchmark_name) else ()

(* main loop happens here *)
let rec extract_benchmark (name : string) (bs : Benchmark.t list) : Benchmark.t = match bs with
  | b :: bs' -> if b.Benchmark.name = name then b else extract_benchmark name bs'
  | _ -> failwith ("can't find provided benchmark: " ^ name)

(* pull the benchmark from the arguments *)
let benchmark = extract_benchmark !benchmark_name Dataset.all

(* construct the strategy *)
(* module BasicStrategy = Solver.Strategy(Solver.Basic)
module FancyStrategy = Solver.Strategy(Solver.Fancy) *)

(* let check : Constraint.t -> (bool * Solver.expr list) = match !strategy with
  | "basic" -> BasicStrategy.check
  | "fancy" -> FancyStrategy.check
  | _ -> FancyStrategy.check *)

let check : Constraint.t -> bool = fun c -> true

(* consruct the frontier from the benchmarks start position *)
let frontier = ref Frontier.empty

(* an exception we throw to exit the loop *)
exception SynthSuccess of Node.t

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
  let _ = frontier := Frontier.push weights !frontier start_node in
  let primitive_proposals = CCList.map Primitive.to_proposal benchmark.Benchmark.grammar in
  
  (* repeatedly pull nodes and check for satisfiability *)
  while true do
    (* pull from frontier *)
    let frontier', node = Frontier.pop !frontier in
    let _ = frontier := frontier' in
    let tm = node.Node.solution in
    let _ = counter := !counter + 1 in

    let start_time = Sys.time () in

    (* PRINTING *)
    let _ = if normal_print then print_endline ("Checking: " ^ (Vterm.format tm)) else () in
    let _ = if more_print then 
      print_endline ("    Obligation: " ^ (Constraint.to_string node.Node.obligation)) else () in
    let _ = if !pause then (let _ = read_line () in ()) else () in

    (* check if the obligation is satisfiable *)
    let meets_obligation = if !dont_prune then true else check node.Node.obligation in
    
    (* update the timer *)
    let sat_check_time = (Sys.time()) -. start_time in 
    let _ = sat_time := sat_check_time +. !sat_time in
    let _ = if !time_it && normal_print then 
      print_endline ("    SAT check time: " ^ (string_of_float sat_check_time)) else () in

    (* if it is, then we either check for termination or expand *)
    let _ = if meets_obligation then

      (* PRINTING *)
      let _ = if more_print then print_endline ("    Satisfiable!") else () in

      (* check if tm is a solution *)
      if (Vterm.wild_closed tm) then
        let meets_examples = 
          try Benchmark.verify tm bm 
          with _ -> false
          in
        if meets_examples then 
          let _ = total_time := ((Sys.time ()) -. start_time) +. !total_time in raise (SynthSuccess node) 
        else ()
      
      (* if not, and there's a wild binder, find all expansions *)
      else
        let root = node.Node.root <+ ("spec_" ^ (string_of_int !counter)) in
        let subproblem = Subproblem.of_node (root <+ "w") node in
        let proposals = primitive_proposals @ (Subproblem.Proposal.variables subproblem) in
      
        let _ = if more_print then print_endline ("    Goal: " ^ (Dtype.to_string subproblem.Subproblem.goal)) else () in

        let f = fun p -> Subproblem.Proposal.specialize root p subproblem.Subproblem.context in
        let solutions = CCList.flat_map f proposals in
        let steps = CCList.filter_map (fun p -> 
          let ans = Subproblem.insert_proposal p subproblem in
          let _ = if more_print then 
            print_endline 
              ("\tExpansion: " ^ "\n\t    " ^ 
              (Subproblem.Proposal.to_string p) ^ "...\n\t    " ^ 
              (Constraint.to_string p.Subproblem.Proposal.obligation) ^ "...\n\t    " ^ 
              (if CCOpt.is_some ans then "ok" else "no")) else ()
            in ans)
          (solutions @ (CCOpt.to_list (Subproblem.Proposal.lambda subproblem))) in
        let _ = if bm_print then print_string ("\tInserting expansions into frontier...") else () in
        let _ = CCList.iter (fun n -> 
          frontier := Frontier.push weights !frontier n) steps in
        if bm_print then print_endline "done." else ()
    else
      if more_print then print_endline ("    Unsatisfiable.\n") else ()
    in let _ = if !pause then (let _ = read_line () in ()) else () in

    (* update total printer *)
    let total_check_time = (Sys.time ()) -. start_time in
    let _ = total_time := total_check_time +. !total_time in
    let _ = if !time_it && normal_print then
      print_endline ("    Total time: " ^ (string_of_float total_check_time)) else () in
    ()
  done;;

(* run the experiment, and catch the output *)
try synthesize benchmark with
  | SynthSuccess node -> 
    let _ = print_endline ("Solution found: " ^ (Vterm.format node.Node.solution)) in
    let _ = if !counting then print_endline ("Solutions explored: " ^ (string_of_int !counter)) else ()in
    let _ = if !time_it then print_endline ("Total time: " ^ (string_of_float !total_time)) else () in
    let _ = if !time_it then print_endline ("SAT time: " ^ (string_of_float !sat_time)) else () in
    let obs =
      node.Node.obligation |> Constraint.flatten
                           |> Simplify.simplify
    in 
    let _ = if !obligation then Simplify.print_constraints obs else () in
    let _ = if !sizing then print_endline ("Solution size: " ^ (node.Node.solution |> Vterm.size |> string_of_int)) else () in
    let _ = if !sizing then print_endline ("Obligation size: " ^ (obs |> CCList.length |> string_of_int)) else ()
    in ()
 