open CCFun
open CCOpt.Infix

(* think of these as nodes in our search *)
type synth_task = Program.t * Constraint.t

(* we move from task to task in our search *)
type task = Program.t * Constraint.t
type t = task Frontier.t

(* expansion is controlled by parameter sets *)
type parameters = Dtype.t * Context.t * (Variable.t list)

(* and we monitor solutions to subproblems *)
type solution = Program.t * Solver.expr

(* this allows us to express our search in a simple pipeline *)

let get_solutions : parameters -> solution list = fun _ -> []
let apply_solution : task -> solution -> task = failwith "undef"

(* step 1: crack open the relevant program *)
(* TODO: fix *)
(* let make_subtasks : task -> task list = function
    | (p, c) ->
        let wild_context = Term.Zipper.preorder_until (not % Program.is_wild) (Term.Zipper.of_term p) in
        let wild_params = [] *)

(* given a program, find the first wildcard if it exists *)
let find_wild (p : Program.t) : Program.context option =
    Term.Zipper.preorder_until (not % Program.is_wild) (Term.Zipper.of_term p)

(* once we've got a zipper representing a wildcard, we just need the goal type and context *)
let wild_parameters (z : Program.context) : (Dtype.t * Context.t) option =
    let f = (fun n -> match n with
            | Program.Wild (d, c) -> Some (d, c)
            | _ -> None) in
    CCOpt.return z >>= Program.get_node >>= f

(* TODO: our end-goal *)
(* let children (p : Program.t) (s : Signature.t) : (Program.t * Constraint.t) list =
    match CCOpt.return p >>= find_wild >>= wild_parameters with
        | None -> []
        | Some (d, c) ->
            (* four primary ways to extend the current wildcard *)
            (* 1: use a variable of the appropriate type *)
            (* 2: use a component from the signature *)
            (* 3: split contexts to fully apply a signature component *)
            (* 4: make an abstraction *)
            match d with
                | Dtype.Function (m, d') ->
                    let bd = Program.context_binding_depth c in
                    let
                | _ -> [] *)
