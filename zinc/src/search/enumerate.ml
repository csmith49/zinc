open CCFun
open CCOpt.Infix

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
let children (p : Program.t) (s : Signature.t) : (Program.t * Constraint.t) list = []
    (* two primary ways to extend a program *)
    (* 1: use a variable of the appropriate type *)
    (* 2: use a component from the signature *)
