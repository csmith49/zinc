(* there shouldn't be too much to this particular file, just an interface to solver and whatnot *)

(* just a list of expressions *)
type t = Solver.expr list

(* with a solver we'll use to check satisfiability *)
let solver = Z3.Solver.mk_solver Solver.context None

(* importantly, we want to be able to determine satisfiability of constraints *)
let check (c : t) : bool = match Z3.Solver.check solver c with
    | Z3.Solver.SATISFIABLE -> true
    | _ -> false

(* and a simple method to add to constraint.t *)
let conjoin (e : Solver.expr) (c : t) : t =
    e :: c
