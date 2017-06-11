(* we start off with our signature - this contains the relevant functions to search over *)
let signature = Utility.StringMap.empty

(* we're guided by the following heuristic *)
let heuristic = (fun p -> 1)

(* with the following goals *)
let goals = []

(* and here's our goal program *)
(* let start = Program.Wild (Dtype.Base Dtype.Real, Context.Variable Variable.make "test") *)

(* so now we can start the search *)

open Enumerate
open Constraint
