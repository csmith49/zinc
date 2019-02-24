(* the exponential mechanism is partially parameterized by the return type *)
(* as well as a population of that type *)
type mechanism =
  | Laplace
  | Exponential of Dtype.t * (Vterm.t list)
  | Partition of Dtype.t * (Vterm.t list)

(* benchmarks maintain a list of all the info we need to test synthesis *)
type t = {
  name : string;
  budget : int;
  mechanism : mechanism;
  grammar : Primitive.t list;
  examples : (Vterm.t * Vterm.t) list;
}

open Make
open Name.Alt

(* goal type depends on the mechanism being used *)
let goal_type : t -> Dtype.t = fun bm -> match bm.mechanism with
  | Laplace -> modal (k, mset (row a, infinity)) -* real
  | Exponential (a, _) -> modal (k, mset (row a, infinity)) -* (a => real)
  | Partition (a, _) -> 
    mset (a, infinity) => (modal (k, mset (row a, infinity)) -* mset (pair (a, real), infinity))

(* eventually, we have to convert it into a node *)
let to_node : t -> Node.t = fun bm -> {
  Node.root = Name.of_string "start";
  obligation = begin
    let budget = Sensitivity.Const (Rational.of_int bm.budget) in
    (* in reality, we should have lower_bound < k and lower_bound = 0 *)
    (* but this works, if we assume all sensitivities are bounded below by 1 *)
    let lower_bound = Sensitivity.Const (Rational.of_int 1) in
    let open Constraint.Alt in
      (k <= budget) & (lower_bound <= k)
  end;
  solution = begin
    let context = Context.Empty in
    let root = Name.of_string bm.name in
    let w = root <+ "wild" in
      Vterm.Wild (context, goal_type bm, Vterm.abstract_one w (Vterm.Var (Vterm.Free w)))
  end;
}

(* starting verification stuff *)
(* apply an fterm to a value to get a new value - only really works if tm is a function *)
let apply (tm : Fterm.t) (i : Value.t) : Value.t = Fterm.eval (Fterm.App (tm, Fterm.Const i))
let apply_binary (tm : Fterm.t) (one : Value.t) (two : Value.t) =
  let curried = Fterm.App (tm, Fterm.Const one) in
    Fterm.eval (Fterm.App (curried, Fterm.Const two))

(* just makes sure the inputs and outputs match up *)
let verify_laplace (tm : Vterm.t) (io : (Vterm.t * Vterm.t) list) : bool =
  let check_pair = fun (i, o) ->
    let output = o |> Vterm.eval in
    let input = Vterm.App (tm, i) |> Vterm.eval in
    let _ = print_endline (Vterm.Evaluation.to_string input) in
    let _ = print_endline (Vterm.Evaluation.to_string output) in
      output = input
  in CCList.for_all check_pair io

let verify_exponential (tm : Vterm.t) (io : (Vterm.t * Vterm.t) list) (pop : Vterm.t list) : bool =
  let open Vterm.Alt in
  let check_pair = fun (i, o) ->
    let score = tm <!> i <!> o |> Vterm.eval in CCList.for_all (fun p ->
      let _ = print_endline (Vterm.Evaluation.to_string score) in
      p = o || Vterm.Evaluation.real_gt score (tm <!> i <!> p |> Vterm.eval)) pop in
  CCList.for_all check_pair io

let verify_partition (tm : Vterm.t) (io : (Vterm.t * Vterm.t) list) (keys : Vterm.t list) : bool =
  let open Vterm.Alt in
  let check_pair = fun (i, o) ->
    let output = o |> Vterm.eval in
    let input = (tm <!> (Vterm.Bag keys) <!> i) |> Vterm.eval in
    let _ = print_endline (Vterm.Evaluation.to_string input) in
    let _ = print_endline (Vterm.Evaluation.to_string output) in
      output = input
    in CCList.for_all check_pair io
(* 
let verify_laplace (tm : Fterm.t) (io : (Value.t * Value.t) list) : bool =
  let check_pair = fun (i, o) -> o = (apply tm i)
  in CCList.for_all check_pair io

(* the io examples are different in this case - they're the values in a that should score highest *)
let verify_exponential (tm : Fterm.t) (io : (Value.t * Value.t) list) (pop : Value.t list) : bool =
  let check_pair = fun (i, o) ->
    let score = apply_binary tm i o in
      CCList.for_all (fun p -> 
        p = o || (Value.geq score (apply_binary tm i p)))
      pop
  in CCList.for_all check_pair io

let verify_partition (tm : Fterm.t) (io : (Value.t * Value.t) list) (keys : Value.t list): bool =
  let check_pair = fun (i, o) -> o = (apply_binary tm (Value.Bag keys) i)
  in CCList.for_all check_pair io *)

(* putting it together is straightforward now *)
let verify (tm : Vterm.t) (bm : t) : bool = match bm.mechanism with
  | Laplace -> verify_laplace tm bm.examples
  | Exponential (_, pop) -> verify_exponential tm bm.examples pop
  | Partition (_, keys) -> verify_partition tm bm.examples keys