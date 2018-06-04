(* the exponential mechanism is partially parameterized by the return type *)
(* as well as a population of that type *)
type mechanism =
  | Laplace
  | Exponential of Dtype.t * (Value.t list)
  | Partition of Dtype.t * (Value.t list)

(* benchmarks maintain a list of all the info we need to test synthesis *)
type t = {
  name : string;
  budget : int;
  mechanism : mechanism;
  grammar : Primitive.t list;
  examples : (Value.t * Value.t) list;
}

open Make
open Name.Alt

(* goal type depends on the mechanism being used *)
let goal_type : t -> Dtype.t = fun bm -> match bm.mechanism with
  | Laplace -> modal (k, mset (row)) -* real
  | Exponential (a, _) -> modal (k, mset (row)) -* (a => real)
  | Partition (a, _) -> 
    mset (a) => (modal (k, mset (row)) -* mset (pair (a, real)))

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
      Fterm.Wild (context, goal_type bm, Fterm.abstract w (Fterm.Free w))
  end;
}

(* starting verification stuff *)
(* apply an fterm to a value to get a new value - only really works if tm is a function *)
let apply (tm : Fterm.t) (i : Value.t) : Value.t = Fterm.eval (Fterm.App (tm, Fterm.Const i))
let apply_binary (tm : Fterm.t) (one : Value.t) (two : Value.t) =
  let curried = Fterm.App (tm, Fterm.Const one) in
    Fterm.eval (Fterm.App (curried, Fterm.Const two))

(* just makes sure the inputs and outputs match up *)
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
  in CCList.for_all check_pair io

(* putting it together is straightforward now *)
let verify (tm : Fterm.t) (bm : t) : bool = match bm.mechanism with
  | Laplace -> verify_laplace tm bm.examples
  | Exponential (_, pop) -> verify_exponential tm bm.examples pop
  | Partition (_, keys) -> verify_partition tm bm.examples keys