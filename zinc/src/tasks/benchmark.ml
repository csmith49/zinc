(* the exponential mechanism is partially parameterized by the return type *)
(* as well as a population of that type *)
type mechanism =
  | Laplace
  | Exponential of Dtype.t * (Value.t list)

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
  | Laplace -> modal (k, mset (row, infinity)) -* real
  | Exponential (a, _) -> modal (k, mset (row, infinity)) -* (a => real)

(* eventually, we have to convert it into a node *)
let to_node : t -> Node.t = fun bm -> {
  Node.root = Name.of_string "start";
  obligation = begin
    let budget = Sensitivity.Const (Rational.of_int bm.budget) in
    let rel = Sensitivity.Relation.LEq (k, budget) in
      Constraint.Alt.s_rel rel 
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

(* just makes sure the inputs and outputs match up *)
let verify_laplace (tm : Fterm.t) (io : (Value.t * Value.t) list) : bool =
  let check_pair = fun (i, o) -> o = (apply tm i)
  in CCList.for_all check_pair io

(* the io examples are different in this case - they're the values in a that should score highest *)
let verify_exponential (tm : Fterm.t) (io : (Value.t * Value.t) list) (pop : Value.t list) : bool =
  let check_pair = fun (i, o) ->
    let score = apply tm i in
      CCList.for_all (fun p -> 
        p = o || (Value.leq (apply tm p) score))
      pop
  in CCList.for_all check_pair io

(* putting it together is straightforward now *)
let verify (tm : Fterm.t) (bm : t) : bool = match bm.mechanism with
  | Laplace -> verify_laplace tm bm.examples
  | Exponential (_, pop) -> verify_exponential tm bm.examples pop