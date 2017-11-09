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

(* PUT BENCHMARKS HERE
let basic_example_01 = {
  name = "basic_01";
  goal_type = modal (k, real) -* real;
  io_examples = [(Value.Real 0.0, Value.Real 1.0); (Value.Real 2.0, Value.Real 5.0)];
  search_grammar = Signature.Basic.signature;
}

let basic_example_02 = {
  name = "basic_02";
  goal_type = modal (k, mset (real, infinity)) -* int;
  io_examples = [(Value.Bag [Value.Real 0.0; Value.Real 11.0], Value.Int 1)];
  search_grammar = Signature.Basic.signature @ Signature.MapReduce.signature @ Signature.Aggregate.signature;
} *)

(* ADULTS *)
(* ["gt_50k"; "gender"; "race"; "work_hours"; "education_level"; "profession"; "work_class"; "capital_gains"] *)

let adult_sig = Signature.Adult.signature @ Signature.MapReduce.signature @ Signature.Aggregate.signature

(* number of women who work more than 40 hrs a week *)
let adult_01 = {
  name = "adult_01";
  mechanism = Laplace;
  budget = 1;
  examples = [
    (Value.Bag 
      [Signature.Adult.make [
        Value.Bool true; Value.Discrete "female"; Value.Discrete "white"; Value.Real 40.0; 
        Value.Real 12.0; Value.Discrete "trade"; Value.Discrete "private"; Value.Real 10000.0 
      ];], Value.Real 1.0);
    (Value.Bag 
      [Signature.Adult.make [
        Value.Bool true; Value.Discrete "female"; Value.Discrete "white"; Value.Real 13.0; 
        Value.Real 12.0; Value.Discrete "trade"; Value.Discrete "private"; Value.Real 10000.0 
      ];], Value.Real 0.0);
    (Value.Bag 
      [Signature.Adult.make [
        Value.Bool true; Value.Discrete "male"; Value.Discrete "white"; Value.Real 40.0; 
        Value.Real 12.0; Value.Discrete "trade"; Value.Discrete "private"; Value.Real 10000.0 
      ];], Value.Real 0.0);
  ];
  grammar = adult_sig;
}
(* cumulative education level in the army *)
let adult_02 = {
  name = "adult_02";
  mechanism = Laplace;
  budget = 20;
  examples = [
    (Value.Bag 
      [Signature.Adult.make [
        Value.Bool true; Value.Discrete "female"; Value.Discrete "black"; Value.Real 20.0; 
        Value.Real 12.0; Value.Discrete "army"; Value.Discrete "federal"; Value.Real 0.0 
      ];], Value.Real 12.0);
    (Value.Bag 
      [Signature.Adult.make [
        Value.Bool true; Value.Discrete "female"; Value.Discrete "black"; Value.Real 20.0; 
        Value.Real 12.0; Value.Discrete "army"; Value.Discrete "federal"; Value.Real 0.0 
      ]; Signature.Adult.make [
        Value.Bool true; Value.Discrete "female"; Value.Discrete "black"; Value.Real 20.0; 
        Value.Real 12.0; Value.Discrete "army"; Value.Discrete "federal"; Value.Real 0.0 
      ]; Signature.Adult.make [
        Value.Bool true; Value.Discrete "female"; Value.Discrete "black"; Value.Real 20.0; 
        Value.Real 12.0; Value.Discrete "trade"; Value.Discrete "federal"; Value.Real 0.0 
      ];], Value.Real 24.0);
  ];
  grammar = adult_sig;
}
(* number of people in trade who make more than 50k *)
let adult_03 = {
  name = "adult_03";
  mechanism = Laplace;
  budget = 168;
  examples = [
    (Value.Bag 
      [Signature.Adult.make [
        Value.Bool true; Value.Discrete "female"; Value.Discrete "black"; Value.Real 20.0; 
        Value.Real 12.0; Value.Discrete "army"; Value.Discrete "federal"; Value.Real 0.0 
      ];], Value.Real 0.0);
    (Value.Bag 
      [Signature.Adult.make [
        Value.Bool true; Value.Discrete "female"; Value.Discrete "black"; Value.Real 20.0; 
        Value.Real 12.0; Value.Discrete "trade"; Value.Discrete "federal"; Value.Real 0.0 
      ]; Signature.Adult.make [
        Value.Bool true; Value.Discrete "female"; Value.Discrete "black"; Value.Real 20.0; 
        Value.Real 12.0; Value.Discrete "trade"; Value.Discrete "federal"; Value.Real 0.0 
      ]; Signature.Adult.make [
        Value.Bool false; Value.Discrete "female"; Value.Discrete "black"; Value.Real 20.0; 
        Value.Real 12.0; Value.Discrete "trade"; Value.Discrete "federal"; Value.Real 0.0 
      ];], Value.Real 2.0);
  ];
  grammar = adult_sig;
}

(* PUT BENCHMARK LISTS HERE *)
let adult = [adult_01; adult_02; adult_03]

let all = adult