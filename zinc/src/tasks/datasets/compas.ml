open Benchmark
(* benchmarks from the pro republica compas dataset *)

let compas_sig = 
  Signature.Compas.signature @ Signature.Aggregate.signature @ Signature.MapReduce.signature
let make = Signature.Compas.make
(* schema: sex; age_cat; race; juv_fel; priors_cnt; recidivism; violence; failure to appear *)

(* number of elderly with high risk of violence *)
let compas_01 = {
  name = "compas_01";
  mechanism = Laplace;
  budget = 1;
  grammar = compas_sig;
  examples = [
    (Value.Bag [
      make "male" "elderly" "other" 0 4 2 8 1;
      make "male" "elderly" "other" 0 4 2 8 1;
      make "male" "elderly" "other" 0 4 2 3 1;      
      make "female" "young" "black" 0 0 1 1 1;
      make "female" "young" "black" 0 4 1 1 1;
      make "female" "young" "white" 0 0 1 9 1;
    ], Value.Real 2.0)
  ]
}

(* average failure to appear for young people with no priors *)
let compas_02 = {
  name = "compas_02";
  mechanism = Laplace;
  budget = 10;
  grammar = compas_sig;
  examples = [
    (Value.Bag [
      make "male" "elderly" "other" 0 5 2 8 9;
      make "male" "elderly" "other" 0 0 2 8 1;
      make "male" "young" "other" 0 0 2 3 8;
    ], Value.Real 8.0);
    (Value.Bag [
      make "male" "young" "white" 0 5 2 8 9;
      make "female" "elderly" "black" 0 0 2 8 1;
      make "male" "young" "other" 0 0 2 3 3;
    ], Value.Real 3.0);
    (Value.Bag [
      make "male" "young" "white" 0 5 2 8 9;
      make "female" "elderly" "black" 0 0 2 8 1;
      make "male" "young" "other" 0 0 2 7 3;
    ], Value.Real 3.0);
  ]
}

(* race with highest average recidivism *)
let compas_03 = {
  name = "compas_03";
  mechanism = Exponential (
    Signature.Compas.race_t,
    [Value.Discrete "black"; Value.Discrete "white"; Value.Discrete "other"]
  );
  budget = 10;
  grammar = compas_sig;
  examples = [
    (Value.Bag [
      make "male" "elderly" "white" 0 5 7 2 9;
      make "male" "elderly" "other" 0 0 4 2 1;
      make "male" "young" "black" 0 0 2 3 8;
    ], Value.Discrete "white");
    (Value.Bag [
      make "male" "elderly" "white" 0 0 7 8 9;
      make "male" "elderly" "black" 0 0 4 8 1;
      make "male" "young" "black" 0 2 2 3 8;
    ], Value.Discrete "white");
  ]
}

(* total priors per sex *)
let compas_04 = {
  name = "compas_04";
  mechanism = Partition (
    Signature.Compas.gender_t,
    [Value.Discrete "male"; Value.Discrete "female"]
  );
  budget = 15;
  grammar = compas_sig @ Signature.Database.signature;
  examples = [
    (Value.Bag [
      make "male" "elderly" "white" 0 5 7 2 9;
      make "male" "young" "other" 0 3 4 2 1;
      make "female" "young" "black" 0 0 2 3 8;
      make "female" "elderly" "white" 0 2 3 4 5;
    ], Value.Bag [
      Value.Pair (Value.Discrete "male", Value.Real 8.0);
      Value.Pair (Value.Discrete "female", Value.Real 2.0);
    ]);
  ]
}

(* number of people with high juvenile felonies and a high risk of recidivism *)
let compas_05 = {
  name = "compas_05";
  mechanism = Laplace;
  budget = 1;
  grammar = compas_sig;
  examples = [
    (Value.Bag [
      make "male" "elderly" "white" 0 5 8 2 9;
      make "female" "elderly" "white" 2 5 0 2 9;
    ], Value.Real 0.0);
    (Value.Bag [
      make "female" "elderly" "white" 15 5 8 2 9;
      make "female" "elderly" "white" 15 5 0 2 9;
      make "female" "elderly" "white" 1 5 8 2 9;
    ], Value.Real 1.0);
    (Value.Bag [
      make "male" "elderly" "white" 15 5 8 2 9;
      make "female" "elderly" "white" 2 5 0 2 9;
    ], Value.Real 1.0);
  ]
}

let all = [compas_01; compas_02; compas_03; compas_04; compas_05]