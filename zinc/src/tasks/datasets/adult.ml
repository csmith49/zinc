open Benchmark

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

(* most common gender working at the local level *)
let adult_04 = {
  name = "adult_04";
  mechanism = Exponential (Signature.Adult.gender_t, [Value.Discrete "male"; Value.Discrete "female"]);
  budget = 1;
  examples = [
    (Value.Bag 
      [Signature.Adult.make [
        Value.Bool true; Value.Discrete "female"; Value.Discrete "black"; Value.Real 20.0; 
        Value.Real 12.0; Value.Discrete "trade"; Value.Discrete "local"; Value.Real 0.0 
      ]; Signature.Adult.make [
        Value.Bool true; Value.Discrete "male"; Value.Discrete "black"; Value.Real 20.0; 
        Value.Real 12.0; Value.Discrete "trade"; Value.Discrete "local"; Value.Real 0.0 
      ]; Signature.Adult.make [
        Value.Bool false; Value.Discrete "female"; Value.Discrete "black"; Value.Real 20.0; 
        Value.Real 12.0; Value.Discrete "trade"; Value.Discrete "local"; Value.Real 0.0 
      ];], Value.Discrete "female");
  (Value.Bag 
    [Signature.Adult.make [
      Value.Bool true; Value.Discrete "female"; Value.Discrete "black"; Value.Real 20.0; 
      Value.Real 12.0; Value.Discrete "trade"; Value.Discrete "local"; Value.Real 0.0 
    ]; Signature.Adult.make [
      Value.Bool true; Value.Discrete "female"; Value.Discrete "black"; Value.Real 20.0; 
      Value.Real 12.0; Value.Discrete "trade"; Value.Discrete "federal"; Value.Real 0.0 
    ]; Signature.Adult.make [
      Value.Bool true; Value.Discrete "male"; Value.Discrete "black"; Value.Real 20.0; 
      Value.Real 12.0; Value.Discrete "trade"; Value.Discrete "local"; Value.Real 0.0 
    ]; Signature.Adult.make [
      Value.Bool false; Value.Discrete "male"; Value.Discrete "black"; Value.Real 20.0; 
      Value.Real 12.0; Value.Discrete "trade"; Value.Discrete "local"; Value.Real 0.0 
    ];], Value.Discrete "male");
  ];
  grammar = adult_sig  @ Signature.Database.signature;
}

(* an easily accessible list *)
let all = [adult_01; adult_02; adult_03; adult_04]