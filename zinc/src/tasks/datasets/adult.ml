open Benchmark

(* ADULTS *)
(* Schema: *)
(* ["gt_50k"; "gender"; "race"; "work_hours"; "education_level"; "profession"; "work_class"; "capital_gains"] *)

let adult_sig = Signature.Adult.signature @ Signature.MapReduce.signature @ Signature.Aggregate.signature
let make = Signature.Adult.make

(* number of women who work more than 40 hrs a week *)
let adult_01 = {
  name = "adult_01";
  mechanism = Laplace;
  budget = 1;
  examples = [
    (Value.Bag [make true "female" "white" 40 12 "trade" "private" 10000], Value.Real 1.0);
    (Value.Bag [make true "female" "white" 13 12 "trade" "private" 10000], Value.Real 0.0);
    (Value.Bag [make true "male" "white" 40 12 "trade" "private" 10000], Value.Real 0.0);
  ];
  grammar = adult_sig;
}
(* cumulative education level in the army *)
let adult_02 = {
  name = "adult_02";
  mechanism = Laplace;
  budget = 20;
  examples = [
    (Value.Bag [make true "female" "black" 20 12 "army" "federa" 0], Value.Real 12.0);
    (Value.Bag [
      make true "female" "black" 20 12 "army" "federal" 0;
      make true "female" "black" 20 12 "army" "federal" 0;
      make true "female" "black" 20 12 "trade" "federal" 0;
    ], Value.Real 24.0)
  ];
  grammar = adult_sig;
}
(* number of people in trade who make more than 50k *)
let adult_03 = {
  name = "adult_03";
  mechanism = Laplace;
  budget = 168;
  examples = [
    (Value.Bag [make true "female" "black" 20 12 "army" "federa" 0], Value.Real 0.0);
    (Value.Bag [
      make true "female" "black" 20 12 "army" "federal" 0;
      make true "female" "black" 20 12 "army" "federal" 0;
      make false "female" "black" 20 12 "trade" "federal" 0;
    ], Value.Real 2.0)
  ];
  grammar = adult_sig;
}

(* most common gender working at the local level *)
let adult_04 = {
  name = "adult_04";
  mechanism = Exponential (Signature.Adult.gender_t, [Value.Discrete "male"; Value.Discrete "female"]);
  budget = 1;
  examples = [
    (Value.Bag [
      make true "female" "black" 20 12 "trade" "local" 0;
      make true "male" "black" 20 12 "trade" "local" 0;
      make false "female" "black" 20 12 "trade" "local" 0;
    ], Value.Discrete "female");
    (Value.Bag [
      make true "female" "black" 20 12 "trade" "local" 0;
      make true "female" "black" 20 12 "trade" "federal" 0;
      make true "male" "black" 20 12 "trade" "local" 0;
      make false "male" "black" 20 12 "trade" "local" 0;
    ], Value.Discrete "male");
  ];
  grammar = adult_sig @ Signature.Database.signature;
}

(* population per race *)
let adult_05 = {
  name = "adult_05";
  mechanism = Partition (Signature.Adult.race_t, [Value.Discrete "white"; Value.Discrete "black"]);
  budget = 1;
  examples = [
    (* example 1 *)
    (* inputs *)
    (Value.Bag [
      make true "female" "black" 20 20 "trade" "local" 0;
      make true "female" "black" 20 20 "trade" "local" 0;
      make true "female" "white" 20 20 "trade" "local" 0;
    ],
    (* outputs *)
    Value.Bag [
      Value.Pair (Value.Discrete "white", Value.Real 1.0);
      Value.Pair (Value.Discrete "black", Value.Real 2.0)
    ])];
  grammar = adult_sig @ Signature.Database.signature;
}

(* profession with the highest cumulative work hours *)
let adult_06 = {
  name = "adult_06";
  mechanism = Exponential (Signature.Adult.profession_t, [Value.Discrete "trade"; Value.Discrete "army"; Value.Discrete "agriculture"]);
  budget = 168;
  examples = [
    (Value.Bag [
      make true "female" "black" 20 12 "trade" "local" 0;
      make true "male" "black" 30 12 "agriculture" "local" 0;
      make false "female" "black" 20 12 "trade" "local" 0;
      make false "female" "black" 10 12 "army" "local" 0;
    ], Value.Discrete "trade");
    (Value.Bag [
      make true "female" "black" 20 12 "trade" "local" 0;
      make true "male" "black" 30 12 "agriculture" "local" 0;
      make false "female" "black" 5 12 "trade" "local" 0;
      make false "female" "black" 10 12 "army" "local" 0;
    ], Value.Discrete "agriculture");
  ];
  grammar = adult_sig @ Signature.Database.signature;
}

(* number of people making gt 50k in trade, per kind of government *)
let adult_07 = {
  name = "adult_07";
  mechanism = Partition (Signature.Adult.work_class_t, [Value.Discrete "federal"; Value.Discrete "state"; Value.Discrete "local"]);
  budget = 1;
  examples = [
    (* example 1 *)
    (* inputs *)
    (Value.Bag [
      make true "female" "black" 20 20 "trade" "local" 0;
      make true "female" "black" 20 20 "army" "local" 0;
      make true "female" "white" 20 20 "trade" "local" 0;
      make false "female" "white" 20 20 "trade" "federal" 0;
      make false "female" "white" 20 20 "trade" "federal" 0;
      make true "female" "white" 20 20 "trade" "federal" 0;
      make false "female" "white" 20 20 "trade" "state" 0; 
      ],
    (* outputs *)
    Value.Bag [
      Value.Pair (Value.Discrete "federal", Value.Real 1.0);
      Value.Pair (Value.Discrete "state", Value.Real 0.0);
      Value.Pair (Value.Discrete "local", Value.Real 2.0);
    ])];
  grammar = adult_sig @ Signature.Database.signature;
}

(* an easily accessible list *)
let all = [adult_01; adult_02; adult_03; adult_04; adult_05; adult_06; adult_07]