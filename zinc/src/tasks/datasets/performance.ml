open Benchmark

let performance_sig = 
  Signature.Performance.signature @ 
  Signature.MapReduce.signature @ 
  Signature.Aggregate.signature

let make = Signature.Performance.make
(* schema: level; satisfaction; absences; participation; resources; discussion; *)


(* number of students with satisfied parents who miss a lot of school (> 7 days) *)
let performance_01 = {
  name = "performance_01";
  mechanism = Laplace;
  budget = 1;
  grammar = performance_sig;
  examples = [
    (Value.Bag [
      make 100 true 2 100 100 100;
      make 80 true 20 100 90 90;
      make 50 false 20 0 0 0;
      make 98 true 10 97 97 97;      
    ], Value.Real 2.0);
    (Value.Bag [
      make 100 true 2 100 100 100;
      make 80 true 20 100 90 90;
      make 50 true 20 0 0 0;
      make 70 true 10 97 97 97;      
    ], Value.Real 3.0)
  ]
}

(* performance level with the highest average participation *)
(* λx.λy.average (map (λz.part_to_val (participation (z))) (filter (λx'.is_bracket (level_to_val (level (x'))) (y)) (x))) *)
let performance_02 = {
  name = "performance_02";
  mechanism = Exponential (
    Signature.Performance.bracket_t,
    [Value.Discrete "low"; Value.Discrete "medium"; Value.Discrete "high"]
  );
  budget = 101;
  grammar = performance_sig;
  examples = [
    (Value.Bag [
      make 100 true 2 100 100 100;
      make 80 true 20 70 90 90;
      make 50 false 20 0 0 0;
      make 98 true 10 97 97 97;      
    ], Value.Discrete "high");
    (Value.Bag [
      make 0 true 2 0 100 100;
      make 80 true 20 100 90 90;
      make 50 false 20 0 0 0;
      make 98 true 10 97 97 97;      
    ], Value.Discrete "medium")
  ]
}

(* average resource usage per parent satisfaction *)
let performance_03 = {
  name = "performance_03";
  mechanism = Partition (
    Make.bool,
    [Value.Bool true; Value.Bool false]
  );
  budget = 101;
  grammar = performance_sig @ Signature.Database.signature;
  examples = [
    (Value.Bag [
      make 100 true 2 100 100 100;
      make 80 true 20 70 90 90;
      make 50 false 20 0 0 0;
      make 98 true 10 97 95 97;      
    ], Value.Bag [
      Value.Pair (Value.Bool true, Value.Real 95.0);
      Value.Pair (Value.Bool false, Value.Real 0.0);
    ]);
  ]
}

(* total hands raised amongst students with low discussion activity *)
(* λx.sum (map (λy.part_to_val (participation (y))) (filter (λz.is_low (to_bracket (disc_to_val (discussion (z))))) (x))) *)
let performance_04 = {
  name = "performance_04";
  mechanism = Laplace;
  budget = 101;
  grammar = performance_sig;
  examples = [
    (Value.Bag [
      make 100 true 2 100 100 100;
      make 80 true 20 70 90 90;
      make 50 false 20 0 0 0;
      make 98 true 10 97 97 97;      
    ], Value.Real 0.0);
    (Value.Bag [
      make 100 true 2 100 100 100;
      make 80 true 20 70 90 90;
      make 50 false 20 72 0 2;
      make 50 false 20 72 0 2;
      make 98 true 10 97 97 80;      
    ], Value.Real 144.0);
  ]
}

(* average grade in the low performance bracket *)
(* λx.average (filter (λy.is_low (to_bracket (y))) (map (λz.level_to_val (level (z))) (x))) *)
let performance_05 = {
  name = "performance_05";
  mechanism = Laplace;
  budget = 101;
  grammar = performance_sig;
  examples = [
    (Value.Bag [
      make 100 true 2 100 100 100;
      make 80 true 20 70 90 90;
      make 50 false 20 0 0 0;
      make 98 true 10 97 97 97;      
    ], Value.Real 50.0);
    (Value.Bag [
      make 100 true 2 100 100 100;
      make 80 true 20 70 90 90;
      make 60 false 20 72 0 2;
      make 50 false 20 72 0 2;
      make 98 true 10 97 97 80;      
    ], Value.Real 55.0);
  ]
}

let all = [performance_01; performance_02; performance_03; performance_04; performance_05]