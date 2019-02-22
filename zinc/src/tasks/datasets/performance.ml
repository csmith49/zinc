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
    (Vterm.Bag [
      make 100 true 2 100 100 100;
      make 80 true 20 100 90 90;
      make 50 false 20 0 0 0;
      make 98 true 10 97 97 97;      
    ], Vterm.Real 2.0);
    (Vterm.Bag [
      make 100 true 2 100 100 100;
      make 80 true 20 100 90 90;
      make 50 true 20 0 0 0;
      make 70 true 10 97 97 97;      
    ], Vterm.Real 3.0)
  ]
}

(* performance level with the highest average participation *)
(* λx.λy.average (map (λz.part_to_val (participation (z))) (filter (λx'.is_bracket (level_to_val (level (x'))) (y)) (x))) *)
let performance_02 = {
  name = "performance_02";
  mechanism = Exponential (
    Signature.Performance.bracket_t,
    [Vterm.Discrete "low"; Vterm.Discrete "medium"; Vterm.Discrete "high"]
  );
  budget = 101;
  grammar = performance_sig;
  examples = [
    (Vterm.Bag [
      make 100 true 2 100 100 100;
      make 80 true 20 70 90 90;
      make 50 false 20 0 0 0;
      make 98 true 10 97 97 97;      
    ], Vterm.Discrete "high");
    (Vterm.Bag [
      make 0 true 2 0 100 100;
      make 80 true 20 100 90 90;
      make 50 false 20 0 0 0;
      make 98 true 10 97 97 97;      
    ], Vterm.Discrete "medium")
  ]
}

(* average resource usage per parent satisfaction *)
let performance_03 = {
  name = "performance_03";
  mechanism = Partition (
    Make.bool,
    [Vterm.Bool True; Vterm.Bool False]
  );
  budget = 101;
  grammar = performance_sig @ Signature.Database.signature;
  examples = [
    (Vterm.Bag [
      make 100 true 2 100 100 100;
      make 80 true 20 70 90 90;
      make 50 false 20 0 0 0;
      make 98 true 10 97 95 97;      
    ], Vterm.Bag [
      Vterm.Pair (Vterm.Bool True, Vterm.Real 95.0);
      Vterm.Pair (Vterm.Bool False, Vterm.Real 0.0);
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
    (Vterm.Bag [
      make 100 true 2 100 100 100;
      make 80 true 20 70 90 90;
      make 50 false 20 0 0 0;
      make 98 true 10 97 97 97;      
    ], Vterm.Real 0.0);
    (Vterm.Bag [
      make 100 true 2 100 100 100;
      make 80 true 20 70 90 90;
      make 50 false 20 72 0 2;
      make 50 false 20 72 0 2;
      make 98 true 10 97 97 80;      
    ], Vterm.Real 144.0);
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
    (Vterm.Bag [
      make 100 true 2 100 100 100;
      make 80 true 20 70 90 90;
      make 50 false 20 0 0 0;
      make 98 true 10 97 97 97;      
    ], Vterm.Real 50.0);
    (Vterm.Bag [
      make 100 true 2 100 100 100;
      make 80 true 20 70 90 90;
      make 60 false 20 72 0 2;
      make 50 false 20 72 0 2;
      make 98 true 10 97 97 80;      
    ], Vterm.Real 55.0);
  ]
}


(* are parents satisfied when their child misses a lot of school *)
let performance_06 = {
  name = "performance_06";
  mechanism = Exponential (
    Make.bool,
    [Vterm.Bool True; Vterm.Bool False;]
  );
  budget = 1;
  grammar = performance_sig @ Signature.Database.signature;
  examples = [
    (Vterm.Bag [
      make 100 true 2 100 100 100;
      make 80 true 20 70 90 90;
      make 50 false 20 0 0 0;
      make 98 true 10 97 97 97;      
    ], Vterm.Bool True);
    (Vterm.Bag [
      make 0 true 2 0 100 100;
      make 80 false 20 100 90 90;
      make 50 false 20 0 0 0;
      make 98 true 10 97 97 97;      
    ], Vterm.Bool False)
  ]
}

(* cumulative resource usage per bracket *)
let performance_07 = {
  name = "performance_07";
  mechanism = Partition (
    Signature.Performance.bracket_t,
    [Vterm.Discrete "low"; Vterm.Discrete "medium"; Vterm.Discrete "high"]
  );
  budget = 100;
  grammar = performance_sig @ Signature.Database.signature;
  examples = [
    (Vterm.Bag [
      make 100 true 2 100 100 100;
      make 80 true 20 70 90 90;
      make 50 false 20 0 0 0;
      make 98 true 10 97 95 97;      
    ], Vterm.Bag [
      Vterm.Pair (Vterm.Discrete "low", Vterm.Real 0.0);
      Vterm.Pair (Vterm.Discrete "medium", Vterm.Real 90.0);
      Vterm.Pair (Vterm.Discrete "high", Vterm.Real 195.0);
    ]);
  ]
}


let all = [performance_01; performance_02; performance_03; performance_04; performance_05; performance_06; performance_07]