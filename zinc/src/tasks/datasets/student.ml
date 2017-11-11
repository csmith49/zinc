open Benchmark

let student_sig = Signature.Student.signature @ Signature.MapReduce.signature @ Signature.Aggregate.signature
(* schema = [grade; reason; wknd; wkdy; address; payed; family; absences] *)


(* number of students paying for extra classes with a moderate amount of weekday alcohol consumption *)
(* \x.count(filter(paying, filter(\y.moderate(wkdy(y))), x)) *)
let student_01 = {
  name = "student_01";
  mechanism = Laplace;
  budget = 1;
  grammar = student_sig;
  examples = [
    (Value.Bag [
      Signature.Student.make 17 "reputation" 4 2 "rural" false 3 10;
      Signature.Student.make 17 "reputation" 4 3 "rural" false 3 10;
      Signature.Student.make 17 "reputation" 4 2 "rural" true 3 10;
      Signature.Student.make 17 "reputation" 4 4 "rural" true 3 10;
    ], Value.Real 1.0)
  ]
}

(* average final grade of students attending for reputation reasons *)
let student_02 = {
  name = "student_02";
  mechanism = Laplace;
  budget = 20;
  grammar = student_sig;
  examples = [
    (Value.Bag [
      Signature.Student.make 10 "reputation" 4 2 "rural" false 3 10;
      Signature.Student.make 20 "reputation" 4 3 "urban" true 2 0;
      Signature.Student.make 16 "proximity" 0 0 "rural" false 5 0;
    ], Value.Real 15.0)
  ]
}

(* average weekend alcohol consumption per address type *)
let student_03 = {
  name = "student_03";
  mechanism = Partition (
    Signature.Student.address_type_t, 
    [Value.Discrete "rural"; Value.Discrete "urban"]);
  budget = 5;
  grammar = student_sig @ Signature.Database.signature;
  examples = [
    (Value.Bag [
      Signature.Student.make 10 "reputation" 5 5 "rural" false 3 10;
      Signature.Student.make 20 "reputation" 0 0 "rural" true 2 0;
      Signature.Student.make 17 "proximity" 4 3 "urban" true 5 3;
    ], Value.Bag [
      Value.Pair (Value.Discrete "rural", Value.Real 2.5);
      Value.Pair (Value.Discrete "urban", Value.Real 4.0);
    ])
  ];
}

(* family relationship status resulting in the highest average final grade *)
(* \x.\y. avg(map(grade_to_val, filter(\z.family_is(z, y), x))) *)
let student_04 = {
  name = "student_04";
  mechanism = Exponential (
    Signature.Student.family_t, 
    [Value.Real 1.0; Value.Real 2.0; Value.Real 3.0; Value.Real 4.0; Value.Real 5.0]);
  budget = 20;
  grammar = student_sig;
  examples = [
    (Value.Bag [
      Signature.Student.make 10 "reputation" 5 5 "rural" false 3 10;
      Signature.Student.make 20 "reputation" 0 0 "rural" true 3 0;
      Signature.Student.make 9 "proximity" 4 3 "urban" true 5 3;
    ], Value.Real 3.0);
    (Value.Bag [
      Signature.Student.make 10 "reputation" 5 5 "rural" false 3 10;
      Signature.Student.make 18 "reputation" 0 0 "rural" true 3 0;
      Signature.Student.make 20 "proximity" 4 3 "urban" true 5 3;
    ], Value.Real 5.0)
  ]
}

(* average final grade of people who don't drink much on the weekend *)
let student_05 = {
  name = "student_05";
  mechanism = Laplace;
  budget = 100;
  grammar = student_sig;
  examples = [
    (Value.Bag [
      Signature.Student.make 10 "reputation" 5 5 "rural" false 1 50;
      Signature.Student.make 20 "reputation" 0 5 "rural" true 2 50;
      Signature.Student.make 17 "reputation" 4 3 "rural" true 1 3;
    ], Value.Real 20.0)
  ]
}

let all = [student_01; student_02; student_03; student_04; student_05]