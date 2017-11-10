open Benchmark

let student_sig = Signature.Student.signature @ Signature.MapReduce.signature @ Signature.Aggregate.signature
(* schema = [grade; reason; wknd; wkdy; address; payed; family; absences] *)


(* number of students paying for extra classes with a moderate amount of weekday alcohol consumption *)
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

let all = [student_01]