(* this just captures all the dataset files into a single module *)
let all = Adult.all @ Student.all @ Performance.all @ Compas.all

let idp = Iterative.all