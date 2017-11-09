(* define some commonly-used variables *)
let k : Sensitivity.t = Sensitivity.Free (Name.of_string "k")

let r : Sensitivity.t = Sensitivity.Free (Name.of_string "r")
let s : Sensitivity.t = Sensitivity.Free (Name.of_string "s")
let n : Sensitivity.t = Sensitivity.Free (Name.of_string "n")

let a : Dtype.t = Dtype.Free (Name.of_string "a")
let b : Dtype.t = Dtype.Free (Name.of_string "b")
let c : Dtype.t = Dtype.Free (Name.of_string "c")

(* some common constants *)
let infinity : Sensitivity.t = Sensitivity.Const (Rational.Infinity)
let zero : Sensitivity.t = Sensitivity.Const (Rational.of_int 0)
let one : Sensitivity.t = Sensitivity.Const (Rational.of_int 1)
let two : Sensitivity.t = Sensitivity.Const (Rational.of_int 2)

(* and type constanst *)
let real : Dtype.t = Dtype.Precise (Dtype.R (Sensitivity.Const (Rational.Infinity)))
let bool : Dtype.t = Dtype.Base "bool"
let int : Dtype.t = Dtype.Base "int"
let bounded : Dtype.t = Dtype.Base "bounded"
let row : Dtype.t = Dtype.Base "row"

let constant_type : string -> Dtype.t = fun s -> Dtype.Base s

(* precise type constructors *)
let p_real (s : Sensitivity.t) : Dtype.t = Dtype.Precise (Dtype.R s)
let p_int (s : Sensitivity.t) : Dtype.t = Dtype.Precise (Dtype.N s)
let mset (p : Dtype.t * Sensitivity.t) : Dtype.t = Dtype.Precise (Dtype.M (snd p, fst p))

(* the modal constructor *)
let modal (p : Sensitivity.t * Dtype.t) : Dtype.modal = Dtype.Modal (fst p, snd p)

(* the function constructors *)
let ( => ) (dom : Dtype.t) (codom : Dtype.t) : Dtype.t = Dtype.Func (modal (infinity, dom), codom)
let ( -* ) (dom : Dtype.modal) (codom : Dtype.t) : Dtype.t = Dtype.Func (dom, codom)

(* the pair constructor *)
let pair (p : Dtype.t * Dtype.t) : Dtype.t = Dtype.Tensor (fst p, snd p)

(* quantifiers *)
let tbind (p : Dtype.t * Dtype.t) : Dtype.t = match fst p with
  | Dtype.Free x -> Dtype.Quant (Dtype.ForAll, Dtype.KType, Dtype.abstract x (snd p))
  | _ -> failwith "can't abstract over a non-variable"
let sbind (p : Sensitivity.t * Dtype.t) : Dtype.t = match fst p with
  | Sensitivity.Free x -> Dtype.Quant (Dtype.ForAll, Dtype.KSens, Dtype.abstract x (snd p))
  | _ -> failwith "can't abstract over a non-variable"