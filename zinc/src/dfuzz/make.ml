(* define some commonly-used variables *)
let k : Sensitivity.t = Sensitivity.Free (Name.of_string "k")

let r : Sensitivity.t = Sensitivity.Free (Name.of_string "r")
let s : Sensitivity.t = Sensitivity.Free (Name.of_string "s")
let n : Sensitivity.t = Sensitivity.Free (Name.of_string "n")

let a : Dtype.t = Dtype.Free (Name.of_string "a")
let b : Dtype.t = Dtype.Free (Name.of_string "b")
let c : Dtype.t = Dtype.Free (Name.of_string "c")

let eps : Sensitivity.t = Sensitivity.Free (Name.of_string "eps")

(* some common constants *)
let infinity : Sensitivity.t = Sensitivity.Const (Rational.Infinity)
let zero : Sensitivity.t = Sensitivity.Const (Rational.of_int 0)
let one : Sensitivity.t = Sensitivity.Const (Rational.of_int 1)
let two : Sensitivity.t = Sensitivity.Const (Rational.of_int 2)
let three : Sensitivity.t = Sensitivity.Const (Rational.of_int 3)

(* and type constanst *)
let real : Dtype.t = Dtype.Bounded (Dtype.Interval (Sensitivity.Const (Rational.Infinity)))
let bool : Dtype.t = Dtype.Base "bool"
let int : Dtype.t = Dtype.Base "int"

(* for rows *)
let key : Dtype.t = Dtype.Base "key"
let row (value : Dtype.t) : Dtype.t = Dtype.Bag (Dtype.Tensor (key, value))

let constant_type : string -> Dtype.t = fun s -> Dtype.Base s

(* precise type constructors *)
let p_real (s : Sensitivity.t) : Dtype.t = Dtype.Precise (Dtype.Real s)
let p_int (s : Sensitivity.t) : Dtype.t = Dtype.Precise (Dtype.Natural s)
let p_list (p : Dtype.t * Sensitivity.t) : Dtype.t = Dtype.Precise (Dtype.List (snd p, fst p))
let mset (p : Dtype.t * Sensitivity.t) : Dtype.t = Dtype.Bounded (Dtype.MSet (snd p, fst p))
let bag (dt : Dtype.t) : Dtype.t = Dtype.Bag dt

(* and bounded type constructors *)
let bounded (s : Sensitivity.t) : Dtype.t = Dtype.Bounded (Dtype.Interval s)
let bounded_by (s : int) : Dtype.t = Dtype.Bounded (Dtype.Interval (Sensitivity.Const (Rational.of_int s)))

(* the modal constructor *)
let modal (p : Sensitivity.t * Dtype.t) : Dtype.modal = Dtype.Modal (fst p, snd p)

(* the function constructors *)
let ( => ) (dom : Dtype.t) (codom : Dtype.t) : Dtype.t = Dtype.Func (modal (infinity, dom), codom)
let ( -* ) (dom : Dtype.modal) (codom : Dtype.t) : Dtype.t = Dtype.Func (dom, codom)

(* the pair constructor *)
let pair (p : Dtype.t * Dtype.t) : Dtype.t = Dtype.Tensor (fst p, snd p)

(* monad constructor *)
let prob (p : Dtype.t) = Dtype.Monad p

(* quantifiers *)
let tbind (p : Dtype.t * Dtype.t) : Dtype.t = match fst p with
  | Dtype.Free x -> Dtype.Quant (Dtype.ForAll, Dtype.KType, Dtype.abstract x (snd p))
  | _ -> failwith "can't abstract over a non-variable"
let sbind (p : Sensitivity.t * Dtype.t) : Dtype.t = match fst p with
  | Sensitivity.Free x -> Dtype.Quant (Dtype.ForAll, Dtype.KSens, Dtype.abstract x (snd p))
  | _ -> failwith "can't abstract over a non-variable"