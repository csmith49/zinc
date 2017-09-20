(* let's be honest, the above is not easy to deal with when making new types *)
(* if we define enough extra functions (including infix), maybe we can cobble together a reasonable meta language *)
(* it'd be really nice to have modular implicits here, so that we could overload constructors based on strings vs ints *)

module Make = struct
  (* non-sensitive function application *)
  let (=>) (d : t) (cd : t) : t =
    Func (Modal (Sensitivity.Const Rational.Infinity, d), cd)

  (* regular function application *)
  let (-*) (m : modal) (cd : t) : t = Func (m, cd)

  (* tensor construction *)
  let ( * ) (l : t) (r : t) : t = Tensor (l, r)

  (* existential quantifier *)
  let exists : (string * t) -> t = function
      (s, dt) -> Quant (Quantifier.Exists, abstract (Name.of_string s) dt)

  (* universial quantifier *)
  let forall : (string * t) -> t = function
      (s, dt) -> Quant (Quantifier.ForAll, abstract (Name.of_string s) dt)
end
