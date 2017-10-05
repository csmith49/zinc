open Constraint.Alt
open Name.Alt
open CCOpt.Infix
open Dtype

let rec subtype (root : Name.t) (bigger : Dtype.t) (smaller : Dtype.t) : Constraint.t =
  (* reflexivity check *)
  if bigger = smaller then top else match (bigger, smaller) with
    (* precise types *)
    | Precise p, Precise q -> begin match p, q with
        | N s, N s' -> s == s'
        | R s, R s' -> s == s'
        | M (s, dt), M (s', dt') -> (s == s') & (subtype root dt dt')
        | _ -> None
      end
    (* function types *)
    | Func (dom, codom), Func (dom', codom') ->
      (subtype root codom codom') & (subtype_modal root dom' dom)
    (* tensor types *)
    | Tensor (l, r), Tensor (l', r') ->
      (subtype root l l') & (subtype root r r')
    | Quant (q, k, body), Quant (q', k', body') when q = q' && k = k' -> begin match k with
        | KSens ->
          let s = root <+ "s" in
          let sens = Sensitivity.Free s in
          subtype s (instantiate_sens sens body) (instantiate_sens sens body')
        | KType ->
          let a = root <+ "a" in
          let dt = Dtype.Free a in
          subtype a (instantiate dt body) (instantiate dt body')
      end
    | _ -> None
and subtype_modal (root : Name.t) (bigger : Dtype.modal) (smaller : Dtype.modal) : Constraint.t =
  (* reflexivity *)
  if bigger = smaller then top else match bigger, smaller with
    (* constrain sensitivities *)
    | Modal (s, dt), Modal (s', dt') -> (subtype root dt dt') & (s' <= s)
