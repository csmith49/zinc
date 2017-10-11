open Constraint.Alt
open Name.Alt
open CCOpt.Infix
open Dtype

(* straightforward dfuzz subtyping *)
let rec subtype (root : Name.t) (bigger : Dtype.t) (smaller : Dtype.t) : Constraint.t =
  (* reflexivity check *)
  if bigger = smaller then top else match (bigger, smaller) with
    (* precise types *)
    | Precise p, Precise q -> begin match p, q with
        | N s, N s' -> s == s'
        | R s, R s' -> s == s'
        | M (s, dt), M (s', dt') -> (s == s') & (subtype root dt dt')
        | _ -> unsat
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
    | _ -> unsat
and subtype_modal (root : Name.t) (bigger : Dtype.modal) (smaller : Dtype.modal) : Constraint.t =
  (* reflexivity *)
  if bigger = smaller then top else match bigger, smaller with
    (* constrain sensitivities *)
    | Modal (s, dt), Modal (s', dt') -> (subtype root dt dt') & (s' <= s)

(* subtyping that tracks constraints on a single type variable *)
module Sub = struct
  (* we either have 0-1 constraints, or there's no solution *)
  type t =
    | S of Dtype.t
    | Empty
    | Failure
  (* we mostly just want to join *)
  let join (l : t) (r : t) : t = match l, r with
    | S x, S y when x = y -> S x
    | Empty, _ -> r
    | _, Empty -> l
    | _ -> Failure
  (* and sometimes we'll apply it to a type *)
  let apply (n : Name.t) (s : t) (dt : Dtype.t) : Dtype.t = match s with
    | S a -> Dtype.instantiate a (Dtype.abstract n dt)
    | _ -> failwith "can't apply an empty/failed substitution"
end

(* a helper function for cleaning up our solutions *)
let join (l : Constraint.t * Sub.t) (r : Constraint.t * Sub.t) : Constraint.t * Sub.t = match l, r with
  | (c, s), (c', s') -> (c & c', Sub.join s s')

let failure : Constraint.t * Sub.t = (unsat, Sub.Failure)

(* now subtype while checking for possible substitutions *)
let rec sub_n_sub (root : Name.t) (var : Name.t) (big : Dtype.t) (small : Dtype.t) : Constraint.t * Sub.t =
  (* reflexivity check *)
  if big = small then (top, Sub.Empty) else match (big, small) with
    (* let's handle the variable case first *)
    | Free n, _ when n = var -> (top, Sub.S small)
    (* precise types *)
    | Precise p, Precise q -> begin match p, q with
        | N s, N s' -> (s == s', Sub.Empty)
        | R s, R s' -> (s == s', Sub.Empty)
        | M (s, dt), M (s', dt') -> join (s == s', Sub.Empty) (sub_n_sub root var dt dt')
        | _ -> failure
      end
    | Func (dom, codom), Func (dom', codom') -> join (sub_n_sub root var codom codom') (sub_n_sub_modal root var dom' dom)
    | Tensor (l, r), Tensor (l', r') -> join (sub_n_sub root var l l') (sub_n_sub root var r r')
    | Quant (q, k, body), Quant (q', k', body') when q = q' && k = k' -> begin match k with
        | KSens ->
          let s = root <+ "s" in
          let sens = Sensitivity.Free s in
          sub_n_sub s var (instantiate_sens sens body) (instantiate_sens sens body')
        | KType ->
          let a = root <+ "a" in
          let dt = Dtype.Free a in
          sub_n_sub a var (instantiate dt body) (instantiate dt body')
      end
    | _ -> failure
and sub_n_sub_modal (root : Name.t) (var : Name.t) (big : Dtype.modal) (small : Dtype.modal) : Constraint.t * Sub.t =
  if big = small then (top, Sub.Empty) else match big, small with
    | Modal (s, dt), Modal (s', dt') -> join (sub_n_sub root var dt dt') (s' <= s, Sub.Empty)
