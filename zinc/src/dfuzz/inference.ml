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
    | Bounded l, Bounded r -> begin match l, r with
        | BR s, BR s' -> s == s'
      end
    | _ -> unsat
and subtype_modal (root : Name.t) (bigger : Dtype.modal) (smaller : Dtype.modal) : Constraint.t =
  (* reflexivity *)
  if bigger = smaller then top else match bigger, smaller with
    (* constrain sensitivities *)
    | Modal (s, dt), Modal (s', dt') -> (subtype root dt dt') & (s' <= s)

(* Substitutions - these only substitute over type variables! how convenient! *)
module Sub = struct
  (* we'll need multiple bindings at several points in time *)
  (* as well as a mechanism for failure *)
  type m = M of Name.t * Dtype.t
  type t =
    | S of m list
    | F (* failure *)
  (* simple constructors *)
  let single (n : Name.t) (dt : Dtype.t) : t = S [M (n, dt)]
  let empty : t = S []
  let failure : t = F
  (* we construct larger subs by composition *)
  let compose (first : t) (second : t) : t = match first, second with
    | S ls, S rs -> S (ls @ rs)
    | _ -> failure
  let rec apply (sub : t) (dt : Dtype.t) : Dtype.t = match sub with
    | S ls -> CCList.fold_left (CCFun.flip apply_m) dt ls
    | _ -> failwith "can't apply a failed substitution"
  and apply_m (map : m) (dt : Dtype.t) : Dtype.t = match map with
    | M (n, img) -> Dtype.instantiate img (Dtype.abstract n dt)
  (* we'll want to make sure certain variables are avoided appropriately *)
  let rec avoids (sub : t) (n : Name.t) : bool = match sub with
    | S ls -> CCList.for_all (fun map -> avoids_m map n) ls
    | _ -> false
  and avoids_m (map : m) (n : Name.t) : bool = match map with
    | M (n', img) -> if n' = n then false else (CCList.mem n (Dtype.free_vars img))
  (* simple pre-baked pattern matching *)
  let is_impossible : t -> bool = function
    | F -> true
    | _ -> false
  (* fterm application *)
  let rec apply_fterm (sub : t) (tm : Fterm.t) : Fterm.t = match tm with
    | Fterm.Abs (tag, dt, body) -> Fterm.Abs (tag, apply sub dt, apply_sc sub body)
    | Fterm.App (l, r) -> Fterm.App (apply_fterm sub l, apply_fterm sub r)
    | Fterm.TyAbs body -> Fterm.TyAbs (apply_sc sub body)
    | Fterm.TyApp (f, arg) -> Fterm.TyApp (apply_fterm sub f, apply sub arg)
    | Fterm.SensAbs body -> Fterm.SensAbs (apply_sc sub body)
    | Fterm.SensApp (f, s) -> Fterm.SensApp (apply_fterm sub f, s)
    | Fterm.Wild (c, dom, body) -> Fterm.Wild (c, apply sub dom, apply_sc sub body)
    | _ -> tm
  and apply_sc (sub : t) : Fterm.scope -> Fterm.scope = function
    | Fterm.Sc tm -> Fterm.Sc (apply_fterm sub tm)
end

(* simple matching over particular variables, ala system Implicit F *)
(* one modification - we assume only one side has the variables we want to find bindings for, but we don't know which *)
let rec unify (root : Name.t) (vars : Name.t list) (left : Dtype.t) (right : Dtype.t) : Sub.t =
  if left = right then Sub.empty else match left, right with
    | Free n, Free m ->
      if CCList.mem n vars then Sub.failure else Sub.empty
    | Free n, (_ as r) ->
      if CCList.mem n vars then Sub.single n r else Sub.failure
    | (_ as l), Free m ->
      if CCList.mem m vars then Sub.single m l else Sub.failure
    | Func (Modal (s, dom), codom), Func (Modal (s', dom'), codom') when s = s' ->
      let sub = unify root vars dom dom' in
      let sub' = unify root vars (Sub.apply sub codom) (Sub.apply sub codom') in
      Sub.compose sub sub'
    | Tensor (l, r), Tensor (l', r') ->
      let sub = unify root vars l l' in
      let sub' = unify root vars (Sub.apply sub r) (Sub.apply sub r') in
      Sub.compose sub sub'
    | Quant (q, k, body), Quant (q', k', body') when q = q' && k = k' ->
      let n = root <+ "UNIFY" in
      let sub = unify n vars (instantiate (Free n) body) (instantiate (Free n) body') in
      if Sub.avoids sub n then sub else Sub.failure
    | Precise p, Precise p' -> begin match p, p' with
        | N s, N s' when s = s' -> Sub.empty
        | M (s, dt), M (s', dt') when s = s' -> unify root vars dt dt'
        | R s, R s' when s = s' -> Sub.empty
        | _ -> Sub.failure
      end
    | Base b, Base b' when b = b' -> Sub.empty
    | Bounded b, Bounded b' -> begin match b, b' with
        | BR s, BR s' when s = s' -> Sub.empty
        | _ -> Sub.failure
      end
    | _ -> Sub.failure

(* unification - assumes all sensitivities are equivalent, so build constraints appropriately *)
let rec subtype_unify (root : Name.t) (left : Dtype.t) (right : Dtype.t) : Constraint.t * Sub.t =
  if left = right then (top, Sub.empty) else match left, right with
    | Free n, (_ as r) -> (top, Sub.single n r)
    | (_ as l), Free m -> (top, Sub.single m l)
    | Func (Modal (s, dom), codom), Func (Modal (s', dom'), codom') ->
      let dom_c, sub = subtype_unify root dom' dom in
      if Sub.is_impossible sub then 
        (dom_c, sub)
      else
        let codom_c, sub' = subtype_unify root (Sub.apply sub codom) (Sub.apply sub codom') in
        (dom_c & codom_c & (s' <= s), Sub.compose sub sub')
    | Tensor (l, r), Tensor (l', r') ->
      let left_c, sub = subtype_unify root l l' in
      if Sub.is_impossible sub then
        (left_c, sub)
      else
        let right_c, sub' = subtype_unify root (Sub.apply sub r) (Sub.apply sub r') in
        (left_c & right_c, Sub.compose sub sub')
    | Quant (q, k, body), Quant (q', k', body') when q = q' && k = k' -> begin match k with
      | KSens ->
        let n = root <+ "SENS_ST_UNIFY" in
        let free = Sensitivity.Free n in
        let c, sub = subtype_unify n (instantiate_sens free body) (instantiate_sens free body') in
          if Sub.avoids sub n then (c, sub) else (c, Sub.failure)
      | KType -> 
        let n = root <+ "DT_ST_UNIFY" in
        let free = Free n in
        let c, sub = subtype_unify n (instantiate free body) (instantiate free body') in
          if Sub.avoids sub n then (c, sub) else (c, Sub.failure)
    end 
    | Precise p, Precise p' -> begin match p, p' with
      | N s, N s' -> (s == s', Sub.empty)
      | M (s, dt), M (s', dt') ->
        let dt_c, sub = subtype_unify root dt dt' in
          (dt_c & (s == s'), sub)
      | R s, R s' -> (s == s', Sub.empty)
      | _ -> (unsat, Sub.failure)
    end
    | Bounded b, Bounded b' -> begin match b, b' with
        | BR s, BR s' -> (s == s', Sub.empty)
      end
    | _ -> (unsat, Sub.failure)
