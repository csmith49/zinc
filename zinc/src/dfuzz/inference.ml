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
        | Natural s, Natural s' -> s == s'
        | Real s, Real s' -> s == s'
        | List (s, dt), List (s', dt') -> (s == s') & (subtype root dt dt')
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
        | Interval s, Interval s' -> s <= s'
        | MSet (s, dt), MSet (s', dt') -> (subtype root dt dt') & (s <= s')
        | _ -> unsat
      end
    | Monad p, Monad q -> subtype root p q
    | _ -> unsat
and subtype_modal (root : Name.t) (bigger : Dtype.modal) (smaller : Dtype.modal) : Constraint.t =
  (* reflexivity *)
  if bigger = smaller then top else match bigger, smaller with
    (* constrain sensitivities *)
    | Modal (s, dt), Modal (s', dt') -> (subtype root dt dt') & (s' <= s)

(* Substitutions - these only substitute over type variables! how convenient! *)
module Sub = struct
  (* we'll need multiple bindings at several points in time *)
  module NameMap = CCMap.Make(Name)
  type t = Dtype.t NameMap.t

  let to_string : t -> string =
    let f = fun (k, v) -> (Name.to_string k) ^ " -> " ^ (Dtype.to_string v) in
    fun s ->
      s |> NameMap.to_list |> (CCList.map f) |> (CCString.concat " | ")

  let empty : t = NameMap.empty
  let single (n : Name.t) (dt : Dtype.t) : t = NameMap.singleton n dt
  let rec apply (sub : t) (dt : Dtype.t) : Dtype.t =
    NameMap.fold 
      (fun n -> fun img -> fun dt -> Dtype.instantiate img (Dtype.abstract n dt)) 
      sub
      dt
  let avoids_pair (k : Name.t) (v : Dtype.t) (ns : Name.t list) : bool =
    if CCList.mem ~eq:Name.eq k ns then
      false
    else CCList.is_empty (CCList.inter ~eq:Name.eq ns (Dtype.free_vars v))
  let avoids (sub : t) (ns : Name.t list) : bool =
    NameMap.for_all (fun k -> fun v -> avoids_pair k v ns) sub

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

  (* vterm application *)
  let rec apply_vterm (sub : t) (tm : Vterm.t) : Vterm.t = let open Vterm in match tm with
    | Abs (dt, Scope body) -> Abs (apply sub dt, Scope (apply_vterm sub body))
    | App (l, r) -> App (apply_vterm sub l, apply_vterm sub r)

    | MatchNat (e, zero, i, Scope succ) ->
      let e' = apply_vterm sub e in
      let zero' = apply_vterm sub zero in
      let succ' = apply_vterm sub succ in
        MatchNat (e', zero', i, Scope succ')
    | MatchCons (dt, e, nil, i, Widen (Scope cons)) ->
      let e' = apply_vterm sub e in
      let nil' = apply_vterm sub nil in
      let cons' = apply_vterm sub cons in
        MatchCons (apply sub dt, e', nil', i, Widen (Scope cons'))
    
    | TypeAbs (Scope body) -> TypeAbs (Scope (apply_vterm sub body))
    | TypeApp (tm, dt) -> TypeApp (apply_vterm sub tm, apply sub dt)
    | SensAbs (Scope body) -> SensAbs (Scope (apply_vterm sub body))
    | SensApp (tm, s) -> SensApp (apply_vterm sub tm, s)

    | Wild (c, dt, Scope body) ->
      Wild (c, apply sub dt, Scope (apply_vterm sub body))
    
    | Nat (Succ tm) -> Nat (Succ (apply_vterm sub tm))
    | ConsList (Cons (hd, tl)) ->
      let hd' = apply_vterm sub hd in
      let tl' = apply_vterm sub tl in
        ConsList (Cons (hd', tl'))
    | Pair (l, r) -> Pair (apply_vterm sub l, apply_vterm sub r)
    | Bag ts -> Bag (CCList.map (apply_vterm sub) ts)

    | Fix (dt, Scope body) -> Fix (apply sub dt, Scope (apply_vterm sub body))

    | Do tm -> Do (apply_vterm sub tm)
    | Return tm -> Return (apply_vterm sub tm)
    | LetDraw (dt, dist, Scope usage) ->
      LetDraw (apply sub dt, apply_vterm sub dist, Scope (apply_vterm sub usage))

    | _ -> tm

  (* there's a very natural notion of a join over two subs *)

  let join (l : t) (r : t) : t option =
    let mismatches = NameMap.union (fun k -> fun v -> fun v' -> if v = v' then None else Some v) l r in
    if NameMap.is_empty mismatches then 
      Some (NameMap.union (fun k -> fun v -> fun v' -> Some v) l r)
    else None

  (* simple wrappers *)
  let binds = NameMap.mem
  let get = NameMap.find
  let add = NameMap.add
end

module Util = struct
  type t = Constraint.t * Sub.t
  type wlist = (Dtype.t * Dtype.t) CCFQueue.t

  let to_string : t -> string = function
    | (c, s) ->
      let c' = Constraint.to_string c in
      let s' = Sub.to_string s in
        c' ^ " => " ^ s'

  let single : (Dtype.t * Dtype.t) -> wlist = CCFQueue.singleton

  let ( ++> ) (p : Dtype.t * Dtype.t) (wl : wlist) : wlist =
    CCFQueue.snoc wl p

  (* modifying pairs easily *)
  let ( >> ) (l : Constraint.t) (r : t option) : t option =
    match r with
    | None -> None
    | Some (c, s) -> match Constraint.join l c with
      | None -> None
      | Some c' -> Some (c', s)
  let ( << ) (l : t option) (p : Name.t * Dtype.t) : t option = match l with
    | None -> None
    | Some (c, s) ->
      let k, v = p in
      if Sub.binds k s then
        if Sub.get k s = v then Some (c, s)
        else None
      else Some (c, Sub.add k v s)

  let ( $> ) (l : Constraint.t) (r : t) : t option =
    match r with
    | (c, s) -> match Constraint.join l c with
      | None -> None
      | Some c' -> Some (c', s)
  let ( <$ ) (l : t) (p : Name.t * Dtype.t) : t option = match l with
    | (c, s) ->
      let k, v = p in
      if Sub.binds k s then
        if Sub.get k s = v then Some (c, s)
        else None
      else Some (c, Sub.add k v s)
end
open Util

let rec st_unify 
  (root : Name.t)
  (avoids : Name.t list)
  (solution : Util.t) 
  (wl : (Dtype.t * Dtype.t) list) : Util.t option = match wl with
  | [] -> begin match solution with
      | (c, s) -> if Sub.avoids s avoids then Some solution else None
    end
  | (l, r) :: xs ->
    if l = r then st_unify root avoids solution xs
    else match l, r with
      | Free n, (_ as r) -> begin match solution <$ (n, r) with
          | None -> None
          | Some sol -> st_unify root avoids sol xs
        end
      | (_ as l), Free m -> begin match solution <$ (m, l) with
          | None -> None
          | Some sol -> st_unify root avoids sol xs
        end
      | Func (Modal (s, dom), codom), Func (Modal (s', dom'), codom') -> begin
        match (s' <= s) $> solution with
          | None -> None
          | Some sol -> st_unify root avoids sol ( (dom', dom) :: (codom, codom') :: xs )
        end
      | Tensor (l, r), Tensor (l', r') ->
        st_unify root avoids solution ( (l, l') :: (r, r') :: xs )
      | Quant (q, k, body), Quant (q', k', body') when q = q' && k = k' -> begin match k with
          | KSens ->
            let n = root <+ "SENS_ST_UNIFY" in
            let free = Sensitivity.Free n in
            let wl' = (instantiate_sens free body, instantiate_sens free body') :: xs in
              st_unify n (n :: avoids) solution wl'
          | KType ->
            let n = root <+ "DT_ST_UNIFY" in
            let free = Free n in
            let wl' = (instantiate free body, instantiate free body') :: xs in
              st_unify n (n :: avoids) solution wl'
        end
      | Precise p, Precise p' -> begin match p, p' with
          | Natural s, Natural s' -> begin match (s == s') $> solution with
              | None -> None
              | Some sol -> st_unify root avoids sol xs
            end
          | List (s, dt), List (s', dt') -> begin match (s == s') $> solution with
              | None -> None
              | Some sol -> st_unify root avoids sol ( (dt, dt') :: xs )
            end
          | Real s, Real s' -> begin match (s == s') $> solution with
              | None -> None
              | Some sol -> st_unify root avoids sol xs
            end
          | _ -> None
        end
      | Bounded b, Bounded b' -> begin match b, b' with
          | Interval s, Interval s' -> begin match (s <= s') $> solution with
              | None -> None
              | Some sol -> st_unify root avoids sol xs
          end
          | MSet (s, dt), MSet (s', dt') -> begin match (s <= s') $> solution with
              | None -> None
              | Some sol -> st_unify root avoids sol xs
          end
          | _ -> None
        end
      | Monad p, Monad q -> st_unify root avoids solution ((p, q) :: xs)
      | _ -> None

let subtype_unify (root : Name.t) (left : Dtype.t) (right : Dtype.t) : Util.t option =
  st_unify root [] (top, Sub.empty) [(left, right)]