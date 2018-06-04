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
    if CCList.mem (=) k ns then
      false
    else CCList.is_empty (CCList.inter (=) ns (Dtype.free_vars v))
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

(* a non-recursive approach *)
(* probably don't use this one *)
let st_un (root : Name.t) (wl : Util.wlist) : Util.t option = 
  (* setting up worklist / result *)
  let worklist = ref wl in
  let avoiding = ref [] in
  let result = ref (Some (top, Sub.empty)) in
  let root = ref root in
  (* loop for ages *)
  let _ = while (not (CCFQueue.is_empty !worklist) && not (CCOpt.is_none !result)) do
    (* update the worklist by pulling out the next *)
    let p, wl = CCFQueue.take_front_exn !worklist in
    let _ = worklist := wl in
    (* then pattern match to update the worklist with new elements *)
    let l, r = p in if l = r then () else match l, r with
      | Free n, (_ as r) -> result := !result << (n, r)
      | (_ as l), Free m -> result := !result << (m, l)
      | Func (Modal (s, dom), codom), Func (Modal (s', dom'), codom') -> begin
          result := (s' <= s) >> !result;
          worklist := (codom, codom') ++> ((dom', dom) ++> !worklist);
        end
      | Tensor (l, r), Tensor (l', r') ->
        worklist := (r, r') ++> ((l, l') ++> !worklist);
      | Quant (q, k, body), Quant (q', k', body') when q = q' && k = k' -> begin match k with
          | KSens ->
            let n = !root <+ "SENS_ST_UNIFY" in
            let _ = root := n in
            let free = Sensitivity.Free n in begin
              avoiding := n :: !avoiding;
              worklist := (instantiate_sens free body, instantiate_sens free body') ++> !worklist;
            end
          | KType -> 
            let n = !root <+ "DT_ST_UNIFY" in
            let _ = root := n in
            let free = Free n in begin
              avoiding := n :: !avoiding;
              worklist := (instantiate free body, instantiate free body') ++> !worklist;
            end
        end
      | Precise p, Precise p' -> begin match p, p' with
          | N s, N s' -> result := (s == s') >> !result
          | M (s, dt), M (s', dt') -> begin
            result := (s == s') >> !result;
            worklist := (dt, dt') ++> !worklist;
          end
          | R s, R s' -> result := (s == s') >> !result
          | _ -> result := None
        end
      | Bounded b, Bounded b' -> begin match b, b' with
          | BR s, BR s' -> result := (s' <= s) >> !result
        end
      | _ -> result := None
  done in match !result with
    | Some (c, s) -> if Sub.avoids s !avoiding then !result else None
    | None -> None

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
            | N s, N s' -> begin match (s == s') $> solution with
                | None -> None
                | Some sol -> st_unify root avoids sol xs
              end
            | M (s, dt), M (s', dt') -> begin match (s == s') $> solution with
                | None -> None
                | Some sol -> st_unify root avoids sol ( (dt, dt') :: xs )
              end
            | R s, R s' -> begin match (s == s') $> solution with
                | None -> None
                | Some sol -> st_unify root avoids sol xs
              end
            | _ -> None
          end
        | Bounded b, Bounded b' -> begin match b, b' with
            | BR s, BR s' -> begin match (s == s') $> solution with
                | None -> None
                | Some sol -> st_unify root avoids sol xs
            end
          end
        | _ -> None

let subtype_unify (root : Name.t) (left : Dtype.t) (right : Dtype.t) : Util.t option =
  st_unify root [] (top, Sub.empty) [(left, right)]