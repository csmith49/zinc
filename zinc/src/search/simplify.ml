type rel = Sensitivity.Relation.t
type t = rel list

(* type just for constant assignment *)
type const_assignment = Name.t * Rational.t

let assignments_to_sub : const_assignment list -> Sensitivity.Sub.t = fun cal ->
  let f = fun (n, c) -> (n, Sensitivity.Const c) in
  Sensitivity.Sub.of_list (CCList.map f cal)

let apply_assignments (c : t) (cal : const_assignment list) : t =
  let sub = assignments_to_sub cal in
  let app = Sensitivity.Relation.fmap (fun s -> Sensitivity.Sub.apply s sub) in
    CCList.map app c

(* constructing simplifications *)
let to_const_assignment : rel -> const_assignment option = function
  | Sensitivity.Relation.Eq (Sensitivity.Free n, Sensitivity.Const c) -> Some (n, c)
  | Sensitivity.Relation.Eq (Sensitivity.Const c, Sensitivity.Free n) -> Some (n, c)
  | _ -> None

let rec separate_assignments (c : t) : (const_assignment list * t) = match c with
  | [] -> [], []
  | c :: cs ->
    let cal', c' = separate_assignments cs in
    match to_const_assignment c with
      | None -> cal', c :: c'
      | Some ca -> ca :: cal', c'

(* weird fixpoint stuff *)
let fixpoint (f : 'a -> 'a) : 'a -> 'a = 
  let rec g = fun a ->
    let step = f a in if a = step then a else g (step)
  in g

(* now simplification *)
let one_step_assignment_simplify : t -> t = fun c ->
  let cal, c = separate_assignments c in apply_assignments c cal
let assignment_simplify : t -> t = fixpoint one_step_assignment_simplify

(* we have a notion of arithmetic simplification as well, but not necessary for building the metric *)
module Constants = struct
  let zero = Sensitivity.Const (Rational.of_int 0)
  let one = Sensitivity.Const (Rational.of_int 1)
  let infinity = Sensitivity.Const (Rational.Infinity)
end

let rec reduce_sensitivity : Sensitivity.t -> Sensitivity.t = function
  (* some nice arithmetic properties *)
  | Sensitivity.Mult (n, s) when n = Constants.one -> s (* 1 x C = C *)
  | Sensitivity.Mult (s, n) when n = Constants.one -> s
  | Sensitivity.Mult (s, n) when n = Constants.infinity -> Constants.infinity
  | Sensitivity.Mult (s, n) when n = Constants.zero -> Constants.zero
  (* more for addition *)
  | Sensitivity.Plus (n, s) when n = Constants.infinity -> Constants.infinity
  | Sensitivity.Plus (s, n) when n = Constants.infinity -> Constants.infinity
  | Sensitivity.Plus (n, s) when n = Constants.zero -> s
  | Sensitivity.Plus (s, n) when n = Constants.zero -> s
  (* converting down to arithmetic when appropriate *)
  | Sensitivity.Mult (Sensitivity.Const c, Sensitivity.Const c') -> Sensitivity.Const (Rational.mult c c')
  | Sensitivity.Plus (Sensitivity.Const c, Sensitivity.Const c') -> Sensitivity.Const (Rational.add c c')
  (* and the recursive / constant bits *)
  | Sensitivity.Plus (l, r) -> Sensitivity.Plus (reduce_sensitivity l, reduce_sensitivity r)
  | Sensitivity.Mult (l, r) -> Sensitivity.Mult (reduce_sensitivity l, reduce_sensitivity r)
  | Sensitivity.Succ s -> Sensitivity.Succ (reduce_sensitivity s)
  | (_ as s) -> s

let reduce_simplify : t -> t = fixpoint (fun rs -> CCList.map (Sensitivity.Relation.fmap reduce_sensitivity) rs)

(* we can remove identity statememnts *)
let is_identity : rel -> bool = function
  | Sensitivity.Relation.Eq (l, r) when l = r -> true
  | Sensitivity.Relation.LEq (l, r) when l = r -> true
  | _ -> false
let identity_simplify : t -> t = CCList.filter (fun n -> n |> is_identity |> not)

(* and we can also just remap variables whenever *)
type link = Name.t * Name.t

let apply_link (c : t) (l : link) : t =
  let n, n' = l in
  let sub = Sensitivity.Sub.of_list [(n, Sensitivity.Free n')] in
    CCList.map (Sensitivity.Relation.fmap (fun s -> Sensitivity.Sub.apply s sub)) c

let to_link : rel -> link option = function
  | Sensitivity.Relation.Eq (Sensitivity.Free n, Sensitivity.Free n') when not (n = n') -> Some (n, n')
  | _ -> None

let one_step_link_simplify : t -> t = fun c ->
  match CCList.find_map to_link c with
    | Some l -> apply_link c l
    | None -> c

let link_simplify : t -> t = fixpoint one_step_link_simplify

open CCFun
let simplify : t -> t = fixpoint (reduce_simplify % assignment_simplify % link_simplify % identity_simplify)

(* we also have some simple ways of extracting what were once sensitivities *)
let extract_sensitivity : rel -> Sensitivity.t option = fun sr ->
  match Sensitivity.Relation.rhs sr with
    | Sensitivity.Plus (_, Sensitivity.Mult (s, _)) -> begin match s with
        | Sensitivity.Const _ -> Some s
        | Sensitivity.Free _ -> Some s
        | _ -> None
      end
    | _ -> None

let extract_sensitivities : t -> Sensitivity.t list = fun c ->
  CCList.filter_map extract_sensitivity c

let is_constant : Sensitivity.t -> bool = function
  | (Sensitivity.Const _ as c) when not (c = Constants.infinity) -> true
  | _ -> false
let is_unconstrained : Sensitivity.t -> bool = function
  | Sensitivity.Free _ -> true
  | _ -> false
let is_infinity : Sensitivity.t -> bool = function
  | (Sensitivity.Const _ as c) when (c = Constants.infinity) -> true
  | _ -> false

let print_constraints : t -> unit = CCList.iter (fun sr -> sr |> Sensitivity.Relation.to_string |> print_endline)