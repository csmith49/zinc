(* simple terms extended with type abstraction and application - system F *)
type t =
  | Free of Name.t
  | Bound of int
  | Abs of Dtype.t * scope
  | App of t * t
  (* type level things *)
  | TyAbs of scope
  | TyApp of t * Dtype.t
  (* sensitivity level things *)
  | SensAbs of scope
  | SensApp of t * Sensitivity.t
  (* but also the simple language extensions we'll use for synthesis and evalution *)
  | Const of Value.t
  | Prim of string * Value.t
  | Wild of Context.t * Dtype.t * scope
(* scopes just have a single dangling free var - to be reinforced by convention *)
and scope = Sc of t

(* mcbride and mckinna movement around binders *)
let rec abstract (n : Name.t) (tm : t) : scope = Sc (abstract' n 0 tm)
and abstract' (n : Name.t) (db : int) (tm : t) : t = match tm with
  (* when the name matches, put in the appropriate db depth *)
  | Free n' -> if n = n' then Bound db else tm
  (* the simple recursive cases *)
  | App (f, arg) -> App (abstract' n db f, abstract' n db arg)
  | TyApp (f, arg) ->
    TyApp (abstract' n db f, Dtype.abstract' n db arg)
  | SensApp (f, arg) ->
    SensApp (abstract' n db f, Sensitivity.abstract' n db arg)
  (* the passing over a scope constructor recursion *)
  | Abs (dom, Sc tm') ->
    Abs (Dtype.abstract' n db dom, Sc (abstract' n (db + 1) tm'))
  | TyAbs (Sc tm') -> TyAbs (Sc (abstract' n (db + 1) tm'))
  | SensAbs (Sc tm') -> SensAbs (Sc (abstract' n (db + 1) tm'))
  | Wild (context, dom, Sc tm') ->
    Wild (
      context,
      Dtype.abstract' n db dom,
      Sc (abstract' n (db + 1) tm')
    )
  (* if there's no recursion, just return *)
  | _ -> tm

let rec instantiate (img : t) (s : scope) : t = match s with
  | Sc tm' -> instantiate' img 0 tm'
and instantiate' (img : t) (db : int) (tm : t) : t = match tm with
  (* when we're at the variable at the right depth, replace w/img *)
  | Bound i -> if i = db then img else tm
  (* simple recursive cases *)
  | App (f, arg) -> App (instantiate' img db f, instantiate' img db arg)
  | TyApp (f, arg) -> TyApp (instantiate' img db f, arg)
  | SensApp (f, arg) -> SensApp (instantiate' img db f, arg)
  (* passing over scope constructors *)
  | Abs (dom, Sc tm') -> Abs (dom, Sc (instantiate' img (db + 1) tm'))
  | TyAbs (Sc tm') -> TyAbs (Sc (instantiate' img (db + 1) tm'))
  | SensAbs (Sc tm') -> SensAbs (Sc (instantiate' img (db + 1) tm'))
  | Wild (context, dom, Sc tm') ->
    Wild (context, dom, Sc (instantiate' img (db + 1) tm'))
  (* for everything else, there's returning the default *)
  | _ -> tm

let rec instantiate_dtype (img : Dtype.t) (s : scope) : t = match s with
  | Sc tm' -> instantiate_dtype' img 0 tm'
and instantiate_dtype' (img : Dtype.t) (db : int) (tm : t) : t = match tm with
  (* note when we break into the Dtype.instantiate' we _don't_ increment db *)
  | Abs (dom, Sc tm') ->
    Abs (Dtype.instantiate' img db dom, Sc (instantiate_dtype' img (db + 1) tm'))
  | App (f, arg) ->
    App (instantiate_dtype' img db f, instantiate_dtype' img db arg)
  | TyAbs (Sc tm') ->
    TyAbs (Sc (instantiate_dtype' img (db + 1) tm'))
  | TyApp (f, arg) ->
    TyApp (instantiate_dtype' img db f, Dtype.instantiate' img db arg)
  | SensAbs (Sc tm') ->
    SensAbs (Sc (instantiate_dtype' img (db + 1) tm'))
  | SensApp (f, arg) ->
    SensApp (instantiate_dtype' img db f, arg)
  | Wild (context, dom, Sc tm') ->
    Wild (
      context,
      Dtype.instantiate' img db dom,
      Sc (instantiate_dtype' img (db + 1) tm')
    )
  | _ -> tm

let rec instantiate_sens (img : Sensitivity.t) (s : scope) : t = match s with
  | Sc tm' -> instantiate_sens' img 0 tm'
and instantiate_sens' (img : Sensitivity.t) (db : int) (tm : t) : t = match tm with
  (* note when we break into the Sensitivity.instantiate' we _don't_ increment db *)
  | Abs (dom, Sc tm') ->
    Abs (Dtype.instantiate_sens' img db dom, Sc (instantiate_sens' img (db + 1) tm'))
  | App (f, arg) ->
    App (instantiate_sens' img db f, instantiate_sens' img db arg)
  | TyAbs (Sc tm') ->
    TyAbs (Sc (instantiate_sens' img (db + 1) tm'))
  | TyApp (f, arg) ->
    TyApp (instantiate_sens' img db f, Dtype.instantiate_sens' img db arg)
  | SensAbs (Sc tm') ->
    SensAbs (Sc (instantiate_sens' img (db + 1) tm'))
  | SensApp (f, arg) ->
    SensApp (instantiate_sens' img db f, Sensitivity.instantiate' img db arg)
  | Wild (context, dom, Sc tm') ->
    Wild (
      context,
      Dtype.instantiate_sens' img db dom,
      Sc (instantiate_sens' img (db + 1) tm')
    )
  | _ -> tm

(* eval only works in restricted cases, should be fine by construction *)
(* STEP *)
(* ^ that's a big-step eval joke *)
let rec eval (tm : t) : Value.t = match tm with
  | App (f, arg) -> begin match (eval f) with
      | Value.F f' -> f' (eval arg)
      | _ -> failwith "can't apply a non-abstraction value"
    end
  | Abs (_, body) ->
    let f = fun v -> eval (instantiate (Const v) body) in Value.F f
  | Const v -> v
  | Prim (_, f) -> f
  | TyApp (TyAbs body, arg) -> eval (instantiate_dtype arg body)
  | SensApp (SensAbs body, arg) -> eval (instantiate_sens arg body)
  | _ -> failwith "can't evaluate provided term"

(* to reference types in submodules *)
type fterm = t

(* allows us to move across binders in zippers and whatnot *)
module Prefix = struct
  open Stack.Alt
  (* bindings contain all the information to recreate the og fterm *)
  type binding =
    | BAbs of Name.t * Dtype.t
    | BTyAbs of Name.t
    | BSensAbs of Name.t
    | BWild of Name.t * Context.t * Dtype.t
  (* and prefixes maintain a stack of bindings *)
  type t = binding Stack.t
  (* I love me some alternative syntax *)
  module Alt = struct
    let (@>) (b : binding) (tm : fterm) : fterm = match b with
      | BAbs (n, dom) -> Abs (dom, abstract n tm)
      | BTyAbs n -> TyAbs (abstract n tm)
      | BSensAbs n -> SensAbs (abstract n tm)
      | BWild (n, context, dom) ->
        Wild (context, dom, abstract n tm)
    let (<@) (n : Name.t) (tm : fterm) : (binding * fterm) option =
      match tm with
      | Abs (dom, body) -> Some (BAbs (n, dom), instantiate (Free n) body)
      | TyAbs body -> Some (BTyAbs n, instantiate_dtype (Dtype.Free n) body)
      | SensAbs body -> Some (BSensAbs n, instantiate_sens (Sensitivity.Free n) body)
      | Wild (context, dom, body) -> Some (BWild (n, context, dom), instantiate (Free n) body)
      | _ -> None
  end
  open Alt
  (* append the entirety of a prefix - not always useful *)
  let rec bind (prefix : t) (tm : fterm) : fterm = match prefix with
    | Stack.Empty -> tm
    | Stack.Cons (b, prefix') -> bind prefix' (b @> tm)
end

(* we use Huet style zippers for unfolding/folding fterms as we maneuver *)
module Zipper = struct
  open Stack.Alt
  open Prefix.Alt
  (* type of branches is the derivative of fterms wrt fterms *)
  type branch =
    | ZAppLeft of fterm
    | ZAppRight of fterm
    | ZTyApp of Dtype.t
    | ZSensApp of Sensitivity.t
    (* this contains several derivatives put together - you'll see *)
    | ZBinding
  (* we maintain a current view, the bindings needed, and the branches *)
  type t = fterm * Prefix.t * branch Stack.t
  (* basic navigation *)
  let up : t -> t option = function
    | (tm, prefix, Stack.Cons (branch, branches)) -> begin match branch with
        | ZAppLeft tm' -> Some (App (tm, tm'), prefix, branches)
        | ZAppRight tm' -> Some (App (tm', tm), prefix, branches)
        | ZTyApp dt -> Some (TyApp (tm, dt), prefix, branches)
        | ZSensApp s -> Some (SensApp(tm, s), prefix, branches)
        | ZBinding -> begin match prefix with
            | Stack.Cons (binding, bindings) ->
              Some (binding @> tm, bindings, branches)
            | _ -> None
          end
      end
    | _ -> None
  let right : t -> t option = function
    | (tm, prefix, Stack.Cons (ZAppLeft tm', branches)) ->
      Some (tm', prefix, branches <+ (ZAppRight tm))
    | _ -> None
  (* because we're going past binders, we'll need to name variables *)
  let down (root : Name.t) (var : string) (z : t) : t option = match z with
    | (tm, prefix, branches) -> let n = root <+ Name.Id (var, Stack.size prefix) in match tm with
      | App (l, r) -> Some (l, prefix, branches <+ (ZAppLeft r))
      | TyApp (f, arg) -> Some (f, prefix, branches <+ (ZTyApp arg))
      | SensApp (f, arg) -> Some (f, prefix, branches <+ (ZSensApp arg))
      | Abs _ | TyAbs _ | SensAbs _ | Wild _ -> begin match n <@ tm with
          | Some (binding, body) -> Some (body, prefix <+ binding, branches <+ ZBinding)
          | _ -> None
        end
      | _ -> None
  (* simple getter/setter *)
  let get : t -> fterm = function
    | (tm, _, _) -> tm
  let set (tm : fterm) : t -> t = function
    | (_, prefix, branches) -> (tm, prefix, branches)
  (* fancier iteration *)
  open CCOpt.Infix
  open CCFun
  (* all the things necessary for preorder traversal *)
  let rec next (z : t ) : t option = (right z) <+> (up z) >>= next
  let preorder (root : Name.t) (var : string) (z : t) : t option = (down root var z) <+> (next z)
  let rec preorder_until (root : Name.t) (var : string) (predicate : fterm -> bool) (z : t) : t option =
    (CCOpt.if_ (predicate % get) z) <+> (preorder root var z) >>= (preorder_until root var predicate)
  (* convert to/from fterms *)
  let of_term : fterm -> t = fun tm -> (tm, Stack.Empty, Stack.Empty)
  let rec to_term : t -> fterm = fun z -> match up z with
    | Some z' -> to_term z'
    | _ -> get z
end

(* we need to be able to determine sizes for heuristic purposes *)
let rec size : t -> int = function
  | Abs (_, Sc body) -> 1 + (size body)
  | App (l, r) -> 1 + (size l) + (size r)
  | TyAbs (Sc body) -> 1 + (size body)
  | TyApp (f, _) -> 1 + (size f)
  | SensAbs (Sc body) -> 1 + (size body)
  | SensApp (f, _) -> 1 + (size f)
  | Wild (_, _, Sc body) -> 1 + (size body)
  | _ -> 1

(* utility for checking if there are any wild binders floating around the term *)
let rec wild_closed : t -> bool = function
  | Abs (_, Sc body) -> wild_closed body
  | App (l, r) -> (wild_closed l) && (wild_closed r)
  | TyAbs (Sc body) -> wild_closed body
  | TyApp (f, _) -> wild_closed f
  | SensAbs (Sc body) -> wild_closed body
  | SensApp (f, _) -> wild_closed f
  | Wild (_) -> false
  | _ -> true

(* printing *)
let rec to_string : t -> string =
  fun tm ->
    let stream = Name.Stream.of_root Stack.Empty in
    fst (to_string' tm stream)
and to_string' (tm : t) (s : Name.Stream.t): string * Name.Stream.t = match tm with
  | Free n -> (Name.to_string n, s)
  | Bound i -> (string_of_int i, s)
  | Abs (dt, body) ->
    let x, s' = Name.Stream.draw_abs s in
    let dt', s'' = Dtype.to_string' dt s' in
    let body', s''' = to_string' (instantiate (Free x) body) s'' in
    ("\\" ^ (Name.to_string x) ^ ":" ^ dt' ^ "." ^ body', s''')
  | App (l, r) ->
    let l', s' = to_string' l s in
    let r', s'' = to_string' r s' in
    (l' ^ " (" ^ r' ^ ")", s'')
  | TyAbs body ->
    let a, s' = Name.Stream.draw_dt s in
    let body', s'' = to_string' (instantiate_dtype (Dtype.Free a) body) s' in
    ("\\" ^ (Name.to_string a) ^ "." ^ body', s'')
  | TyApp (f, dt) ->
    let f', s' = to_string' f s in
    let dt', s'' = Dtype.to_string' dt s' in
    (f' ^ " [" ^ dt' ^ "]", s'')
  | SensAbs body ->
    let k, s' = Name.Stream.draw_sens s in
    let body', s'' = to_string' (instantiate_sens (Sensitivity.Free k) body) s' in
    ("\\" ^ (Name.to_string k) ^ "." ^ body', s'')
  | SensApp (f, sens) ->
    let f', s' = to_string' f s in
    let sens' = Sensitivity.to_string sens in
    (f' ^ " [" ^ sens' ^ "]", s')
  | Const c -> (Value.to_string c, s)
  | Prim (n, src) -> (n, s)
  | Wild (context, dom, body) ->
    let w, s' = Name.Stream.draw_wild s in
    let context' = Context.to_string context in
    let dt', s'' = Dtype.to_string' dom s' in
    let body', s''' = to_string' (instantiate (Free w) body) s'' in
    ("\\" ^ (Name.to_string w) ^ ":<" ^ dt' ^ ", " ^ context' ^ ">." ^ body', s''')
