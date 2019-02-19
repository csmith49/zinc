let unit = ()

type o = O
type 'a s = Succ : 'a -> 'a s

module N = struct
    type (_, _) t =
        | Base : 'a -> ('a, o) t
        | Cons : 'a * ('a, 'n) t -> ('a, 'n s) t
    
    let rec length : type a n . (a, n) t -> int = function
        | Base _ -> 1
        | Cons (_, rest) -> 1 + (length rest)

    let of_one : type a . a -> (a, o) t = fun x -> Base x
    let of_two : type a . a -> a -> (a, o s) t = fun x -> fun y -> Cons (x, Base y)
end

type t =
    (* basic lambda calculus constructions *)
    | Var : variable -> t
    | Abs : Dtype.t * (o scope) -> t
    | App : t * t -> t
    (* pattern-matching naturals and lists *)
    | MatchNat : t * t * (o scope) -> t
    | MatchCons : t * t * ((o s) scope) -> t
    (* type and sensitivity polymorphism *)
    | TypeAbs : (o scope) -> t
    | TypeApp : t * Dtype.t -> t
    | SensAbs : (o scope) -> t
    | SensApp : t * Sensitivity.t -> t
    (* External function calls *)
    | External : string -> t
    (* wildcards for search *)
    | Wild : Context.t * Dtype.t * (o scope) -> t
    (* constant values *)
    | Nat of natural
    | ConsList of conslist
    | Bool of boolean
    | Discrete of string
    | Pair : t * t -> t
    | Real of float
    | Bag of t list
    | Function : string * (t -> t) -> t
    (* recursion *)
    | LetRec : (o scope) * (o scope) -> t
    (* probability layer *)
    | Do : t -> t
    | LetDraw : t * (o scope) -> t
    | Return : t -> t
and _ scope =
    | Scope : t -> o scope
    | Widen : 'n scope -> ('n s) scope
and variable =
    | Free of Name.t
    | Bound of int
and natural =
    | Zero
    | Succ of t
and conslist =
    | Nil
    | Cons of t * t
and boolean =
    | True
    | False

let rec binding_depth : type n . n scope -> int = function
    | Scope _ -> 1
    | Widen rest -> 1 + (binding_depth rest)

let rec scope_map : type n . (t -> t) -> n scope -> n scope = fun f -> function
    | Scope tm -> Scope (f tm)
    | Widen rest -> Widen (scope_map f rest)

let rec bind_map  : type n . (int -> t -> t) -> n scope -> n scope = fun f -> fun s -> bind_map' 0 f s
and bind_map' : type n . int -> (int -> t -> t) -> n scope -> n scope = fun depth -> fun f -> function
    | Scope tm -> Scope (f (depth + 1) tm)
    | Widen rest -> Widen (bind_map' (depth + 1) f rest)

(* mcbride and mckinna movement around binders *)
let rec abstract : type n . (Name.t, n) N.t -> t -> n scope = fun ns -> fun tm -> match ns with
    | N.Base n -> Scope (abstract' n 0 tm)
    | N.Cons (n, ns) ->
        let s = abstract ns tm in
        let s' = bind_map (abstract' n) s in
            Widen s'
and abstract_depth (n : Name.t) (db : int) : int -> t -> t = fun depth -> fun tm -> abstract' n (db + depth) tm
and abstract' (n : Name.t) (db : int) (tm : t) : t = match tm with
    (* when the name matches, put in the right depth *)
    | Var (Free x) -> if Name.eq n x then Var (Bound db) else tm
    | Abs (dt, s) ->
        let dt' = Dtype.abstract' n db dt in
        let s' = bind_map (abstract_depth n db) s in
            Abs (dt', s')
    | App (l, r) -> App (abstract' n db l, abstract' n db r)
    (* pattern-matching *)
    | MatchNat (e, zero, succ) ->
        let e' = abstract' n db e in
        let zero' = abstract' n db zero in
        let succ' = bind_map (abstract_depth n db) succ in
            MatchNat (e', zero', succ')
    | MatchCons (e, nil, cons) ->
        let e' = abstract' n db e in
        let nil' = abstract' n db nil in
        let cons' = bind_map (abstract_depth n db) cons in
            MatchCons (e', nil', cons')
    (* type and sensitivity polymorphism *)
    | TypeAbs s ->
        let s' = bind_map (abstract_depth n db) s in
            TypeAbs s'
    | TypeApp (tm, dt) ->
        TypeApp (abstract' n db tm, Dtype.abstract' n db dt)
    | SensAbs s ->
        let s' = bind_map (abstract_depth n db) s in
            SensAbs s'
    | SensApp (tm, sens) ->
        SensApp (abstract' n db tm, Sensitivity.abstract' n db sens)
    (* wildcards *)
    | Wild (context, dt, s) ->
        let dt' = Dtype.abstract' n db dt in
        let s' = bind_map (abstract_depth n db) s in
            Wild (context, dt', s')
    (* recurse over values *)
    | Nat (Succ t) ->
        Nat (Succ (abstract' n db t))
    | ConsList (Cons (hd, tl)) ->
        let hd' = abstract' n db hd in
        let tl' = abstract' n db tl in
            ConsList (Cons (hd', tl'))
    | Pair (l, r) -> Pair (abstract' n db l, abstract' n db r)
    | Bag ts -> Bag (CCList.map (abstract' n db) ts)
    (* recursion *)
    | LetRec (body, usage) ->
        let body' = bind_map (abstract_depth n db) body in
        let usage' = bind_map (abstract_depth n db) usage in
            LetRec (body', usage')
    (* probability layer *)
    | Do tm ->
        Do (abstract' n db tm)
    | LetDraw (dist, usage) ->
        let dist' = abstract' n db dist in
        let usage' = bind_map (abstract_depth n db) usage in
            LetDraw (dist', usage')
    | Return tm ->
        Return (abstract' n db tm)
    (* if there's no recursion, just return *)
    | _ -> tm

let rec instantiate : type n . (t, n) N.t -> n scope -> t = fun imgs -> fun s -> match imgs with
    | N.Base img -> begin match s with
        | Scope tm -> instantiate' img 0 tm
    end
    | N.Cons (img, rest) -> begin match s with
        | Widen s ->
            let tm = instantiate rest s in
            let depth = binding_depth s in
                instantiate' img depth tm
    end
and instantiate_depth (img : t) (db : int) : int -> t -> t = fun depth -> fun tm -> instantiate' img (db + depth) tm
and instantiate' (img : t) (db : int) (tm : t) : t = match tm with
    (* when we're at the right depth, put the image in *)
    | Var (Bound i) -> if i = db then img else tm
    | Abs (dt, s) ->
        let s' = bind_map (instantiate_depth img db) s in
            Abs (dt, s')
    | App (l, r) ->
        App (instantiate' img db l, instantiate' img db r)
    (* pattern-matching *)
    | MatchNat (e, zero, succ) ->
        let e' = instantiate' img db e in
        let zero' = instantiate' img db zero in
        let succ' = bind_map (instantiate_depth img db) succ in
            MatchNat (e', zero', succ')
    | MatchCons (e, nil, cons) ->
        let e' = instantiate' img db e in
        let nil' = instantiate' img db nil in
        let cons' = bind_map (instantiate_depth img db) cons in
            MatchCons (e', nil', cons')
    (* type and sensitivity polymorphism - note the lack of recursion into apps *)
    | TypeAbs s ->
        let s' = bind_map (instantiate_depth img db) s in
            TypeAbs s'
    | TypeApp (tm, dt) ->
        TypeApp (instantiate' img db tm, dt)
    | SensAbs s ->
        let s' = bind_map (instantiate_depth img db) s in
            SensAbs s'
    | SensApp (tm, sens) ->
        SensApp (instantiate' img db tm, sens)
    (* wildcards *)
    | Wild (context, dt, s) ->
        let s' = bind_map (instantiate_depth img db) s in
            Wild (context, dt, s')
    (* recursing over values *)
    | Nat (Succ t) ->
        Nat (Succ (instantiate' img db t))
    | ConsList (Cons (hd, tl)) ->
        let hd' = instantiate' img db hd in
        let tl' = instantiate' img db tl in
            ConsList (Cons (hd', tl'))
    | Pair (l, r) -> Pair (instantiate' img db l, instantiate' img db r)
    | Bag ts -> Bag (CCList.map (instantiate' img db) ts)
    (* recursion *)
    | LetRec (body, usage) ->
        let body' = bind_map (instantiate_depth img db) body in
        let usage' = bind_map (instantiate_depth img db) usage in
            LetRec (body', usage')
    (* probability layer *)
    | Do tm ->
        Do (instantiate' img db tm)
    | LetDraw (dist, usage) ->
        let dist' = instantiate' img db dist in
        let usage' = bind_map (instantiate_depth img db) usage in
            LetDraw (dist', usage')
    | Return tm ->
        Return (instantiate' img db tm)
    (* for everything else, there's the default value *)
    | _ -> tm

(* modification to instantiate dtypes instead *)
let rec instantiate_dtype : type n . (Dtype.t, n) N.t -> n scope -> t = fun imgs -> fun s -> match imgs with
    | N.Base img -> begin match s with
        | Scope tm -> instantiate_dtype' img 0 tm
    end
    | N.Cons (img, rest) -> begin match s with
        | Widen s ->
            let tm = instantiate_dtype rest s in
            let depth = binding_depth s in
                instantiate_dtype' img depth tm
    end
and instantiate_dtype_depth (img : Dtype.t) (db : int) : int -> t -> t = 
    fun depth -> fun tm -> instantiate_dtype' img (db + depth) tm
and instantiate_dtype' (img : Dtype.t) (db : int) (tm : t) : t = match tm with
    (* don't increment db when we break into dtype.instantiate' *)
    | Abs (dt, s) ->
        let dt' = Dtype.instantiate' img db dt in
        let s' = bind_map (instantiate_dtype_depth img db) s in
            Abs (dt', s')
    | App (l, r) ->
        App (instantiate_dtype' img db l, instantiate_dtype' img db r)
    (* pattern-matching *)
    | MatchNat (e, zero, succ) ->
        let e' = instantiate_dtype' img db e in
        let zero' = instantiate_dtype' img db zero in
        let succ' = bind_map (instantiate_dtype_depth img db) succ in
            MatchNat (e', zero', succ')
    | MatchCons (e, nil, cons) ->
        let e' = instantiate_dtype' img db e in
        let nil' = instantiate_dtype' img db nil in
        let cons' = bind_map (instantiate_dtype_depth img db) cons in
            MatchCons (e', nil', cons')
    (* type and sensitivity polymorphism - note the lack of recursion into apps *)
    | TypeAbs s ->
        let s' = bind_map (instantiate_dtype_depth img db) s in
            TypeAbs s'
    | TypeApp (tm, dt) ->
        TypeApp (instantiate_dtype' img db tm, Dtype.instantiate' img db dt)
    | SensAbs s ->
        let s' = bind_map (instantiate_dtype_depth img db) s in
            SensAbs s'
    | SensApp (tm, sens) ->
        SensApp (instantiate_dtype' img db tm, sens)
    (* wildcards *)
    | Wild (context, dt, s) ->
        let dt' = Dtype.instantiate' img db dt in
        let s' = bind_map (instantiate_dtype_depth img db) s in
            Wild (context, dt', s')
    (* recursing over values *)
    | Nat (Succ t) ->
        Nat (Succ (instantiate_dtype' img db t))
    | ConsList (Cons (hd, tl)) ->
        let hd' = instantiate_dtype' img db hd in
        let tl' = instantiate_dtype' img db tl in
            ConsList (Cons (hd', tl'))
    | Pair (l, r) -> Pair (instantiate_dtype' img db l, instantiate_dtype' img db r)
    | Bag ts -> Bag (CCList.map (instantiate_dtype' img db) ts)
    (* recursion *)
    | LetRec (body, usage) ->
        let body' = bind_map (instantiate_dtype_depth img db) body in
        let usage' = bind_map (instantiate_dtype_depth img db) usage in
            LetRec (body', usage')
    (* probability layer *)
    | Do tm ->
        Do (instantiate_dtype' img db tm)
    | LetDraw (dist, usage) ->
        let dist' = instantiate_dtype' img db dist in
        let usage' = bind_map (instantiate_dtype_depth img db) usage in
            LetDraw (dist', usage')
    | Return tm ->
        Return (instantiate_dtype' img db tm)
    (* for everything else, there's the default value *)
    | _ -> tm

(* modification to instantiate sensitivities *)
let rec instantiate_sens : type n . (Sensitivity.t, n) N.t -> n scope -> t = fun imgs -> fun s -> match imgs with
    | N.Base img -> begin match s with
        | Scope tm -> instantiate_sens' img 0 tm
    end
    | N.Cons (img, rest) -> begin match s with
        | Widen s ->
            let tm = instantiate_sens rest s in
            let depth = binding_depth s in
                instantiate_sens' img depth tm
    end
and instantiate_sens_depth (img : Sensitivity.t) (db : int) : int -> t -> t =
    fun depth -> fun tm -> instantiate_sens' img (db + depth) tm
and instantiate_sens' (img : Sensitivity.t) (db : int) (tm : t) : t = match tm with
    (* don't increment db when moving into sensitivity.instantiate' *)
    | Abs (dt, s) ->
        let dt' = Dtype.instantiate_sens' img db dt in
        let s' = bind_map (instantiate_sens_depth img db) s in
            Abs (dt', s')
    | App (l, r) ->
        App (instantiate_sens' img db l, instantiate_sens' img db r)
    (* pattern-matching *)
    | MatchNat (e, zero, succ) ->
        let e' = instantiate_sens' img db e in
        let zero' = instantiate_sens' img db zero in
        let succ' = bind_map (instantiate_sens_depth img db) succ in
            MatchNat (e', zero', succ')
    | MatchCons (e, nil, cons) ->
        let e' = instantiate_sens' img db e in
        let nil' = instantiate_sens' img db nil in
        let cons' = bind_map (instantiate_sens_depth img db) cons in
            MatchCons (e', nil', cons')
    (* type and sensitivity polymorphism - note the lack of recursion into apps *)
    | TypeAbs s ->
        let s' = bind_map (instantiate_sens_depth img db) s in
            TypeAbs s'
    | TypeApp (tm, dt) ->
        TypeApp (instantiate_sens' img db tm, Dtype.instantiate_sens' img db dt)
    | SensAbs s ->
        let s' = bind_map (instantiate_sens_depth img db) s in
            SensAbs s'
    | SensApp (tm, sens) ->
        SensApp (instantiate_sens' img db tm, Sensitivity.instantiate' img db sens)
    (* wildcards *)
    | Wild (context, dt, s) ->
        let dt' = Dtype.instantiate_sens' img db dt in
        let s' = bind_map (instantiate_sens_depth img db) s in
            Wild (context, dt', s')
    (* recursing over values *)
    | Nat (Succ t) ->
        Nat (Succ (instantiate_sens' img db t))
    | ConsList (Cons (hd, tl)) ->
        let hd' = instantiate_sens' img db hd in
        let tl' = instantiate_sens' img db tl in
            ConsList (Cons (hd', tl'))
    | Pair (l, r) -> Pair (instantiate_sens' img db l, instantiate_sens' img db r)
    | Bag ts -> Bag (CCList.map (instantiate_sens' img db) ts)
    (* recursion *)
    | LetRec (body, usage) ->
        let body' = bind_map (instantiate_sens_depth img db) body in
        let usage' = bind_map (instantiate_sens_depth img db) usage in
            LetRec (body', usage')
    (* probability layer *)
    | Do tm ->
        Do (instantiate_sens' img db tm)
    | LetDraw (dist, usage) ->
        let dist' = instantiate_sens' img db dist in
        let usage' = bind_map (instantiate_sens_depth img db) usage in
            LetDraw (dist', usage')
    | Return tm ->
        Return (instantiate_sens' img db tm)
    (* for everything else, there's the default value *)
    | _ -> tm

(* for reference in submodules *)
type vterm = t

(* allows us to move across binders in zippers *)
module Prefix = struct
    (* bindings contain all the information to recreate the og term *)
    type binding =
        | BAbs of Name.t * Dtype.t
        | BTypeAbs of Name.t
        | BSensAbs of Name.t
        | BWild of Name.t * Context.t * Dtype.t
        | BMatchNat of Name.t * vterm * vterm
        | BMatchCons of Name.t * Name.t * vterm * vterm
        | BLetRecBody of Name.t * (o scope)
        | BLetRecUsage of Name.t * (o scope)
        | BLetDraw of Name.t * vterm
    (* and prefixes maintain a list of bindings *)
    type t = binding list
    (* alternate syntax - common across all zippers and whatnot *)
    module Alt = struct
        let (@>) (b : binding) (tm : vterm) : vterm = match b with
            | BAbs (n, dt) -> Abs (dt, abstract (N.of_one n) tm)
            | BTypeAbs n -> TypeAbs (abstract (N.of_one n) tm)
            | BSensAbs n -> SensAbs (abstract (N.of_one n) tm)
            | BWild (n, context, dt) ->
                Wild (context, dt, abstract (N.of_one n) tm)
            | BMatchNat (n, e, zero) ->
                MatchNat (e, zero, abstract (N.of_one n) tm)
            | BMatchCons (n, ns, e, nil) ->
                MatchCons (e, nil, abstract (N.of_two n ns) tm)
            | BLetRecBody (n, usage) ->
                LetRec (abstract (N.of_one n) tm, usage)
            | BLetRecUsage (n, body) ->
                LetRec (body, abstract (N.of_one n) tm)
            | BLetDraw (n, dist) ->
                LetDraw (dist, abstract (N.of_one n) tm)
        let (<@) (n : Name.t) (tm : vterm) : (binding * vterm) list = match tm with
            | Abs (dt, tm) -> 
                let n' = Var (Free n) in [
                    (BAbs (n, dt), instantiate (N.of_one n') tm)
                ]
            | TypeAbs tm ->
                let n' = Dtype.Free n in [
                    (BTypeAbs n, instantiate_dtype (N.of_one n') tm)
                ]
            | SensAbs tm ->
                let n' = Sensitivity.Free n in [
                    (BSensAbs n, instantiate_sens (N.of_one n') tm)
                ]
            | Wild (context, dt, tm) ->
                let n' = Var (Free n) in [
                    (BWild (n, context, dt), instantiate (N.of_one n') tm)
                ]
            | MatchNat (e, zero, succ) ->
                let n' = Var (Free n) in [
                    (BMatchNat (n, e, zero), instantiate (N.of_one n') succ)
                ]
            | MatchCons (e, nil, cons) ->
                let open Name.Alt in
                    let n_head = n <+ "hd" in
                    let n_tail = n <+ "tl" in
                    let n_head' = Var (Free n_head) in
                    let n_tail' = Var (Free n_tail) in [
                        (BMatchCons (n_head, n_tail, e, nil), instantiate (N.of_two n_head' n_tail') cons)
                    ]
            | LetRec (body, usage) ->
                let n' = Var (Free n) in
                let body' = (
                    BLetRecBody (n, usage), instantiate (N.of_one n') body
                ) in
                let usage' = (
                    BLetRecUsage (n, body), instantiate (N.of_one n') usage
                ) in [
                    body'; usage'
                ]
            | LetDraw (dist, usage) ->
                let n' = Var (Free n) in [
                    (BLetDraw (n, dist), instantiate (N.of_one n') usage)
                ]
            | _ -> []
    end
    open Alt
    (* append the entirety of a prefix *)
    let rec bind (prefix : t) (tm : vterm) : vterm = match prefix with
        | [] -> tm
        | b :: rest -> bind rest (b @> tm)
end