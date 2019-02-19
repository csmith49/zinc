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

(* mcbride and mckinna movement around binders *)
let rec abstract : type n . (Name.t, n) N.t -> t -> n scope = fun ns -> fun tm -> match ns with
    | N.Base n -> Scope (abstract' n 0 tm)
    | N.Cons (n, ns) ->
        let s = abstract ns tm in
        let bd = binding_depth s in
        let s' = scope_map (abstract' n bd) s in
            Widen s'
and abstract' (n : Name.t) (db : int) (tm : t) : t = match tm with
    (* when the name matches, put in the right depth *)
    | Var (Free x) -> if Name.eq n x then Var (Bound db) else tm
    | Abs (dt, s) ->
        let dt' = Dtype.abstract' n db dt in
        let depth = binding_depth s in
        let s' = scope_map (abstract' n (db + depth)) s in
            Abs (dt', s')
    | App (l, r) -> App (abstract' n db l, abstract' n db r)
    (* pattern-matching *)
    | MatchNat (e, zero, succ) ->
        let e' = abstract' n db e in
        let zero' = abstract' n db zero in
        let depth = binding_depth succ in
        let succ' = scope_map (abstract' n (db + depth)) succ in
            MatchNat (e', zero', succ')
    | MatchCons (e, nil, cons) ->
        let e' = abstract' n db e in
        let nil' = abstract' n db nil in
        let depth = binding_depth cons in
        let cons' = scope_map (abstract' n (db + depth)) cons in
            MatchCons (e', nil', cons')
    (* type and sensitivity polymorphism *)
    | TypeAbs s ->
        let depth = binding_depth s in
        let s' = scope_map (abstract' n (db + depth)) s in
            TypeAbs s'
    | TypeApp (tm, dt) ->
        TypeApp (abstract' n db tm, Dtype.abstract' n db dt)
    | SensAbs s ->
        let depth = binding_depth s in
        let s' = scope_map (abstract' n (db + depth)) s in
            SensAbs s'
    | SensApp (tm, sens) ->
        SensApp (abstract' n db tm, Sensitivity.abstract' n db sens)
    (* wildcards *)
    | Wild (context, dt, s) ->
        let dt' = Dtype.abstract' n db dt in
        let depth = binding_depth s in
        let s' = scope_map (abstract' n (db + depth)) s in
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
        let body_depth = binding_depth body in
        let body' = scope_map (abstract' n (db + body_depth)) body in
        let usage_depth = binding_depth usage in
        let usage' = scope_map (abstract' n (db + usage_depth)) usage in
            LetRec (body', usage')
    (* probability layer *)
    | Do tm ->
        Do (abstract' n db tm)
    | LetDraw (dist, usage) ->
        let dist' = abstract' n db dist in
        let depth = binding_depth usage in
        let usage' = scope_map (abstract' n (db + depth)) usage in
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
and instantiate' (img : t) (db : int) (tm : t) : t = match tm with
    (* when we're at the right depth, put the image in *)
    | Var (Bound i) -> if i = db then img else tm
    | Abs (dt, s) ->
        let depth = binding_depth s in
        let s' = scope_map (instantiate' img (db + depth)) s in
            Abs (dt, s')
    | App (l, r) ->
        App (instantiate' img db l, instantiate' img db r)
    (* pattern-matching *)
    | MatchNat (e, zero, succ) ->
        let e' = instantiate' img db e in
        let zero' = instantiate' img db zero in
        let depth = binding_depth succ in
        let succ' = scope_map (instantiate' img (db + depth)) succ in
            MatchNat (e', zero', succ')
    | MatchCons (e, nil, cons) ->
        let e' = instantiate' img db e in
        let nil' = instantiate' img db nil in
        let depth = binding_depth cons in
        let cons' = scope_map (instantiate' img (db + depth)) cons in
            MatchCons (e', nil', cons')
    (* type and sensitivity polymorphism - note the lack of recursion into apps *)
    | TypeAbs s ->
        let depth = binding_depth s in
        let s' = scope_map (instantiate' img (db + depth)) s in
            TypeAbs s'
    | TypeApp (tm, dt) ->
        TypeApp (instantiate' img db tm, dt)
    | SensAbs s ->
        let depth = binding_depth s in
        let s' = scope_map (instantiate' img (db + depth)) s in
            SensAbs s'
    | SensApp (tm, sens) ->
        SensApp (instantiate' img db tm, sens)
    (* wildcards *)
    | Wild (context, dt, s) ->
        let depth = binding_depth s in
        let s' = scope_map (instantiate' img (db + depth)) s in
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
        let body_depth = binding_depth body in
        let body' = scope_map (instantiate' img (db + body_depth)) body in
        let usage_depth = binding_depth usage in
        let usage' = scope_map (instantiate' img (db + usage_depth)) usage in
            LetRec (body', usage')
    (* probability layer *)
    | Do tm ->
        Do (instantiate' img db tm)
    | LetDraw (dist, usage) ->
        let dist' = instantiate' img db dist in
        let depth = binding_depth usage in
        let usage' = scope_map (instantiate' img (db + depth)) usage in
            LetDraw (dist', usage')
    | Return tm ->
        Return (instantiate' img db tm)
    (* for everything else, there's the default value *)
    | _ -> tm
