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
    | MatchNat : t * t * Sensitivity.t * (o scope) -> t
    | MatchCons : Dtype.t * t * t * Sensitivity.t * ((o s) scope) -> t
    (* type and sensitivity polymorphism *)
    | TypeAbs : (o scope) -> t
    | TypeApp : t * Dtype.t -> t
    | SensAbs : (o scope) -> t
    | SensApp : t * Sensitivity.t -> t
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
    | Function : string * (t -> t option) -> t
    (* recursion *)
    | Fix : Dtype.t * (o scope) -> t
    (* probability layer *)
    | Do : t -> t
    | LetDraw : Dtype.t * t * (o scope) -> t
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
    | MatchNat (e, zero, i, succ) ->
        let e' = abstract' n db e in
        let zero' = abstract' n db zero in
        let i' = Sensitivity.abstract' n db i in
        let succ' = bind_map (abstract_depth n db) succ in
            MatchNat (e', zero', i', succ')
    | MatchCons (dt, e, nil, i, cons) ->
        let dt' = Dtype.abstract' n db dt in
        let e' = abstract' n db e in
        let nil' = abstract' n db nil in
        let i' = Sensitivity.abstract' n db i in
        let cons' = bind_map (abstract_depth n db) cons in
            MatchCons (dt', e', nil', i', cons')
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
    | Fix (dt, body) ->
        let dt' = Dtype.abstract' n db dt in 
        let body' = bind_map (abstract_depth n db) body in
            Fix (dt', body')
    (* probability layer *)
    | Do tm ->
        Do (abstract' n db tm)
    | LetDraw (dt, dist, usage) ->
        let dt' = Dtype.abstract' n db dt in 
        let dist' = abstract' n db dist in
        let usage' = bind_map (abstract_depth n db) usage in
            LetDraw (dt', dist', usage')
    | Return tm ->
        Return (abstract' n db tm)
    (* if there's no recursion, just return *)
    | _ -> tm

(* alternate abstractions *)
let abstract_one = fun n -> fun tm -> abstract (N.of_one n) tm
let abstract_two = fun n -> fun m -> fun tm -> abstract (N.of_two n m) tm

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
    | MatchNat (e, zero, i, succ) ->
        let e' = instantiate' img db e in
        let zero' = instantiate' img db zero in
        let succ' = bind_map (instantiate_depth img db) succ in
            MatchNat (e', zero', i, succ')
    | MatchCons (dt, e, nil, i, cons) ->
        let e' = instantiate' img db e in
        let nil' = instantiate' img db nil in
        let cons' = bind_map (instantiate_depth img db) cons in
            MatchCons (dt, e', nil', i, cons')
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
    | Fix (dt, body) ->
        let body' = bind_map (instantiate_depth img db) body in
            Fix (dt, body')
    (* probability layer *)
    | Do tm ->
        Do (instantiate' img db tm)
    | LetDraw (dt, dist, usage) ->
        let dist' = instantiate' img db dist in
        let usage' = bind_map (instantiate_depth img db) usage in
            LetDraw (dt, dist', usage')
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
    | MatchNat (e, zero, i, succ) ->
        let e' = instantiate_dtype' img db e in
        let zero' = instantiate_dtype' img db zero in
        let succ' = bind_map (instantiate_dtype_depth img db) succ in
            MatchNat (e', zero', i, succ')
    | MatchCons (dt, e, nil, i, cons) ->
        let dt' = Dtype.instantiate' img db dt in
        let e' = instantiate_dtype' img db e in
        let nil' = instantiate_dtype' img db nil in
        let cons' = bind_map (instantiate_dtype_depth img db) cons in
            MatchCons (dt', e', nil', i, cons')
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
    | Fix (dt, body) ->
        let dt' = Dtype.instantiate' img db dt in
        let body' = bind_map (instantiate_dtype_depth img db) body in
            Fix (dt', body')
    (* probability layer *)
    | Do tm ->
        Do (instantiate_dtype' img db tm)
    | LetDraw (dt, dist, usage) ->
        let dt' = Dtype.instantiate' img db dt in
        let dist' = instantiate_dtype' img db dist in
        let usage' = bind_map (instantiate_dtype_depth img db) usage in
            LetDraw (dt', dist', usage')
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
    | MatchNat (e, zero, i, succ) ->
        let e' = instantiate_sens' img db e in
        let zero' = instantiate_sens' img db zero in
        let i' = Sensitivity.instantiate' img db i in
        let succ' = bind_map (instantiate_sens_depth img db) succ in
            MatchNat (e', zero', i', succ')
    | MatchCons (dt, e, nil, i, cons) ->
        let dt' = Dtype.instantiate_sens' img db dt in
        let e' = instantiate_sens' img db e in
        let nil' = instantiate_sens' img db nil in
        let i' = Sensitivity.instantiate' img db i in
        let cons' = bind_map (instantiate_sens_depth img db) cons in
            MatchCons (dt', e', nil', i', cons')
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
    | Fix (dt, body) ->
        let dt' = Dtype.instantiate_sens' img db dt in
        let body' = bind_map (instantiate_sens_depth img db) body in
            Fix (dt', body')
    (* probability layer *)
    | Do tm ->
        Do (instantiate_sens' img db tm)
    | LetDraw (dt, dist, usage) ->
        let dt' = Dtype.instantiate_sens' img db dt in
        let dist' = instantiate_sens' img db dist in
        let usage' = bind_map (instantiate_sens_depth img db) usage in
            LetDraw (dt', dist', usage')
    | Return tm ->
        Return (instantiate_sens' img db tm)
    (* for everything else, there's the default value *)
    | _ -> tm

(* printing *)
let rec to_string : t -> string = function
    | Var (Free n) -> Name.to_string n
    | Var (Bound i) -> string_of_int i
    | Abs (dt, Scope body) ->
        let dt' = Dtype.to_string dt in
        let body' = to_string body in
            "Abs(" ^ dt' ^ ", " ^ body' ^ ")"
    | App (f, arg) ->
        let f' = to_string f in
        let arg' = to_string arg in
            "App(" ^ f' ^ ", " ^ arg' ^ ")"
    | MatchNat (e, zero, i, Scope succ) ->
        let e' = to_string e in
        let zero' = to_string zero in
        let i' = Sensitivity.to_string i in
        let succ' = to_string succ in
            "MatchNat(" ^ e' ^ ", " ^ zero' ^ ", " ^ ", " ^ i' ^ ", " ^ succ' ^ ")"
    | MatchCons (dt, e, nil, i, Widen (Scope cons)) ->
        let dt' = Dtype.to_string dt in
        let e' = to_string e in
        let nil' = to_string nil in
        let i' = Sensitivity.to_string i in
        let cons' = to_string cons in
            "MatchNat(" ^ dt' ^ ", " ^ e' ^ ", " ^ nil' ^ ", " ^ i' ^ ", " ^ cons' ^ ")"
    | TypeAbs (Scope tm) ->
        to_string tm
    | TypeApp (tm, dt) -> to_string tm
    | SensAbs (Scope tm) ->
        to_string tm
    | SensApp (tm, sens) -> to_string tm
    | Wild (context, dt, Scope body) ->
        let context' = Context.to_string context in
        let dt' = Dtype.to_string dt in
        let body' = to_string body in
            "Wild(" ^ context' ^ ", " ^ dt' ^ ", " ^ body' ^ ")"
    | Nat (Zero) -> "Zero"
    | Nat (Succ tm) -> "Succ(" ^ (to_string tm) ^ ")"
    | ConsList (Nil) -> "Nil"
    | ConsList (Cons (hd, tl)) ->
        let hd' = to_string hd in
        let tl' = to_string tl in
            "Cons(" ^ hd' ^ ", " ^ tl' ^ ")"
    | Bool (True) -> "True"
    | Bool (False) -> "False"
    | Discrete s -> s
    | Pair (l, r) ->
        let l' = to_string l in
        let r' = to_string r in
            "(" ^ l' ^ ", " ^ r' ^ ")"
    | Real f -> string_of_float f
    | Bag ts -> "BAG"
    | Function (id, _) -> id
    | Fix (dt, Scope body) -> "Fix(" ^ (Dtype.to_string dt) ^ ", " ^ (to_string body) ^ ")"
    | Do tm -> "Do(" ^ (to_string tm) ^ ")"
    | Return tm -> "Return(" ^ (to_string tm) ^ ")"
    | LetDraw (dt, dist, Scope usage) ->
        let dt' = Dtype.to_string dt in
        let dist' = to_string dist in
        let usage' = to_string usage in
            "LetDraw(" ^ dt' ^ ", " ^ dist' ^ ", " ^ usage' ^ ")"

(* evaluation *)
open CCOpt.Infix

let rec eval : t -> t option = function
    (* compile function by wrapping it in a closure *)
    | Abs (_, body) ->
        let f = fun v -> eval (instantiate (N.of_one v) body) in
            Some (Function ("CLOSURE", f))

    (* function application happens in stages *)
    | App (Function (_, f), arg) -> f arg >>= eval
    | App ((Fix (_, body) as fix), arg) ->
        let e = instantiate (N.of_one fix) body in
        let tm = App (e, arg) in
            eval tm
    | App (tm, arg) -> begin match eval tm with
        | Some tm -> eval (App (tm, arg))
        | _ -> None
        end
    
    (* pattern matching *)
    | MatchNat (e, zero, _, succ) -> begin match (eval e) with
        | Some (Nat Zero) -> eval zero
        | Some (Nat (Succ tm)) -> eval (instantiate (N.of_one tm) succ)
        | _ -> None
        end
    | MatchCons (_, e, nil, _, cons) -> begin match (eval e) with
        | Some (ConsList Nil) -> eval nil
        | Some (ConsList (Cons (hd, tl))) -> eval (instantiate (N.of_two hd tl) cons)
        | _ -> None
        end

    (* we basically skip these *)
    | TypeApp (tm, dt) -> eval tm
    | SensApp (tm, dt) -> eval tm
    
    (* evaluating values *)
    | Nat (Succ tm) -> CCOpt.map (fun x -> Nat (Succ x)) (eval tm)
    | ConsList (Cons (hd, tl)) -> 
        CCOpt.map2 (fun x -> fun y -> ConsList (Cons (x, y))) (eval hd) (eval tl)
    | Pair (l, r) ->
        CCOpt.map2 (fun x -> fun y -> Pair (x, y)) (eval l) (eval r)
    | Bag ts -> ts
        |> CCList.map eval
        |> CCOpt.sequence_l
        |> CCOpt.map (fun x -> Bag x)

    (* probability layer *)
    | Do tm -> eval tm
    | Return tm -> eval tm
    | LetDraw (_, dist, usage) -> eval (instantiate (N.of_one dist) usage)

    (* catch-all *)
    | (_ as tm) -> Some tm

(* ease of construction *)
module Alt = struct
    (* wrapping simple constructions *)
    let var : string -> t = fun s -> Var (Free (Name.of_string s))

    let rec nat : int -> t = fun n -> match n with
        | 0 -> Nat (Zero)
        | n -> Nat (Succ (nat (n - 1)))

    let rec conslist : t list -> t = function
        | [] -> ConsList (Nil)
        | x :: xs -> ConsList (Cons (x, conslist xs))

    let fix (v : t) (body : t) : t = match v with
        | Var (Free n) -> Fix (Dtype.Base "test", abstract (N.of_one n) body)
        | _ -> v

    let abs (v : t) (body : t) : t = match v with
        | Var (Free n) -> Abs (Dtype.Base "test", abstract (N.of_one n) body)
        | _ -> v

    let match_nat (e : t) (zero : t) (sp : t * t) : t = match sp with
        | (Var (Free n), succ) ->
            MatchNat (e, zero, Sensitivity.Free (Name.of_string "i"), abstract (N.of_one n) succ)
        | _ -> e
end

type vterm = t