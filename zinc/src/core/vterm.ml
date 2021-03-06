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
    | Abs : abs_tag * (o scope) -> t
    | App : t * t -> t
    (* pattern-matching naturals and lists *)
    | MatchNat : nat_tag * t * t * (o scope) -> t
    | MatchCons : cons_tag * t * t * ((o s) scope) -> t
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
    | Function : string * (t -> evaluation) -> t
    (* recursion *)
    | Fix : fix_tag * (o scope) -> t
    (* probability layer *)
    | Do : t -> t
    | LetDraw : draw_tag * t * (o scope) -> t
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
and evaluation =
    | Diverge
    | Value of t
and abs_tag = { a_var : Name.t; a_dt : Dtype.t; }
and nat_tag = { n_var : Name.t; n_sens : Sensitivity.t; }
and cons_tag = { c_hd : Name.t; c_tl : Name.t; c_dt : Dtype.t; c_sens : Sensitivity.t; }
and fix_tag = { f_var : Name.t; f_dt : Dtype.t; }
and draw_tag = { d_var : Name.t; d_dt : Dtype.t; }

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
    | Abs (tag, s) ->
        let dt = Dtype.abstract' n db tag.a_dt in
        let s' = bind_map (abstract_depth n db) s in
            Abs ({tag with a_dt = dt}, s')
    | App (l, r) -> App (abstract' n db l, abstract' n db r)
    (* pattern-matching *)
    | MatchNat (tag, e, zero, succ) ->
        let e' = abstract' n db e in
        let zero' = abstract' n db zero in
        let sens = Sensitivity.abstract' n db tag.n_sens in
        let succ' = bind_map (abstract_depth n db) succ in
            MatchNat ({tag with n_sens = sens}, e', zero', succ')
    | MatchCons (tag, e, nil, cons) ->
        let dt = Dtype.abstract' n db tag.c_dt in
        let e' = abstract' n db e in
        let nil' = abstract' n db nil in
        let sens = Sensitivity.abstract' n db tag.c_sens in
        let cons' = bind_map (abstract_depth n db) cons in
            MatchCons ({tag with c_dt = dt; c_sens = sens}, e', nil', cons')
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
    | Fix (tag, body) ->
        let dt = Dtype.abstract' n db tag.f_dt in 
        let body' = bind_map (abstract_depth n db) body in
            Fix ({tag with f_dt = dt}, body')
    (* probability layer *)
    | Do tm ->
        Do (abstract' n db tm)
    | LetDraw (tag, dist, usage) ->
        let dt = Dtype.abstract' n db tag.d_dt in 
        let dist' = abstract' n db dist in
        let usage' = bind_map (abstract_depth n db) usage in
            LetDraw ({tag with d_dt = dt}, dist', usage')
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
    | Abs (tag, s) ->
        let s' = bind_map (instantiate_depth img db) s in
            Abs (tag, s')
    | App (l, r) ->
        App (instantiate' img db l, instantiate' img db r)
    (* pattern-matching *)
    | MatchNat (tag, e, zero, succ) ->
        let e' = instantiate' img db e in
        let zero' = instantiate' img db zero in
        let succ' = bind_map (instantiate_depth img db) succ in
            MatchNat (tag, e', zero', succ')
    | MatchCons (tag, e, nil, cons) ->
        let e' = instantiate' img db e in
        let nil' = instantiate' img db nil in
        let cons' = bind_map (instantiate_depth img db) cons in
            MatchCons (tag, e', nil', cons')
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
    | Fix (tag, body) ->
        let body' = bind_map (instantiate_depth img db) body in
            Fix (tag, body')
    (* probability layer *)
    | Do tm ->
        Do (instantiate' img db tm)
    | LetDraw (tag, dist, usage) ->
        let dist' = instantiate' img db dist in
        let usage' = bind_map (instantiate_depth img db) usage in
            LetDraw (tag, dist', usage')
    | Return tm ->
        Return (instantiate' img db tm)
    (* for everything else, there's the default value *)
    | _ -> tm

let instantiate_one = fun n -> fun tm -> instantiate (N.of_one n) tm
let instantiate_two = fun n -> fun m -> fun tm -> instantiate (N.of_two n m) tm

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
    | Abs (tag, s) ->
        let dt = Dtype.instantiate' img db tag.a_dt in
        let s' = bind_map (instantiate_dtype_depth img db) s in
            Abs ({tag with a_dt = dt}, s')
    | App (l, r) ->
        App (instantiate_dtype' img db l, instantiate_dtype' img db r)
    (* pattern-matching *)
    | MatchNat (tag, e, zero, succ) ->
        let e' = instantiate_dtype' img db e in
        let zero' = instantiate_dtype' img db zero in
        let succ' = bind_map (instantiate_dtype_depth img db) succ in
            MatchNat (tag, e', zero', succ')
    | MatchCons (tag, e, nil, cons) ->
        let dt = Dtype.instantiate' img db tag.c_dt in
        let e' = instantiate_dtype' img db e in
        let nil' = instantiate_dtype' img db nil in
        let cons' = bind_map (instantiate_dtype_depth img db) cons in
            MatchCons ({tag with c_dt = dt}, e', nil', cons')
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
    | Fix (tag, body) ->
        let dt = Dtype.instantiate' img db tag.f_dt in
        let body' = bind_map (instantiate_dtype_depth img db) body in
            Fix ({tag with f_dt = dt}, body')
    (* probability layer *)
    | Do tm ->
        Do (instantiate_dtype' img db tm)
    | LetDraw (tag, dist, usage) ->
        let dt = Dtype.instantiate' img db tag.d_dt in
        let dist' = instantiate_dtype' img db dist in
        let usage' = bind_map (instantiate_dtype_depth img db) usage in
            LetDraw ({tag with d_dt = dt}, dist', usage')
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
    | Abs (tag, s) ->
        let dt = Dtype.instantiate_sens' img db tag.a_dt in
        let s' = bind_map (instantiate_sens_depth img db) s in
            Abs ({tag with a_dt = dt}, s')
    | App (l, r) ->
        App (instantiate_sens' img db l, instantiate_sens' img db r)
    (* pattern-matching *)
    | MatchNat (tag, e, zero, succ) ->
        let e' = instantiate_sens' img db e in
        let zero' = instantiate_sens' img db zero in
        let sens = Sensitivity.instantiate' img db tag.n_sens in
        let succ' = bind_map (instantiate_sens_depth img db) succ in
            MatchNat ({tag with n_sens = sens}, e', zero', succ')
    | MatchCons (tag, e, nil, cons) ->
        let dt = Dtype.instantiate_sens' img db tag.c_dt in
        let e' = instantiate_sens' img db e in
        let nil' = instantiate_sens' img db nil in
        let sens = Sensitivity.instantiate' img db tag.c_sens in
        let cons' = bind_map (instantiate_sens_depth img db) cons in
            MatchCons ({tag with c_dt = dt; c_sens = sens}, e', nil', cons')
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
    | Fix (tag, body) ->
        let dt = Dtype.instantiate_sens' img db tag.f_dt in
        let body' = bind_map (instantiate_sens_depth img db) body in
            Fix ({tag with f_dt = dt}, body')
    (* probability layer *)
    | Do tm ->
        Do (instantiate_sens' img db tm)
    | LetDraw (tag, dist, usage) ->
        let dt = Dtype.instantiate_sens' img db tag.d_dt in
        let dist' = instantiate_sens' img db dist in
        let usage' = bind_map (instantiate_sens_depth img db) usage in
            LetDraw ({tag with d_dt = dt}, dist', usage')
    | Return tm ->
        Return (instantiate_sens' img db tm)
    (* for everything else, there's the default value *)
    | _ -> tm

(* printing *)
let rec to_string : t -> string = function
    | Var (Free n) -> Name.to_string n
    | Var (Bound i) -> string_of_int i
    | Abs (tag, Scope body) ->
        let dt' = Dtype.to_string tag.a_dt in
        let body' = to_string body in
            "Abs(" ^ dt' ^ ", " ^ body' ^ ")"
    | App (f, arg) ->
        let f' = to_string f in
        let arg' = to_string arg in
            "App(" ^ f' ^ ", " ^ arg' ^ ")"
    | MatchNat (tag, e, zero, Scope succ) ->
        let e' = to_string e in
        let zero' = to_string zero in
        let i' = Sensitivity.to_string tag.n_sens in
        let succ' = to_string succ in
            "MatchNat(" ^ e' ^ ", " ^ zero' ^ ", " ^ ", " ^ i' ^ ", " ^ succ' ^ ")"
    | MatchCons (tag, e, nil, Widen (Scope cons)) ->
        let dt' = Dtype.to_string tag.c_dt in
        let e' = to_string e in
        let nil' = to_string nil in
        let i' = Sensitivity.to_string tag.c_sens in
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
    | Fix (tag, Scope body) -> "Fix(" ^ (Dtype.to_string tag.f_dt) ^ ", " ^ (to_string body) ^ ")"
    | Do tm -> "Do(" ^ (to_string tm) ^ ")"
    | Return tm -> "Return(" ^ (to_string tm) ^ ")"
    | LetDraw (tag, dist, Scope usage) ->
        let dt' = Dtype.to_string tag.d_dt in
        let dist' = to_string dist in
        let usage' = to_string usage in
            "LetDraw(" ^ dt' ^ ", " ^ dist' ^ ", " ^ usage' ^ ")"

let rec format : t -> string = fun tm ->
    let stream = Name.Stream.of_root (Name.of_string "") in
    fst (format' tm stream)
and format' (tm : t) (s : Name.Stream.t) : string * Name.Stream.t = match tm with
    | Var (Free n) -> (Name.to_string n, s)
    | Var (Bound i) -> (string_of_int i, s)
    | Abs (tag, body) ->
        let x, s = Name.Stream.draw_abs s in
        let body, s = format' (instantiate_one (Var (Free x)) body) s in
            ("λ" ^ (Name.to_string x) ^ "." ^ body, s)
    | App (l, r) ->
        let l, s = format' l s in
        let r, s = format' r s in
            (l ^ " " ^ r, s)
    | MatchNat (tag, e, zero, succ) ->
        let e, s = format' e s in
        let zero, s = format' zero s in
        let n, s = Name.Stream.draw_nat s in
        let succ, s = format' (instantiate_one (Var (Free n)) succ) s in
            ("match " ^ e ^ ": Z -> " ^ zero ^ " | " ^ (Name.to_string n) ^ " + 1 -> " ^ succ, s)
    | MatchCons (tag, e, nil, cons) ->
        let e, s = format' e s in
        let nil, s = format' nil s in
        let l, s = Name.Stream.draw_hd s in
        let ls, s = Name.Stream.draw_tl s in
        let cons, s  = format' (instantiate_two (Var (Free l)) (Var (Free ls)) cons) s in
            ("match " ^ e ^ ": [] -> " ^ nil ^ " | " ^ (Name.to_string l) ^ ":" ^ (Name.to_string ls) ^ " -> " ^ cons, s)
    | TypeAbs (Scope body) | SensAbs (Scope body) -> format' body s
    | TypeApp (f, _) | SensApp (f, _) -> format' f s
    | Wild (_, _, body) ->
        let w, s = Name.Stream.draw_wild s in
        let body, s = format' (instantiate_one (Var (Free w)) body) s in
        ("\\" ^ (Name.to_string w) ^ "." ^ body, s)
    | Nat (Zero) -> "Z", s
    | Nat (Succ tm) ->
        let tm, s = format' tm s in ("S(" ^ tm ^ ")", s)
    | ConsList (Nil) -> "[]", s
    | ConsList (Cons (hd, tl)) ->
        let hd, s = format' hd s in
        let tl, s = format' tl s in
            (hd ^ ":" ^ tl, s)
    | Bool True -> "true", s
    | Bool False -> "false", s
    | Discrete str -> str, s
    | Pair (l, r) ->
        let l, s = format' l s in
        let r, s = format' r s in
            ("(" ^ l ^ ", " ^ r ^ ")", s)
    | Real f -> string_of_float f, s
    | Bag ts -> ts
        |> CCList.map (fun tm -> format' tm s)
        |> CCList.map fst
        |> CCString.concat " ; "
        |> fun bag -> "[" ^ bag ^ "]", s
    | Function (id, _) -> id, s
    | Fix (_, body) ->
        let x, s = Name.Stream.draw_fix s in
        let body, s = format' (instantiate_one (Var (Free x)) body) s in
            ("fix " ^ (Name.to_string x) ^ "." ^ body, s)
    | Do tm ->
        let tm, s = format' tm s in
        ("do " ^ tm, s)
    | Return tm ->
        let tm, s = format' tm s in
        ("return " ^ tm, s)
    | LetDraw (_, dist, usage) ->
        let dist, s = format' dist s in
        let x, s = Name.Stream.draw_abs s in
        let usage, s = format' (instantiate_one (Var (Free x)) usage) s in
            ("sample " ^ (Name.to_string x) ^ " = " ^ dist ^ " in " ^ usage, s)

type vterm = t

(* evaluation *)
module Evaluation = struct
    type t = evaluation

    let return : vterm -> t = fun tm -> Value tm
    let pure = return

    let bind : t -> (vterm -> t) -> t = function
        | Diverge -> fun _ -> Diverge
        | Value tm -> fun f -> f tm

    let map : (vterm -> vterm) -> t  -> t = fun f -> function
        | Diverge -> Diverge
        | Value tm -> Value (f tm)
    let flat_map f x = bind x f
    let map2 : (vterm -> vterm -> vterm) -> t -> t -> t = fun f -> function
        | Diverge -> fun _ -> Diverge
        | Value tm -> map (f tm)
    let flat_map2 : (vterm -> vterm -> t) -> t -> t -> t = fun f -> function
        | Diverge -> fun _ -> Diverge
        | Value tm -> flat_map (f tm)

    module Infix = struct
        let (>>=) = bind

        let (<$>) = map
        
        let (>|=) = fun f -> fun v -> v <$> f

        let (<+>) : t -> t -> t = function
            | Diverge -> fun v -> v
            | (Value _ as v) -> fun _ -> v
    end
    
    let to_string : t -> string = function
        | Diverge -> "Diverge"
        | Value tm -> format tm
    
    let to_option : t -> vterm option = function
        | Diverge -> None
        | Value tm -> Some tm

    let of_option : vterm option -> t = function
        | None -> Diverge
        | Some tm -> Value tm

    let diverges : t -> bool = function
        | Diverge -> true
        | _ -> false

    (* for messing with particular kinds of values *)
    let real_map : (float -> float) -> t -> t = fun f -> function
        | Value (Real r) -> Value (Real (f r))
        | _ -> Diverge
    let real_map2 : (float -> float -> float) -> t -> t -> t = fun f -> function
        | Value (Real x) -> real_map (f x)
        | _ -> fun _ -> Diverge
    
    let real_filter : (float -> bool) -> t -> t = fun f -> function
        | Value (Real r) -> if f r then Value (Bool True) else Value (Bool False)
        | _ -> Diverge

    let to_real : t -> float option = function
        | Value (Real f) -> Some f
        | _ -> None
    let of_real : float -> t = fun f -> Value (Real f)

    let is_true : t -> bool = function
        | Value (Bool True) -> true
        | _ -> false
    let is_false : t -> bool = function
        | Value (Bool False) -> true
        | _ -> false
    let of_bool : bool -> t = function
        | true -> Value (Bool True)
        | false -> Value (Bool False)

    let bag_map : (vterm -> t) -> t -> t = fun f -> function
        | Value (Bag ts) -> ts
            |> CCList.map f
            |> CCList.map to_option
            |> CCOpt.sequence_l
            |> CCOpt.map (fun v -> Bag v)
            |> of_option
        | _ -> Diverge

    let rec list_map : (vterm -> t) -> vterm -> t = fun f -> function
        | ConsList Nil -> Value (ConsList Nil)
        | ConsList (Cons (hd, tl)) ->
            let tl = list_map f tl in
            let hd = f hd in
                map2 (fun h -> fun t -> ConsList (Cons (h, t))) hd tl
        | _ -> Diverge

    let real_gt : t -> t -> bool = fun b -> fun s ->
        CCOpt.map2 (>) (to_real b) (to_real s) |> CCOpt.get_or ~default:false

    let get_exn : t -> vterm = function
        | Value tm -> tm
        | _ -> failwith "cannot cast divergence"
end

open CCOpt.Infix

open Evaluation.Infix

let rec eval : t -> Evaluation.t = function
    (* compile function by wrapping it as a closure *)
    | Abs (_, body) ->
        let closure v = eval (instantiate_one v body) in
            Function ("closure", closure) |> Evaluation.return

    (* function application has several options for success *)
    | App (Function (name, f), arg) -> f arg >>= eval
    | App ((Fix (_, body) as fix), arg) ->
        let e = instantiate_one fix body in
        let tm = App (e, arg) in
            eval tm
    | App (tm, arg) -> 
        eval tm >|= (fun tm -> App (tm, arg)) >>= eval

    (* pattern matching *)
    | MatchNat (_, e, zero, succ) -> begin match eval e with
        | Value (Nat Zero) -> eval zero
        | Value (Nat (Succ tm)) -> eval (instantiate_one tm succ)
        | _ -> Diverge
        end
    | MatchCons (_, e, nil, cons) -> begin match eval e with
        | Value (ConsList Nil) -> eval nil
        | Value (ConsList (Cons (hd, tl))) ->
            cons |> instantiate_two hd tl |> eval
        | _ -> Diverge
        end

    (* skip over these *)
    | TypeApp (tm, _) -> eval tm
    | SensApp (tm, _) -> eval tm

    (* values *)
    | Nat (Succ tm) -> eval tm >|= fun v -> Nat (Succ v)
    | ConsList (Cons (hd, tl)) ->
        let hd = eval hd in
        let tl = eval tl in
            Evaluation.map2 (fun x -> fun y -> ConsList (Cons (x, y))) hd tl
    | Pair (l, r) ->
        let l = eval l in
        let r = eval r in
            Evaluation.map2 (fun x -> fun y -> Pair (x, y)) l r
    | (Bag _ as tm) -> Value tm |> Evaluation.bag_map eval

    (* probability layer *)
    | Do tm -> eval tm
    | Return tm -> eval tm
    | LetDraw (_, dist, usage) ->
        usage |> instantiate_one dist |> eval
    
    (* otherwise, just lift *)
    | (_ as tm) -> Evaluation.return tm

(* how big is the term *)
let rec size : t -> int = function
    | Var (_) -> 1
    | Abs (_, Scope body) -> 1 + (size body)
    | App (l, r) -> 1 + (size l) + (size r)
    
    | MatchNat (_, e, zero, Scope succ) ->
        1 + (size e) + (size zero) + (size succ)
    | MatchCons (_, e, nil, Widen (Scope cons)) ->
        1 + (size e) + (size nil) + (size cons)
    
    | TypeAbs (Scope body) -> size body
    | TypeApp (body, _) -> size body
    | SensAbs (Scope body) -> size body
    | SensApp (body, _) -> size body

    | Wild (_, _, Scope body) -> 1 + (size body)
    
    | Nat Zero -> 1
    | Nat (Succ tm) -> 1 + (size tm)
    | ConsList Nil -> 1
    | ConsList (Cons (hd, tl)) -> 1 + (size hd) + (size tl)
    | Bool _ -> 1
    | Discrete _ -> 1
    | Pair (l, r) -> 1 + (size l) + (size r)
    | Real _ -> 1
    | Bag ts -> ts
        |> CCList.map size
        |> CCList.fold_left (+) 0
    | Function _ -> 1
    
    | Fix (_, Scope body) -> 1 + (size body)
    
    | Do tm -> 1 + (size tm)
    | LetDraw (_, tm, Scope body) ->
        1 + (size tm) + (size body)
    | Return tm -> 1 + (size tm)

(* is this a wild binder *)
let is_wild_binder : t -> bool = function
    | Wild _ -> true
    | _ -> false

let rec wild_closed : t -> bool = function
    | Var _ -> true
    | Abs (_, Scope body) -> wild_closed body
    | App (l, r) -> (wild_closed l) && (wild_closed r)
    | MatchNat (_, e, zero, Scope succ) ->
        (wild_closed e) && (wild_closed zero) && (wild_closed succ)
    | MatchCons (_, e, nil, Widen (Scope cons)) ->
        (wild_closed e) && (wild_closed nil) && (wild_closed cons)
    | TypeAbs (Scope body) | SensAbs (Scope body) -> wild_closed body
    | TypeApp (tm, _) | SensApp (tm, _) -> wild_closed tm
    | Wild _ -> false
    | Nat (Succ tm) -> wild_closed tm
    | ConsList (Cons (hd, tl)) -> (wild_closed hd) && (wild_closed tl)
    | Pair (l, r) -> (wild_closed l) && (wild_closed r)
    | Bag ts -> ts |> CCList.for_all wild_closed
    | Fix (_, Scope body) -> wild_closed body
    | Do tm | Return tm -> wild_closed tm
    | LetDraw (_, dist, Scope usage) -> (wild_closed dist) && (wild_closed usage)
    | _ -> true

let equal l r = l = r

(* ease of construction *)
module Alt = struct
    (* wrapping simple constructions *)
    let var : string -> t = fun s -> Var (Free (Name.of_string s))

    let bool : bool -> t = function
        | true -> Bool True
        | false -> Bool False

    let real : float -> t = fun f -> Real f

    let discrete : string -> t = fun s -> Discrete s

    let pair : t -> t -> t = fun l -> fun r -> Pair (l, r)

    let bag : t list -> t = fun ls -> Bag ls

    let rec nat : int -> t = fun n -> match n with
        | 0 -> Nat (Zero)
        | n -> Nat (Succ (nat (n - 1)))

    let rec conslist : t list -> t = function
        | [] -> ConsList (Nil)
        | x :: xs -> ConsList (Cons (x, conslist xs))

    let fix (v : t) (dt : Dtype.t) (body : t) : t = match v with
        | Var (Free n) -> 
            let tag = {f_var = n; f_dt = dt} in
                Fix (tag, abstract (N.of_one n) body)
        | _ -> v

    let abs (v : t) (dt : Dtype.t) (body : t) : t = match v with
        | Var (Free n) -> 
            let tag = {a_var = n; a_dt = dt} in
                Abs (tag, abstract (N.of_one n) body)
        | _ -> v

    let app (l : t) (r : t) : t = App (l, r)
    let (<!>) = app

    let match_nat (e : t) (zero : t) (sp : Sensitivity.t * t * t) : t = match sp with
        | (sens, Var (Free n), succ) ->
            let tag = {n_var = n; n_sens = sens} in
                MatchNat (tag, e, zero, abstract (N.of_one n) succ)
        | _ -> e

    let match_cons (e : t) (nil : t) (sp : Dtype.t * Sensitivity.t * t * t * t) = match sp with
        | (dt, sens, Var (Free x), Var (Free y), cons) ->
            let tag = {c_hd = x; c_tl = y; c_sens = sens; c_dt = dt} in
                MatchCons (tag, e, nil, abstract_two x y cons)
        | _ -> e

    let sample (v : t) (dt : Dtype.t) (dist : t) (usage : t) : t = match v with
        | Var (Free n) ->
            let tag = {d_var = n; d_dt = dt} in
                LetDraw (tag, dist, abstract (N.of_one n) usage)
        | _ -> v

    let return (e : t) : t = Return e

    let row : (string * vterm) list -> t = fun ps ->
        let convert (k, v) = Pair (Discrete k, v) in
        Bag (CCList.map convert ps)
end
