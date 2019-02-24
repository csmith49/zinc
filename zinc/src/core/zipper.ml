open Vterm

type branch =
    (* basic lambda calculus *)
    | ZAbs of Name.t * Dtype.t * Name.t
    | ZAppLeft of vterm
    | ZAppRight of vterm
    
    (* pattern-matching *)
    | ZMatchNatE of vterm * Sensitivity.t * (o scope)
    | ZMatchNatZero of vterm * Sensitivity.t * (o scope)
    | ZMatchNatSucc of vterm * vterm * Sensitivity.t * Name.t
    
    | ZMatchConsE of Dtype.t * vterm * Sensitivity.t * ((o s) scope)
    | ZMatchConsNil of Dtype.t * vterm * Sensitivity.t * ((o s) scope)
    | ZMatchConsCons of Dtype.t * vterm * vterm * Sensitivity.t * Name.t * Name.t

    (* type and sens polymorphism *)
    | ZTypeAbs of Name.t
    | ZTypeApp of Dtype.t
    | ZSensAbs of Name.t
    | ZSensApp of Sensitivity.t

    (* wildcard *)
    | ZWild of Context.t * Dtype.t * Name.t
    
    (* values *)
    | ZNatSucc
    | ZConsListHd of vterm
    | ZConsListTl of vterm
    | ZPairLeft of vterm
    | ZPairRight of vterm
    | ZBag of (vterm list) * (vterm list)

    (* recursion *)
    | ZFix of Dtype.t * Name.t

    (* probability layer *)
    | ZDo
    | ZLetDrawDist of Dtype.t * (o scope)
    | ZLetDrawUsage of Dtype.t * t * Name.t
    | ZReturn

(* zipper is a current view and a way of inverting to get the original *)
type t = vterm * branch list

(* getters and setters *)
let get : t -> vterm = function
    | (tm, _) -> tm
let set (tm : vterm) : t -> t = function
    | (_, branches) -> (tm, branches)

(* basic navigation *)
let up : t -> t option = function
    | (tm, branch :: branches) ->
        let tm' = match branch with
            (* base *)
            | ZAbs (tag, dt, x) -> Abs (tag, dt, abstract_one x tm)
            | ZAppLeft tm' -> App (tm, tm')
            | ZAppRight tm' -> App (tm', tm)
            
            (* pm *)
            | ZMatchNatE (zero, i, succ) -> MatchNat (tm, zero, i, succ)
            | ZMatchNatZero (e, i, succ) -> MatchNat (e, tm, i, succ)
            | ZMatchNatSucc (e, zero, i, x) -> MatchNat (e, zero, i, abstract_one x tm)
            
            | ZMatchConsE (dt, nil, i, cons) -> MatchCons (dt, tm, nil, i, cons)
            | ZMatchConsNil (dt, e, i, cons) -> MatchCons (dt, e, tm, i, cons)
            | ZMatchConsCons (dt, e, nil, i, hd, tl) -> MatchCons (dt, e, nil, i, abstract_two hd tl tm)

            (* type and sens polymorphism *)
            | ZTypeAbs x -> TypeAbs (abstract_one x tm)
            | ZTypeApp dt -> TypeApp (tm, dt)
            | ZSensAbs x -> SensAbs (abstract_one x tm)
            | ZSensApp sens -> SensApp (tm, sens)

            (* wild *)
            | ZWild (context, dt, x) -> Wild (context, dt, abstract_one x tm)

            (* values *)
            | ZNatSucc -> Nat (Succ tm)
            | ZConsListHd tl -> ConsList (Cons (tm, tl))
            | ZConsListTl hd -> ConsList (Cons (hd, tm))
            | ZPairLeft r -> Pair (tm, r)
            | ZPairRight l -> Pair (l, tm)
            | ZBag (ls, rs) -> Bag (ls @ [tm] @ rs)
            
            (* recursion *)
            | ZFix (dt, x) -> Fix (dt, abstract_one x tm)

            (* prob *)
            | ZDo -> Do tm
            | ZLetDrawDist (dt, usage) -> LetDraw (dt, tm, usage)
            | ZLetDrawUsage (dt, dist, x) -> LetDraw (dt, dist, abstract_one x tm)
            | ZReturn -> Return tm
        in Some (tm', branches)
    | _ -> None

let right (root : Name.t) (var : string) : t -> t option = function
    | (tm, branch :: branches) -> let open Name.Alt in
        let n = root <+ (var ^ (branches |> CCList.length |> string_of_int)) in
            begin match branch with
                (* base *)
                | ZAppLeft r ->
                    Some (r, ZAppRight tm :: branches)

                (* pattern matching *)
                | ZMatchNatE (zero, i, succ) ->
                    Some (zero, ZMatchNatZero (tm, i, succ) :: branches)
                | ZMatchNatZero (e, i, succ) ->
                    let x = Var (Free n) in
                    let succ = instantiate (N.of_one x) succ in
                        Some (succ, ZMatchNatSucc (e, tm, i, n) :: branches)

                | ZMatchConsE (dt, nil, i, cons) ->
                    Some (nil, ZMatchConsNil (dt, tm, i, cons) :: branches)
                | ZMatchConsNil (dt, e, i, cons) ->
                    let n_hd = n <+ "hd" in
                    let n_tl = n <+ "tl" in
                    let hd = Var (Free n_hd) in
                    let tl = Var (Free n_tl) in
                    let cons = instantiate (N.of_two hd tl) cons in
                        Some (cons, ZMatchConsCons (dt, e, tm, i, n_hd, n_tl) :: branches)
                
                (* values *)
                | ZPairLeft r ->
                    Some (r, ZPairRight tm :: branches)
                | ZBag (ls, r :: rs) ->
                    Some (r, ZBag (ls, rs) :: branches)
                
                (* probability *)
                | ZLetDrawDist (dt, usage) ->
                    let x = Var (Free n) in
                    let usage = instantiate (N.of_one x) usage in
                        Some (usage, ZLetDrawUsage (dt, tm, n) :: branches)
                
                (* otherwise no *)
                | _ -> None
            end
    | _ -> None

(* descending - stick to "the left" *)
let down (root : Name.t) (var : string) : t -> t option = function
    | (tm, branches) -> let open Name.Alt in
        let n = root <+ (var ^ (branches |> CCList.length |> string_of_int)) in
            match tm with
                | Abs (tag, dt, body) ->
                    let x = Var (Free n) in
                    let tm = instantiate (N.of_one x) body in
                        Some (tm, ZAbs (tag, dt, n) :: branches)
                | App (l, r) ->
                    Some (l, ZAppLeft r :: branches)

                (* pattern matching *)
                | MatchNat (e, zero, i, succ) ->
                    Some (e, ZMatchNatE (zero, i, succ) :: branches)
                | MatchCons (dt, e, nil, i, cons) ->
                    Some (e, ZMatchConsE (dt, nil, i, cons) :: branches)

                (* type and sens poly *)
                | TypeAbs body ->
                    let x = Dtype.Free n in
                    let tm = instantiate_dtype (N.of_one x) body in
                        Some (tm, ZTypeAbs n :: branches)
                | TypeApp (tm, dt) ->
                    Some (tm, ZTypeApp dt :: branches)
                | SensAbs body ->
                    let x = Sensitivity.Free n in
                    let tm = instantiate_sens (N.of_one x) body in
                        Some (tm, ZSensAbs n :: branches)
                | SensApp (tm, sens) ->
                    Some (tm, ZSensApp sens :: branches)

                | Wild (context, dt, body) ->
                    let w = Var (Free n) in
                    let tm = instantiate (N.of_one w) body in
                        Some (tm, ZWild (context, dt, n) :: branches)

                (* values *)
                | Nat (Succ tm) ->
                    Some (tm, ZNatSucc :: branches)
                | ConsList (Cons (hd, tl)) ->
                    Some (hd, ZConsListHd tl :: branches)
                | Pair (l, r) ->
                    Some (l, ZPairLeft r :: branches)
                | Bag (tm :: rest) ->
                    Some (tm, ZBag ([], rest) :: branches) 
                
                (* recursion *)
                | Fix (dt, body) ->
                    let x = Var (Free n) in
                    let tm = instantiate (N.of_one x) body in
                        Some (tm, ZFix (dt, n) :: branches)

                (* probability *)
                | Do tm ->
                    Some (tm, ZDo :: branches)
                | LetDraw (dt, dist, usage) ->
                    Some (dist, ZLetDrawDist (dt, usage) :: branches)
                | Return tm ->
                    Some (tm, ZReturn :: branches)
                | _ -> None

(* fancy iteration *)
open CCOpt.Infix

(* building up to preorder traversal *)
let rec next (root : Name.t) (var : string) : t -> t option =
    fun z -> 
        let _ = print_endline "NEXT" in
        (right root var z) <+> ((up z) >>= (next root var))

let preorder (root : Name.t) (var : string) : t -> t option =
    fun z -> 
        let _ = print_endline "PREORDER" in
        (down root var z) <+> (next root var z)

let rec preorder_until (root : Name.t) (var : string) (pred : vterm -> bool) : t -> t option =
    fun z -> 
        let _ = print_endline "PO_UNTIL" in
        (CCOpt.if_ (fun c -> pred (get c)) z) <+>
        ((preorder root var z) >>= (preorder_until root var pred))

(* term to and from conversion *)
let of_term : vterm -> t = fun tm -> (tm, [])
let rec to_term : t -> vterm = fun z -> match up z with
    | Some z -> to_term z
    | _ -> get z

(* scoping *)
let bindings_of_branch : branch -> (Name.t * Dtype.t) list = function
    | ZAbs (tag, dt, n) -> [(tag, dt)]
    | ZMatchNatSucc (_, _, i, n) -> [(n, Dtype.Precise (Dtype.Natural i))]
    | ZMatchConsCons (dt, _, _, i, hd, tl) -> 
        [(hd, dt) ; (tl, Dtype.Precise (Dtype.List (i, dt)))]
    | ZFix (dt, n) -> [(n, dt)]
    | ZLetDrawUsage (dt, _, n) -> [(n, dt)]
    | _ -> []

let scope : t -> (Name.t * Dtype.t) list = function
    | (_, branches) -> CCList.flat_map bindings_of_branch branches