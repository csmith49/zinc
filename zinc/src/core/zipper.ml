open Vterm

type branch =
    (* basic lambda calculus *)
    | ZAbs of abs_tag * Name.t
    | ZAppLeft of vterm
    | ZAppRight of vterm
    
    (* pattern-matching *)
    | ZMatchNatE of nat_tag * vterm * (o scope)
    | ZMatchNatZero of nat_tag * vterm * (o scope)
    | ZMatchNatSucc of nat_tag * vterm * vterm * Name.t
    
    | ZMatchConsE of cons_tag * vterm * ((o s) scope)
    | ZMatchConsNil of cons_tag * vterm * ((o s) scope)
    | ZMatchConsCons of cons_tag * vterm * vterm * Name.t * Name.t

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
    | ZFix of fix_tag * Name.t

    (* probability layer *)
    | ZDo
    | ZLetDrawDist of draw_tag * (o scope)
    | ZLetDrawUsage of draw_tag * t * Name.t
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
            | ZAbs (tag, x) -> Abs (tag, abstract_one x tm)
            | ZAppLeft tm' -> App (tm, tm')
            | ZAppRight tm' -> App (tm', tm)
            
            (* pm *)
            | ZMatchNatE (tag, zero, succ) -> MatchNat (tag, tm, zero, succ)
            | ZMatchNatZero (tag, e, succ) -> MatchNat (tag, e, tm, succ)
            | ZMatchNatSucc (tag, e, zero, x) -> MatchNat (tag, e, zero, abstract_one x tm)
            
            | ZMatchConsE (tag, nil, cons) -> MatchCons (tag, tm, nil, cons)
            | ZMatchConsNil (tag, e, cons) -> MatchCons (tag, e, tm, cons)
            | ZMatchConsCons (tag, e, nil, hd, tl) -> MatchCons (tag, e, nil, abstract_two hd tl tm)

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
            | ZFix (tag, x) -> Fix (tag, abstract_one x tm)

            (* prob *)
            | ZDo -> Do tm
            | ZLetDrawDist (tag, usage) -> LetDraw (tag, tm, usage)
            | ZLetDrawUsage (tag, dist, x) -> LetDraw (tag, dist, abstract_one x tm)
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
                | ZMatchNatE (tag, zero, succ) ->
                    Some (zero, ZMatchNatZero (tag, tm, succ) :: branches)
                | ZMatchNatZero (tag, e, succ) ->
                    let x = Var (Free n) in
                    let succ = instantiate (N.of_one x) succ in
                        Some (succ, ZMatchNatSucc (tag, e, tm, n) :: branches)

                | ZMatchConsE (tag, nil, cons) ->
                    Some (nil, ZMatchConsNil (tag, tm, cons) :: branches)
                | ZMatchConsNil (tag, e, cons) ->
                    let n_hd = n <+ "hd" in
                    let n_tl = n <+ "tl" in
                    let hd = Var (Free n_hd) in
                    let tl = Var (Free n_tl) in
                    let cons = instantiate (N.of_two hd tl) cons in
                        Some (cons, ZMatchConsCons (tag, e, tm, n_hd, n_tl) :: branches)
                
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
                | Abs (tag, body) ->
                    let x = Var (Free n) in
                    let tm = instantiate (N.of_one x) body in
                        Some (tm, ZAbs (tag, n) :: branches)
                | App (l, r) ->
                    Some (l, ZAppLeft r :: branches)

                (* pattern matching *)
                | MatchNat (tag, e, zero, succ) ->
                    Some (e, ZMatchNatE (tag, zero, succ) :: branches)
                | MatchCons (tag, e, nil, cons) ->
                    Some (e, ZMatchConsE (tag, nil, cons) :: branches)

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
                | Fix (tag, body) ->
                    let x = Var (Free n) in
                    let tm = instantiate (N.of_one x) body in
                        Some (tm, ZFix (tag, n) :: branches)

                (* probability *)
                | Do tm ->
                    Some (tm, ZDo :: branches)
                | LetDraw (tag, dist, usage) ->
                    Some (dist, ZLetDrawDist (tag, usage) :: branches)
                | Return tm ->
                    Some (tm, ZReturn :: branches)
                | _ -> None

(* fancy iteration *)
open CCOpt.Infix

(* building up to preorder traversal *)
let rec next (root : Name.t) (var : string) : t -> t option =
    fun z ->
        (right root var z) <+> ((up z) >>= (next root var))

let preorder (root : Name.t) (var : string) : t -> t option =
    fun z -> 
        (down root var z) <+> (next root var z)

let rec preorder_until (root : Name.t) (var : string) (pred : vterm -> bool) : t -> t option =
    fun z -> 
        (CCOpt.if_ (fun c -> pred (get c)) z) <+>
        ((preorder root var z) >>= (preorder_until root var pred))

let rec preorder_list (root : Name.t) (var : string) (pred : vterm -> bool) : t -> t list = fun z ->
    match preorder_until root var pred z with
        | Some z ->
            let rest = z
                |> preorder root var
                |> CCOpt.map (preorder_list root var pred)
                |> CCOpt.get_or ~default:[] in
            z :: rest
        | _ -> []
    
(* term to and from conversion *)
let of_term : vterm -> t = fun tm -> (tm, [])
let rec to_term : t -> vterm = fun z -> match up z with
    | Some z -> to_term z
    | _ -> get z

(* scoping *)
let bindings_of_branch : branch -> (Name.t * Name.t * Dtype.t) list = function
    | ZAbs (tag, n) -> [(tag.a_var, n, tag.a_dt)]
    | ZMatchNatSucc (tag, _, _, n) -> [(tag.n_var, n, Dtype.Precise (Dtype.Natural tag.n_sens))]
    | ZMatchConsCons (tag, _, _, hd, tl) -> 
        [(tag.c_hd, hd, tag.c_dt) ; (tag.c_tl, tl, Dtype.Precise (Dtype.List (tag.c_sens, tag.c_dt)))]
    (* | ZFix (tag, n) -> [(tag.f_var, n, tag.f_dt)] *)
    | ZLetDrawUsage (tag, _, n) -> [(tag.d_var, n, tag.d_dt)]
    | _ -> []

let scope : t -> (Name.t * Name.t * Dtype.t) list = function
    | (_, branches) -> CCList.flat_map bindings_of_branch branches

let rec_binding : branch -> (Name.t * Name.t * Dtype.t) option = function
    | ZFix (tag, n) -> Some (tag.f_var, n, tag.f_dt)
    | _ -> None
let rec_bindings : t -> (Name.t * Name.t * Dtype.t) list = function
    | (_, branches) -> CCList.filter_map rec_binding branches

(* assumes structure is: f x y ... z hole *)
let base_call : t -> Name.t list = function
    | (_, branches) -> let pred = function
        | ZAbs (tag, _) -> Some tag.a_var
        | ZFix (tag, _) -> Some tag.f_var
        | _ -> None in
    branches |> CCList.rev |> CCList.filter_map pred

(* predicates for manipulating when to apply certain proposals *)
let in_dist : t -> bool = function
    | (_, branches) ->
        let pred b = match b with
            | ZLetDrawDist _ -> true
            | _ -> false
        in CCList.exists pred branches
let in_usage : t -> bool = function
    | (_, branches) ->
        let pred b = match b with
            | ZLetDrawUsage _ -> true
            | _ -> false
        in CCList.exists pred branches
let sample_depth : t -> int = function
    | (_, branches) ->
        let pred b = match b with
            | ZLetDrawDist _ -> true
            | ZLetDrawUsage _ -> true
            | _ -> false
        in CCList.count pred branches
let in_nat_match : t -> bool = function
    | (_, branches) ->
        let pred b = match b with
            | ZMatchNatE _ -> true
            | ZMatchNatZero _ -> true
            | ZMatchNatSucc _ -> true
            | _ -> false
        in CCList.exists pred branches
let in_cons_match : t -> bool = function
    | (_, branches) ->
        let pred b = match b with
            | ZMatchConsE _ -> true
            | ZMatchConsNil _ -> true
            | ZMatchConsCons _ -> true
            | _ -> false
        in CCList.exists pred branches
let in_match : t -> bool = fun z -> (in_nat_match z) || (in_cons_match z)
            
let to_string : t -> string = fun z ->
    z |> set (Var (Free (Name.of_string "HOLE"))) |> to_term |> Vterm.to_string

let rec calling_context : t -> t = function
    | (tm, branch :: branches) as z -> begin match branch with
        | ZAppLeft _ | ZAppRight _ -> up z |> CCOpt.get_exn |> calling_context
        | _ -> z
        end
    | _ as z -> z