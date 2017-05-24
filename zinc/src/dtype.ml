(* our simple recursive type - not sure if all inhabitants are dfuzz types *)
type t =
    | Base of base
    | Precise of precise
    | Quantified of quantifier * t
    | Function of modal * t
    | Tensor of t * t
    | Multiset of t
and base =
    | Real
    | Database
and precise = M of Sensitivity.t * t
and quantifier =
    | Exists of Variable.t list
    | ForAll of Variable.t list
and modal =
    | S of Sensitivity.t * t

(* where we start to encode the basic functionality - printing, most importantly *)
let rec to_string : t -> string = function
    | Base b -> base_to_string b
    | Precise p -> precise_to_string p
    | Quantified (q, ty) ->
        let qs = quantifier_to_string q in
        let tys = to_string ty in
            qs ^ "." ^ ts
    | Function (m, ty) -> ""
    | Tensor (l, r) -> ""
    | Multiset ty -> ""
and base_to_string : base -> string = function
    | Real -> "R"
    | Database -> "DB"
and precise_to_string : precise -> string = function
    | M (s, ty) -> ""
and quantifier_to_string : quantifier -> string = function
    | Exists xs -> ""
    | Forall xs -> ""
and modal_to_string : modal -> string = function
    | S (s, ty) -> ""



(* which we immediately figure out how to print *)
let rec to_string : t -> string = function
    | Base b -> begin match b with
        | Real -> "R"
        | Database -> "DB"
    end
    | Precise p -> begin match p with
        | N size -> "N[" ^ (Size.to_string size) ^ "]"
        | M (size, ty) -> "M(" ^ (to_string ty) ^ ")[" ^ (Size.to_string size) ^ "]"
    end
    | Quantified (q, vl, ty) ->
        let qs = Quantifier.to_string q in
        let vls = vl
                |> TypeEnvironment.to_list
                |> CCList.map (fun (k, v) -> (Variable.to_string k) ^ ": " ^ (to_string v))
                |> Utility.fold (fun x y -> x ^ ", " ^ y) in
        let tys = to_string ty in
            qs ^ "(" ^ vls ^ ")." ^ tys
    | Function (l, r) ->
        let ls = to_string l in
        let rs = to_string r in
            ls ^ " -o " ^ rs
    | Tensor (l, r) ->
        let ls = to_string l in
        let rs = to_string r in
            ls ^ " * " ^ rs
    | Multiset ty ->
        let tys = to_string ty in
            "M(" ^ tys ^ ")"
    | Modal of (s, ty) ->
        let ss = Sensitivity.to_string s in
        let tys = to_string ty in
            "!_(" ^ ss ^ "):" ^ tys

(* and enforce basic comparison operations *)
let compare (l : t) (r : t) : int = match l, r with
    | Top, Top -> 0
    | Top, _ -> 1
    | _, Top -> -1
    | _, _ -> Pervasives.compare l r

(* and then lift our arithmetic up *)
let rec add (l : t) (r : t) : t option = match l, r with
    | Top, Top -> Some Top
    | Top, _ -> Some Top
