(* our simple recursive type - not sure if all inhabitants are dfuzz types *)
type t =
    | Base of base
    | Precise of precise
    | Quantified of quantifier * t
    | Function of modal * t
    | Tensor of t * t
    | Multiset of t
    | List of t
and base =
    | Real
    | Database
    | String
    | Int
    | Bool
and precise = M of Sensitivity.t * t
and quantifier =
    | Exists of Variable.t
    | ForAll of Variable.t
and modal =
    | S of Sensitivity.t * t

(* there are some very important accessors we'll want to do for curried types *)
let strip_modality : modal -> t = function
    | S (_, s) -> s
let strip_type : modal -> Sensitivity.t = function
    | S (sens, _) -> sens

let rec output_type : t -> t = function
    | Function (m, s) -> output_type s
    | (_ as s) -> s

let rec input_types : t -> modal list = function
    | Function (m, s) -> m :: (input_types s)
    | (_ as s) -> []

(* where we start to encode the basic functionality - printing, most importantly *)
let rec to_string : t -> string = function
    | Base b -> base_to_string b
    | Precise p -> precise_to_string p
    | Quantified (q, ty) ->
        let qs = quantifier_to_string q in
        let tys = to_string ty in
            qs ^ "." ^ tys
    | Function (m, ty) ->
        let ms = modal_to_string m in
        let tys = to_string ty in
            ms ^ " -o " ^ tys
    | Tensor (l, r) ->
        let ls = to_string l in
        let rs = to_string r in
            ls ^ " * " ^ rs
    | Multiset ty ->
        let tys = to_string ty in
            "M(" ^ tys ^ ")"
    | List ty ->
        let tys = to_string ty in
            "L(" ^ tys ^ ")"
and base_to_string : base -> string = function
    | Real -> "R"
    | Database -> "DB"
    | Int -> "N"
    | String -> "S"
    | Bool -> "2"
and precise_to_string : precise -> string = function
    | M (s, ty) ->
        let ss = Sensitivity.to_string s in
        let tys = to_string ty in
            "M(" ^ tys ^ ")[" ^ ss ^ "]"
and quantifier_to_string : quantifier -> string = function
    | Exists x ->
        let xs = Variable.to_string x in
            "Exists(" ^ xs ^ ")"
    | ForAll x ->
        let xs = Variable.to_string x in
            "ForAll(" ^ xs ^ ")"
and modal_to_string : modal -> string = function
    | S (s, ty) ->
        let ss = Sensitivity.to_string s in
        let tys = to_string ty in
            "!(" ^ tys ^ ")[" ^ ss ^ "]"

(* we use default comparisons *)
let compare = Pervasives.compare
