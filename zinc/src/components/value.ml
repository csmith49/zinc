(* elements of type value.t are instances of the types introduced in dtype.ml *)

type t =
    | Int of int
    | Bool of bool
    | String of string
    | Real of Rational.t
    | Tensor of t * t
    | Function of abstraction
    | MSet of (t list)
    | List of (t list)
and abstraction = (t -> t)

let rec to_string : t -> string = function
    | Int i -> string_of_int i
    | Bool b -> string_of_bool b
    | Real r -> Rational.to_string r
    | String s -> s
    | Tensor (l, r) ->
        let ls = to_string l in
        let rs = to_string r in
            "(" ^ ls ^ ", " ^ rs ^ ")"
    | Function f -> "<fun>"
    | MSet m ->
        let ms = CCList.map (fun x -> to_string x) m in
        let mss = Utility.fold (fun x y -> x ^ ", " ^ y) ms in
            "{" ^ mss ^ "}"
    | List l ->
        let ls = CCList.map (fun x -> to_string x) l in
        let lss = Utility.fold (fun x y -> x ^ ", " ^ y) ls in
            "[" ^ lss ^ "]"

(* unknown: can pervasives handle comparing our abstraction type? *)
let compare = Pervasives.compare

(* some tricky stuff - we want to be able to load these up from files *)
module Parser = struct
    open CCParse.Infix
    open CCParse

    (* we have to wrap all the constructors *)
    let mk_real x y = Real (Rational.Q (x, y))
    let mk_bool b = Bool b
    let mk_string s = String s
    let mk_int i = Int i
    let mk_mset m = MSet m
    let mk_tensor l r = Tensor (l, r)
    let mk_list ls = List ls

    (* and now we start defining parsers *)

    (* numbers *)
    let p_int = pure mk_int <*> U.int
    let p_rational = pure mk_real <*> (U.int <* char '/') <*> U.int

    (* booleans *)
    let p_true =  string "true" *> return true
    let p_false = string "false" *> return false
    let p_bool = pure mk_bool <*> (p_true <|> p_false)

    (* string *)
    let p_quote = char '\"'
    let p_quotes p = try_ (p_quote *> p <* p_quote)
    let p_string = pure mk_string <*> p_quotes (chars_if (fun c -> c != '\"'))

    (* tensor *)
    let p_tensor p = U.pair p p

    (* mset *)

end
