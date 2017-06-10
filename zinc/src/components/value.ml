(* elements of type value.t are instances of the types introduced in dtype.ml *)

type t =
    | Real of Rational.t
    | Tensor of t * t
    | Function of abstraction
    | MSet of (t list)
and abstraction = (t -> t)

let rec to_string : t -> string = function
    | Real r -> Rational.to_string r
    | Tensor (l, r) ->
        let ls = to_string l in
        let rs = to_string r in
            "(" ^ ls ^ ", " ^ rs ^ ")"
    | Function f -> "<fun>"
    | MSet m ->
        let ms = CCList.map (fun x -> to_string x) m in
        let mss = Utility.fold (fun x y -> x ^ ", " ^ y) ms in
            "{" ^ mss ^ "}"

(* unknown: can pervasives handle comparing our abstraction type? *)
let compare = Pervasives.compare
