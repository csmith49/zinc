(* we're going to use a lot of these, let's make sure we're creating them correctly *)
module Variable = struct
    type t = string
    let to_string (v : t) : string = v
    let compare = CCString.compare
end
module TypeEnvironment = CCMap.Make(Variable)

(* quantifier just tells us how we quantifiy vars --- better than a boolean flag *)
module Quantifier = struct
    type t =
        | Exists
        | ForAll
    let to_string : t -> string = function
        | Exists -> "Exists"
        | ForAll -> "ForAll"
    let is_existential : t -> bool = function
        | Exists -> true
        | _ -> false
    let is_universal : t -> bool = function
        | ForAll -> true
        | _ -> false
end

(* we maintain a special module for size and sensitivity annotations *)
module Size = struct
    type t =
        | Var of Variable.t
        | Val of int
        | Plus of t * int
    let rec to_string : t -> string = function
        | Var v -> Variable.to_string v
        | Val i -> string_of_int i
        | Plus (s, i) -> (to_string s) ^ " + " ^ (string_of_int i)
end

module Sensitivity = struct
    type t =
        | Var of Variable.t
        | Val of float
        | Size of Size.t
        | Plus of t * t
        | Times of t * t
        | Infinity
    let rec to_string : t -> string = function
        | Var v -> Variable.to_string v
        | Val f -> string_of_float f
        | Size s -> Size.to_string s
        | Plus (l, r) -> (to_string l) ^ " + " ^ (to_string r)
        | Times (l, r) -> (to_string l) ^ " * " ^ (to_string r)
        | Infinity -> "INF"
end

(* our recursive data type *)
type t =
    | Base of base
    | Precise of precise
    | Quantified of Quantifier.t * (t TypeEnvironment.t) * t
    | Function of modal * t
    | Tensor of t * t
    (* note Multiset(t) has same meaning as Exists(i: s).M(t)[s] *)
    | Multiset of t
and base =
    | Real
    | Database
and precise =
    | N of Size.t
    | M of Size.t * t
and modal =
    | Scale of Sensitivity.t * t

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
    | Function (Scale (s, l), r) ->
        let ls = to_string l in
        let rs = to_string r in
        let ss = Sensitivity.to_string s in
            "!_(" ^ ss ^ ") " ^ ls ^ " -o " ^ rs
    | Tensor (l, r) ->
        let ls = to_string l in
        let rs = to_string r in
            ls ^ " * " ^ rs
    | Multiset ty ->
        let tys = to_string ty in
            "M(" ^ tys ^ ")"

(* of course, we have to deal with typing contexts, also as variables *)
module Context = struct
    type t =
        | Empty
        | Add of t * t
        | Scale of float * t
        | Var of Variable.t
end
