module ExtendedModal = struct
    (* 1: define a lattice over regular types to add infinity *)
    type extended_type =
        | Concrete of Dtype.t
        | Top
    (* with the only operation we care about - lower bounds *)
    let glb (l : extended_type) (r: extended_type) : extended_type = match l, r with
        | Top, _ -> r
        | _, Top -> l
        | Concrete lt, Concrete rt ->
            if lt = rt then l else failwith "types not equal"

    (* 2: lift modal type constructor to extended types *)
    type t = S of Sensitivity.t * extended_type
    (* with the appropriate arithmetic operations lifted from sensitivities *)
    let add (l : t) (r : t) : t = match l, r with
        | S (ls, lt), S (rs, rt) ->
            let s = Sensitivity.add ls rs in
            let lb = glb lt rt in
                S (s, lb)
    let mult (l : Sensitivity.t) (r : t) : t = match r with
        | S (rs, rt) ->
            let s = Sensitivity.mult l rs in
                S (s, rt)
    (* and a handy-dandy accessor function *)
    let sensitivity : t -> Sensitivity.t = function
        | S (s, _) -> s
    (* and printing, of course *)
    let to_string : t -> string = function
        | S (s, m) ->
            let ss = Sensitivity.to_string s in
            let ms = match m with
                | Top -> "T"
                | Concrete sigma -> Dtype.to_string sigma
            in
                "!(" ^ ms ^ ")[" ^ ss ^ "]"
    (* with a simple constructor *)
    let empty = S (Sensitivity.Value Rational.zero_q, Top)
    (* and a slightly less simple constructor *)
    let symbolic (x : Variable.t) : t =
        S (Sensitivity.Variable x, Top)
    (* and finally a type covnerter *)
    let of_dtype (d : Dtype.t) : t =
        S (Sensitivity.Value (Rational.Q (1, 1)), Concrete d)
end

(* simple grammar to express contetxs *)
type t =
    | Context of Variable.t * ExtendedModal.t
    | Variable of Variable.t
    | Plus of t * t
    | Times of Sensitivity.t * t

(* the ever-present printing method *)
let rec to_string : t -> string = function
    | Context (x, m) -> "{" ^ (Variable.to_string x) ^ ": " ^ (ExtendedModal.to_string m) ^ "}"
    | Variable v -> Variable.to_string v
    | Plus (l, r) ->
        let ls = to_string l in
        let rs = to_string r in
            ls ^ " + " ^ rs
    | Times (s, c) ->
        let ss = Sensitivity.to_string s in
        let cs = to_string c in
            ss ^ " * " ^ cs

(* importantly, we want to know the finite support of a context *)
let rec support : t -> Variable.t list = function
    | Context (x, s) -> [x]
    | Variable v -> []
    | Plus (l, r) -> (support l) @ (support r)
    | Times (s, c) -> support c
(* so that we only really access the types of variables we need to check *)
let rec get_type (x : Variable.t) (c : t) : ExtendedModal.t = match c with
    | Context (y, s) ->
        if x = y then s else ExtendedModal.empty
    | Variable v ->
        let s = Variable.concat x "@" v in
            ExtendedModal.symbolic s
    | Plus (l, r) ->
        let lt = get_type x l in
        let rt = get_type x r in
            ExtendedModal.add lt rt
    | Times (s, c) ->
        let ct = get_type x c in
            ExtendedModal.mult s ct
(* so that we can easily grab the appropriate sensitivity *)
let get_sensitivity (x : Variable.t) (c : t) : Sensitivity.t =
    ExtendedModal.sensitivity (get_type x c)

(* and the reason we use contexts at all... *)
let equality_to_z3 (l : t) (r : t) : Solver.expr =
    let vars = CCList.sort_uniq ((support l) @ (support r)) in
    let eqs = CCList.map (fun x ->
                Sensitivity.equality_to_z3 (get_sensitivity x l) (get_sensitivity x r))
            vars in
        Solver.make_and_list eqs
