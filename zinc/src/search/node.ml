type t = {
    root : Name.t;
    obligation : Constraint.t;
    solution : Vterm.t;
    recursion : Termination.Filter.t option;
}

let add_filter : t -> Termination.Call.t -> t = fun n -> fun call ->
    { n with recursion =
      Some {Termination.Filter.base = call; order = []}
    }

let is_terminating : t -> bool = fun n -> match n.solution with
  | Vterm.Fix (tag, body) ->
    let _ = print_string "  Checking termination..." in
    (* instantiate a free var for the recursive call *)
    let f = Vterm.Var (Vterm.Free (Name.of_string "f")) in
    let tm = Vterm.instantiate_one f body in
    
    (* get the zipper *)
    let zipper = Zipper.of_term tm in
    let rec_binding = (tag.Vterm.f_var, Name.of_string "f", tag.Vterm.f_dt) in

    (* now get all zippers looking at an f *)
    let zips = zipper
      |> Zipper.preorder_list n.root "" (fun tm -> tm = f) in

    let uses = zips
      |> CCList.map (fun z -> (
          rec_binding :: (Zipper.scope z), 
          z |> Zipper.calling_context |> Zipper.get
        )
      ) in
    let _ = print_endline ((string_of_int (CCList.length uses)) ^ " use(s) found.") in

    let check (scope, tm) = 
      let _ = print_endline ("  + " ^ (Vterm.format tm)) in
      let ans = match n.recursion with
        | Some filter -> Termination.Filter.check scope filter tm
        | _ -> false in
      let _ = print_endline ("  | " ^ (string_of_bool ans)) in ans
    in

    let ans = CCList.for_all check uses in
    let _ = print_endline ("  + done - " ^ (string_of_bool ans)) in ans
  | _ -> true

(* for wrapping in the priority structure *)
type node = t

(* priority lets us cache computation of priority while manipulating the frontier *)
module Priority = struct
    type t = P of int * node

    let compare (l : t) (r : t) : int = match l, r with
        | P (i, n), P (j, m) -> match Pervasives.compare i j with
            | 0 -> Pervasives.compare n.root m.root
            | (_ as ans) -> ans
    
    let to_node : t -> node = function
        | P (_, n) -> n
    
    let of_node (w : node -> int) : node -> t = fun n ->
        P (w n, n)
end

module Vector = struct
    (* singleton types for sizes of vectors *)
    type z = Z
    type 'a s = S : 'a -> 'a s
    type (_, _) dlist =
        | Nil : (_, z) dlist
        | Cons : 'a * ('a, 'n) dlist -> ('a, 'n s) dlist

    type 'n t = (int, 'n) dlist
    type 'n coefficients = (int, 'n) dlist

    let rec to_int : type n . n coefficients -> n t -> int = fun c -> fun v ->
        match c, v with
            | Nil, Nil -> 0
            | Cons (c, cs), Cons (v, vs) ->
                (c * v) + (to_int cs vs)

    (* easier to manipulate the current views of node *)
    type four = ((((z s) s) s) s)
    let of_four : 'a -> 'a -> 'a -> 'a -> ('a, four) dlist =
        fun a1 -> fun a2 -> fun a3 -> fun a4 ->
            Cons (a1, Cons (a2, Cons (a3, Cons (a4, Nil))))

    (* if this changes, type-checking will force us to change the number of weights *)
    let of_node : node -> (four t) = fun n ->
        let ob = n.obligation
            |> Constraint.flatten
            |> Simplify.assignment_simplify
        in let sensitivities = Simplify.extract_sensitivities ob in
            of_four
                (Vterm.size n.solution)
                (sensitivities 
                    |> CCList.filter Simplify.is_constant
                    |> CCList.length)
                (sensitivities
                    |> CCList.filter Simplify.is_unconstrained
                    |> CCList.length)
                (sensitivities
                    |> CCList.filter Simplify.is_infinity
                    |> CCList.length)
end

(* module Vector = struct
  type t  = {
    size : int;
    constant : int;
    unconstrained : int;
    infinity : int;
  }
  
  type weight = int * int * int * int
  
  let to_int : weight -> t -> int = fun w -> fun v ->
    let a, b, c, d = w in
    a * v.size + b * v.constant + c * v.unconstrained + d * v.infinity
end

type t = {
  root : Name.t;
  obligation : Constraint.t;
  solution : Fterm.t;
}

(* mostly just for testing *)
let to_string (n : t) : string = Fterm.to_string n.solution

module Priority = struct
  type t = {
    root : Name.t;
    obligation : Constraint.t;
    solution : Fterm.t;
    priority : int;
  }

  let compare (l : t) (r : t) : int =
    let ans = compare l.priority r.priority in
    match ans with
      | 0 -> compare l.solution r.solution
      | _ -> ans
  
  let leq (l : t) (r : t) : bool = (compare l r) < 1
end

let to_vector : t -> Vector.t = fun n ->
let c = n.obligation |> Constraint.flatten |> Simplify.assignment_simplify in
(* let c = n.obligation |> Constraint.flatten in *)
let sensitivities = Simplify.extract_sensitivities (c) in  
  {
    Vector.size = Fterm.size n.solution;
    constant = CCList.length (CCList.filter Simplify.is_constant sensitivities);
    unconstrained = CCList.length (CCList.filter Simplify.is_unconstrained sensitivities);
    infinity = CCList.length (CCList.filter Simplify.is_infinity sensitivities);
  }

let of_priority : Priority.t -> t = fun p ->
  {
    root = p.Priority.root;
    obligation = p.Priority.obligation;
    solution  = p.Priority.solution;
  }

let to_priority : Vector.weight -> t -> Priority.t = fun w -> fun n ->
  {
    Priority.root = n.root;
    obligation = n.obligation;
    solution = n.solution;
    priority = n |> to_vector |> (Vector.to_int w);
  } *)