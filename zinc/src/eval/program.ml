type t = (Value.t, Dtype.t) Term.t

(* evaluation only works in restricted cases, but we make sure we only construct progs where it'll work *)
let rec eval (p : t) : Value.t = match p with
  | Term.App (f, args) -> begin match (eval f) with
      | Value.F f' -> f' (eval args)
      | _ -> failwith "can't apply a non-abstraction"
  end
  | Term.Abs (dom, body) -> let f v = eval (Term.instantiate (Term.Const v) body) in Value.F f
  | Term.Const v -> v
  | _ -> failwith "can't evaluate provided program"

(* so submodules can reference the type *)
type program = t

(* stubs are what we'll end up replacing wildcards with *)
module Stub = struct
  (* we maintain a prefix as context and a program with some free variables *)
  type t = Dtype.t Term.Prefix.t * program
  (* our primary operation is inserting a program into a program that begins with a wild binder *)
  let insert (stub : t) (p : program) : program = match p with
    | Term.Wild (dom, body) -> begin match stub with
        | (prefix, subterm) -> Term.Prefix.bind prefix (Term.instantiate subterm body)
    end
    | _ -> failwith "can't insert a stub at a non-wild binder"
end

(*
framework for expansion goes something like this

let expansions (root : Name.t) (dom : Dtype.t) (c : Constraint.t) : (Stub.t * Constraint.t) list

let expand (root : Name.t) (p : t) (c : Constraint.t): t list =
  match (root <+ (Name.of_string "expand")) <@ p with
    | Some ((n, dom, Term.Prefix.PWild), body) ->
      let ls = expansions n dom c in
      map f ls where
      f (s, c') = (Stub.insert s p, c & c')
    | _ -> []
 *)
