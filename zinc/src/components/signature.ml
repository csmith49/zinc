(* signatures just maintain a list of functions *)
(* we put them into their own module because we want some extra stuff *)

type t = Function.t Utility.StringMap.t

(* when evaluating, we have to pass in values as well *)
(* ideally, this would be a VariableMap, but it's just easier this way *)
type environment = Value.t Utility.StringMap.t

(* evaluate a program with respect to a signature and environment *)
let rec eval (p : Program.t) (s : t) (e : environment) : Value.t = match p with
    (* introduce a new bound variable, generate an evaluating closure, and wrapping as a value *)
    | Term.Abs sub_p ->
        let bd = Variable.binding_depth e in
        let x = Variable.make_fresh_debruijn bd in
        let f = (fun v -> let e' = Utility.StringMap.add (Variable.to_string x) v e in eval sub_p s e')
        in Value.Function f
    (* just evaluate the left and then apply it to the right *)
    | Term.App (l, r) -> begin match eval l s e with
        | Value.Function f -> eval r s e |> f
        | _ -> failwith "Can't apply non-function values"
    end
    (* look up the symbol as is appropriate *)
    | Term.Symbol n -> match n with
        | Program.Wild (_, _) ->
            failwith "Can't evaluate non-ground programs"
        | Program.Function l ->
            (Utility.StringMap.find l s) |> Function.to_value
        | Program.Variable l -> Utility.StringMap.find (Variable.to_string l) e
