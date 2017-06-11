type t = {
    symbol : string;
    dtype : Dtype.t;
    source : Value.abstraction;
}

let to_string (f : t) : string = f.symbol

let eval (f : t) (x : Value.t) = f.source x

let to_value (f : t) : Value.t = Value.Function f.source

let name (f : t) : string = f.symbol

(* for synthesis, we need to convert a function to an appropriate term *)
let to_term (f : t) (c : Context.t) : (Program.t * Solver.expr) =
    (* we'll create our program using fold-left *)
    let mk_app l r = Term.App (l, r) in
    let acc = Term.Symbol (Program.Function f.symbol) in

    (* splitting input types into the appropriate intermediate structs of types, senses, and contexts *)
    let inputs = Dtype.input_types f.dtype in

    let types = CCList.map Dtype.strip_modality inputs in
    let senses = CCList.map Dtype.strip_type inputs in
    let contexts = CCList.map (fun d -> Context.make_fresh_variable ()) inputs in

    (* now we can make the wildcards *)
    let wilds = CCList.map2 (fun d c -> Term.Symbol (Program.Wild (d, c))) types contexts in

    (* and the output program *)
    let program = CCList.fold_left mk_app acc wilds in

    (* now we start building the constraint *)
    let mk_add l r = Context.Plus (l, r) in
    let scaled = CCList.map2 (fun s c -> Context.Times (s, c)) senses contexts in
    let linear_context = CCList.fold_left mk_add Context.Empty scaled in

    (* and finally, we can return the result *)
    (program, Context.equality_to_z3 linear_context c)
