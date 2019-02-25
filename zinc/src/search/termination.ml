type comparison = LT of Name.t * Name.t
let (<!) : Name.t -> Name.t -> comparison = fun l -> fun r -> LT (l, r)

(* some other useful types *)
type po = comparison list
type scope = (Name.t * Name.t * Dtype.t) list
type context = Vterm.t list

(* helper functions *)
let find : scope -> Vterm.t -> Name.t option = fun scope -> function
    | Vterm.Var (Vterm.Free n) ->
        let f (ref_x, x, _) = if Name.eq n x then Some ref_x else None in
            CCList.filter_map f scope |> CCList.head_opt
    | _ -> None

let rec context_of_vterm : Vterm.t -> context = function
    | Vterm.App (l, r) -> (context_of_vterm l) @ [r]
    | _ as tm -> [tm]

let scope_to_string : scope -> string = fun s -> s
    |> CCList.map (fun (ref_n, n, _) -> (Name.to_string ref_n) ^ " -> " ^ (Name.to_string n))
    |> CCString.concat " & "

(* the order defined *)
let rec is_lt (order : po) : Name.t -> Name.t -> bool = fun l -> fun r ->
    if CCList.mem ~eq:(=) (l <! r) order then true else order
        |> CCList.filter_map (fun (LT (a, b)) -> 
            if Name.eq b r then Some a else None)
        |> CCList.exists (is_lt order l)

module Call = struct
    type t = Name.t list

    let rec context_lt (order : po) : scope -> context -> t -> bool = fun scope -> fun context -> fun call ->
        match context, call with
            | l :: ls, r :: rs -> 
                begin match find scope l with
                | Some n ->
                    if is_lt order n r then true else
                        if Name.eq n r then context_lt order scope ls rs
                        else false
                | _ -> false
            end
            | _ -> false
end

module Filter = struct
    type t = {
        base : Call.t;
        order : po;
    }

    let add : t -> comparison -> t = fun f -> fun c -> {
        f with order = c :: f.order
    }
    
    let check (scope : scope) : t -> Vterm.t -> bool = fun f -> fun tm ->
        let context = context_of_vterm tm in
            Call.context_lt f.order scope context f.base
end