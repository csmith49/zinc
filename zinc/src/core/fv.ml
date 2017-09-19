type name = string

type expr = 
    | F of name
    | B of int
    | App of expr * expr
    | Quant of expr * scope
and scope = S of expr

let rec name_to (n : name) (outer : int) (e : expr) : expr = match e with
    | F n' -> if n == n' then (B outer) else e
    | B _ -> e
    | App (l, r) -> App (name_to n outer l, name_to n outer r)
    | Quant (l, r) -> match r with
        | S e -> Quant (name_to n outer l, S (name_to n (outer + 1) e))

let abstract (n : name) (e : expr) : scope =
    S (name_to n 0 e)

let rec replace (image : expr) (outer : int) (body : expr) : expr = match body with
    | B index -> if index == outer then image else body
    | F _ -> body
    | App (l, r) -> App (replace image outer l, replace image outer r)
    | Quant (l, r) -> match r with
        | S e -> Quant (replace image outer l, S (replace image (outer + 1) body))

let instantiate (image : expr) (s : scope) : expr = match s with
    | S body -> replace image 0 body
