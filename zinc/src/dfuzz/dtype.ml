(* base type *)
type t =
  | Free of Name.t
  | Bound of int
  | Precise of precise
  | Quant of quantifier * kind * scope
  | Func of modal * t
  | Tensor of t * t
  | Base of base
  | Payload of payload
and precise =
  | N of Size.t
  | M of Size.t * t
  | R of Sensitivity.t
and quantifier =
  | Exists
  | ForAll
and kind =
  | KSens
  | KSize
  | KType
and scope = Sc of t
and modal = Modal of Sensitivity.t * t
and base =
  | Real
  | Integer
  | Bool
  | String
  | Database
and payload =
  | PSens of Sensitivity.t
  | PSize of Size.t

(* mcbride and mckinna *)
let rec name_to (n : Name.t) (db : int) (dt : t) : t = match dt with
  | Free n' -> if n == n' then (Bound db) else dt
  | Precise p -> begin match p with
      | N s -> Precise (N (Size.name_to n db s))
      | M (s, dt') -> Precise (M (Size.name_to n db s, name_to n db dt'))
      | R s -> Precise (R (Sensitivity.name_to n db s))
  end
  | Quant (q, k, Sc body) -> Quant (q, k, Sc (name_to n (db + 1) body))
  | Func (m, codom) -> begin match m with
      | Modal (s, dom) -> Func (Modal (Sensitivity.name_to n db s, name_to n db dom), name_to n db codom)
  end
  | Tensor (l, r) -> Tensor (name_to n db l, name_to n db r)
  | Payload _ -> failwith "can't modify payload in transit"
  | _ -> dt (* catches Base and Bound case *)

let rec replace (img : t) (db : int) (dt : t) : t = match dt with
  | Bound i -> if i == db then img else dt
  | Precise p -> begin match p with
      | N s ->
        let s' = begin match img with
          | Payload (PSize size) -> Size.replace size db s
          | _ -> s
        end in Precise (N s')
      | M (s, dt') ->
        let s' = begin match img with
          | Payload (PSize size) -> Size.replace size db s
          | _ -> s
        end in Precise (M (s', replace img db dt'))
      | R s ->
        let s' = begin match img with
          | Payload (PSens sens) -> Sensitivity.replace sens db s
          | _ -> s
        end in Precise (R s')
    end
  | Quant (q, k, Sc body) -> Quant (q, k, Sc (replace img (db + 1) body))
  | Func (m, codom) -> begin match m with
      | Modal (s, dom) ->
        let s' = begin match img with
          | Payload (PSens sens) -> Sensitivity.replace sens db s
          | _ -> s
        end in Func (Modal (s', replace img db dom), replace img db codom)
    end
  | Tensor (l, r) -> Tensor (replace img db l, replace img db r)
  | Payload _ -> failwith "can't replace payload in transit"
  | _ -> dt (* catches base and bound case *)

(* the payoff *)
let abstract (n : Name.t) (dt : t) : scope = Sc (name_to n 0 dt)
let instantiate (img: t) (s : scope) : t = match s with
  | Sc body -> replace img 0 body
