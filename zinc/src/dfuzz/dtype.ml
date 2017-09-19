type t =
  | Base of base
  | Precise of precise
  | Quant of quantifier * scope
  | Func of modal * t
  | Tensor of t * t
and base =
  | Real
  | Integer
  | Bool
  | String
  | Database
and precise =
  | N of Size.t
  | M of Size.t * t
  | R of Sensitivity.t
and quantifier =
  | Exists
  | ForAll
and scope = Scope of t
and modal = Modal of Sensitivity.t * t
