type t = {
    symbol : string;
    dtype : Dtype.t;
    source : Value.abstraction;
}

let to_string (f : t) : string = f.symbol

let eval (f : t) (x : Value.t) = f.source x

let to_value (f : t) : Value.t = Value.Function f.source
