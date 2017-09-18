module PrimitiveNode = struct
    type t = {
        repr : string;
        value : Value.t -> Value.t;
        dtype : Dtype.t;
    }
end

module ConstantNode = struct
    type t = {
        repr : string;
        value : Value.t;
        dtype : Dtype.t;
    }
end

module VariableNode = struct
    type t = {
        repr : string;
        value : int;
        dtype : Dtype.t;
    }
end

module WildcardNode = struct
    type t = {
        repr : string;
        value : int;
        dtype : Dtype.t;
    }
end

module LambdaNode = struct
    type t = {
        repr : string;
        value : unit;
        dtype : Dtype.t;
    }
end

type node =
    | Primitive of PrimitiveNode.t
    | Constant of ConstantNode.t
    | Variable of VariableNode.t
    | Wildcard of WildcardNode.t
    | Abstraction of LambdaNode.t
    | Application

type t = node Term.t

