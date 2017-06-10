(* we're going to be using a lot of variables, might as well make them easy to use *)
type t = string

(* t is a string, but just in case we change it here we go *)
let to_string (x : t) : string = x

module Internal = struct
    (* we don't intend this module to be accessible elsewhere *)
    module PrefixMap = CCMap.Make(String)
    let counts = ref PrefixMap.empty
    (* we don't really care about having multiple counts, so we'll encapsulate it here *)
    let get_count (prefix : string) : int = PrefixMap.get_or ~default:0 prefix !counts
    let set_count (prefix : string) (count : int) : unit = begin
            counts := PrefixMap.add prefix count !counts
        end
end

let make (s : string) : t = s
(* and the most important part, making fresh variables *)
let make_fresh prefix =
    let i = Internal.get_count prefix in
    let _ = Internal.set_count prefix (i + 1) in
        prefix ^ "_" ^ (string_of_int i)
(* and we occasionally make some fancy ones *)
let make_fresh_debruijn bd =
    "debruijn_" ^ (string_of_int bd)

(* to make debruijn, we need to know binding depth *)
let binding_depth (e : 'a Utility.StringMap.t) : int =
    e   |> Utility.StringMap.filter
            (fun k v -> CCString.prefix ~pre:"debruijn_" k)
        |> Utility.StringMap.cardinal

(* and some other misc helper functions *)
let compare = Pervasives.compare

(* some combiner operations *)
let concat (l : t) (s : string) (r : t) : t =
    let ls = to_string l in
    let rs = to_string r in
        make (ls ^ s ^ rs)
