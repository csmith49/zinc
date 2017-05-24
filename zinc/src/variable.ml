(* we're going to be using a lot of variables, might as well make them easy to use *)
type t = string

(* t is a string, but just in case we change it here we go *)
let to_string (x : t) : string = x

module Internal = struct
    (* we don't intend this module to be accessible elsewhere *)
    module PrefixMap = CCMap.Make(String)
    let counts = ref PrefixMap.empty
    (* we don't really care about having multiple counts, so we'll encapsulate it here *)
    let get_count (prefix : string) : int = PrefixMap.get_or prefix !counts ~default:0
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

(* and some other misc helper functions *)
let compare = Pervasives.compare
