(* we effectively have heirarchical names, but they're folded up using hash functions *)
type t = N of string * int

(* some simple constructors and printers *)
let of_string (s : string) : t = N (s, 0)
let to_string : t -> string = function
  N (s, i) -> s ^ (if i = 0 then "" else ":" ^ (string_of_int i))

(* comparison is straightforward - by index, then name *)
(* no clue if the order actually matters *)
let compare (l : t) (r : t) : int = match l, r with
  | N (s, i), N (t, j) ->
    let ans = CCInt.compare i j in match ans with
      | 0 -> String.compare s t
      | _ -> ans

let eq (l : t) (r : t) : bool = (compare l r) == 0
(* hashing *)
(* let hash : t -> int = function
  | N (s, i) -> (CCHash.poly s) lxor (CCInt.hash i) *)
let hash : t -> int = CCHash.poly

(* the thing that makes heirarchical names work *)
let extend (n : t) (s : string) : t = N (s, hash n)
let extend_by_name (l : t) (r : t) : t = match l, r with
  | _, N (t, j) -> N (t, (hash l) lxor (CCInt.hash j))

(* the alternate structure makes them more bearable to work with *)
module Alt = struct
  let ( <+ ) : t -> string -> t = extend
  let ( ++ ) : t -> t -> t = extend_by_name
end

(* for the stream submodule *)
type name = t

(* naming cycles *)
module Stream = struct
  (* cycles maintain a list of variable symbols, and warp around whenever we use too many *)
  module Cycle = struct
    type t = {
      symbols : string list;
      index : int;
      loop_counter : int;
    }
    (* pulling a new symbol just means we have to update the pointers *)
    let draw : t -> name * t = fun c ->
      let n = N (CCList.nth c.symbols c.index, c.loop_counter) in
      let index = CCInt.rem (c.index + 1) (CCList.length c.symbols) in
      let loop = if index = 0 then c.loop_counter + 1 else c.loop_counter in
        (n, {c with index = index; loop_counter = loop;})
    (* base construction just takes a symbol list *)
    let of_list : string list -> t = fun ss -> {symbols = ss; index = 0; loop_counter = 0}
  end

  (* a stream maintains a separate cycle for each of the kinds of variables we have floating around *)
  type t = {
    root : name;
    abs_symbols : Cycle.t;
    nat_symbols : Cycle.t;
    hd_symbols : Cycle.t;
    tl_symbols : Cycle.t;
    fix_symbols : Cycle.t;
    wild_symbols : Cycle.t;
    sens_symbols : Cycle.t;
    dt_symbols : Cycle.t;
  }

  open Alt
  (* drawing a different kind of symbol needs a different kind of function *)
  let draw_abs : t -> name * t = fun s ->
    let (n, c') = Cycle.draw s.abs_symbols in
    (s.root ++ n, {s with abs_symbols = c'})
  let draw_wild : t -> name * t = fun s ->
    let (n, c') = Cycle.draw s.wild_symbols in
    (s.root ++ n, {s with wild_symbols = c'})
  let draw_sens : t -> name * t = fun s ->
    let (n, c') = Cycle.draw s.sens_symbols in
    (s.root ++ n, {s with sens_symbols = c'})
  let draw_dt : t -> name * t = fun s ->
    let (n, c') = Cycle.draw s.dt_symbols in
    (s.root ++ n, {s with dt_symbols = c'})
  let draw_fix : t -> name * t = fun s ->
    let (n, c') = Cycle.draw s.fix_symbols in
    (s.root ++ n, {s with fix_symbols = c'})
  let draw_hd : t -> name * t = fun s ->
    let (n, c') = Cycle.draw s.hd_symbols in
    (s.root ++ n, {s with hd_symbols = c'})
  let draw_tl : t -> name * t = fun s ->
    let (n, c') = Cycle.draw s.tl_symbols in
    (s.root ++ n, {s with tl_symbols = c'})
  let draw_nat : t -> name * t = fun s ->
    let (n, c') = Cycle.draw s.nat_symbols in
    (s.root ++ n, {s with nat_symbols = c'})
(* simplest constructor just takes in a root *)
  let of_root : name -> t = fun r -> {
      root = r;
      abs_symbols = Cycle.of_list ["x";"y";"z"];
      nat_symbols = Cycle.of_list ["n"; "m"];
      hd_symbols = Cycle.of_list ["l"];
      tl_symbols = Cycle.of_list ["ls"];
      fix_symbols = Cycle.of_list ["f"; "g"; "h"];
      wild_symbols = Cycle.of_list ["w"];
      sens_symbols = Cycle.of_list ["s";"k";"t"];
      dt_symbols = Cycle.of_list ["a";"b";"c"];
  }
end
