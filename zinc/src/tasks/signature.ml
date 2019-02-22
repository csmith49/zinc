open Make
open Vterm.Evaluation.Infix

(* utilities for making functions easier *)

(* we have some utility functions for treating values a little more cleanly *)
let binarize (f : Value.abstraction) = fun x -> fun y -> match (f x) with
  | Value.F f' -> f' y
  | _ -> failwith "can't binarize"

(* and pulling out reals so we can map over a list *)
let unpack_real (r : Value.t) : float = match r with
  | Value.Real f -> f
  | Value.Int i -> float_of_int i
  | _ -> failwith "not a real number"

(* each module will maintain several lists of primitives *)
type t = Primitive.t list

(* basics first *)
module Basic = struct
  let succ = {
    Primitive.name = "succ";
    dtype = modal (one, real) -* real;
    source = Vterm.Function ("succ",
      let f i = i +. 1.0 in
      fun v -> v |> Vterm.eval |> Vterm.Evaluation.real_map f)
  }

  let square = {
    Primitive.name = "square";
    dtype = real => real;
    source = Vterm.Function ("square",
      let f i = i *. i in
      fun v -> v |> Vterm.eval |> Vterm.Evaluation.real_map f)
  }

  let double = {
    Primitive.name = "double";
    dtype = modal (two, real) -* real;
    source = Vterm.Function ("double",
      let f i = i +. i in
      fun v -> v |> Vterm.eval |> Vterm.Evaluation.real_map f)
  }
    
  let cast_int = {
    Primitive.name = "cast";
    dtype = modal (one, int) -* real;
    source = Vterm.Function ("cast",
      fun v -> let open Vterm.Alt in
        let f = var "cast_func" in
        let x = var "cast_var" in
        let tm = fix f (match_nat v (nat 0) (x, app succ.source x)) in
          Vterm.eval tm)
  }

  let big = {
    Primitive.name = "big";
    dtype = real => bool;
    source = Vterm.Function ("big",
      let pred i = i >= 10.0 in
      fun v -> v |> Vterm.eval |> Vterm.Evaluation.real_filter pred)
  }

  let signature = [succ; square; double; cast_int; big]
end

(* some polymorphic functions *)
module MapReduce = struct
  let filter = {
    Primitive.name = "filter";
    dtype = 
      tbind (a, sbind (n,
        modal (infinity, a => bool) -* (modal (one, mset (a, n)) -* mset (a, n))
      ));
    source = Vterm.Function ("filter", fun pred ->
      Vterm.Value ( Vterm.Function ("filter pred", fun mset ->
        let pred' tm = Vterm.App (pred, tm) |> Vterm.eval |> Vterm.Evaluation.is_true in
        match Vterm.eval mset with
          | Value (Vterm.Bag ts) -> ts |> CCList.filter pred' |> (fun v -> Vterm.Bag v) |> Vterm.Evaluation.return
          | _ -> Vterm.Diverge
      )))
  }

  let map = {
    Primitive.name = "map";
    dtype =
      (* is this actually the right type? what's the sensitivity on the mset input? *)
      tbind (a, tbind (b, sbind (s, sbind (n, 
        modal (n, modal (s, a) -* b) -* (modal (one, mset (a, n)) -* mset (b, n))
      ))));
    source = Vterm.Function ("map", fun f ->
      Vterm.Value (Vterm.Function ("map f", fun mset ->
        let eval_f tm = Vterm.App (f, tm) |> Vterm.eval in
          Vterm.eval mset |> Vterm.Evaluation.bag_map eval_f
    )))
  }

  let signature = [filter; map]
end

(* most aggregations have to operate over bounded types *)
module Aggregate = struct
  let count = {
    Primitive.name = "count";
    dtype = tbind (a, sbind (n, modal (one, mset (a, n)) -* real));
    source = Vterm.Function ("count", fun v -> match Vterm.eval v with
      | Vterm.Value (Vterm.Bag ts) -> Vterm.Value (Vterm.Real (float_of_int (CCList.length ts)))
      | _ -> Vterm.Diverge)
  }

  let sum = {
    Primitive.name = "sum";
    dtype = sbind (n, sbind (s, modal (s, mset (bounded s, n)) -* real));
    source = Vterm.Function ("sum", fun mset -> match Vterm.eval mset with
      | Vterm.Value (Vterm.Bag ts) -> 
        let reals = ts 
          |> CCList.map Vterm.Evaluation.return
          |> CCList.map Vterm.Evaluation.to_real
          |> CCOpt.sequence_l in
        begin match reals |> CCOpt.map (CCList.fold_left (+.) 0.0) with
          | Some f -> Vterm.Evaluation.of_real f
          | _ -> Diverge
        end
      | _ -> Diverge
    )
  }

  let average = {
    Primitive.name = "average";
    dtype = sbind (n, sbind (s, modal (s, mset (bounded s, n)) -* real));
    source = Vterm.Function ("sum", fun mset -> match Vterm.eval mset with
      | Vterm.Value (Vterm.Bag ts) -> 
        let reals = ts 
          |> CCList.map Vterm.Evaluation.return
          |> CCList.map Vterm.Evaluation.to_real
          |> CCOpt.sequence_l in
        let count = CCList.length ts |> float_of_int in
        begin match reals |> CCOpt.map (CCList.fold_left (+.) 0.0) with
          | Some f -> Vterm.Evaluation.of_real (f /. count)
          | _ -> Diverge
        end
      | _ -> Diverge
    )
  }

  let signature = [sum; average; count]
end

(* combinatorics for defining predicates *)
module Predicate = struct
  let pred_and = {
    Primitive.name = "and";
    dtype = tbind (a, 
      (a => bool) => ((a => bool) => (a => bool))
    );
    source = Vterm.Function ("and", fun pred1 ->
      Vterm.Value (Vterm.Function ("and pred", fun pred2 ->
        Vterm.Value (Vterm.Function ("result", fun tm ->
          let p1 = Vterm.App (pred1, tm) |> Vterm.eval |> Vterm.Evaluation.is_true in
          let p2 = Vterm.App (pred2, tm) |> Vterm.eval |> Vterm.Evaluation.is_true in
            Vterm.Evaluation.of_bool (p1 && p2)
    )))))
  }

  let pred_or = {
    Primitive.name = "or";
    dtype = tbind (a, 
      (a => bool) => ((a => bool) => (a => bool))
    );
    source = Vterm.Function ("and", fun pred1 ->
      Vterm.Value (Vterm.Function ("and pred", fun pred2 ->
        Vterm.Value (Vterm.Function ("result", fun tm ->
          let p1 = Vterm.App (pred1, tm) |> Vterm.eval |> Vterm.Evaluation.is_true in
          let p2 = Vterm.App (pred2, tm) |> Vterm.eval |> Vterm.Evaluation.is_true in
            Vterm.Evaluation.of_bool (p1 || p2)
    )))))
  }

  let pred_not = {
    Primitive.name = "not";
    dtype = tbind (a,
      (a => bool) => (a => bool)
    );
    source = Vterm.Function ("not", fun pred ->
      Vterm.Value (Vterm.Function ("result", fun tm ->
        let p = Vterm.App (pred, tm) |> Vterm.eval |> Vterm.Evaluation.is_true in
          Vterm.Evaluation.of_bool (not p)
      )))
  }

  let signature = [pred_and; pred_or; pred_not]
end

module Constants = struct
  let value_true = {
    Primitive.name = "true";
    dtype = bool;
    source = Vterm.Bool Vterm.True;
  }
  let value_false = {
    Primitive.name = "false";
    dtype = bool;
    source = Vterm.Bool Vterm.False;
  }

  let signature = [value_true; value_false]
end

module Database = struct
  let compare_with = {
    Primitive.name = "compare_with";
    dtype = tbind (a, tbind (b, sbind (s,
      modal (s, (row b) => a) -* (a => (modal (one, mset ((row b), s)) -* mset ((row b), infinity)))
    )));
    source = Vterm.Function ("cw", fun project ->
      Vterm.Value (Vterm.Function ("cw p", fun constant ->
        Vterm.Value (Vterm.Function ("cw p c", fun v ->
          let pred = Vterm.Function ("pred", fun row ->
            let p = Vterm.App (project, row) |> Vterm.eval in
            let c = Vterm.eval constant in
              Vterm.Evaluation.of_bool (p = c)) in
          let open Vterm.Alt in
            (MapReduce.filter.Primitive.source <!> pred <!> v) |> Vterm.eval
    )))))
  }

  let partition = {
    Primitive.name = "partition";
    dtype = tbind (a, tbind (b, sbind (s, sbind (n, 
      modal (one, mset (a, n)) -* (
        modal (s, (b => a)) -* (
          modal (one, mset (b, s)) -* (
            mset (pair (a, mset (b, infinity)), n)
          )
        ) 
      )
    ))));
    source = Vterm.Function ("partition", fun keys ->
      Vterm.Value (Vterm.Function ("partition keys", fun project ->
        Vterm.Value (Vterm.Function ("partition keys project", fun mset ->
          let p key = let open Vterm.Alt in
            let elts = compare_with.Primitive.source <!> project <!> key <!> mset in
            let pair = Vterm.Pair (key, elts) in
              pair |> Vterm.eval
          in Vterm.Evaluation.bag_map p (Vterm.eval keys)
        )))))
  }

  let group_map = {
    Primitive.name = "group_map";
    dtype = tbind (a, tbind (b, tbind (c, sbind (s, sbind (n, 
      modal (one, mset (pair (a, b), n)) -* (modal (n, b => c) -* mset (pair (a, c), n))
    )))));
    source = Vterm.Function ("gm", fun mset ->
      Vterm.Value (Vterm.Function ("gm bag", fun f ->
        let mapper tm = match tm with
          | Vterm.Pair (l, r) -> Vterm.Pair (l, Vterm.App (f, r)) |> Vterm.eval
          | _ -> Vterm.Diverge in
        Vterm.Evaluation.bag_map mapper (Vterm.eval mset)
    )))
  }

  let signature = [partition; group_map; compare_with]
end

(* signature for ADULT benchmarks *)
module Adult = struct
  open Primitive.Utility

  (* the dtype encodings *)
  let gender_t = constant_type "Gender"
  let race_t = constant_type "Race"
  let hours_t = constant_type "Hours"
  let profession_t = constant_type "Profession"
  let work_class_t = constant_type "Work Class"
  let education_t = constant_type "Education"

  (* all the keys in our dataset *)
  let gt_50k = projection "gt_50k" bool
  let gender = projection "gender" gender_t
  let race = projection "race" race_t
  let work_hours = projection "work_hours" hours_t
  let education_level = projection "education_level" education_t
  let profession = projection "profession" profession_t
  let work_class = projection "work_class" work_class_t
  let capital_gains = projection "capital_gains" real

  let keys = [gt_50k; race; gender; work_hours; education_level; profession; work_class; capital_gains]

  (* with simple conversions *)
  let hours_to_val = bounded_conversion "hours_to_val" hours_t 168
  let education_to_val = bounded_conversion "edu_to_val" education_t 20

  let conversions = [hours_to_val; education_to_val]

  (* and some simple predicates for the search *)
  let gt_40_hrs = {
    Primitive.name = "gt_40_hrs";
    dtype = hours_t => bool;
    source = Vterm.Function ("get_40", fun r ->
      r |> Vterm.eval |> Vterm.Evaluation.real_filter (fun r -> r >= 40.0)
    )
  }

  let is_female = discrete_check "is_female" "female" gender_t
  let is_male = discrete_check "is_male" "male" gender_t
  let is_army = discrete_check "is_army" "army" profession_t
  let is_trade = discrete_check "is_trade" "trade" profession_t
  let is_agriculture = discrete_check "is_agriculture" "agriculture" profession_t
  let is_local = discrete_check "is_local" "local" work_class_t
  let is_federal = discrete_check "is_federal" "federal" work_class_t

  let checks = [gt_40_hrs; is_female; is_male; is_army; is_trade; is_local; is_federal; is_agriculture]

  (* the total signature *)
  let signature = checks @ keys @ conversions
  
  (* and a utility for constructing examples *)
  let make (gt : bool) (gender : string) (race : string) (hours : int) (education : int) (profession : string) (w_class : string) (cg : int) : Vterm.t =
    let open Vterm.Alt in row [
      ("gt_50k", bool gt);
      ("gender", discrete gender);
      ("race", discrete race);
      ("work_hours", hours |> float_of_int |> real);
      ("education_level", education |> float_of_int |> real);
      ("profession", discrete profession);
      ("work_class", discrete w_class);
      ("capital_gains", cg |> float_of_int |> real)
    ]
end

(* arithmetic *)
module Arithmetic = struct
  let add = {
    Primitive.name = "add";
    dtype = sbind (s, sbind (n,
      modal (one, real) -* (modal (one, real) -* real)
    ));
    source = Vterm.Function ("add", fun x ->
      Vterm.Value (Vterm.Function ("add x", fun y ->
        let f a b = a +. b in
        Vterm.Evaluation.real_map2 f (Vterm.eval x) (Vterm.eval y)
      )))
  }
  let mult = {
    Primitive.name = "mult";
    dtype = sbind (s, sbind (n,
      modal (s, p_real n) -* (modal (n, p_real s) -* real)
    ));
    source = Vterm.Function ("add", fun x ->
      Vterm.Value (Vterm.Function ("add x", fun y ->
        let f a b = a *. b in
        Vterm.Evaluation.real_map2 f (Vterm.eval x) (Vterm.eval y)
      )))
  }
  let succ = {
    Primitive.name = "succ";
    dtype = sbind (s, 
      modal (one, p_real s) -* real
    );
    source = Vterm.Function ("succ", fun r ->
      Vterm.Evaluation.real_map (fun x -> x +. 1.0) (Vterm.eval r)
    )
  }

  let bad_mult = {
    Primitive.name = "bad_mult";
    dtype = sbind (s, sbind (n,
      (p_real s) => ( (p_real n) => real)
    ));
    source = Vterm.Function ("bad_mult", fun x ->
      Vterm.Value (Vterm.Function ("add x", fun y ->
        let f a b = a *. b in
        Vterm.Evaluation.real_map2 f (Vterm.eval x) (Vterm.eval y)
      )))
  }

  let signature = [add; mult; succ]
end

(* student alcohol consumption study *)
module Student = struct
  open Primitive.Utility

  (* types encoding our fields *)
  let grade_t = constant_type "Grade"
  let reason_t = constant_type "Reason"
  let weekend_consumption_t = constant_type "Weekend Alcohol"
  let weekday_consumption_t = constant_type "Weekday Alcohol"
  let address_type_t = constant_type "Address Type"
  let family_t = constant_type "Family"
  let absences_t = constant_type "Absences"

  (* projections *)
  let grade = projection "grade" grade_t
  let reason = projection "reason" reason_t
  let weekend_consumption = projection "weekend_consumption" weekend_consumption_t
  let weekday_consumption = projection "weekday_consumption" weekday_consumption_t
  let address_type = projection "address_type" address_type_t
  let payed = projection "payed" bool
  let family = projection "family" family_t
  let absences = projection "absences" absences_t

  let keys = [grade; reason; weekend_consumption; weekday_consumption; address_type; payed; family; absences]

  (* conversions, where appropriate *)
  let grade_to_val = bounded_conversion "grade_to_val" grade_t 20
  let wknd_to_val = bounded_conversion "wknd_to_val" weekend_consumption_t 5
  let wkdy_to_val = bounded_conversion "wkdy_to_val" weekday_consumption_t 5
  let fam_to_val = bounded_conversion "fam_to_val" family_t 5
  let abs_to_val = bounded_conversion "abs_to_val" absences_t 100

  let conversions = [grade_to_val; wknd_to_val; wkdy_to_val; fam_to_val; abs_to_val]

  (* and some utility functions *)
  let moderate = {
    Primitive.name = "moderate";
    dtype = (bounded_by 5) => bool;
    source = Vterm.Function ("moderate", fun r ->
      let f x = x >= 3.0 in
      Vterm.Evaluation.real_filter f (Vterm.eval r))
  }
  let poor = {
    Primitive.name = "poor";
    dtype = (bounded_by 5) => bool;
    source = Vterm.Function ("poor", fun r ->
      let f x = x < 3.0 in
      Vterm.Evaluation.real_filter f (Vterm.eval r))
  }
  
  let is_rural = discrete_check "is_rural" "rural" address_type_t
  let is_urban = discrete_check "is_urban" "urban" address_type_t
  let is_rep = discrete_check "is_rep" "reputation" reason_t
  let is_prox = discrete_check "is_prox" "proximity" reason_t

  (* let family_is = {
    Primitive.name = "family_is";
    dtype = row => (family_t => bool);
    source = Value.F (fun v -> match v with
      | Value.Row r -> Value.F (fun v -> Value.Bool ((Value.StringMap.get "family" r) = (Some v)))
      | _ -> failwith "not a row");
  } *)

  let checks = [moderate; poor; is_rural; is_urban; is_rep; is_prox]

  (* put it all together *)
  let signature = keys @ conversions @ checks

  (* and help make some rows *)
  (* schema = [grade; reason; wknd; wkdy; address; payed; family; absences] *)
  let make (grade : int) (reason : string) (wknd : int) (wkdy : int) (address : string) (payed : bool) (family : int) (absences : int) : Vterm.t =
    let open Vterm.Alt in row [
      ("grade", real (float_of_int grade)); 
      ("reason", discrete reason);
      ("weekend_consumption", real (float_of_int wknd));
      ("weekday_consumption", real (float_of_int wkdy));
      ("address_type", discrete (address));
      ("payed", bool (payed));
      ("family", real (float_of_int family));
      ("absences", real (float_of_int absences))
    ]
end

(* student academic performance study *)
module Performance = struct
  open Primitive.Utility

  (* types encoding the fields *)
  let level_t = constant_type "Level"
  let absences_t = constant_type "Absences"
  let participation_t = constant_type "Participation"
  let discussion_t = constant_type "Discussion"
  let resources_t = constant_type "Resources"

  (* and one not *)
  let bracket_t = constant_type "Bracket"

  (* projections defining keys *)
  let level = projection "level" level_t
  let satisfaction = projection "satisfaction" bool
  let absences = projection "absences" absences_t
  let participation = projection "participation" participation_t
  let discussion = projection "discussion" discussion_t
  let resources = projection "resources" resources_t

  let keys = [level; satisfaction; absences; participation; discussion; resources]

  (* conversions *)
  let level_to_val = bounded_conversion "level_to_val" level_t 100
  let part_to_val = bounded_conversion "part_to_val" participation_t 100
  let disc_to_val = bounded_conversion "disc_to_val" discussion_t 100
  let res_to_val = bounded_conversion "res_to_val" resources_t 100

  let conversions = [level_to_val; part_to_val; disc_to_val; res_to_val]

  (* and utility functions *)
  let low = {
    Primitive.name = "low";
    dtype = (bounded_by 100) => bool;
    source = Vterm.Function ("low", fun r ->
      let f x = x <= 69.0 in
      Vterm.Evaluation.real_filter f (Vterm.eval r))
  }
  let medium = {
    Primitive.name = "medium";
    dtype = (bounded_by 100) => bool;
    source = Vterm.Function ("medium", fun r ->
      let f x = x > 69.0 && x <= 89.0 in
      Vterm.Evaluation.real_filter f (Vterm.eval r))
  }
  let high = {
    Primitive.name = "high";
    dtype = (bounded_by 100) => bool;
      source = Vterm.Function ("high", fun r ->
      let f x = x > 89.0 in
      Vterm.Evaluation.real_filter f (Vterm.eval r))
  }

  let to_bracket = {
    Primitive.name = "to_bracket";
    dtype = (bounded_by 100) => bracket_t;
    source = Vterm.Function ("to_bracket", fun r ->
      let disc tm = match tm with
        | Vterm.Real r ->
          if r <= 69.0 then
            Vterm.Value (Vterm.Discrete "low")
          else if r <= 89.0 then
            Vterm.Value (Vterm.Discrete "medium")
          else Vterm.Value (Vterm.Discrete "high")
        | _ -> Vterm.Diverge in
      r |> Vterm.eval >>= disc
    )
  }

  let is_bracket = {
    Primitive.name = "is_bracket";
    dtype = (bounded_by 100) => (bracket_t => bool);
    source = Vterm.Function ("is_bracket", fun r ->
      Vterm.Value (Vterm.Function ("is_bracket r", fun brack ->
      let check brack r = match brack with
        | Vterm.Discrete b -> begin match r with
          | Vterm.Real r ->
            if r <= 69.0 && b = "low" then
              Vterm.Evaluation.of_bool true
          else if r <= 89.0 && r > 69.0 && b = "medium" then
            Vterm.Evaluation.of_bool true
          else if r > 89.0 && b = "high" then
            Vterm.Evaluation.of_bool true
          else
            Vterm.Evaluation.of_bool false
          | _ -> Diverge
        end
        | _ -> Diverge in
      Vterm.Evaluation.flat_map2 check (Vterm.eval brack) (Vterm.eval r)
    )))
  }

  let is_low = discrete_check "is_low" "low" bracket_t
  let is_medium = discrete_check "is_medium" "medium" bracket_t
  let is_high = discrete_check "is_high" "high" bracket_t
  
  (* including those for absences *)
  let gt_7 = {
    Primitive.name = "gt_7";
    dtype = absences_t => bool;
    source = Vterm.Function ("gt_7", fun r ->
      let f r = r > 7.0 in
        r |> Vterm.eval |> Vterm.Evaluation.real_filter f
    )
  }
  let leq_7 = {
    Primitive.name = "leq_7";
    dtype = absences_t => bool;
    source = Vterm.Function ("leq_7", fun r ->
      let f r = r <= 7.0 in
        r |> Vterm.eval |> Vterm.Evaluation.real_filter f
    )
  }

  let utilities = [to_bracket; is_bracket; is_low; is_medium; is_high; gt_7; leq_7]

  (* put it together *)
  let signature = keys @ conversions @ utilities

  (* and make it easier to construct examples *)
  (* schema: level; satisfaction; absences; participation; resources; discussion; *)
  let make (level : int) (satisfaction : bool) (absences : int) (participation : int) (resources : int) (discussion : int) : Vterm.t =
    let open Vterm.Alt in row [
      ("level", real (float_of_int level));
      ("satisfaction", bool satisfaction);
      ("absences", real (float_of_int absences));
      ("participation", real (float_of_int participation));
      ("resources", real (float_of_int resources));
      ("discussion", real (float_of_int discussion));
    ]
end

(* compas data set *)
module Compas = struct
  open Primitive.Utility

  (* types for projections and whatnot *)
  let gender_t = constant_type "Gender"
  let age_cat_t = constant_type "Age Category"
  let race_t = constant_type "Race"

  let score = bounded_by 10
  let counts = bounded_by 15
  let bracket_t = constant_type "bracket"

  (* schema: sex; age_cat; race; juv_fel; priors_cnt; recidivism; violence; failure to appear *)
  let gender = projection "gender" gender_t
  let age_cat = projection "age_cat" age_cat_t
  let race = projection "race" race_t
  let juv_felonies = projection "juv_felonies" counts
  let priors = projection "priors" counts
  let recidivism = projection "recidivism" score
  let violence = projection "violence" score
  let fta = projection "fta" score

  let keys = [gender; age_cat; race; juv_felonies; priors; recidivism; violence; fta]

  (* conversions *)
  let is_young = discrete_check "is_young" "young" age_cat_t
  let is_mid_age = discrete_check "is_mid_age" "mid_age" age_cat_t
  let is_elderly = discrete_check "is_elderly" "elderly" age_cat_t

  let conversions = [is_young; is_mid_age; is_elderly]

  (* utilities *)
  (* let race_is = {
    Primitive.name = "race_is";
    dtype = row => (race_t => bool);
    source = Value.F (fun v -> match v with
    | Value.Row r -> Value.F (fun v -> Value.Bool ((Value.StringMap.get "race" r) = (Some v)))
    | _ -> failwith "not a row");
  } *)
  let is_bracket = {
    Primitive.name = "is_bracket";
    dtype = score => (bracket_t => bool);
    source = Vterm.Function ("is_bracket", fun r ->
      Vterm.Value (Vterm.Function ("is_bracket r", fun brack ->
      let check brack r = match brack with
        | Vterm.Discrete b -> begin match r with
          | Vterm.Real r ->
            if r < 5.0 && b = "low" then
              Vterm.Evaluation.of_bool true
          else if r < 8.0 && r >= 5.0 && b = "medium" then
            Vterm.Evaluation.of_bool true
          else if r >= 8.0 && b = "high" then
            Vterm.Evaluation.of_bool true
          else
            Vterm.Evaluation.of_bool false
          | _ -> Diverge
        end
        | _ -> Diverge in
      Vterm.Evaluation.flat_map2 check (Vterm.eval brack) (Vterm.eval r)
    )))
  }
  let to_bracket = {
    Primitive.name = "to_bracket";
    dtype = score => bracket_t;
    source = Vterm.Function ("to_bracket", fun r ->
      let disc tm = match tm with
        | Vterm.Real r ->
          if r < 5.0 then
            Vterm.Value (Vterm.Discrete "low")
          else if r < 8.0 then
            Vterm.Value (Vterm.Discrete "medium")
          else Vterm.Value (Vterm.Discrete "high")
        | _ -> Vterm.Diverge in
      r |> Vterm.eval >>= disc
    )
  }
  let is_low = discrete_check "is_low" "low" bracket_t
  let is_medium = discrete_check "is_medium" "medium" bracket_t
  let is_high = discrete_check "is_high" "high" bracket_t

  let is_male = discrete_check "is_male" "male" gender_t
  let is_female = discrete_check "is_female" "female" gender_t

  (* let gender_is = {
    Primitive.name = "gender_is";
    dtype = row => (gender_t => bool);
    source = Value.F (fun v -> match v with
    | Value.Row r -> Value.F (fun v -> Value.Bool ((Value.StringMap.get "gender" r) = (Some v)))
    | _ -> failwith "not a row");
  } *)

  let none = {
    Primitive.name = "none";
    dtype = counts => bool;
    source = Vterm.Function ("none", fun r ->
      let f r = r = 0.0 in
        r |> Vterm.eval |> Vterm.Evaluation.real_filter f
    )
  }
  let lots = {
    Primitive.name = "lots";
    dtype = counts => bool;
    source = Vterm.Function ("lots", fun r ->
      let f r = r = 15.0 in
        r |> Vterm.eval |> Vterm.Evaluation.real_filter f
    )
  }

  let utilities = [is_bracket; to_bracket; is_low; is_medium; is_high; none; lots; is_male; is_female]

  (* put it together *)
  let signature = keys @ conversions @ utilities

  (* and a make function *)
  (* schema: sex; age_cat; race; juv_fel; priors_cnt; recidivism; violence; failure to appear *)
  let make 
    (sex : string) (age_cat : string) (race : string) 
    (juv_fel : int) (priors : int) (recid : int) 
    (violence : int) (fta : int) : Vterm.t =
    let open Vterm.Alt in row [
      ("gender", discrete sex);
      ("age_cat", discrete age_cat);
      ("race", discrete race);
      ("juv_felonies", real (float_of_int juv_fel));
      ("priors", real (float_of_int priors));
      ("recidivism", real (float_of_int recid));
      ("violence", real (float_of_int violence));
      ("fta", real (float_of_int fta));
    ]
end