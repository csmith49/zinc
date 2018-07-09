open Make

(* utility functions for treating values more cleanly *)
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
    Primitive.dtype = modal (one, real) -* real;
    Primitive.source = Value.F (fun v -> match v with
        | Value.Real r -> Value.Real (r +. 1.0)
        | _ -> failwith "can't eval on this type")
  }

  let square = {
    Primitive.name = "square";
    Primitive.dtype = real => real;
    Primitive.source = Value.F (fun v -> match v with
        | Value.Real r -> Value.Real (r *. r)
        | _ -> failwith "can't eval on this type")
  }

  let double = {
    Primitive.name = "double";
    Primitive.dtype = modal (two, real) -* real;
    Primitive.source = Value.F (fun v -> match v with
        | Value.Real r -> Value.Real (r *. 2.0)
        | _ -> failwith "can't eval on this type")
  }

  let cast_int = {
    Primitive.name = "cast";
    Primitive.dtype = modal (one, int) -* real;
    Primitive.source = Value.F (fun v -> match v with
      | Value.Int i -> Value.Real (float_of_int i)
      | _ -> failwith "can't cast a non-int");
  }

  let big = {
    Primitive.name = "big";
    Primitive.dtype = real => bool;
    Primitive.source = Value.F (fun v -> match v with
      | Value.Real r -> Value.Bool (r >= 10.0)
      | _ -> failwith "can't check if a non-real is big");
  }

  let signature = [succ; square; double; cast_int; big]
end

(* some polymorphic functions *)
module MapReduce = struct
  let filter = {
    Primitive.name = "filter";
    Primitive.dtype = 
      tbind (a, sbind (n,
        modal (n, a => bool) -* (modal (one, mset (a, n)) -* mset (a, n))
      ));
    Primitive.source = Value.F (fun v -> match v with
      | Value.F p -> Value.F (fun v -> match v with
        | Value.Bag ts -> Value.Bag (CCList.filter (fun b -> (p b) = (Value.Bool true)) ts)
        | _ -> failwith "can't apply filter to a non-bag")
      | _ -> failwith "can't apply a non-function predicate");
  }

  let map = {
    Primitive.name = "map";
    Primitive.dtype =
      tbind (a, tbind (b, sbind (s, sbind (n, 
        modal (n, modal (s, a) -* b) -* (modal (one, mset (a, n)) -* mset (b, n))
      ))));
    Primitive.source = Value.F (fun v -> match v with
      | Value.F f -> Value.F (fun v -> match v with
        | Value.Bag ts -> Value.Bag (CCList.map f ts)
        | _ -> failwith "can't apply map to a non-bag")
      | _ -> failwith "can't apply a non-function mapper");
  }

  let signature = [filter; map]
end

(* most aggregations have to operate over bounded types *)
module Aggregate = struct
  let count = {
    Primitive.name = "count";
    dtype = tbind (a, sbind (n, modal (one, mset (a, n)) -* real));
    source = Value.F (fun v -> match v with
      | Value.Bag ts -> Value.Real (float_of_int (CCList.length ts))
      | _ -> failwith "can't count a non-bag");
  }

  let sum = {
    Primitive.name = "sum";
    dtype = sbind (n, sbind (s, modal (s, mset (bounded s, n)) -* real));
    source = Value.F (fun v -> match v with
      | Value.Bag ts -> Value.Real (CCList.fold_left (+.) 0.0 (CCList.map unpack_real ts))
      | _ -> failwith "can't sum a non-bag");
  }

  let average = {
    Primitive.name = "average";
    dtype = sbind (n, sbind (s, modal (s, mset (bounded s, n)) -* real));
    source = Value.F (fun v -> match v with
      | Value.Bag ts -> begin match ts with
        | [] -> Value.Real 0.0
        | _ ->
        let total = CCList.fold_left (+.) 0.0 (CCList.map unpack_real ts) in
        let count = float_of_int (CCList.length ts) in
          Value.Real (total /. count) end
      | _ -> failwith "can't average a non-bag");
  }

  let signature = [sum; average; count]
end

(* combinatorics for defining predicates *)
module Predicate = struct
  (* some simple helpers *)
  let value_and (l : Value.t) (r : Value.t) : Value.t = match l, r with
    | Value.Bool p, Value.Bool q -> Value.Bool (p && q)
    | _ -> failwith "can't and non-bools"
  let value_or (l : Value.t) (r : Value.t) : Value.t = match l, r with
    | Value.Bool p, Value.Bool q -> Value.Bool (p && q)
    | _ -> failwith "can't or non-bools"
  let value_not (b : Value.t) : Value.t = match b with
    | Value.Bool b -> Value.Bool (not b)
    | _ -> failwith "can't not non-bools"

  let pred_and = {
    Primitive.name = "and";
    dtype = tbind (a, 
      (a => bool) => ((a => bool) => (a => bool))
    );
    source = Value.F (fun v -> match v with
      | Value.F p1 -> Value.F (fun v -> match v with
        | Value.F p2 -> Value.F (fun a -> value_and (p1 a) (p2 a))
        | _ -> failwith "not a func")
      | _ -> failwith "not a func");
  }

  let pred_or = {
    Primitive.name = "or";
    dtype = tbind (a, 
      (a => bool) => ((a => bool) => (a => bool))
    );
    source = Value.F (fun v -> match v with
      | Value.F p1 -> Value.F (fun v -> match v with
        | Value.F p2 -> Value.F (fun a -> value_or (p1 a) (p2 a))
        | _ -> failwith "not a func")
      | _ -> failwith "not a func");
  }

  let pred_not = {
    Primitive.name = "not";
    dtype = tbind (a,
      (a => bool) => (a => bool)
    );
    source = Value.F (fun v -> match v with
      | Value.F p -> Value.F (fun v -> value_not (p v))
      | _ -> failwith "not a func");
  }

  let signature = [pred_and; pred_or; pred_not]
end

module Constants = struct
  let value_true = {
    Primitive.name = "true";
    dtype = bool;
    source = Value.Bool true
  }
  let value_false = {
    Primitive.name = "false";
    dtype = bool;
    source = Value.Bool false;
  }

  let signature = [value_true; value_false]
end

module Database = struct
  let compare_with = {
    Primitive.name = "compare_with";
    dtype = tbind (a, sbind (s,
      modal (s, row => a) -* (a => (modal (one, mset (row, s)) -* mset (row, infinity)))
    ));
    source = Value.F (fun v -> match v with
      | Value.F project -> Value.F (fun c -> Value.F (
        fun v -> match v with
          | Value.Bag ts -> Value.Bag (CCList.filter (fun row -> (project row) = c) ts)
          | _ -> failwith "not a bag"))
      | _ -> failwith "not a function");
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
    source = Value.F (fun v -> match v with
      | Value.Bag keys -> Value.F (fun v -> match v with
        | Value.F project -> Value.F (fun v -> match v with
          | Value.Bag xs -> Value.Bag (
            CCList.map (fun k ->
              Value.Pair (k, Value.Bag (CCList.filter (fun v -> (project v) = k) xs))
            ) keys)
          | _ -> failwith "not a bag")
        | _ -> failwith "not a projection")
      | _ -> failwith "not a set of keys");
  }

  let group_map = {
    Primitive.name = "group_map";
    dtype = tbind (a, tbind (b, tbind (c, sbind (s, sbind (n, 
      modal (one, mset (pair (a, b), n)) -* (modal (n, b => c) -* mset (pair (a, c), n))
    )))));
    source = Value.F (fun v -> match v with
      | Value.Bag xs -> Value.F (fun v -> match v with
        | Value.F f -> Value.Bag (CCList.map (fun v -> match v with
          | Value.Pair (k, v) -> Value.Pair (k, f v)
          | _ -> failwith "not a pair") xs)
        | _ -> failwith "not a function")
      | _ -> failwith "not a bag");
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
    source = Value.F (fun v -> match v with
      | Value.Real r -> Value.Bool (r >= 40.0)
      | _ -> failwith "not an hour");
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
  let make (gt : bool) (gender : string) (race : string) (hours : int) (education : int) (profession : string) (w_class : string) (cg : int) : Value.t =
    Value.row_of_list [
      ("gt_50k", Value.Bool gt);
      ("gender", Value.Discrete gender);
      ("race", Value.Discrete race);
      ("work_hours", Value.Real (float_of_int hours));
      ("education_level", Value.Real (float_of_int education));
      ("profession", Value.Discrete profession);
      ("work_class", Value.Discrete w_class);
      ("capital_gains", Value.Real (float_of_int cg));
    ]
end

(* arithmetic *)
module Arithmetic = struct
  let add = {
    Primitive.name = "add";
    dtype = sbind (s, sbind (n,
      modal (one, real) -* (modal (one, real) -* real)
    ));
    source = Value.F (fun v -> match v with
      | Value.Real l -> Value.F (fun v -> match v with
        | Value.Real r -> Value.Real (l +. r)
        | _ -> failwith "not a number")
      | _ -> failwith "not a number");
  }
  let mult = {
    Primitive.name = "mult";
    dtype = sbind (s, sbind (n,
      modal (s, p_real n) -* (modal (n, p_real s) -* real)
    ));
    source = Value.F (fun v -> match v with
      | Value.Real l -> Value.F (fun v -> match v with
        | Value.Real r -> Value.Real (l *. r)
        | _ -> failwith "not a number")
      | _ -> failwith "not a number");
  }
  let succ = {
    Primitive.name = "succ";
    dtype = sbind (s, 
      modal (one, p_real s) -* real
    );
    source = Value.F (fun v -> match v with
      | Value.Real r -> Value.Real (r +. 1.0)
      | _ -> failwith "not a number");
  }

  let bad_mult = {
    Primitive.name = "bad_mult";
    dtype = sbind (s, sbind (n,
      (p_real s) => ( (p_real n) => real)
    ));
    source = Value.F (fun v -> match v with
    | Value.Real l -> Value.F (fun v -> match v with
      | Value.Real r -> Value.Real (l *. r)
      | _ -> failwith "not a number")
    | _ -> failwith "not a number");
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
    source = Value.F (fun v -> match v with
      | Value.Real r -> Value.Bool (r >= 3.0)
      | _ -> failwith "not a real value")
  }
  let poor = {
    Primitive.name = "poor";
    dtype = (bounded_by 5) => bool;
    source = Value.F (fun v -> match v with
      | Value.Real r -> Value.Bool (r < 3.0)
      | _ -> failwith "not a real value")
  }
  
  let is_rural = discrete_check "is_rural" "rural" address_type_t
  let is_urban = discrete_check "is_urban" "urban" address_type_t
  let is_rep = discrete_check "is_rep" "reputation" reason_t
  let is_prox = discrete_check "is_prox" "proximity" reason_t

  let family_is = {
    Primitive.name = "family_is";
    dtype = row => (family_t => bool);
    source = Value.F (fun v -> match v with
      | Value.Row r -> Value.F (fun v -> Value.Bool ((Value.StringMap.get "family" r) = (Some v)))
      | _ -> failwith "not a row");
  }

  let checks = [moderate; poor; is_rural; is_urban; is_rep; is_prox; family_is]

  (* put it all together *)
  let signature = keys @ conversions @ checks

  (* and help make some rows *)
  (* schema = [grade; reason; wknd; wkdy; address; payed; family; absences] *)
  let make (grade : int) (reason : string) (wknd : int) (wkdy : int) (address : string) (payed : bool) (family : int) (absences : int) : Value.t =
    Value.row_of_list [
      ("grade", Value.Real (float_of_int grade)); 
      ("reason", Value.Discrete reason);
      ("weekend_consumption", Value.Real (float_of_int wknd));
      ("weekday_consumption", Value.Real (float_of_int wkdy));
      ("address_type", Value.Discrete (address));
      ("payed", Value.Bool (payed));
      ("family", Value.Real (float_of_int family));
      ("absences", Value.Real (float_of_int absences))
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
    source = Value.F (fun v -> match v with
      | Value.Real r -> Value.Bool (r <= 69.0)
      | _ -> failwith "not a real value");
  }
  let medium = {
    Primitive.name = "medium";
    dtype = (bounded_by 100) => bool;
    source = Value.F (fun v -> match v with
      | Value.Real r -> Value.Bool (r > 69.0 && r <= 89.0)
      | _ -> failwith "not a real value");
  }
  let high = {
    Primitive.name = "high";
    dtype = (bounded_by 100) => bool;
    source = Value.F (fun v -> match v with
      | Value.Real r -> Value.Bool (r > 89.0)
      | _ -> failwith "not a real value");
  }

  let to_bracket = {
    Primitive.name = "to_bracket";
    dtype = (bounded_by 100) => bracket_t;
    source = Value.F (fun v -> match v with
      | Value.Real r ->
        if r <= 69.0 then
          Value.Discrete "low"
        else if r <= 89.0 then
          Value.Discrete "medium"
        else
          Value.Discrete "high"
      | _ -> failwith "not a real value")
  }

  let is_bracket = {
    Primitive.name = "is_bracket";
    dtype = (bounded_by 100) => (bracket_t => bool);
    source = Value.F (fun v -> match v with
      | Value.Real r -> Value.F (fun v -> match v with
        | Value.Discrete b ->
          if r <= 69.0 && b = "low" then
            Value.Bool true
          else if r <= 89.0 && r > 69.0 && b = "medium" then
            Value.Bool true
          else if r > 89.0 && b = "high" then
            Value.Bool true
          else
            Value.Bool false 
        | _ -> failwith "not a bracket")
      | _ -> failwith "not a real")
  }

  let is_low = discrete_check "is_low" "low" bracket_t
  let is_medium = discrete_check "is_medium" "medium" bracket_t
  let is_high = discrete_check "is_high" "high" bracket_t
  
  (* including those for absences *)
  let gt_7 = {
    Primitive.name = "gt_7";
    dtype = absences_t => bool;
    source = Value.F (fun v -> match v with
      | Value.Real r -> Value.Bool (r > 7.0)
      | _ -> failwith "not a real value");
  }
  let leq_7 = {
    Primitive.name = "leq_7";
    dtype = absences_t => bool;
    source = Value.F (fun v -> match v with
      | Value.Real r -> Value.Bool (r <= 7.0)
      | _ -> failwith "not a real value");
  }

  let utilities = [to_bracket; is_bracket; is_low; is_medium; is_high; gt_7; leq_7]

  (* put it together *)
  let signature = keys @ conversions @ utilities

  (* and make it easier to construct examples *)
  (* schema: level; satisfaction; absences; participation; resources; discussion; *)
  let make (level : int) (satisfaction : bool) (absences : int) (participation : int) (resources : int) (discussion : int) : Value.t =
    Value.row_of_list [
      ("level", Value.Real (float_of_int level));
      ("satisfaction", Value.Bool satisfaction);
      ("absences", Value.Real (float_of_int absences));
      ("participation", Value.Real (float_of_int participation));
      ("resources", Value.Real (float_of_int resources));
      ("discussion", Value.Real (float_of_int discussion));
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
  let race_is = {
    Primitive.name = "race_is";
    dtype = row => (race_t => bool);
    source = Value.F (fun v -> match v with
    | Value.Row r -> Value.F (fun v -> Value.Bool ((Value.StringMap.get "race" r) = (Some v)))
    | _ -> failwith "not a row");
  }
  let is_bracket = {
    Primitive.name = "is_bracket";
    dtype = score => (bracket_t => bool);
    source = Value.F (fun v -> match v with
      | Value.Real r -> Value.F (fun v -> match v with
        | Value.Discrete b ->
          if r < 5.0 && b = "low" then
            Value.Bool true
          else if r >= 5.0 && r < 8.0 && b = "medium" then
            Value.Bool true
          else if r >= 8.0 && b = "high" then
            Value.Bool true
          else
            Value.Bool false
        | _ -> failwith "not a discrete value")
      | _ -> failwith "not a real number")
  }
  let to_bracket = {
    Primitive.name = "to_bracket";
    dtype = score => bracket_t;
    source = Value.F (fun v -> match v with
      | Value.Real r ->
        if r < 5.0 then
          Value.Discrete "low"
        else if r < 8.0 then
          Value.Discrete "medium"
        else
          Value.Discrete "high"
      | _ -> failwith "not a real value")
  }
  let is_low = discrete_check "is_low" "low" bracket_t
  let is_medium = discrete_check "is_medium" "medium" bracket_t
  let is_high = discrete_check "is_high" "high" bracket_t

  let is_male = discrete_check "is_male" "male" gender_t
  let is_female = discrete_check "is_female" "female" gender_t

  let gender_is = {
    Primitive.name = "gender_is";
    dtype = row => (gender_t => bool);
    source = Value.F (fun v -> match v with
    | Value.Row r -> Value.F (fun v -> Value.Bool ((Value.StringMap.get "gender" r) = (Some v)))
    | _ -> failwith "not a row");
  }

  let none = {
    Primitive.name = "none";
    dtype = counts => bool;
    source = Value.F (fun v -> match v with
      | Value.Real r -> Value.Bool (r = 0.0)
      | _ -> failwith "not a real value")
  }
  let lots = {
    Primitive.name = "lots";
    dtype = counts => bool;
    source = Value.F (fun v -> match v with
      | Value.Real r -> Value.Bool (r = 15.0)
      | _ -> failwith "not a real value")
  }

  let utilities = [is_bracket; to_bracket; is_low; is_medium; is_high; none; lots; race_is; is_male; is_female]

  (* put it together *)
  let signature = keys @ conversions @ utilities

  (* and a make function *)
  (* schema: sex; age_cat; race; juv_fel; priors_cnt; recidivism; violence; failure to appear *)
  let make 
    (sex : string) (age_cat : string) (race : string) 
    (juv_fel : int) (priors : int) (recid : int) 
    (violence : int) (fta : int) : Value.t =
    Value.row_of_list [
      ("gender", Value.Discrete sex);
      ("age_cat", Value.Discrete age_cat);
      ("race", Value.Discrete race);
      ("juv_felonies", Value.Real (float_of_int juv_fel));
      ("priors", Value.Real (float_of_int priors));
      ("recidivism", Value.Real (float_of_int recid));
      ("violence", Value.Real (float_of_int violence));
      ("fta", Value.Real (float_of_int fta));
    ]
end