(* My test cases *)

(* Test cases for question 1 *)
let my_subset_test0 = subset [] []
let my_subset_test1 = subset [] [2; 1; 5]
let my_subset_test2 = subset [5; 8; 3] [3; 8; 5]
let my_subset_test3 = subset [7; 2] [2; 5; 7]
let my_subset_test4 = not (subset [6; 229; 2] [1])
let my_subset_test5 = not (subset [5] [7])

(* Test cases for question 2 *)
let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = equal_sets [7; 8; 5] [7; 8; 5]
let my_equal_sets_test2 = equal_sets [7; 8; 5] [8; 7; 5]
let my_equal_sets_test3 = equal_sets [9; 3; 1] [1; 3; 3; 9]
let my_equal_sets_test4 = equal_sets [1; 9; 9; 9] [9; 1]
let my_equal_sets_test5 = not (equal_sets [] [13])
let my_equal_sets_test6 = not (equal_sets [9] [])
let my_equal_sets_test7 = not (equal_sets [6] [1; 3])
let my_equal_sets_test8 = not (equal_sets [9; 8; 5] [5; 8; 2])

(* Test cases for question 3 *)
let my_set_union_test0 = equal_sets (set_union [] []) []
let my_set_union_test1 = equal_sets (set_union [] [9]) [9]
let my_set_union_test2 = equal_sets (set_union [8; 9; 13] [1; 2; 4]) [1; 2; 4; 8; 9; 13]
let my_set_union_test3 = equal_sets (set_union [9; 0] [0; 9]) [0; 9]
let my_set_union_test3 = equal_sets (set_union [11; 1; 3] [1; 3]) [1; 3; 11]

(* Test cases for question 4 *)
let my_set_intersection_test0 = equal_sets(set_intersection [] []) []
let my_set_intersection_test1 = equal_sets(set_intersection [9; 8] []) []
let my_set_intersection_test2 = equal_sets(set_intersection [] [1; 6; 9; 13]) []
let my_set_intersection_test3 = equal_sets(set_intersection [7; 9; 12] [9]) [9]
let my_set_intersection_test4 = equal_sets(set_intersection [1; 5; 8] [2; 5; 8]) [5; 8]
let my_set_intersection_test5 = equal_sets(set_intersection [3; 6; 9] [3; 6; 9]) [3; 6; 9]

(* Test cases for question 5 *)
let my_set_diff_test0 = equal_sets (set_diff [] []) []
let my_set_diff_test1 = equal_sets (set_diff [4; 5; 6; 8] [6; 8]) [4; 5]
let my_set_diff_test0 = equal_sets (set_diff [8; 9; 10] [9]) [8; 10]
let my_set_diff_test1 = equal_sets (set_diff [2; 3; 5; 8] [3; 4; 5; 8]) [2]
let my_set_diff_test1 = equal_sets (set_diff [4; 6; 8] [4; 6; 8; 9]) []

(* Test cases for question 6 *)
let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x) 3 = 3
let my_computed_fixed_point_test1 = computed_fixed_point (=) (fun x -> x *. x) 9. = infinity
let my_computed_fixed_point_test2 = computed_fixed_point (=) (fun x -> x *. 9.) 3. = infinity
let my_computed_fixed_point_test3 = computed_fixed_point (=) (fun x -> x / 9) 99999999 = 0
let my_computed_fixed_point_test4 = computed_fixed_point (=) sqrt 999999. = 1.


(* Test cases for question 7 *)

type major_nonterminals = 
  | Science | Sports | Music | Languages | Engineering

let major_rules = 
  [Science, [N Sports; N Languages; N Engineering];
   Sports, [N Music; N Languages];
   Music, [T "singing"; T "violin"; T "piano"; N Languages];
   Languages, [T "english"; T "korean"; T "spanish"];
   Languages, [T "chinese"; N Science];
   Engineering, [T "computer"; T "mechanical"; T "chemical"]]


let my_filter_reach_test0 = filter_reachable (Science, major_rules) = (Science, major_rules)

let my_filter_reach_test1 = filter_reachable (Sports, major_rules) = (Sports, major_rules)

let my_filter_reach_test2 = filter_reachable (Music, major_rules) = (Music, major_rules)

let my_filter_reach_test3 = filter_reachable (Languages, major_rules) = (Languages, major_rules)

let my_filter_reach_test4 = filter_reachable (Engineering, major_rules) = (Engineering,
                                                                           [Engineering, [T "computer"; T "mechanical"; T "chemical"]])