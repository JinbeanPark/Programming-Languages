let accept_all string = Some string;;

let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x
;;

type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num
;;  

let awkish_grammar =
  (Expr,
    function
      | Expr ->
          [[N Term; N Binop; N Expr];
          [N Term]]
      | Term ->
        [[N Num];
        [N Lvalue];
        [N Incrop; N Lvalue];
        [N Lvalue; N Incrop];
        [T"("; N Expr; T")"]]
      | Lvalue ->
        [[T"$"; N Expr]]
      | Incrop ->
        [[T"++"];
        [T"--"]]
      | Binop ->
        [[T"+"];
        [T"-"]]
      | Num ->
        [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
        [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])
;;


(* Question 5 *)
(* Write one good, nontrivial test case for your make-matcher function *)

type major_nonterminals = 
  | Science | Sports | Music | Languages | Engineering

let major_grammar = 
  (Science,
    function
      | Science ->
        [[N Languages];
        [N Sports];
        [T "chemical"];
        [T "biology"]]
      | Languages ->
        [[T "english"; T "spanish"; T "korean"];
        [T "chinese"; T "korean"; N Music]]
      | Sports ->
        [[T "football"; T "basketball"; T "soccer"];
        [T "swimming"; N Languages]]
      | Music ->
        [[T "violin"; T "piano"; T "singing"];
        [N Engineering]]
      | Engineering ->
        [[T "computer"; T "mechanical"; T "chemical"]]
  )

let make_matcher_test0 =
  ((make_matcher major_grammar accept_all ["chinese"; "korean"; "violin"; "piano"; "singing"]) = Some [])
;;

let make_matcher_test1 =
  ((make_matcher major_grammar accept_all ["swimming"; "english"; "spanish"; "korean"; "soccer"]) = Some ["soccer"])
;;

let make_matcher_test2 =
  ((make_matcher major_grammar accept_all ["computer"]) = None)
;;

let make_matcher_test3 =
  ((make_matcher major_grammar accept_all ["chinese"; "korean"; "computer"; "mechanical"; "chemical"; "football"; "soccer"]) = Some ["football"; "soccer"])
;;


(* Question 6 *)
(* Write a good test case make_parser_test for your make_parser function using your same test grammar *)

let make_parser_test0 =
  match make_parser awkish_grammar ["$"; "7"; "--"; "+"; "3"] with
  | Some tree -> parse_tree_leaves tree = ["$"; "7"; "--"; "+"; "3"]
  | _ -> false
;;

let make_parser_test1 =
  match make_parser awkish_grammar ["3"] with
  | Some tree -> parse_tree_leaves tree = ["3"]
  | _ -> false
;;

let test_frag = ["("; "$"; "5"; ")"; "+"; "$"; "--"; "$"; "++"; "$"; "1"; "-";
"("; "$"; "--"; "$"; "7"; "-"; "("; "9"; ")"; "-"; "2"; ")";
"-"; "("; "$"; "$"; "$"; "$"; "$"; "++"; "$"; "$"; "5"; "++";
"--"; "++"; ")"; "-"; "++"; "$"; "$"; "("; "$"; "2"; "--"; ")";
"++"; "+"; "7"]
;;

let make_parser_test2 =
  match make_parser awkish_grammar test_frag with
  | None -> false
  | Some tree -> parse_tree_leaves tree = test_frag
;;