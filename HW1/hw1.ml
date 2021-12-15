(* Problem 1
Write a function subset a b that returns ture iff a ⊆ b, i.e.,
if the set represented by the list a is a subset of the set represented
by the list b. Every set is a subset of itself. This function should be
generic to lists of any type: that is, the type of subset should be a
generalization of 'a list -> 'a list -> bool. 
*)

let rec subset a b = match a with
  | [] -> true
  | head::rest ->
    if List.exists (fun x -> head = x) b then subset rest b
    else false
;;

(* Solved it through other way.
let isSubset a b =
  List.for_all (fun x -> List.exists (fun y -> x = y) b) a
*)

(* Problem 2
Write a function equal_sets a b that returns true iff the represented sets
are equal *)

let rec equal_sets a b =
  subset a b && subset b a
;;

(* Problem 3
Write a function set_union a b that returns a list representing a∪b. 
*)

let rec set_union a b = match a with
  | [] -> b
  | head::rest ->
    head::set_union rest b
;;

(* Problem 4
Write a function set_intersection a b that returns a list representing a∩b.
*)

let set_intersection a b =
  List.filter (fun x -> List.mem x b) a
;;

(* Problem 5
Write a function set_diff a b that returns a list representing a - b,
that is, the set of all members of a that are not also members of b.
*)

let set_diff a b =
  List.filter (fun x -> not (List.mem x b)) a
;;

(* Problem 6
Write a function computed_fixed_point eq f x that returns the computed fixed
point for f with respect to x, assuming that eq is the equality predicate
for f's domain. A common case is that eq will be (=). that is, the built
in equality predicate of OCaml; but any predicate can be used. If there
is no computed fixed point, your implementation can do whatever it wants:
for example, it can print a diagnostic, or go into a loop, or send nasty
email messages to the user's relatives
*)

let rec computed_fixed_point eq f x =
  if eq (f x) x then x
  else computed_fixed_point eq f (f x)
;;

(* Problem 7
Write a function filter_reachable g that returns a copy of the grammar g 
with all unreachable rules removed. This function should preserve 
the order of rules: that is, all rules that are returned should be
in the same order as the rules in g.
*)

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

(* Compare only the first element in tuple. *)
let equalSecElmt leftSyms rightSyms =
  let leftFirElmt, _ = leftSyms in
  let rightFirElmt, _ = rightSyms in
  equal_sets leftFirElmt rightFirElmt
;;

(* If rightSyms is Nonterminal, it appends the nonterminal symbols into list *)
(* If rightSyms is Terminal, it skips appending the terminal symbols into list *)
let rec findNonterminal rightSyms = match rightSyms with
  | [] -> []
  | N head::rest -> head::findNonterminal rest
  | T head::rest -> findNonterminal rest
;;

let filter_reachable g =
  (* It separates the symbol and awksub_rules *)  
  let startSymbol, rules = g in  
  
  (* It explores awksub_rules and appends Nonterminal symbols
  if the searched symbol is member of list having reachable symbols.
  Otherwise, it skips appending Nonterminal symbols if the searched symbol
  is not member of list having reachable symbols *)
  let rec findReachSym reachSymWithSubrule =
    let reachSyms, subRules = reachSymWithSubrule in
    match subRules with
    | [] -> (reachSyms, rules)
    | head::rest ->
      let subLeftSym, subRightSyms = head in
      if List.mem subLeftSym reachSyms then 
        let nonTerminalSyms = findNonterminal subRightSyms in
        findReachSym ((set_union reachSyms nonTerminalSyms), rest)
      else
        findReachSym (reachSyms, rest)
    in
    
  (* It explores and finds every reachable nonterminal symbols by using 
  the computed_fixed_point function *)
  let filterReachSyms, _ =
    computed_fixed_point equalSecElmt findReachSym ([startSymbol], rules)
  in
  
  (* The getCombinedSyms has every awksub_rules having the symbol 
  belonging to the filterReachSyms *)
  let getCombinedSyms =
    List.filter (fun (symbol, _) -> List.mem symbol filterReachSyms) rules
  in
  
  (* Returns the tuple (startSymbol, getCombinedSyms) *)
  startSymbol, getCombinedSyms
;;