type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal


(* Question 1 *)
(* Write a function convert_grammar gram1 that returns a Homework 2-style grammar,
which is converted from the Homework 1-style grammar gram1. *)

let rec findSym subRules nonTerminal =
  match subRules with
  | [] -> []
  | head::rest ->
    let subLeftSym, subRightSyms = head in
    if subLeftSym = nonTerminal then
      subRightSyms::(findSym rest nonTerminal)
    else
      findSym rest nonTerminal
;;

let convert_grammar gram1 = 
  let startSymbol, rules = gram1 in
  
  startSymbol, findSym rules
;;


(* Question 2 *)
(* As another warmup, write a function parse_tree_leave tree that traverse the
parse tree tree left to right and yields a list of the leaves encountered *)

let parse_tree_leaves tree =

  let rec dfs treeList = match treeList with
    | Leaf nonTerSym -> [nonTerSym]
    | Node (terSym, subList) -> bfs subList
    
  and bfs treeList = match treeList with
    | [] -> []
    | head::rest ->
      (dfs head)@(bfs rest)
  in

  dfs tree
;;


(* Question 3 *)
(* Write a function make_matcher gram that returns a matcher for the grammar gram. *)

let make_matcher gram = 
  
  let startSym, producFunc = gram in
  
  fun accept frag ->
  let alternativeList = producFunc startSym in

  let rec searchAlternativeList alternativeList accept frag =
    match alternativeList with
    | [] -> None
    | headAlternativeList::restAlternativeList ->
      let matchableList = chkMatchable headAlternativeList accept frag in
      match matchableList with
      | None -> searchAlternativeList restAlternativeList accept frag
      | Some x -> Some x
  
  and chkMatchable headAlternativeList accept frag =
    match (headAlternativeList, frag) with
    | ([], _) -> accept frag
    | (_, []) -> None
    | (headRightHandSide::restRightHandSide, headFrag::restFrag) ->
      match headRightHandSide with
      | N nonTerSym ->
        searchAlternativeList (producFunc nonTerSym) (chkMatchable restRightHandSide accept) frag
      | T terSym ->
        if terSym = headFrag
        then chkMatchable restRightHandSide accept restFrag
        else None
  in

  searchAlternativeList alternativeList accept frag;;
;;


(* Question 4 *)
(* Write a function make_parser gram that returns a parser for the grammar gram. *)

let accept_empty_suffix = function
  | _::_, _ -> None
  | _, x -> Some x
;;

let findParseTree gram frag =
  
  let startSym, producFunc = gram in
  let alternativeList = producFunc startSym in

  let rec searchAlternativeList nonTerSym alternativeList accept (frag, searchedTrees) =
    match alternativeList with
    | [] -> None
    | headAlternativeList::restAlternativeList ->
      let matchableList = chkMatchable headAlternativeList accept (frag, searchedTrees @ [nonTerSym, headAlternativeList]) in
      match matchableList with
      | None -> searchAlternativeList nonTerSym restAlternativeList accept (frag, searchedTrees)
      | Some x -> Some x
  
  and chkMatchable headAlternativeList accept (frag, searchedTrees) =
    match (headAlternativeList, frag) with
    | ([], _) -> accept (frag, searchedTrees)
    | (_, []) -> None
    | (headRightHandSide::restRightHandSide, headFrag::restFrag) ->
      match headRightHandSide with
      | N nonTerSym ->
        searchAlternativeList nonTerSym (producFunc nonTerSym) (chkMatchable restRightHandSide accept) (frag, searchedTrees)
      | T terSym ->
        if terSym = headFrag
        then chkMatchable restRightHandSide accept (restFrag, searchedTrees)
        else None
  in

  searchAlternativeList startSym alternativeList accept_empty_suffix (frag, [])
;;

let failwith msg = raise (Failure msg);;

let getParseTree searchedTrees =

  let rec exploreSearchedTrees searchedTrees =
    match searchedTrees with
    | [] -> assert false
    | (nonTerSym, rightHandSide)::searchedTreesTail ->
      let unExploredSearchedTrees, parseTree = buildParseTree searchedTreesTail rightHandSide in
      unExploredSearchedTrees, Node (nonTerSym, parseTree)

  and buildParseTree searchedTrees rightHandSide =
    match rightHandSide with
    | [] -> searchedTrees, []
    | (N nonTerSym)::rightHandSideTail ->
      let unExploredSearchedHead, parseTreeHead = exploreSearchedTrees searchedTrees in
      let unExploredSearchedTail, parseTreeTail = buildParseTree unExploredSearchedHead rightHandSideTail in
      unExploredSearchedTail, parseTreeHead::parseTreeTail
    | (T terSym)::rightHandSideTail ->
      let unExploredSearchedTrees, parseTree = buildParseTree searchedTrees rightHandSideTail in
      unExploredSearchedTrees, (Leaf terSym)::parseTree
  in

  let (_, parseTree) = exploreSearchedTrees searchedTrees in
  parseTree
;;

let make_parser gram =
  
  fun frag ->
  let parseableSearchedTrees = (findParseTree gram frag) in

  match parseableSearchedTrees with
  | None -> None
  | Some searchedTrees -> Some (getParseTree searchedTrees)
;;
