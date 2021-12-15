% 1. Write a predicate tower/3.
tower(N, T, C) :-
    % Check the length of Row and Column.
    chkLenRow(N, T),
    chkLenCol(N, T),
    % Define all values in List to be between 1 and N.
    setDomain(N, T),
    % Define all values in List to be different.
    maplist(fd_all_different, T),
    transposeRC(T, TransposedPuzzle),
    maplist(fd_all_different, TransposedPuzzle),
    maplist(fd_labeling, T),
    C = counts(Top, Bottom, Left, Right),
    % Check whether the values are satisfied every constraints.
    chkValues(T, Left),
    chkValues(TransposedPuzzle, Top),
    reversePuzzle(T, ReversedT),
    chkValues(ReversedT, Right),
    reversePuzzle(TransposedPuzzle, ReversedTP),
    chkValues(ReversedTP, Bottom).


% Check the length of Row.
chkLenRow(N, T) :-
    length(T, N).

% Check the length of Column.
chkLenCol(N, []).
chkLenCol(N, [HL | TL]) :-
    length(HL, N),
    chkLenCol(N, TL).

% Define all values in List to be between 1 and N.
setDomain(N, []).
setDomain(N, [HL | TL]) :-
    fd_domain(HL, 1, N),
    setDomain(N, TL).

% Transpose the Row and Column.
transposeRC([], []).
transposeRC([H | T], [TH | TT]) :-
    transposeRC(H, [H | T], [TH | TT]).
transposeRC([], _, []).
transposeRC([_ | T], Puzzle, [TH | TT]) :-
    getTransposedRC(Puzzle, TH, ReducedPuzzle),
    transposeRC(T, ReducedPuzzle, TT).
getTransposedRC([], [], []).
getTransposedRC([[HE | TE] | TL], [HE | TTE], [TE | TTL]) :-
    getTransposedRC(TL, TTE, TTL).


% Check whether the values are satisfied every constraints.
chkValues([], []).
chkValues([HL | TL], [HE | TE]) :-
    chkEachRow(HL, 0, [], TotalVisibleList),
    length(TotalVisibleList, HE),
    chkValues(TL, TE).

chkEachRow([], _, VisibleList, VisibleList).
chkEachRow([H | T], Min, VisibleElmts, TotalVisibleList) :-
    Min < H,
    append([H], VisibleElmts, AppendedVisibleElmts),
    chkEachRow(T, H, AppendedVisibleElmts, TotalVisibleList).
chkEachRow([H | T], Min, VisibleElmts, TotalVisibleList) :-
    H < Min,
    chkEachRow(T, Min, VisibleElmts, TotalVisibleList).

reversePuzzle(T, ReversedT) :-
    maplist(reverse, T, ReversedT).



% 2. Write a predicate plain_tower/3.
plain_tower(N, T, C) :-
    % Check the length of Row and Column.
    chkLenRow(N, T),
    chkLenCol(N, T),
    C = counts(Top, Bottom, Left, Right),
    chkUniqVisible(N, T, Left, Right),
    transposeRC(T, TransposedPuzzle),
    chkUniqVisible(N, TransposedPuzzle, Top, Bottom).


unique_list(List, N) :- 
    length(List, N),
    elements_between(List, 1, N), 
    all_unique(List).

elements_between(List, Min, Max) :- maplist(between(Min,Max), List).

all_unique([]).
all_unique([H | T]) :- member(H, T), !, fail.
all_unique([H | T]) :- all_unique(T).

chkUniqVisible(N, [], [], []).
chkUniqVisible(N, [HL | TL], [LHE | LTE], [RHE | RTE]) :-
    unique_list(HL, N),
    chkEachRow(HL, 0, [], TotalVisibleListL),
    length(TotalVisibleListL, LHE),
    reverse(HL, ReversedHL),
    chkEachRow(ReversedHL, 0, [], TotalVisibleListR),
    length(TotalVisibleListR, RHE),
    chkUniqVisible(N, TL, LTE, RTE).



% 3. Illustrate the performance difference on a test case of your own design,
%    measuring performance with statistics/0 or statistics/2.
%  1) The performance of tower
testPerformanceTower(CpuTime) :-
    statistics(cpu_time, [Start | _]),
    tower(5, _, counts([1, 2, 4, 2, 3], [3, 2, 1, 2, 3], [1, 2, 2, 2, 3], [3, 2, 1, 4, 3])),
    statistics(cpu_time, [End | _]),
    CpuTime is (End - Start).

%  2) The performance of plain_tower
testPerformancePlainTower(CpuTime) :-
    statistics(cpu_time, [Start | _]),
    plain_tower(5, _, counts([1, 2, 4, 2, 3], [3, 2, 1, 2, 3], [1, 2, 2, 2, 3], [3, 2, 1, 4, 3])),
    statistics(cpu_time, [End | _]),
    CpuTime is (End - Start).



% 4. Package up your test case in a predicate speedup/1.
speedup(CpuRatio) :-
    testPerformancePlainTower(PlainTowerCpuTime),
    testPerformanceTower(TowerCpuTime),
    CpuRatio is PlainTowerCpuTime / TowerCpuTime.
    


% 5. Write a Prolog predicate ambiguous(N, C, T1, T2).
ambiguous(N, C, T1, T2) :-
    tower(N, T1, C),
    tower(N, T2, C),
    T1 \= T2.