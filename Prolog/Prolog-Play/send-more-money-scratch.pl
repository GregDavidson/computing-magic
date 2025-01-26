% SEND + MORE = MONEY -- from scratch

% This version doesn't work yet
% see send-more-money-is.pl instead!

% This version is attempting to avoid
% using built-in arithmetic or library predicates.

% Constraint:
%    SEND
% +  MORE
% = MONEY
% where each letter represents a single digit.

letters([_S,_E,_N,_D,_M,_O,_R,_Y]).
digits([0,1,2,3,4,5,6,7,8,9]).

% Problem: Associate each letter in SEND + MORE = MONEY
% with a unique single-digit integer which satisfies
% the constraint.

% Constraint explicitly showing carries:
%   C4 C3 C2 C1
%       S  E  N  D
% +     M  O  R  E
% =  M  O  N  E  Y
% where C4 = M

% check(List) :- List satisfies our constraint.
check([S,E,N,D,M,O,R,Y]) :-
    S =\= 0,
    M =\= 0,
    add(D,E,Y,C1),
    add(N,R,S2,C2a), add(C1,S2,E,C2b), add(C2a, C2b, C2, 0),
    add(E,O,S3,C3a), add(C2,S3,N,C3b), add(C3a, C3b, C3, 0),
    add(S,M,S4,C4a), add(C3,S4,O,C4b), add(C4a, C4b, M, 0).

test_check :- check([9,5,6,7,1,0,8,2]).

test_test :- 10652 is 9567 + 1085.

solve(L) :- letters(L), digits(D),
            copy_term(L, LL), combi(D,LL),
            perm(LL,P), check(P).

test_solve :- solve([9,5,6,7,1,0,8,2]).

%% ** Efficiency Concerns

% We could explore the state space quicker by
% immediately rejecting permutations with
% S or M assigned 0
% S + M <= 10

% And do we need our fancy add predicate?
% Not for check, since everything is instantiated!

%% ** Library Predicates

% combi(List, Combi)
% where List and Combi are lists
% length(List) >= length(Combi)
% unifies Combi with a subset of List.
% On backtracking, all combinations
% are explored.
combi(_,[]).
combi([X|T],[X|Comb]):-combi(T,Comb).
combi([_|T],[X|Comb]):-combi(T,[X|Comb]).

% perm(List, Perm) :- Perm is a permutation of List.
% On backtraacking, all permutations are explored.
perm(List,[H|Perm]):-delete(H,List,Rest),perm(Rest,Perm).
perm([],[]).

% delete(X, List1, List2) :- List2 is List1 without X.
delete(X,[X|T],T).
delete(X,[H|T],[H|NT]):-delete(X,T,NT).

% Addition of single digit integers is simple enough
% to define directly.  Alternatively, we could bring
% in a reversible addition facility for arbitrary integers.

% ad(X, Y, Sum, Carry) :- X + Y = Sum + 10 * Carry.
% ad is complete for X,Y in 0..9 when X=0 or X<Y.
ad(0, Y, Y, 0).

ad(1, 2, 3, 0).
ad(1, 3, 4, 0).
ad(1, 4, 5, 0).
ad(1, 5, 6, 0).
ad(1, 7, 8, 0).
ad(1, 8, 9, 0).
ad(1, 9, 0, 1).

ad(2, 3, 5, 0).
ad(2, 4, 6, 0).
ad(2, 5, 7, 0).
ad(2, 6, 8, 0).
ad(2, 7, 9, 0).
ad(2, 8, 0, 1).
ad(2, 9, 1, 1).

ad(3, 4, 7, 0).
ad(3, 5, 8, 0).
ad(3, 6, 9, 0).
ad(3, 7, 0, 1).
ad(3, 8, 1, 1).
ad(3, 9, 2, 1).

ad(4, 5, 9, 0).
ad(4, 6, 0, 1).
ad(4, 7, 1, 1).
ad(4, 8, 2, 1).
ad(4, 9, 3, 1).

ad(5, 6, 1, 1).
ad(5, 7, 2, 1).
ad(5, 8, 3, 1).
ad(5, 9, 4, 1).

ad(6, 7, 3, 1).
ad(6, 8, 4, 1).
ad(6, 9, 5, 1).

ad(7, 8, 5, 1).
ad(7, 9, 6, 1).

ad(8, 9, 7, 1).

add(1, 1, 2, 0).
add(2, 2, 4, 0).
add(3, 3, 6, 0).
add(4, 4, 8, 0).
add(5, 5, 0, 1).
add(6, 6, 2, 1).
add(7, 7, 4, 1).
add(8, 8, 6, 1).
add(9, 9, 8, 1).
add(X, Y, Sum, Carry) :- ad(X, Y, Sum, Carry).
add(X, Y, Sum, Carry) :- ad(Y, X, Sum, Carry).

% ** Run All Tests

failed(X) :- write('test '), write(X), write(' failed!'), nl.
tests :- (test_test ; failed('test')),
         (test_check ; failed(check)),
         (test_solve; failed('solve')).
