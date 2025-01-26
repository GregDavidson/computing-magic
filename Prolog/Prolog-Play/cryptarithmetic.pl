% In process!
% Working on generalizing send-more-money-is.pl
% to be able to take any similar world triple
% and try to solve it.

% We've gotten as far as comment HERE!! below:

% This solution requires the predicates
% is/2 - non-logical arithmetic
% perm/2 - generate permutations

% We want to solve problems similar to
%    SEND
%  + MORE
% = MONEY
% where each letter represents a single digit.

problem(sum('SEND', 'MORE', 'MONEY')).
problem_letters(Ls) :-
    problem(sum(W1,W2,W3)),
    atom_chars(W1, Cs1),
    atom_chars(W2, Cs2),
    atom_chars(W3, Cs3),
    append(Cs1, Cs2, Cs12),
    append(Cs12, Cs3, Cs),
    sort(Cs, Ls).

% HERE!!

letters(['S','E','N','D','M','O','R','Y']).
letter_vars([_S,_E,_N,_D,_M,_O,_R,_Y]).
digits([0,1,2,3,4,5,6,7,8,9]).

% Problem: Associate each letter in SEND + MORE = MONEY
% with a unique single-digit integer which satisfies
% the constraint.

% Constraint explicitly showing carries:
%   C4 C3 C2 C1
%       S  E  N  D
%  +    M  O  R  E
% =  M  O  N  E  Y
% where C4 = M

% check(List) :- List satisfies our constraint.
% mode(in), i.e. List must be fully ground.
check([S,E,N,D,M,O,R,Y]) :-
    S =\= 0,
    M =\= 0,
    add(D,E,Y,C1),
    add(N,R,S2,C2a), add(C1,S2,E,C2b), add(C2a, C2b, C2, 0),
    add(E,O,S3,C3a), add(C2,S3,N,C3b), add(C3a, C3b, C3, 0),
    add(S,M,S4,C4a), add(C3,S4,O,C4b), add(C4a, C4b, M, 0).

the_solution([9,5,6,7,1,0,8,2]).

test_check :- the_solution(S), check(S).

solve(P) :-
    digits(Ds), letter_vars(Ls),
    copy_term(Ls, C), combi(Ds, C),
    perm(C,P), check(P).

test_solve :- solve(S), !, the_solution(S).

%% ** Efficiency Concerns

% We could explore the state space quicker by
% immediately rejecting permutations with
% S + M <= 10

%% ** Supporting Predicates

% combi(List, Combi)
% where List and Combi are lists
% length(List) >= length(Combi)
% unifies Combi with a subset of List.
% On backtracking, all combinations
% are explored.
combi(_,[]).
combi([X|T],[X|Comb]) :- combi(T,Comb).
combi([_|T],[X|Comb]) :- combi(T,[X|Comb]).

% test_combi
% 10 choose 8 = 10! / (8! * (10-8)!) = 45
% combi shouldn't create duplicates so
% we should be able to use bagof rather than setof
test_combi :-
    digits(Ds),
    %    letter_vars(Ls),
    %    bagof(C, copy_term(Ls, C) combi(D, C), Cs),
    bagof(C, (letter_vars(C), combi(Ds, C)), Cs),
    length(Cs, 45),
    sort(Cs, Ss),
    length(Ss, 45).

% X and Y should be instantiated as single-digit integers
add(X, Y, Sum, Carry) :-
    Total is X + Y,
    Sum is Total mod 10,
    Carry is Total div 10.

% Alternatively in SWI-Prolog
swi_add(X, Y, Sum, Carry) :-
    between(0, 9, X),
    between(0, 9, Y),
    plus(X, Y, Total),
    divmod(Total, 10, Carry, Sum).

% ** Run All Tests

failed(X) :- write('test '), write(X), write(' failed!'), nl.
tests :- (test_check ; failed(check)),
         (test_solve; failed('solve')).

% ** Fancy Check Solution

letter_solution_digit(L, Ds, D) :-
    letters(Ls), letter_solution_digit(Ls, L, Ds, D).
letter_solution_digit([L|_],L, [D|_], D).
letter_solution_digit([X|Ls],L,[_|Ds], D) :-
    X \= L,
    letter_solution_digit(Ls, L, Ds, D).

word_solution_value(W,Ds,V) :-
    atom_chars(W, Cs),
    chars_solution_value(Cs, Ds, V).
chars_solution_value(Cs, S, V) :-
    chars_solution_value(Cs, S, 0, V).
chars_solution_value([], _, V, V).
chars_solution_value([C|Cs], Ds, Accum, Value) :-
    letter_solution_digit(C, Ds, V),
    A is Accum * 10 + V,
    chars_solution_value(Cs, Ds, A, Value).

check_solution(S) :-
    word_solution_value('SEND', S, Send),
    word_solution_value('MORE', S, More),
    word_solution_value('MONEY', S, Money),
    Money is Send + More,
    write('SEND'), write(' + '), write('MORE'),
    write(' = '), write('MONEY'), nl,
write(Send), write(' + '), write(More),
    write(' = '), write(Money), nl.

fancy_test :- solve(S), check_solution(S).
