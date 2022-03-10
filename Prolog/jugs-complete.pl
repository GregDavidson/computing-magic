% -*-Mode:prolog;-*-
%% * Water jug problem

% Given little, a 3-liter jug and big, a 5-liter jug
% The state of the world is little_big(liters in little, liters in big)
% e.g. little_big(0, 0), little_big(3, 4), etc.
% Amounts are always an even number of liters: little_big({0..3}, {0..5})
% variable names begin with a CapitalLetter
% The "don't care" variable is _

%% ** Asking for solutions

% jugs1, jugs2: test problems
jugs1 :- jugs_goal_path( little_big(_,4), _ ).
jugs2 :- jugs_goal_path( little_big(4,_), _ ).

% jugs_goal_path(Goal, Path from empty jugs to Goal)
jugs_goal_path(Goal,Path) :- Start = little_big(0,0), path(Start,Goal,[Start],Path).

%% ** Domain Knowledge

% action( State before action, State after action)
% Empty either jug.
action( little_big(_, Big), little_big(0, Big) ).
action( little_big(Little, _), little_big(Little, 0) ).
% Fill either jug full from the tap.
action( little_big(_, Big), little_big(3, Big) ).
action( little_big(Little, _), little_big(Little, 5) ).
% Empty one jug into the other, if there's room
action( little_big(Little, Big), little_big(0, NewBig) ) :-
    add(Little, Big, NewBig), NewBig =< 5.
action( little_big(Little, Big), little_big(NewLittle, 0) ) :-
    add(Little, Big, NewLittle), NewLittle =< 3.
% Fill one jug from the other, if it has enough to do it
action( little_big(Little, Big), little_big(3, NewBig) ) :-
    add(Little, AmtPoured, 3), add(NewBig, AmtPoured, Big).
action( little_big(Little, Big), little_big(NewLittle, 5) ) :-
    add(Big, AmtPoured, 5), add(NewLittle, AmtPoured, Little).

% change(State, Different state after 1 action)
% Not all actions change anything, e.g. emptying an already empty jug
change(OldState, NewState) :- action(OldState, NewState), OldState \= NewState.

%% ** Let's Invent Arithmetic!

% ad(integer1, integer2, integer1+integer2)
% where ad({0..5}, {0..5}, {0..5})
ad(0, X, X). % additive identity
ad(1, 1, 2).  ad(1, 2, 3).
ad(1, 3, 4).  ad(1, 4, 5).
ad(2, 2, 4).  ad(2, 3, 5).

% add(integer1, integer2, integer1+integer2)
% where add({0..5}, {0..5}, {0..5})
% Given that addition is commutative:
add(X, Y, XplusY) :- ad(X, Y, XplusY).
add(X, Y, XplusY) :- ad(Y, X, XplusY).

%% ** Generic Path Finding Predicates

% member( Item, List of items)
% is true iff Item is in the list
% Uncomment if your Prolog doesn't provide member
% member(X, [X|_]).   % X is the head of the list
% member(X, [Y|Tail]) :- X \= Y,  member(X, Tail).

% path( Path from beginning, Goal we want )
% It is possible to complete the Path to the Goal
path([Goal | PathTail], Goal) :-
    write('Solution with (reversed) path = '), write([Goal|PathTail]), nl.
path(Path, Goal) :-
    Path = [State|_],
    State \= Goal,
    change(State, NewState),
    \+ member(NewState, Path),  % not already on path
    path( [NewState|Path], Goal ).
