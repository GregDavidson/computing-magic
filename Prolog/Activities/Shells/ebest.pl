% -*-Mode:prolog;-*-
%% * ebest: a best-first (heuristic) search package
% Author: J. Greg Davidson
% Copyright (c) 1985  Virtual Infinity Systems
% adapted for SWI Prolog: 9 March 2022
% Free use permitted as long as acknowledgement is retained.

%% ** Provided Predicates

% path(++Start, ++Goal, --Path) :-
%   Path is a sequence of Moves from Start to Goal

%% ** Required (domain specific) Predicates

% move( ++State1, --State2 ) - generates valid adjacent states
%	heuristic( ++State, ++Goal, --H:int )	:-
%    H is guess of length of path from State to Goal

%% ** Implementation

use_module(library(heaps)).

path(Start, Goal, GoalPath) :-
    StartingPath = [Start],
    estimate(StartingPath, Goal, E),
    empty_heap(EmptyHeap),
    add_to_heap(EmptyHeap, E, StartingPath, OpenHeap),
    path(OpenHeap, [Start], Goal, GoalPath).

% estimate(++[State|]Path], ++Goal, --H:int)
%   H is estimate of total path length from Path through State to Goal
estimate([State|Path], Goal, E) :-
    heuristic(State, Goal, H),
    length(Path, L),
    E is H + L + 1.

% path(++OpenHeap, ++ClosedStates, ++Goal, --GoalPath) :-
% GoalPath is a path completing one of the paths in OpenHeap to the Goal State
% ClosedStates are states which have already been generated
path(OpenHeap, ClosedStates, Goal, GoalPath) :-
    % Invariant: Heads of Paths in OpenHeap are on Closed States List
    get_from_heap(OpenHeap, _, [State|Path], HeapTail),
    path(State, Path, HeapTail, ClosedStates, Goal, GoalPath).

% path(State, Path, OpenHeap, ClosedStates, GoalState, Path from Start to Goal)
% Invariant: Heads of Paths in OpenHeap are on Closed States List
path(Goal, Path, _, _, Goal, [Goal|Path]).
path(State, Path, OpenHeap, ClosedStates, Goal, GoalPath) :-
    ( bagof(NewNode, new_nodes(State,Path,ClosedStates,Goal,NewNode), NewNodes)
    ; NewNodes = []
    ),
    % NewNodes are just Heuristic-Path pairs
    % heads of paths in NewNodes are the children of State
    % which are not already on ClosedStates via an earlier seen path
    list_to_heap(NewNodes, NewHeap),
    merge_heaps(OpenHeap, NewHeap, NewOpenHeap),
    % the new children must not be regenerated
    append_heads( NewNodes, ClosedStates, NewClosedStates ),
    path( NewOpenHeap, NewClosedStates, Goal, GoalPath ).

new_nodes(S, P, ClosedStates, Goal, H-NewPath) :-
    move(S, S1), \+ member(S1, ClosedStates),
    NewPath = [S1,S|P],
    estimate(NewPath, Goal, E),
    length(NewPath, L),
    H is E + L.

% append_heads([X-List], List2, Append of Heads of each List with List2)
append_heads([], L, L).
append_heads([_-[H|_]|L1], L2, [H|L3]) :- append_heads(L1, L2, L3).
