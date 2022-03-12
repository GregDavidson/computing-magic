% -*-Mode:prolog;-*-
%% * ebreadth - a breadth-first search skeleton
%	author:	J. Greg Davidson
%	date:	1 May 1985
% adapted for SWI Prolog: 9 March 2022

%% ** Provided Predicates

% path(++Start, ++Goal, --Path) :-
%   Path is a sequence of Moves from Start to Goal

%% ** Required (domain specific) Predicates

% move( ++State1, --State2 ) - generates valid adjacent states

%% ** Implementation

path(Start, Goal, GoalPath) :-
  path([[Start]], [Start], Goal, GoalPath).

% path(Open Paths, Closed (already generated) States, Goal State, Path from Start to Goal)
% Invariant: Heads of Open Paths are on Closed States List
% Invariant: Shorter paths come before longer paths on Open Paths list
path([[Goal|Path]|_], _, Goal, [Goal|Path]).
path([FirstOpenPath|MoreOpenPaths], ClosedStates, Goal, GoalPath) :-
    ( bagof( NewPath, new_paths(FirstOpenPath,ClosedStates,NewPath), NewPaths )
    ; NewPaths=[] ),
    % heads of NewPaths are children of the Head of FirstOpenPath
    % adding NewPaths to the end of the OpenPaths means that they will only be explored
    % after their parent's siblings, giving breadth-first search
    append( MoreOpenPaths, NewPaths, NewOpenPaths ),
    % the new children must not be regenerated
    append_heads( NewPaths, ClosedStates, NewClosedStates ),
    path( NewOpenPaths, NewClosedStates, Goal, GoalPath ).

new_paths([S|P],ClosedStates,[S1,S|P]) :- move(S, S1), \+ member(S1, ClosedStates).

% append_heads(ListOfLists, List2, Append of Heads of ListOfLists with List2)
append_heads([], L, L).
append_heads([[H|_] | L1], L2, [H|L3]) :- append_heads(L1, L2, L3).
