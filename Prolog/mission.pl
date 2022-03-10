% -*-Mode:prolog;-*-
%% * Missionaries & Cannibals
% Author: J. Greg Davidson
% Date: 5 February, 1987
% adapted for SWI Prolog: 9 March 2022

% Three missionaries and three cannibals need to cross a river.
% They have a boat which will hold at most one.
% The missionaries must avoid tempting the cannibals by
% ever being outnumbered by the cannibals.

% state(Number of cannibals, number of missionaries, location of boat).

cross(P) :- path(state(3,3,west), state(0,0,east), P).

heuristic(state(C,M,_), state(CC,MM,_), H) :-
    H is abs(CC-C) + abs(MM-M).

move(S1, S2) :- action(S1, S2), good(S2).

action(state(C1,M1,B1),state(C2,M2,B2)) :-
	opp(B1, B2), d(DM,DC),
	delta(DM,B1,M1,M2), delta(DC,B1,C1,C2).

opp(east, west).	opp(west, east).

d(0,1).  d(1,0).  d(0,2).  d(2,0).  d(1,1).

delta(DM,B1,D1,D2) :- del(DM,B1,D1,D2), D2 >= 0, D2 =< 3.

del(D, east, D1, D2) :- D2 is D1 + D.
del(D, west, D1, D2) :- D2 is D1 - D.

good(state(X,X,_)).  good(state(0,_,_)).  good(state(3,_,_)).
