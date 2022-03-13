% -*-Mode:prolog;-*-
% Water jug problem
% J. Greg Davidson, 8 May 1990
% adapted for SWI Prolog: 9 March 2022

% Provides:
%	move(State1, State2) :-
%   State2 is a state adjacent to State1,
%   State2 will be a Child of State1 in the spanning tree
%	heuristic( State, Goal, Cost ) :-
%		Cost estimates length of path from Start to Goal thru State

% Requires:
%	path( Start, Goal, Path ) :- there is a Path from Start to Goal

% state(# gallons in 3-gallon jug, # gallons in 5-gallon jug)
% state({0..3}, {0..5})

% Find a way to get 4 liters in the larger jug
jugs_test(P) :- path( state(0,0), state(0,4), P ).
% Note: Goals have to be fully ground because of heuristic/3

move( state(Jug1, Jug2), state(NewJug1, NewJug2) ) :-
	mv( Jug1, NewJug1, 3, Jug2, NewJug2, 5 ) ;
	mv( Jug2, NewJug2, 5, Jug1, NewJug1, 3 ).

% mv(OldJug1, NewJug1, MaxJug1, OldJug2, NewJug2, MaxJug2)
mv( _, 0, _, Jug2, Jug2, _ ). % Empty jug
mv( _, F, F, Jug2, Jug2, _ ). % Fill jug
mv( Jug1, 0, _, Jug2, NewJug2, MaxJug2 ) :-		% empty first into second
	add(Jug1, Jug2, NewJug2), Jug1 > 0, NewJug2 =< MaxJug2.
mv( Jug1, NewJug1, _, Jug2, MaxJug2, MaxJug2 ) :-	% fill second from first
	add(Jug2, AmtToFillJug2, MaxJug2), add(NewJug1, AmtToFillJug2, Jug1).

% add(integer1, integer2, integer1+integer2)
% add({0..5}, {0..5}, {0..5})

add(X,Y,XplusY) :- ad(X,Y,XplusY); ad(Y,X,XplusY). % commutativity

% We need half-adds up to total of largest jug
ad(0,X,X). % additive identity
ad(1,1,2).  ad(1,2,3).  ad(1, 3, 4).  ad(1, 4, 5).
ad(2,2,4).  ad(2,3,5).

% If we're doing heuristic search, we need a heuristic.
% We could use our add/3 if it went to MaxJug1+MaxJug2
% but let's use is/2 non-logical arithmetic for contrast
% heuristic(++state(J1:int, J2:int), ++state(J1:int, J2:int), --H:int) is det.
heuristic( state(J1, J2), state(JJ1, JJ2), H ) :-
	  DJug1 is abs(J1-JJ1), DJug2 is abs(J2-JJ2), DJugs is DJug1 + DJug2,
    H is (DJugs + DJug1) * (DJugs + DJug2).
% Yes, this heuristic is terrible - do you have a better one?
