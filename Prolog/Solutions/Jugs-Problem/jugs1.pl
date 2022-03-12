% -*-Mode:prolog;-*-
% Water jug problem
% J. Greg Davidson, 8 May 1990

% Provides:
%	move( ++State1, --State2 )	- generates valid adjacent states

% Requires:
%	path( Start, Goal ) :- there is a path from Start to Goal

% state(# gallons in 3-gallon jug, # gallons in 5-gallon jug)
% state({0..3}, {0..5})

% Find a way to get 4 liters in the larger jug
jugs_test(P) :- path( state(0,0), state(0,4), P ).
% Note: Goals have to be fully ground because of heuristic/3

move( state(_, J), state(3, J) ). % fill little jug from tap
move( state(_, J), state(0, J) ). % empty little jug

move( state(J, _), state(J, 5) ). % fill big jug from tap
move( state(J, _), state(J, 0) ). % empty big jug

move( state(J1, J2), state(0, JJ) ) :- % empty little into big
	add(J1, J2, JJ), JJ =< 5.
move( state(J1, J2), state(JJ, 5) ) :- % fill big with little
	add(J2, J, 5), add(JJ, J, J1).

move( state(J1, J2), state(JJ, 0) ) :- % empty big into little
	add(J1, J2, JJ), JJ =< 3.
move( state(J1, J2), state(3, JJ) ) :- % fill little with big
	add(J1, J, 3), add(JJ, J, J2).

% add(integer1, integer2, integer1+integer2)
% add({0..5}, {0..5}, {0..5})

add(X,Y,XplusY) :- ad(X,Y,XplusY); ad(Y,X,XplusY). % commutativity

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
