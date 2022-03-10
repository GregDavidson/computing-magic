% -*-Mode:prolog;-*-
% eight - the Eight Puzzle
% J. Greg Davidson, 8 May 1990
% Copyright (c) 1990 Virtual Infinity Systems

% The classic 8-puzzle: There are eight tiles within a 3x3 grid. One grid
% position is empty. Any tile adjacent to the empty position can be slid into
% it. Can you rearrange all of the tiles into a desired configuration with a
% sequence of slidings?

% 1 2 3  1 2 3  2 8 3  2 8 1  1 2 6
% 4 5 6  8   4  1 6 4  1 6 4  4   8
% 7 8 9  7 6 5  7   5  7   5  7 5 3
% index  goal   b_5    b_9    b_18

%  index:   1 2 3  4 5 6  7 8 9  blank-location
the_goal(b( 1,2,3, 8,o,4, 7,6,5, 5)).
     b_5(b( 2,8,3, 1,6,4, 7,o,5, 8)).
     b_9(b( 2,8,1, o,4,3, 7,6,5, 4)).
    b_18(b( 2,1,6, 4,o,8, 7,5,3, 5)).

solve(S, P) :- the_goal(G), path(S, G, P).
t1(P) :- b_5(S), solve(S, P).  % easy
t2(P) :- b_9(S), solve(S, P).  % needs heuristic
t3(P) :- b_18(S),solve(S, P).  % needs good heuristic

% A simplifying insight is to think of the empty space moving, rather than
% thinking of the tiles moving.

move(B1, B2) :- mv(B1, B2, 1,2,3, -3).
move(B1, B2) :- mv(B1, B2, 7,8,9,  3).
move(B1, B2) :- mv(B1, B2, 1,4,7, -1).
move(B1, B2) :- mv(B1, B2, 3,6,9,  1).

% mv(Board1, Board2, 3 edge indices, Movement ) :-
%	The blank is not on indicated edge locations AND
%	Board2 is Board1 with the Blank moved Movement positions
mv(B1, B2, I1,I2,I3, D) :-
	arg(10, B1, I_blank),	% index of blank
	I_blank \= I1, I_blank \= I2, I_blank \= I3,
	I_tile is I_blank + D,	% index of tile to move
	arg(I_tile, B1, Tile),		% find out which Tile it is
	B2=b(_,_,_,_,_,_,_,_,_,I_tile),	% create the new board
	arg(I_blank, B2, Tile),	% move tile into blank
	arg(I_tile, B2, o),		% move blank into tile
	f_unify(B1, B2, 1, 9, 7), 	% copy the other 7 tiles
	write('New board: '), write(B2), nl.

heuristic(Board, Goal, H) :-
	f_unify(Board, Goal, 1, 9, M), H is 9-M.

% f_unify(functor1, functor2, Lo, Hi, Count) :-
%	Count is number of corresponding arguments which
%	unify between indices Lo and Hi, inclusive.
f_unify(_, _, Lo, Hi, 0) :- Lo > Hi, !.
f_unify(B, G, Lo, Hi, N) :- arg(Lo, B, X), arg(Lo, G, X), !,
	Next is Lo + 1, f_unify(B, G, Next, Hi, NN), N is NN + 1.
f_unify(B, G, Lo, Hi, N) :-
	Next is Lo + 1, f_unify(B, G, Next, Hi, N).
