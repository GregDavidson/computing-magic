% -*-Mode:prolog;-*-

% We don't need to represent the location of the boat,
% it's always the same as the location of the farmer.
% The location of the Farmer, Wolf, Goat and Cabbage
% is either east_side or west_side -- we could just
% abbreviate those as e and w for compactness.

% We could use the term fwgc/4 as our state like this:
% Start state: fwgc(e,e,e,e)
% Goal state: fwgc(w,w,w,w)
% etc.

% Not all moves are acceptable, so we need to filter:
move(S1, S2) :- action(S1, S2), \+ unsafe(S2).

% action(Predecessor State, Successor State)
% Here's one of the 4 actions:
action(fwgc(X,X,G,C), fwgc(Y,Y,G,C)) :- opposite_sides(X,Y). % farmer takes wolf

% opposite_sides(One Side, Other Side)
% This is all Prolog needs to know about e and w!
opposite_sides(e,w).
opposite_sides(w,e).

% Here's one of the two unsafe rules:
unsafe(fwgc(F,X,X,_C)) :- opposite_sides(F,X).
