% -*-Mode:prolog;-*-
%% * Missionaries & Cannibals

% Three missionaries and three cannibals need to cross a river.
% They have a boat which will hold at most one.
% The missionaries must avoid tempting the cannibals by
% ever being outnumbered by the cannibals.

% state(Number of cannibals, number of missionaries, location of boat).

% You can use any of the generic search algorithm in
% Prolog/Activities/Shells
% the simplest is edepth.pl

cross(P) :- path(state(3,3,west), state(0,0,east), P).

move(S1, S2) :- action(S1, S2), good(S2).

% Here's one of 3 rules for a 'good' state:
good(state(X,X,_)).  

% What are the legal actions?
