% -*-Mode:prolog;-*-
%% * edepth		a depth-first search skeleton
%	author: J. Greg Davidson
%	date: 1 May 1985
% adapted for SWI Prolog: 9 March 2022

%% ** Required (domain specific) Predicates

%	move( ++State1, --State2 )	- generates valid adjacent states

%% ** Provided (generic search)

% path( ++Current State, ++Goal State, --Path from start to Goal )
path(S,G,P) :- path(S,G,[S],P).

% path( ++Current State, ++Goal State, --Path from start to Goal )
path(G,G,P,P).
path(S,G,P,L) :- move(S, S1),        % get a new state
               \+ member(S1, P),   % prune if we've seen it before
               path(S1, G, [S1|P], L). % complete path
