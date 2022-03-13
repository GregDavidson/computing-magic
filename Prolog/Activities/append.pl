% -*-Mode:prolog;-*-
% most Prologs provide a list append predicate, this is my version
% a 3-argument predicate corresponds to 2^3 = 8 procedures!

% my_append(first-part-of-the-list, rest-of-the-list, the-whole-list)
my_append([], L, L).
my_append([H|L1], L2, [H|L3]) :- my_append(L1, L2, L3).

%% Let's append two lists:
% ?- append([black, white], [red, green, blue], L).

%% Let's find the right suffix:
% ?- append([black, white], L, [black, white, red, green, blue]).

%% Let's find the right prefix:
% ?- append(L, [red, green, blue], [black, white, red, green, blue]).

%% Let's split a list in all possible ways:
%% - between terms, ";" means "or"
%% - the ";" was typed interactively to ask for another result
% ?- append(L1, L2, [red, green, blue]).
%% You'll want to enter ; to get the next solution
% L2 = [] ;

%% Do these lists satisfy the append relationship?
% ?- append([black, white], [red, green, blue], [black, white, red, green, blue]).

%% Do these lists satisfy the append relationship?
% ?- append([black, white], [red, green, blue], [white green]).

%% We can combine terms with "," meaning "and"
% ?- append(L1, L2, [red, green, blue]), length(L2, 1).

%% and order doesn't matter:
%   my_append([First|Rest], L2, [First|L3]) :- append(Rest, L2, L3).
%   my_append([], L, L).
%   ?- length(L1, 2), my_append(L1, L2, [a, b, c, d, e]).
