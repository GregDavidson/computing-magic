% -*-Mode:prolog;-*-
% most Prologs provide a list append predicate, this is my version
% a 3-argument predicate corresponds to 2^3 = 8 procedures!

% my_append(first-part-of-the-list, rest-of-the-list, the-whole-list)
my_append([], L, L).
my_append([H|L1], L2, [H|L3]) :- my_append(L1, L2, L3).
