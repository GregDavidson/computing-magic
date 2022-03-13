% A Family Tree

child_parent(greg, eve).
child_parent(greg, jim).
child_parent(heather, eve).
child_parent(heather, jim).
child_parent(eve, evelyn).
child_parent(eve, alfred).
child_parent('walter junior', idabelle).
child_parent('walter junior', walter).
child_parent(jim, idabelle).
child_parent(jim, walter).
child_parent(mary, idabelle).
child_parent(mary, walter).

sibling(X,Y) :- child_parent(X, P), child_parent(Y, P), X \= Y.

person_ancestor(Person, Ancestor) :- child_parent(Person, Ancestor).
person_ancestor(P, A) :- child_parent(P, Parent), person_ancestor(Parent, A).

%% Local Variables: 
%% mode: prolog
%% End:
