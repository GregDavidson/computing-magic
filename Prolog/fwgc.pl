% -*-Mode:prolog;-*-

fwgc(P) :- path( fwgc(e,e,e,e), fwgc(w,w,w,w), P).
fwgc(S,G,P) :- path(S, G, [S], P).

% in our domain states are: fwgc(Farmer, Wolf, Goat, Cabbage)
% where the variables are either e or w for the East or West side of the River.
% example state: fwgc(e,e,e,e)

move(S1, S2) :- action(S1, S2), \+ unsafe(S2).

% action(Predecessor State, Successor State)
action(fwgc(X,X,G,C), fwgc(Y,Y,G,C)) :- opp(X,Y). % farmer takes wolf
action(fwgc(X,W,X,C), fwgc(Y,W,Y,C)) :- opp(X,Y). % farmer takes goat
action(fwgc(X,W,G,X), fwgc(Y,W,G,Y)) :- opp(X,Y). % farmer takes cabbage
action(fwgc(X,W,G,C), fwgc(Y,W,G,C)) :- opp(X,Y). % farmer goes alone

% opp(One Side, Other Side)
opp(e,w).
opp(w,e).

unsafe(fwgc(F,X,X,_C)) :- opp(F,X).
unsafe(fwgc(F,_W,X,X)) :- opp(F,X).
% example: unsafe(fwgc(e,w,w,e))?

heuristic(fwgc(F1,W1,G1,C1), fwgc(F2,W2,G2,C2), H) :-
    delta(F1,F2,DF), delta(W1,W2,DW), delta(G1,G2,DG), delta(C1,C2,DC),
    H is DF+DW+DG+DC.           % anyone got a better heuristic?
delta(X,X,0).
delta(X,Y,1) :- X \= Y.

%% ** Extra Logical Print Procedures

show_state(fwgc(F,W,G,C)) :- write(F), write(W), write(G), write(C), write(' ').
