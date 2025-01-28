:- use_module(library(clpfd)).

mm([S,E,N,D,M,O,R,Y], Type) :-
    domain([S,E,N,D,M,O,R,Y], 0, 9),      % step 1
    S#>0, M#>0,
    all_different([S,E,N,D,M,O,R,Y]),     % step 2
    sum(S,E,N,D,M,O,R,Y),
    labeling(Type, [S,E,N,D,M,O,R,Y]).    % step 3

sum(S, E, N, D, M, O, R, Y) :-
                  1000*S + 100*E + 10*N + D
    +            1000*M + 100*O + 10*R + E
    #= 10000*M + 1000*O + 100*N + 10*E + Y.

% mm([S,E,N,D,M,O,R,Y], []).
