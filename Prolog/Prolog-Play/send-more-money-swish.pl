% https://swish.swi-prolog.org/p/ltc_send_more_money.swinb

main(S,E,N,D,M,O,R,Y) :-
    di(S), S \== 0,
    di(E), S \== E,
    di(N), N \== S, N \== E, 
    di(D), D \== N, D \== S, D \== E,
    to_int([S,E,N,D], VL),
    di(M), M \== 0, M \== D, M \== N, M \== E, M \== S,
    di(O), O \== M, O \== D, O \== N, O \== E, O \== S,
    di(R), R \== O, R \== M, R \== D, M \== N, M \== E, M \== S,
    to_int([M,O,R,E], VR), 
    di(Y), Y \== R, Y \== O, Y \== M, Y \== D, Y \== N, Y \== E, Y \== S,
    to_int([M,O,N,E,Y], VS),
    VS is VL+VR.

to_int([A,B,C,D], V)   :- V is 1000*A+100*B+10*C+D.
to_int([A,B,C,D,E], V) :- V is 10000*A+1000*B+100*C+D*10+E.

di(0). di(1). di(2). di(3). di(4). di(5). di(6). di(7). di(8). di(9).
