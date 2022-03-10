% English - A small but context sensitive subset of English
% Author: J. Greg Davidson

utt(X) :- sentence(X, []).

sentence(X, Z) :- np(X, Y, Number), vp(Y, Z, Number).

np([Det, Noun | Z], Z, Number) :- det(Det, Number), noun(Noun, Number).

vp([Verb | Rest], Z, Number) :- verb(Verb, Number), np(Rest, Z, _).

noun(Noun, sing) :- n2(Noun,_).
noun(Noun, plural) :- n2(_,Noun).

n2(man, men).
n2(dog, dogs).
n2(woman, women).

verb(Verb, sing) :- v2(Verb,_).
verb(Verb, plural) :- v2(_,Verb).

v2(bites, bite).
v2(likes, like).

det(the,_).
det(Det, sing) :- d2(Det,_).
det(Det, plural) :- d2(_,Det).

d2(this, these).
