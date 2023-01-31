/* Atividade 5 */

fdic([]).
fdic([_=_|D]) :- fdic(D).

flookup([K=V|_], K, V).
flookup([A=_ |_], K, _) :- K@<A, !, fail.
flookup([_|D], K, V):- flookup(D, K, V).

finsert([], K, V, [K=V]).
finsert([K=_|D], K, V, [K=V|D]) :- !.
finsert([A=B|D], K, V, [K=V, A=B|D]):- K@<A.
finsert([KV|DI], K, V, [KV|DO]) :- finsert(DI, K, V, DO).

fremove([], _, []).
fremove([K=_|D], K, D) :- !.
fremove([KV|DI], K, [KV|DO]) :- fremove(DI, K, DO).

abp(nil).
abp(no(_, L, R)) :- abp(L), abp(R).

alookup(no(K=V, _, _), K, V).
alookup(no(X=_, L, _), K, V) :- K @< X, !, alookup(L, K, V).
alookup(no(X=_, _, R), K, V) :- K @> X, !, alookup(R, K, V).

ainsert(nil, K, V, no(K=V, nil, nil)).
ainsert(no(K=_,L,R), K, V, no(K=V,L,R)):- !.
ainsert(no(X=VX, L, R), K, V, no(X=VX, LL, R)) :- K@<X, ainsert(L, K, V, LL).
ainsert(no(X=VX, L, R), K, V, no(X=VX, L, RR)) :- K@>X, ainsert(R, K, V, RR).