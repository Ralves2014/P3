/* Atividade 2 - Axiomas de Peano */

num(z).
num(s(X)) :- num(X).

le(z,_).
le(s(A),s(B)) :- le(A,B).

lt(z,s(_)).
lt(s(A),s(B)) :- lt(A,B).

soma(z,X,X) :- num(X).
soma(s(X),Y,s(Z)) :- soma(X,Y,Z).

sub(A,B,X) :- soma(X,B,A).

mult(z,_,z).
mult(s(A),B,X) :-
    mult(A,B,Y),
    soma(B,Y,X).

div(A, B, X) :- mult(X, B, A).

div_resto(A, B, Q, R) :-
    mult(B, Q, X),
    soma(X, R, A),
    lt(R, B).

num_aux(z, 0).
num_aux(s(X), SY) :- num_aux(X, Y), SY is Y+1.

primo(2).
primo(3).
primo(P) :- P > 3, P mod 2 =\= 0 , \+tem_fator(P,3).
tem_fator(N,L) :- N mod L =:= 0.
tem_fator(N,L) :- L * L < N, L2 is L + 2, tem_fator(N,L2).


/* ------------------------------------------------------------------------------ */

pol([],_,0).
pol([H|T],X,SY) :- pol(T,X,Y), SY is H + X * Y.

fact(z,s(z)).
fact(s(X),R) :- fact(X,D), mult(s(X),D,R).

fact_rec(X,Y) :- fact_rec(X,X,Y).
fact_rec(z,z,s(z)).
fact_rec(s(x),s(x),R) :- fact_rec(X,X,D), mult(s(X),D,R).

divisao(X,Y) :- mult(Y,_,X).

pares([],[]).
pares([_],[]).
pares([_,H2|T],[H2|L2]) :- pares(T,L2).

impares([],[]).
impares([H],[H]).
impares([H1,_|L1],[H1|L2]) :- impares(L1,L2).

avalia([],_,0).
avalia([H|T],X,Ys) :- avalia(T,X,Y), Ys is H + Y*X.

fib(0,1).
fib(1,1).
fib(N,R):-
    N>1,
    N1 is N-1,
    N2 is N-2,
    fib(N1,R1),
    fib(N2,R2),
    R is R1+R2.

fib_peano(z,s(z)).
fib_peano(s(z),s(z)).
fib_peano(s(s(N)),R):-
    fib_peano(N,A),
    fib_peano(s(N),B),
    soma(A,B,R).