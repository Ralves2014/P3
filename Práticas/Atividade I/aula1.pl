/* Atividade 1 - familia & estradas/caminhos */

homem(joao).
homem(rui).
homem(manuel).
homem(ricardo).
homem(francisco).
homem(ze).

mulher(maria).
mulher(ana).
mulher(rita).
mulher(silvia).
mulher(joana).
progenitor(joao, maria).
progenitor(joao, rui).
progenitor(manuel, joao).
progenitor(manuel, francisco).
progenitor(francisco,ze).
progenitor(manuel, silvia).
progenitor(ricardo, manuel).
progenitor(ana, rui).
progenitor(ana, maria).

progenitor(rita, joao).
progenitor(rita, francisco).
progenitor(rita, silvia).

pai(X,Y) :- homem(X), progenitor(X,Y).
mae(X,Y) :- mulher(X), progenitor(X,Y).
irmao_generico(X,Y) :- pai(A,X), pai(A,Y), mae(B,X), mae(B,Y), X \= Y.
irmao(X,Y) :- homem(X), pai(A,X), pai(A,Y), mae(B,X), mae(B,Y), X \= Y.
irma(X,Y) :- mulher(X), pai(A,X), pai(A,Y), mae(B,X), mae(B,Y), X \= Y.
avos_generico(X,Y) :- progenitor(X,A), progenitor(A,Y).
tio_generico(X,Y) :- irmao(A,X), progenitor(A,Y).
tio(X,Y) :- homem(X), irmao(A,X), progenitor(A,Y).
primo_direito(A, B) :- progenitor(X,A), progenitor(Y,B), irmao(X,Y).

antepassado(X,Y) :- progenitor(X,Y).
antepassado(X,Y) :- progenitor(X,A), antepassado(A,Y).

descendente(X,Y) :- progenitor(Y,X).
descendente(X,Y) :- progenitor(Y,A), descendente(X,A), X \= Y.

meias_irmas(X,Y) :- mulher(X), mulher(Y), pai(A,X), pai(A,Y).
meias_irmas(X,Y) :- mulher(X), mulher(Y), mae(A,X), mae(A,Y).

grau_parentesco(X,Y) :- antepassado(X,Y); descendente(X,Y); tio_generico(X,Y); primo_direito(X,Y).

/*quantificador_parentesco(X,Y,G)*/


/*------------------------------------------------------------------------------------------------------------------*/

e(lisboa, santarem).
e(santarem, coimbra).
e(santarem, caldas).
e(caldas, lisboa).
e(coimbra, porto).
e(lisboa, evora).
e(evora, beja).
e(lisboa, faro).
e(beja, faro).
e(viseu, braga).

a(X,Y) :- e(X,Y).
a(X,Y) :- e(Y,X).

cam(A, B) :- cam(A,B,A).
cam(A, B, X) :- a(A, B), nao_figura(B, X).
cam(A, B, X) :- a(A, C), nao_figura(C, X), cam(C, B, c(C,X)).

nao_figura(N,K) :- \+ figura(N,K).

figura(N, N).
figura(N, c(N,_)).
figura(N, c(_,K)) :- figura(N, K).
