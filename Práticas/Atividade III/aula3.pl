/* Atividade 3 - listas */

membro(X,[X|_]).
membro(X,[_|L]) :- membro(X,L).

prefixo([],_).
prefixo([X1|L1],[X1|L2]) :- prefixo(L1,L2).

sufixo(X,X).
sufixo(X,[_|Y]) :- sufixo(X,Y).

sublista(X,Y) :- prefixo(X,Y).
sublista(X,[_|Y]) :- sublista(X,Y).

concatena([],L2,L2).
concatena([H|L1],L2,[H|L3]) :- concatena(L1,L2,L3).

nrev([],[]).
nrev([X|A],B) :-
    nrev(A,AR),
    concatena(AR,[X],B).

inverte(L,R) :- inverte(L,[],R).
inverte([],R,R).
inverte([A|B],X,R) :- inverte(B,[A|X],R).

tamanho([],0).
tamanho([_|L],X) :- tamanho(L,Y), X is Y+1.

tamanho_rec(L,R) :- tamanho_rec(L,L,R).
tamanho_rec([],[],0).
tamanho_rec([_|L1],[_|L2],X) :- tamanho_rec(L1,L2,R), X is R +1.

/*------------------------------------------------------------------------------------------------------------------*/

sequencia(A,B,L) :-
    A =< B,
    sequencia_aux(A,B,L).

sequencia_aux(A,B,[A|T]) :-
    A<B,
    A1 is A+1,
    sequencia_aux(A1,B,T).
sequencia_aux(_,B,[B]).

double([],[]).
double([H|L],[H,H|L2]) :- double(L,L2).

adj(E1,E2,[E1,E2|_]).
adj(E1,E2,[E2,E1|_]).
adj(E1,E2,[_|T]) :- adj(E1,E2,T).

sele(X,[X|L],L).
sele(X,[E|L1],[E|L2]) :- sele(X,L1,L2).

somatorio([],0).
somatorio([N|L],S) :- somatorio(L,Y), S is Y+N.

ord([]).
ord([_]).
ord([N1,N2|L]) :- N2 >= N1, ord([N2|L]).

perm([], []).
perm(L, [X|LP]) :-
    sel(X, L, LX),
    perm(LX, LP).

sel(E, [E|L], L).
sel(E, [X|L], [X|M]) :- sel(E, L, M).

sort(I, S) :- sort(I, [], S).
sort([], S, S).
sort([X|Xs], SI, SO) :-
    insord(X, SI, SX),
    sort(Xs, SX, SO).

insord(X, [],[X]).
insord(X, [A|As], [X,A|As]) :- X=<A.
insord(X, [A|As], [A|AAs]) :-
    X>A,
    insord(X, As, AAs).

/*------------------------------------------------------------------------------------------------------------------*/

ocorrencias([],_,0).
ocorrencias([X|L],H,O) :-  ocorrencias(L,H,Y), X =:= H, O is Y+1.
ocorrencias([_|L],H,O) :-  ocorrencias(L,H,O).

