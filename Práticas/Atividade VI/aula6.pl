/* Atividade 6 - Rainhas & Quandrados Mágicos */

cok([]).
cok([R|Rs]) :- cok(Rs,R,1), cok(Rs).

cok([],_,_).
cok([Rj|Rs],Ri,I) :-
    I1 is I+1,
    cok(Rs,Ri,I1),
    Ri #\= Rj, Ri #\= Rj+I, Ri+I #\= Rj.


cqueens(N,QS) :- 
    length(QS,N),
    fd_domain(QS,1,N),
    cok(QS).                    % lista de variaveis livres sobre as quais estao entre 1 e X
    fd_labeling(QS).            % se metermos isto em comentario


ok([]).
ok([R|Rs]) :- ok(Rs,R,1), ok(Rs).

ok([],_,_).
ok([Rj|Rs],Ri,I) :-
    Ri =\= Rj,  Ri =\= Rj+I,  Rj =\= Ri+I,
    I1 is I+1,
    ok(Rs,Ri,I1).


queens(N,QS) :-
    length(QS,N),
    fd_domain(QS,1,N),fd_all_different(QS),
    fd_labeling(QS),
    ok(QS).


/*------------------------------------------------------------------------------------------------------------------*/


% quadrados mágicos

% ideia geral: representar NxN variáveis (uma para cada posição do
% quadrado), agrupar tudo num termo e fazer predicados para extraír
% listas que representem uma linha, uma coluna ou uma diagonal.
%
% fazer predicados para impor uma constraint de soma para cada linha,
% coluna e diagonal, em que a soma seja sempre igual a uma variável
% determinada.
%
% (código incompleto; falta percorrer as linhas, colunas e diagonais,
% falta dizer que todas as variáveis são diferentes, etc.)

matrix(N, M) :-
    functor(M, m, N),
    matrix(M, 0, N).

matrix(_, N, N).
matrix(M, I, N) :-
    I < N, J is I+1,
    functor(V, v, N),
    V =.. [_|VV],
    NN is N*N,
    fd_domain(VV, 1, NN),
    arg(J, M, V),
    matrix(M, J, N).

x(M, I, J, X) :- arg(I, M, V), arg(J, V, X).


soma_linhas(M, S) :- functor(M, _, N), soma_linhas(M, 0, N, S).

soma_linhas(_, N, N, _).
soma_linhas(M, J, N, S) :- I is J+1, linha(M, I, L), fd_sum(L, S),
			   soma_linhas(M, I, N, S).


soma_colunas(M, S) :- functor(M, _, N), soma_colunas(M, 0, N, S).

soma_colunas(_, N, N, _).
soma_colunas(M, J, N, S) :- I is J+1, coluna(M, I, L), fd_sum(L, S),
			    soma_colunas(M, I, N, S).


linha(M, I, L) :- arg(I, M, LX), LX =.. [v | L].

coluna(M, J, C) :- M =.. [_|MM], coluna_aux(MM, J, C).

coluna_aux([], _, []).
coluna_aux([L|Ls], J, [X|Xs]) :- arg(J, L, X), coluna_aux(Ls, J, Xs).



fd_sum([X], V) :- !, X #= V.
fd_sum([X|Xs], V) :- V #= X+VV, fd_sum(Xs, VV).