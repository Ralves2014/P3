/* Atividade 4 */

gets(S) :- get0(C), gets([], C, S).

gets(S, 10, S).		% 10 é o newline
gets(S, -1, S).		% -1 é o end-of-file
gets(I, C, [C|O]) :- get0(CC), gets(I, CC, O).


flatten(Xs, Ys) :- flatten(Xs, [], Ys).
flatten([], [X|S], Ys) :- flatten (X, S, Ys).
flatten([X|Xs], S, Ys) :-
    list(X),
    flatten(X, [Xs|S], Ys).
flatten([X|Xs], S, [X|Ys]) :-
    atomic(X),
    X \== [],
    flatten(Xs, S, Ys).


capicua(S) :- inverte(S,S1), igual(S1,S).

inverte(L, R) :- inverte(L, [], R).

inverte([],R,R).
inverte([A|B], X, R).
inverte(B, [A,X], R).

igual([],[]).
igual([A|Y],[B|X]:- A == B , igual(Y,X)