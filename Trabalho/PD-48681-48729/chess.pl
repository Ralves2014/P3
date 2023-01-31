% (Gramatic) Castles
cast('O-O') --> "O-O".
cast('O-O-O') --> "O-O-O".


% (Gramatic) Figures
f('R') --> "R".
f('K') --> "K".
f('Q') --> "Q".
f('B') --> "B".
f('N') --> "N".


% (Gramatic) Lines
l(1) --> "1".
l(2) --> "2".
l(3) --> "3".
l(4) --> "4".
l(5) --> "5".
l(6) --> "6".
l(7) --> "7".
l(8) --> "8".


% (Gramatic) Columns
c(a) --> "a".
c(b) --> "b".
c(c) --> "c".
c(d) --> "d".
c(e) --> "e".
c(f) --> "f".
c(g) --> "g".
c(h) --> "h".


% (Gramatic) Signals
signal('+') --> "+".
signal('#') --> "#".
signal('x') --> "x".
signal('=') --> "=".


% (Gramatic) Plays of File
jb(X) --> [X],{char_type(X, jb)}.
jp(X) --> [X],{char_type(X, jp)}.

% Extract col & line jogada_branca
extract_characters_b(jb(P,L1,S1,C,L2,S2)) :- extract_characters_b(jb(P,L1,S1,C,L2,S2),P,L1,S1,C,L2,S2).
extract_characters_b(jb(P,C1,S1,C2,L,S2)) :- extract_characters_b(jb(P,C1,S1,C2,L,S2),P,C1,S1,C2,L,S2).
extract_characters_b(jb(C1,S1,C2,L,S2)) :- extract_characters_b(jb(C1,S1,C2,L,S2),C1,S1,C2,L,S2).
extract_characters_b(jb(P,L1,S,C2,L2)) :- extract_characters_b(jb(P,L1,S,C2,L2),P,L1,S,C2,L2).
extract_characters_b(jb(P,L1,C2,L2,S)) :- extract_characters_b(jb(P,L1,C2,L2,S),P,L1,C2,L2,S).
extract_characters_b(jb(P,C1,S,C2,L)) :- extract_characters_b(jb(P,C1,S,C2,L),P,C1,S,C2,L).
extract_characters_b(jb(P,C1,C2,L,S)):- extract_characters_b(jb(P,C1,C2,L,S),P,C1,C2,L,S).
extract_characters_b(jb(P,S1,C,L,S2)):- extract_characters_b(jb(P,S1,C,L,S2),P,S1,C,L,S2).
extract_characters_b(jb(P,L1,C2,L2)):- extract_characters_b(jb(P,L1,C2,L2),P,L1,C2,L2).
extract_characters_b(jb(C1,S,C2,L)):- extract_characters_b(jb(C1,S,C2,L),C1,S,C2,L).
extract_characters_b(jb(P,C1,C2,L)):- extract_characters_b(jb(P,C1,C2,L),P,C1,C2,L).
extract_characters_b(jb(P,S,C,L)):- extract_characters_b(jb(P,S,C,L),P,S,C,L).
extract_characters_b(jb(P,C,L,S)):- extract_characters_b(jb(P,C,L,S),P,C,L,S).
extract_characters_b(jb(P,C,L)):- extract_characters_b(jb(P,C,L),P,C,L).
extract_characters_b(jb(C,L,S)):- extract_characters_b(jb(C,L,S),C,L,S).
extract_characters_b(jb(C, L)) :- extract_characters_b(jb(C, L), C, L).
extract_characters_b(jb(Cast)):- extract_characters_b(jb(Cast), Cast).

extract_characters_b(jb(P,L1,S1,C,L2,S2),P,L1,S1,C,L2,S2):-
    write(P),write(L1),write(S1),write(C),write(L2),write(S2).

extract_characters_b(jb(P,C1,S1,C2,L,S2),P,C1,S1,C2,L,S2):-
    write(P),write(C1),write(S1),write(C2),write(L),write(S2).

extract_characters_b(jb(C1,S1,C2,L,S2),C1,S1,C2,L,S2):-
    write(C1),write(S1),write(C2),write(L),write(S2).

extract_characters_b(jb(P,L1,S,C2,L2),P,L1,S,C2,L2):-
    write(P),write(L1),write(S),write(C2),write(L2).

extract_characters_b(jb(P,L1,C2,L2,S),P,L1,C2,L2,S):-
    write(P),write(L1),write(C2),write(L2),write(S).

extract_characters_b(jb(P,C1,S,C2,L),P,C1,S,C2,L):-
    write(P),write(C1),write(S),write(C2),write(L).

extract_characters_b(jb(P,C1,C2,L,S),P,C1,C2,L,S):-
    write(P),write(C1),write(C2),write(L),write(S).

extract_characters_b(jb(P,S1,C,L,S2),P,S1,C,L,S2):-
    write(P),write(S1),write(C),write(L),write(S2).

extract_characters_b(jb(P,L1,C2,L2),P,L1,C2,L2):-
    write(P),write(L1),write(C2),write(L2).

extract_characters_b(jb(C1,S,C2,L),C1,S,C2,L):-
    write(C1),write(S),write(C2),write(L).

extract_characters_b(jb(P,C1,C2,L),P,C1,C2,L):-
    write(P),write(C1),write(C2),write(L).

extract_characters_b(jb(P,S,C,L),P,S,C,L):-
    write(P),write(S),write(C),write(L).

extract_characters_b(jb(P,C,L,S),P,C,L,S):-
    write(P),write(C),write(L),write(S).

extract_characters_b(jb(P,C,L),P,C,L):-
    write(P),write(C),write(L).

extract_characters_b(jb(C,L,S),C,L,S):-
    write(C),write(L),write(S).

extract_characters_b(jb(C, L), C, L):-
    write(C),write(L).

extract_characters_b(jb(Cast), Cast):-
    write(Cast).


% Extract col & line jogada_preta
extract_characters_p(jp(P,L1,S1,C,L2,S2)) :- extract_characters_p(jp(P,L1,S1,C,L2,S2),P,L1,S1,C,L2,S2).
extract_characters_p(jp(P,C1,S1,C2,L,S2)) :- extract_characters_p(jp(P,C1,S1,C2,L,S2),P,C1,S1,C2,L,S2).
extract_characters_p(jp(C1,S1,C2,L,S2)) :- extract_characters_p(jp(C1,S1,C2,L,S2),C1,S1,C2,L,S2).
extract_characters_p(jp(P,L1,S,C2,L2)) :- extract_characters_p(jp(P,L1,S,C2,L2),P,L1,S,C2,L2).
extract_characters_p(jp(P,L1,C2,L2,S)) :- extract_characters_p(jp(P,L1,C2,L2,S),P,L1,C2,L2,S).
extract_characters_p(jp(P,C1,S,C2,L)) :- extract_characters_p(jp(P,C1,S,C2,L),P,C1,S,C2,L).
extract_characters_p(jp(P,C1,C2,L,S)):- extract_characters_p(jp(P,C1,C2,L,S),P,C1,C2,L,S).
extract_characters_p(jp(P,S1,C,L,S2)):- extract_characters_p(jp(P,S1,C,L,S2),P,S1,C,L,S2).
extract_characters_p(jp(P,L1,C2,L2)):- extract_characters_p(jp(P,L1,C2,L2),P,L1,C2,L2).
extract_characters_p(jp(C1,S,C2,L)):- extract_characters_p(jp(C1,S,C2,L),C1,S,C2,L).
extract_characters_p(jp(P,C1,C2,L)):- extract_characters_p(jp(P,C1,C2,L),P,C1,C2,L).
extract_characters_p(jp(P,S,C,L)):- extract_characters_p(jp(P,S,C,L),P,S,C,L).
extract_characters_p(jp(P,C,L,S)):- extract_characters_p(jp(P,C,L,S),P,C,L,S).
extract_characters_p(jp(P,C,L)):- extract_characters_p(jp(P,C,L),P,C,L).
extract_characters_p(jp(C,L,S)):- extract_characters_p(jp(C,L,S),C,L,S).
extract_characters_p(jp(C, L)) :- extract_characters_p(jp(C, L), C, L).
extract_characters_p(jp(Cast)):- extract_characters_p(jp(Cast), Cast).

extract_characters_p(jp(P,L1,S1,C,L2,S2),P,L1,S1,C,L2,S2):-
    write(P),write(L1),write(S1),write(C),write(L2),write(S2).

extract_characters_p(jp(P,C1,S1,C2,L,S2),P,C1,S1,C2,L,S2):-
    write(P),write(C1),write(S1),write(C2),write(L),write(S2).

extract_characters_p(jp(C1,S1,C2,L,S2),C1,S1,C2,L,S2):-
    write(C1),write(S1),write(C2),write(L),write(S2).

extract_characters_p(jp(P,L1,S,C2,L2),P,L1,S,C2,L2):-
    write(P),write(L1),write(S),write(C2),write(L2).

extract_characters_p(jp(P,L1,C2,L2,S),P,L1,C2,L2,S):-
    write(P),write(L1),write(C2),write(L2),write(S).

extract_characters_p(jp(P,C1,S,C2,L),P,C1,S,C2,L):-
    write(P),write(C1),write(S),write(C2),write(L).

extract_characters_p(jp(P,C1,C2,L,S),P,C1,C2,L,S):-
    write(P),write(C1),write(C2),write(L),write(S).

extract_characters_p(jp(P,S1,C,L,S2),P,S1,C,L,S2):-
    write(P),write(S1),write(C),write(L),write(S2).

extract_characters_p(jp(P,L1,C2,L2),P,L1,C2,L2):-
    write(P),write(L1),write(C2),write(L2).

extract_characters_p(jp(C1,S,C2,L),C1,S,C2,L):-
    write(C1),write(S),write(C2),write(L).

extract_characters_p(jp(P,C1,C2,L),P,C1,C2,L):-
    write(P),write(C1),write(C2),write(L).

extract_characters_p(jp(P,S,C,L),P,S,C,L):-
    write(P),write(S),write(C),write(L).

extract_characters_p(jp(P,C,L,S),P,C,L,S):-
    write(P),write(C),write(L),write(S).

extract_characters_p(jp(P,C,L),P,C,L):-
    write(P),write(C),write(L).

extract_characters_p(jp(C,L,S),C,L,S):-
    write(C),write(L),write(S).

extract_characters_p(jp(C, L), C, L):-
    write(C),write(L).

extract_characters_p(jp(Cast), Cast):-
    write(Cast).


% (Gramatic) Types of Strings
type(movimentos(X,Y)) --> jogada_branca(X), " " , jogada_preta(Y).
type(movimentos(X,R)) --> jogada_branca(X), " ", resultado(R).
type(movimentos(X,Y,R)) --> jogada_branca(X), " " , jogada_preta(Y), " " , resultado(R).


% (Gramatic) jogada_branca
jogada_branca(jb(P,L1,S1,C,L2,S2)) --> f(P),l(L1),signal(S1),c(C),l(L2),signal(S2). %R2xb8+
jogada_branca(jb(P,C1,S1,C2,L,S2)) --> f(P),l(C1),signal(S1),c(C2),l(L),signal(S2). %Rbxb8+
jogada_branca(jb(P,L1,C2,L2,S)) --> f(P),l(L1),c(C2),l(L2),signal(S).               % N3d2+ / N3d2#       error
jogada_branca(jb(P,C1,C2,L,S)) --> f(P),c(C1),c(C2),l(L),signal(S).                 % Nbd2+ / Nbd2#       error
jogada_branca(jb(P,S1,C,L,S2)) --> f(P),signal(S1),c(C),l(L),signal(S2).            % Nxf6+ / Nxf6#   
jogada_branca(jb(P,C,L,S)) --> f(P),c(C),l(L),signal(S).                            % Nf6+ / Nf6#
jogada_branca(jb(C1,S1,C2,L,S2)) --> c(C1),signal(S1),c(C2),l(L),signal(S2).        % dxf6+ / dxf6#
jogada_branca(jb(C,L,S)) --> c(C),l(L),signal(S).                                   % e6+ / e6#
jogada_branca(jb(P,L1,S,C2,L2)) --> f(P),l(L1),signal(S),c(C2),l(L2).               % N1xf6               error
jogada_branca(jb(P,C1,S,C2,L)) --> f(P),c(C1),signal(S),c(C2),l(L).                 % Ndxf6               error
jogada_branca(jb(P,S,C,L)) --> f(P),signal(S),c(C),l(L).                            % Nxf6
jogada_branca(jb(C1,S,C2,L)) --> c(C1),signal(S),c(C2),l(L).                        % dxf6
jogada_branca(jb(P,L1,C2,L2)) --> f(P),l(L1),c(C2),l(L2).                           % N3d2                error
jogada_branca(jb(P,C1,C2,L)) --> f(P),c(C1),c(C2),l(L).                             % Nbd2                error
jogada_branca(jb(P,C,L)) --> f(P),c(C),l(L).                                        % Nf6
jogada_branca(jb(C,L)) --> c(C),l(L).                                               % e6
jogada_branca(jb(Cast)) --> cast(Cast).                                             % O-O / O-O-O


% (Gramatic) jogada_preta
jogada_preta(jp(P,L1,S1,C,L2,S2)) --> f(P),l(L1),signal(S1),c(C),l(L2),signal(S2). %R2xb8+
jogada_preta(jp(P,C1,S1,C2,L,S2)) --> f(P),l(C1),signal(S1),c(C2),l(L),signal(S2). %Rbxb8+
jogada_preta(jp(P,L1,C2,L2,S)) --> f(P),l(L1),c(C2),l(L2),signal(S).               % N3d2+ / N3d2#       error
jogada_preta(jp(P,C1,C2,L,S)) --> f(P),c(C1),c(C2),l(L),signal(S).                 % Nbd2+ / Nbd2#       error
jogada_preta(jp(P,S1,C,L,S2)) --> f(P),signal(S1),c(C),l(L),signal(S2).            % Nxf6+ / Nxf6#
jogada_preta(jp(P,C,L,S)) --> f(P),c(C),l(L),signal(S).                            % Nf6+ / Nf6#
jogada_preta(jp(C1,S1,C2,L,S2)) --> c(C1),signal(S1),c(C2),l(L),signal(S2).        % dxf6+ / dxf6#
jogada_preta(jp(C,L,S)) --> c(C),l(L),signal(S).                                   % e6+ / e6#
jogada_preta(jp(P,L1,S,C2,L2)) --> f(P),l(L1),signal(S),c(C2),l(L2).               % N1xf6               error
jogada_preta(jp(P,C1,S,C2,L)) --> f(P),c(C1),signal(S),c(C2),l(L).                 % Ndxf6               error
jogada_preta(jp(P,S,C,L)) --> f(P),signal(S),c(C),l(L).                            % Nxf6
jogada_preta(jp(C1,S,C2,L)) --> c(C1),signal(S),c(C2),l(L).                        % dxf6
jogada_preta(jp(P,L1,C2,L2)) --> f(P),l(L1),c(C2),l(L2).                           % N3d2                error
jogada_preta(jp(P,C1,C2,L)) --> f(P),c(C1),c(C2),l(L).                             % Nbd2                error
jogada_preta(jp(P,C,L)) --> f(P),c(C),l(L).                                        % Nf6
jogada_preta(jp(C,L)) --> c(C),l(L).                                               % e6
jogada_preta(jp(Cast)) --> cast(Cast).                                             % O-O / O-O-O


% (Gramatic) resultado
resultado(branca_vence) --> "1-0".
resultado(preta_vence) --> "0-1". 
resultado(empate) --> "1/2-1/2".


% Define initial positions of pieces
:- dynamic(piece/3).

init_b :-
    retractall(piece(_,_,_)),
    assertz(piece(wr,a,1)),
    assertz(piece(wr,h,1)),
    assertz(piece(wn,b,1)),
    assertz(piece(wn,g,1)),
    assertz(piece(wb,c,1)),
    assertz(piece(wb,f,1)),
    assertz(piece(wq,e,1)),
    assertz(piece(wk,d,1)),
    

    assertz(piece(wp,a,2)),
    assertz(piece(wp,b,2)),
    assertz(piece(wp,c,2)),
    assertz(piece(wp,d,2)),
    assertz(piece(wp,e,2)),
    assertz(piece(wp,f,2)),
    assertz(piece(wp,g,2)),
    assertz(piece(wp,h,2)),

    assertz(piece(bp,a,7)),
    assertz(piece(bp,b,7)),
    assertz(piece(bp,c,7)),
    assertz(piece(bp,d,7)),
    assertz(piece(bp,e,7)),
    assertz(piece(bp,f,7)),
    assertz(piece(bp,g,7)),
    assertz(piece(bp,h,7)),

    assertz(piece(br,a,8)),
    assertz(piece(br,h,8)),
    assertz(piece(bn,b,8)),
    assertz(piece(bn,g,8)),
    assertz(piece(bb,c,8)),
    assertz(piece(bb,f,8)),
    assertz(piece(bq,e,8)),
    assertz(piece(bk,d,8)).


% Predicate used to convert to char
from_char_code(Code, Char) :-
    char_code(Char, Code).


% Print Board
print_board :- 
    findall(piece(Piece, Col, Row), 
    piece(Piece, Col, Row), Board),
    print_board(Board).

print_board(Board) :-
    write('  a  b  c  d  e  f   g  h'), nl,
    print_board(Board,8).

print_board(_,0) :- !.

print_board(Board,Row) :-
    Row1 is Row - 1,
    write(Row), write(' '),
    char_code('a',X),
    print_row(Board,Row,X),
    nl,
    print_board(Board,Row1).


print_row(_,_,H) :- char_code('i',H),!.

print_row(Board,Row,Col) :-
    from_char_code(Col,C),
    (   member(piece(Piece,C,Row),Board)
    ->  write(Piece), write(' '),
        char_code(C,Col),
        Col1 is Col + 1,
        print_row(Board,Row,Col1)
    ;   write('.  '),
        Col1 is Col + 1,
        print_row(Board,Row,Col1)
    ).


% Move piece (Test Predicate)
move:-
    move_piece(wp,'a',2,'a',7),
    print_board.

move_piece(Piece, FromCol, FromRow, ToCol, ToRow) :-
    
    (   piece(_, ToCol, ToRow)
    ->  retract(piece(_, ToCol, ToRow)),
        retract(piece(Piece, FromCol, FromRow)),
        assertz(piece(Piece, ToCol, ToRow))
    ;   retract(piece(Piece, FromCol, FromRow)),
        assertz(piece(Piece, ToCol, ToRow))
    ).


% Read
gets(S) :- get0(C), gets([], C, S).
gets(S, 10, S).     % 10 é o newline
gets(S, -1, S).     % -1 é o end-of-file
gets(I, C, [C|O]) :- get0(CC), gets(I, CC, O).


% Read file
read_file_and_splitline(Filename) :-
    open(Filename, read, Stream),
    set_input(Stream),
    read_lines(movimentos(X,Y)),
    extract_characters_b(X),
    write(' '),
    extract_characters_p(Y),
    nl,
    read_file_and_splitline.
    

read_file_and_splitline :-
    read_lines(movimentos(X,Y)),
    extract_characters_b(X),
    write(' '),
    extract_characters_p(Y),
    nl,
    read_file_and_splitline.

read_lines(X) :-
    gets(L),
    phrase(type(X), L, []),!. 


% Play
play_chess :-
    init_b,
    print_board,
    read_file_and_splitline('jogo-1.txt').
   