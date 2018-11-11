%Checks if the desired sublist exists in the list
sublist(Sub, List) :-
    prefix(Sub, List).

sublist(Sub, [HL|TL]) :-
    sublist(Sub, TL).

prefix([], List).

prefix([H|TP], [H|TL]) :-
    prefix(TP, TL).

suffix(List, List).

suffix(Suf, [HL|TL]) :-
    suffix(Suf, TL).

%Get Piece
getPiece(Row, Column, Board, Piece) :-
    getRow(Row, Board, OutRow),
    getColumn(Column, OutRow, Piece).

getRow(1, [Row|_], Row).

getRow(N, [_|Rest], Row) :-
    N>1,
    Previous is N-1,
    getRow(Previous, Rest, Row).

getColumn(1, [Column|_], Column).

getColumn(N, [_|Rest], Column) :-
    N>1,
    Previous is N-1,
    getColumn(Previous, Rest, Column).

%Changes member on a certain position of the board
changePiece(BoardIn, Column, Row, NewPiece, BoardOut) :-
    setRow(Row, Column, BoardIn, NewPiece, BoardOut).

setRow(1, Column, [HIn|T], NewPiece, [HOut|T]) :-
    setColumn(Column, HIn, NewPiece, HOut).

setRow(N, Column, [H|TIn], NewPiece, [H|TOut]) :-
    NewN is N-1,
    setRow(NewN, Column, TIn, NewPiece, TOut).

setColumn(1, [HIn|T], NewPiece, [NewPiece|T] ).

setColumn(N, [H|TIn], NewPiece, [H|TOut]) :-
    NewN is N-1,
    setColumn(NewN, TIn, NewPiece, TOut).

%Get nth ocurrence of a piece starting from
getNthPiecePos(Board, Piece, Row, Column, N) :-
    R is 1,
    getNPRow(R, C, Board, Piece, Row, Column, N).

getNPRow(R, C, [H|Rest], Piece, Row, Column, N) :-
    (C is 1,
    getNPColumn(C, H, Piece, Column, N, NewN), !, Row is R);
    (Next is R + 1, format('~w ', N), format('~w\n', NewN),
    getNPRow(Next, New, Rest, Piece, Row, Column, NewN)).

getNPColumn(C, [H|[]], Piece, Column, 1, NewN) :-
    write('kek2\n'),
    (H = Piece, Column is C);
    fail.

getNPColumn(C, [H|Rest], Piece, Column, 1, NewN) :-
    write('kek1\n'),
    (H = Piece, Column is C);
    (Next is C + 1,
    getNPColumn(Next, Rest, Piece, Column, 1, NewN)).

getNPColumn(C, [H|[]], Piece, Column, N, NewN) :-
    N > 1,
    write('kek2\n'),
    ((H = Piece, NextN is N - 1); NextN is N),
    NewN is NextN,
    fail.

getNPColumn(C, [H|Rest], Piece, Column, N, NewN) :-
    N > 1,
    write('kek1\n'),
    ((H = Piece, NextN is N - 1); NextN is N),
    Next is C + 1,
    getNPColumn(Next, Rest, Piece, Column, NextN, NewN).


