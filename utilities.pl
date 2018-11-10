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

%Get first ocurrence of a piece
getFirstPiecePos(Board, Piece, Row, Column) :-
    R is 1,
    getFPRow(R, C, Board, Piece, Row, Column).

getFPRow(R, C, [H|Rest], Piece, Row, Column) :-
    (C is 1,
    getFPColumn(C, H, Piece, Column), Row is R);
    (Next is R + 1,
    getFPRow(Next, New, Rest, Piece, Row, Column)).

getFPColumn(C, [H|Rest], Piece, Column) :-
    (H = Piece, Column is C);
    (Next is C + 1,
    getFPColumn(Next, Rest, Piece, Column)).
