prefix([], List).

prefix([H|TP], [H|TL]) :-
    prefix(TP, TL).

suffix(List, List).

suffix(Suf, [HL|TL]) :-
    suffix(Suf, TL).

sublist(Sub, List) :-
    prefix(Sub, List).

sublist(Sub, [HL|TL]) :-
    sublist(Sub, TL).

%Get Piece
getPiece(Row, Column, Board, Piece) :-
    getRow(Row, Board, OutRow),
    getColumn(Column, OutRow, Piece).

getRow(1, [Row|_], Row).

getRow(N, [_|Resto], Row) :-
    N>1,
    Previous is N-1,
    getRow(Previous, Resto, Row).

getColumn(1, [Column|_], Column).

getColumn(N, [_|Resto], Column) :-
    N>1,
    Previous is N-1,
    getColumn(Previous, Resto, Column).

%Checks if Elem is member of the list
member(Elem, [Elem|_]).

member(Elem, [H|T]) :-
    member(Elem, T).

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

%Converts letter(A-E) to respective row and checks if row is valid
validateRow(RowCode, Row) :-
    char_code(RowCode, Number),
    Number >= 97, %a
    Number =< 101, %e
    Row is Number-96.

%Checks if column is valid
validateColumn(ColumnCode, Column) :-
    char_code(ColumnCode, Number),
    Number >= 49, %1
    Number =< 53, %5
    Column is Number-48.
