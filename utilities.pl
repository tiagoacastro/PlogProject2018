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

member(Elem, [Elem|_]).

member(Elem, [H|T]) :-
    member(Elem, T).