print_list([]).
print_list([A|B]) :-
    format("~w", A),
    print_list(B).

get_min(Nrows, Ncols, Min):-
    Nrows =< Ncols,
    Min is Nrows.

get_min(Nrows, Ncols, Min):-
    Nrows > Ncols,
    Min is Ncols.

%-----------------------------------------------------------

%Updates a specified position of the board

changePosition(BoardIn, Row, Column, Piece, BoardOut) :-
    setRow(Row, Column, BoardIn, Piece, BoardOut).

setRow(1, Column, [HIn|T], Piece, [HOut|T]) :-
    setColumn(Column, HIn, Piece, HOut).

setRow(N, Column, [H|TIn], Piece, [H|TOut]) :-
    NewN is N-1,
    setRow(NewN, Column, TIn, Piece, TOut).

setColumn(1, [HIn|T], Piece, [Piece|T] ).

setColumn(N, [H|TIn], Piece, [H|TOut]) :-
    NewN is N-1,
    setColumn(NewN, TIn, Piece, TOut).

%-----------------------------------------------------------

display_board([], Ncols) :-
    display_border(Ncols).

display_board([H|T], Ncols) :-
    display_border(Ncols),
    display_line(H),
    put_code(0x2503), nl,
    display_board(T, Ncols).

display_border(0) :-
    put_code(0x2501),
    put_code(0x2501),
    nl.

display_border(Ncols) :-
    Ncols > 0,
    write_border(5),
    N is Ncols - 1,
    display_border(N).

write_border(0).

write_border(N) :-
    N > 0,
    put_code(0x2501),
    NewN is N - 1,
    write_border(NewN).
   
display_line([]).

display_line([H|T]) :-
    put_code(0x2503),
    display_position(H),
    display_line(T).

display_position(Elem) :-
    write(' '),
    write(Elem),
    write(' ').
