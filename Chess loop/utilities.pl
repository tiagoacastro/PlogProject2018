%Returns the max and the min values

get_min_max(Nrows, Ncols, Min, Max):-
    Nrows =< Ncols,
    Min is Nrows, 
    Max is Ncols, 
    !.

get_min_max(Nrows, Ncols, Min, Max):-
    Nrows > Ncols,
    Min is Ncols, 
    Max is Nrows.

%-----------------------------------------------------------

%Updates a specified position of the board 

changePosition(BoardIn, Row, Column, Piece, BoardOut) :-
    setRow(Row, Column, BoardIn, Piece, BoardOut).

setRow(1, Column, [HIn|T], Piece, [HOut|T]) :-
    setColumn(Column, HIn, Piece, HOut).

setRow(N, Column, [H|TIn], Piece, [H|TOut]) :-
    NewN is N-1,
    setRow(NewN, Column, TIn, Piece, TOut).

setColumn(1, [_|T], Piece, [Piece|T] ).

setColumn(N, [H|TIn], Piece, [H|TOut]) :-
    NewN is N-1,
    setColumn(NewN, TIn, Piece, TOut).

%-----------------------------------------------------------

%Board display functions

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
    convert_element(Elem),
    write(' ').

convert_element(1) :-
    put_code(0x2654) , !.

convert_element(2) :-
    put_code(0x2655) , !.

convert_element(3) :-
    put_code(0x2656) , !.

convert_element(4) :-
    put_code(0x2657) , !.

convert_element(5) :-
    put_code(0x2658) , !.

convert_element(_) :-
    write(' ').