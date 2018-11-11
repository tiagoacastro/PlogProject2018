:- dynamic previousBoards/2.

%Start board
startBoard( [['x', 'w', 'x', 'w', 'x'],
             ['x', 'x', 'b', 'x', 'x'],
             ['x', 'x', 'x', 'x', 'x'],
             ['x', 'x', 'w', 'x', 'x'],
             ['x', 'b', 'x', 'b', 'x']] ).

%Displays the board
display_gameAux([], Player, _) :-
    display_border(31),
    display_columns.

display_gameAux([A|B], Player, N) :-
    display_border(31),
    display_line(A, Player, N),
    New is N+1,
    display_gameAux(B, Player, New).

display_game(List, Player) :-
    display_gameAux(List, Player, 1).

display_border(0) :- 
    write('\n').

display_border(N) :-
    N > 0,
    put_code(0x2501),
    N1 is N-1,
    display_border(N1).

display_lineAux([], Player) :-
    write('\n').

display_lineAux([A|B], Player) :-
    write(' '),
    write_char(A),
    write(' '),
    put_code(0x2503),
    display_lineAux(B, Player).

display_line(List, Player, N) :-
    format('~w ', N),
    put_code(0x2503),
    display_lineAux(List,Player).

write_char('x') :-
    write(' ').

write_char('w') :-
    put_code(0x25CB).

write_char('b') :-
    put_code(0x25CF).

display_columns :- 
    write('  '),
    put_code(0x2503),
    write(' a '),
    put_code(0x2503),
    write(' b '),
    put_code(0x2503),
    write(' c '),
    put_code(0x2503),
    write(' d '),
    put_code(0x2503),
    write(' e '),
    put_code(0x2503),
    write('\n').

saveBoard(N, Board) :-
    assert(previousBoards(N, Board)).

previousBoards(N, Board).
