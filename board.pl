:- dynamic previousBoards/2.

%Start board
startBoard( [['x', 'w', 'x', 'w', 'x'],
             ['x', 'x', 'b', 'x', 'x'],
             ['x', 'x', 'x', 'x', 'x'],
             ['x', 'x', 'w', 'x', 'x'],
             ['x', 'b', 'x', 'b', 'x']] ).

%---------------------------------------------

%Code responsible for displaying board

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
%---------------------------------------------

%Displays horizontal lines separating rows

display_border(0) :- 
    write('\n').

display_border(N) :-
    N > 0,
    put_code(0x2501),
    N1 is N-1,
    display_border(N1).

%---------------------------------------------

%Displays rows 

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

%---------------------------------------------

%Converts chars to unicode

write_char('x') :-
    write(' ').

write_char('w') :-
    put_code(0x25CB).

write_char('b') :-
    put_code(0x25CF).

%---------------------------------------------

%Displays columns coordinates

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

%---------------------------------------------

%Code used to store board in the end of each turn

saveBoard(N, Board) :-
    assert(previousBoards(N, Board)).

previousBoards(N, Board).

%---------------------------------------------
