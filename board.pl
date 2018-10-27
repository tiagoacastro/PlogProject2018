:-use_module(library(lists)).

board( [ ['x', 'w', 'x', 'w', 'x'],
         ['x', 'x', 'b', 'x', 'x'],
         ['x', 'x', 'x', 'x', 'x'],
         ['x', 'x', 'w', 'x', 'x'],
         ['x', 'b', 'x', 'b', 'x']] ).

getPiece(Nlinha, Ncoluna, Board, Peca) :-
    nth1(Nlinha, Board, Linha),        
    nth1(Ncoluna, Linha, Peca). 

display_game([], Player) :-
    display_border(28),
    write('\n Now playing: ').

display_game([A|B], Player) :-
    display_border(28),
    put_code(0x2503),
    display_line(A, Player),
    display_game(B, Player).

display_border(0) :- 
    write('\n').

display_border(N) :-
    N > 0,
    put_code(0x2501),
    N1 is N-1,
    display_border(N1).

display_line([], Player) :-
    write('\n').

display_line([A|B], Player) :-
    write(' '),
    write_char(A),
    write(' '),
    put_code(0x2503),
    display_line(B, Player).

write_char('x') :-
    write(' ').

write_char('w') :-
    put_code(0x25CB).

write_char('b') :-
    put_code(0x25CF).

/*guardaTabuleiro(Njogada, Board) :-
    assert (jogadaAnterior(Njogada, Board)).

jogadaAnterior (Njogada, Board)

empate(Board) :-
    jogadaAnterior(N1, Board),
    jogadaAnterior(N2, Board),
    N1 \= N2.*/

    
  
