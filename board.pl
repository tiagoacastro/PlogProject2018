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
    write(' ___________________\n' ),
    write('  a   b   c   d   e ').

display_game([A|B], Player) :-
    write(' ___________________\n' ),
    write('|'),
    display_line(A, Player  ),
    display_game(B, Player). 

display_line([], Player) :-
    write('\n' ).

display_line([A|B], Player) :-
    A = 'x',
    write('   |'),
    display_line(B, Player).

display_line([A|B], Player) :-
    A = 'w',
    write(' '),
    put_code(0x25CB),
    write(' |'),
    display_line(B, Player).

display_line([A|B], Player) :-
    A = 'b',
    write(' '),
    put_code(0x25CF),
    write(' |'),
    display_line(B, Player).

/*guardaTabuleiro(Njogada, Board) :-
    assert (jogadaAnterior(Njogada, Board)).

jogadaAnterior (Njogada, Board)

empate(Board) :-
    jogadaAnterior(N1, Board),
    jogadaAnterior(N2, Board),
    N1 \= N2.*/

    
  
