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
    format('| ~w | ~w | ~w | ~w | ~w |\n', A),
    display_game(B, Player). 


guardaTabuleiro(Njogada, Board) :-
    assert (jogadaAnterior(Njogada, Board)).

jogadaAnterior (Njogada, Board)

empate(Board) :-
    jogadaAnterior(N1, Board),
    jogadaAnterior(N2, Board),
    N1 \= N2.

    
  
