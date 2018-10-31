mainMenu :-
    printMenu, 
    read(Input),
    (
        Input = 1 -> playMenu;
        Input = 2 -> rulesMenu;
        Input = 3 -> write('Thanks for playing')
    ).

printMenu :-    
    write('------------------\n'),
    write('|    Neutreeko   |\n'),
    write('------------------\n'),
    write('1 - Play\n'),
    write('2 - Rules\n'),
    write('3 - Exit\n').
    
playMenu :-
    printPlayMenu,
    read(Input),
    (
        Input = 1 -> initGame(Player1, Player2);
        Input = 2 -> initGame(Player1, Player2);
        Input = 3 -> initGame(Player1, Player2);
        Input = 4 -> mainMenu
    ).

printPlayMenu :-
    write('------------------\n'),
    write('|      Play      |\n'),
    write('------------------\n'),
    write('1 - Human vs Human\n'),
    write('2 - Human vs Computer\n'),
    write('3 - Computer vs Computer\n'),
    write('4 - Back\n').

rulesMenu :-
    printRules,
    read(Input),
    (
        Input = 1 -> mainMenu
    ).

printRules :-
    write('-------------------\n'),
    write('|      Rules      |\n'),
    write('-------------------\n\n'),
    write('- Each players has 3 pieces (white or black).\n'),
    write('- Only one piece is moved in each turn.\n'),
    write('- Pieces slide orthogonally or diagonally until stopped by another piece or the border of the board.\n'),
    write('- Black always plays first.\n'),
    write('- If the same position occurs three times, the match is declared a draw\n'),
    write('- When a player connects his pieces in a row, either orthogonally or diagonally, he is declared the winner\n\n'),
    write('1 - Back\n').
   