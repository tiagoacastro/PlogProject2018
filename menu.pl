%Main menu
mainMenu :-
    repeat,
    printMenu, 
    get_code(Input),
    skip_line,
    (
        Input = 49, playMenu, mainMenu;
        Input = 50, rulesMenu, mainMenu;
        Input = 51, write('Thanks for playing\n');
        write('\nInvalid input\n'),
        fail
    ).

printMenu :-    
    display_border(25),
    put_code(0x2503),
    write('    Neutreeko   '),
    put_code(0x2503),
    write('\n'),
    display_border(25),
    write('1 - Play\n'),
    write('2 - Rules\n'),
    write('3 - Exit\n').

%Play menu
playMenu :-
    repeat,
    printPlayMenu,
    get_code(Input),
    skip_line,
    (
        Input = 49, initGame(1);
        Input = 50, difficultyMenu(Choice), playMenu;
        Input = 51, initGame(3);
        Input = 52;
        write('\nInvalid input\n'),
        fail
    ).

printPlayMenu :-
    display_border(25),
    put_code(0x2503),
    write('      Play      '),
    put_code(0x2503),
    write('\n'),
    display_border(25),
    write('1 - Human vs Human\n'),
    write('2 - Human vs Computer\n'),
    write('3 - Computer vs Computer\n'),
    write('4 - Back\n').

%Difficulty menu
difficultyMenu(Choice) :-
    repeat,
    printDifficultyMenu,
    get_code(Input),
    skip_line,
    (
        Input = 49, initGame(2, 1);
        Input = 50, initGame(2, 2);
        Input = 51;
        write('\nInvalid input\n'),
        fail
    ).

printDifficultyMenu :-
    display_border(25),
    put_code(0x2503),
    write('   Difficulty   '),
    put_code(0x2503),
    write('\n'),
    display_border(25),
    write('1 - Easy\n'),
    write('2 - Hard\n'),
    write('3 - Back\n').

%Rules menu
rulesMenu :-
    repeat,
    printRules,
    get_code(Input),  
    skip_line,
    (
        Input = 49;
        write('\nInvalid input\n'),
        fail
    ).

printRules :-
    display_border(26),
    put_code(0x2503),
    write('      Rules      '),
    put_code(0x2503),
    write('\n\n'),
    display_border(26),
    write('- Each players has 3 pieces (white or black).\n'),
    write('- Only one piece is moved in each turn.\n'),
    write('- Pieces slide orthogonally or diagonally until stopped by another piece or the border of the board.\n'),
    write('- Black always plays first.\n'),
    write('- If the same position occurs three times, the match is declared a draw\n'),
    write('- When a player connects his pieces in a row, either orthogonally or diagonally, he is declared the winner\n\n'),
    write('1 - Back\n').