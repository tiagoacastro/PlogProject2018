% Starts a game with the start board 
initGame(Player1, Player2) :-
    startBoard(Board),
%    playTurn(Board).
    checkIfWin(Board, Player1).

% Game loop
playTurn(Board):-
    write('Do turn'),
    blackTurn(Board, IntBoard),
    checkIfWin(Board,'black'),
    whiteTurn(Board, FinalBoard),
    checkIfWin(Board,'white'),
    playTurn(Board).

% Proccesses black turn
blackTurn(inBoard, outBoard) :-
    write('simulate black turn\n').
    
% Proccesses white turn
whiteTurn(inBoard, outBoard) :-
    write('simulate white turn\n').    

% Checks all conditions that end the game
checkIfWin(Board, Player) :-
    (
       (checkRow(Board, Player),  write('Row'))     
    ).

% Checks if any row has a win condition
checkRow([H|T], Player) :-
    sublist(['w','x','w'], H);
    checkRow(T, Player).
    
    
