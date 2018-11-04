% Starts a game with the start board 
initGame(Player1, Player2) :-
    startBoard(Board),
    playTurn(Board).
%    checkIfWin(Board, Player1).

% Game loop
playTurn(Board):-
    write('Do turn\n'),
%    blackTurn(Board, IntBoard),
    checkIfWin(Board, 'b').
    %whiteTurn(Board, FinalBoard),
    %checkIfWin(Board,'white'),
    %playTurn(Board).

% Proccesses black turn
blackTurn(inBoard, outBoard) :-
    write('simulate black turn\n').
    
% Proccesses white turn
whiteTurn(inBoard, outBoard) :-
    write('simulate white turn\n').    

% Checks all conditions that end the game
checkIfWin(Board, Player) :-
    (
       (checkRow(Board, Player),  write('Row'));
       (checkColumn(Board, Player, 1), write('Column'));
       (checkDiagonalNWSE(Board, Player, 1), write('Diagonal NW-SE'));
       (checkDiagonalNESW(Board, Player, 1), write('Diagonal NE-SW'))          
    ).

% Checks if any row has a game ending condition
checkRow([H|T], Player) :-
    sublist([Player, Player, Player], H);
    checkRow(T, Player).

% Gets the first row where the specified piece is found  
getRow([H|T], Player, N, Nrow) :- 
    member(Player, H), Nrow is N; 
    NewN is N + 1,
    getRow(T, Player, NewN, Nrow).

% Checks if any column has a game ending condition
checkColumn(Board, Player, Ncolumn) :-
    getRow(Board, Player, 1, Nrow),
    getPiece(Nrow, Ncolumn, Board, Piece), Piece = Player,
    Nrow2 is Nrow + 1, getPiece(Nrow2, Ncolumn, Board, Piece2), Piece2 = Player, 
    Nrow3 is Nrow + 2, getPiece(Nrow3, Ncolumn, Board, Piece3), Piece3 = Player;
    NewN is Ncolumn + 1,
    NewN < 6,
    checkColumn(Board, Player, NewN).

% Checks if any diagonal has a game ending condition (NW-SE orientation)
checkDiagonalNWSE(Board, Player, Ncolumn) :-
    getRow(Board, Player, 1, Nrow),
    getPiece(Nrow, Ncolumn, Board, Piece), Piece = Player,
    Nrow2 is Nrow + 1, Ncolumn2 is Ncolumn + 1, getPiece(Nrow2, Ncolumn2, Board, Piece2), Piece2 = Player, 
    Nrow3 is Nrow + 2, Ncolumn3 is Ncolumn + 2, getPiece(Nrow3, Ncolumn3, Board, Piece3), Piece3 = Player;
    NewN is Ncolumn + 1,
    NewN < 6,
    checkDiagonalNWSE(Board, Player, NewN).

% Checks if any diagonal has a game ending condition (NE-SW orientation)
checkDiagonalNESW(Board, Player, Ncolumn) :-
    getRow(Board, Player, 1, Nrow),
    getPiece(Nrow, Ncolumn, Board, Piece), Piece = Player,
    Nrow2 is Nrow + 1, Ncolumn2 is Ncolumn - 1, getPiece(Nrow2, Ncolumn2, Board, Piece2), Piece2 = Player, 
    Nrow3 is Nrow + 2, Ncolumn3 is Ncolumn - 2, getPiece(Nrow3, Ncolumn3, Board, Piece3), Piece3 = Player;
    NewN is Ncolumn + 1,
    NewN < 6,
    checkDiagonalNESW(Board, Player, NewN).