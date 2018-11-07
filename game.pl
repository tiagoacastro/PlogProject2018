% Starts a game with the start board 
initGame(Player1, Player2) :-
    startBoard(Board),
    playTurn(Board).
%    checkIfWin(Board, Player1).

% Game loop
playTurn(Board):-
%    blackTurn(Board, IntBoard),
%    display_game(Board, Player),
%    changePiece(Board, 3, 3, 'w', IntBoard),
%    changePiece(IntBoard, 3, 4, 'x', FBoard),
%    display_game(FBoard, Player). 
%    checkIfWin(Board, 'b'),
    display_game(Board, Player),
    findNewPosition('south', Board, 1, 4, 'w', BoardOut),
    display_game(BoardOut, Player).
%    display_game(BoardOut, Player).
    %whiteTurn(IntBoard, FinalBoard),
%    checkIfWin(Board,'w').
    %playTurn(FinalBoard).

%Finds the position to where the piece is going to move and updates board
findNewPosition('south', Board, Row, Column, Player, OutBoard) :-
move('south', Board, Row, Column, OutRow),
    changePiece(Board, Column, Row, 'x', IntBoard), 
    changePiece(IntBoard, Column, OutRow, Player, OutBoard).

%When a move function was prematurely ended because a piece was found before reaching the border (TODO outColumn)
move('end', Board, Row, Column, OutRow) :-
    OutRow is Row.

%When a move 'south' function reaches the border
move('south', Board, 5, Column, OutRow) :-
    OutRow is 5.

%Gets the new position for a piece
move('south', Board, Row, Column, OutRow) :-
    NewRow is Row + 1, 
    (isMoveValid(Board, NewRow, Column) -> 
        move('south', Board, NewRow, Column, OutRow);
        move('end', Board, Row, Column, OutRow)).

%Checks if position (Row, Column) is free
isMoveValid(Board, Row, Column) :-
    getPiece(Row, Column, Board, Piece),
    Piece = 'x'.


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
getRowWithPlayer([H|T], Player, N, Nrow) :- 
    member(Player, H), Nrow is N; 
    NewN is N + 1,
    getRowWithPlayer(T, Player, NewN, Nrow).

% Checks if any column has a game ending condition
checkColumn(Board, Player, Ncolumn) :-
    getRowWithPlayer(Board, Player, 1, Nrow),
    getPiece(Nrow, Ncolumn, Board, Piece), Piece = Player,
    Nrow2 is Nrow + 1, getPiece(Nrow2, Ncolumn, Board, Piece2), Piece2 = Player, 
    Nrow3 is Nrow + 2, getPiece(Nrow3, Ncolumn, Board, Piece3), Piece3 = Player;
    NewN is Ncolumn + 1,
    NewN < 6,
    checkColumn(Board, Player, NewN).

% Checks if any diagonal has a game ending condition (NW-SE orientation)
checkDiagonalNWSE(Board, Player, Ncolumn) :-
    getRowWithPlayer(Board, Player, 1, Nrow),
    getPiece(Nrow, Ncolumn, Board, Piece), Piece = Player,
    Nrow2 is Nrow + 1, Ncolumn2 is Ncolumn + 1, getPiece(Nrow2, Ncolumn2, Board, Piece2), Piece2 = Player, 
    Nrow3 is Nrow + 2, Ncolumn3 is Ncolumn + 2, getPiece(Nrow3, Ncolumn3, Board, Piece3), Piece3 = Player;
    NewN is Ncolumn + 1,
    NewN < 6,
    checkDiagonalNWSE(Board, Player, NewN).

% Checks if any diagonal has a game ending condition (NE-SW orientation)
checkDiagonalNESW(Board, Player, Ncolumn) :-
    getRowWithPlayer(Board, Player, 1, Nrow),
    getPiece(Nrow, Ncolumn, Board, Piece), Piece = Player,
    Nrow2 is Nrow + 1, Ncolumn2 is Ncolumn - 1, getPiece(Nrow2, Ncolumn2, Board, Piece2), Piece2 = Player, 
    Nrow3 is Nrow + 2, Ncolumn3 is Ncolumn - 2, getPiece(Nrow3, Ncolumn3, Board, Piece3), Piece3 = Player;
    NewN is Ncolumn + 1,
    NewN < 6,
    checkDiagonalNESW(Board, Player, NewN).