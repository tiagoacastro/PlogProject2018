% Starts a game with the start board 
initGame(Player1, Player2) :-
    startBoard(Board),
    playTurn(Board, 1).

% Game loop
playTurn(Board, N):-
    blackTurn(Board, IntBoard),
    (
        checkIfWin(IntBoard, 'b');
        (
            whiteTurn(IntBoard, FinalBoard),
            (
                checkIfWin(FinalBoard, 'w');
                saveBoard(N, Board),
                NewN is N + 1,
                playTurn(FinalBoard, NewN)
            )
        )
    ).

% Proccesses black turn
blackTurn(InBoard, OutBoard) :-
    display_game(InBoard, 'b'),
    write('\nNow playing: BLACK\n\n'),
    move(Direction, InBoard, Row, Column, 'b', OutBoard).

% Proccesses white turn
whiteTurn(InBoard, OutBoard) :-
    display_game(InBoard, 'w'),
    write('\nNow playing: WHITE\n\n'),
    move(Direction, InBoard, Row, Column, 'w', OutBoard).    

%Finds the position to where the piece is going to findNewPosition and updates board
move(Direction, InBoard, Row, Column, Player, OutBoard) :-
    getMovingPiece(InBoard, Row, Column, Player),
    readDirection(Direction),
    findNewPosition(Direction, InBoard, Row, Column, OutRow, OutColumn),
    changePiece(InBoard, Column, Row, 'x', IntBoard),
    changePiece(IntBoard, OutColumn, OutRow, Player, OutBoard).

%When a findNewPosition function was prematurely ended because a piece was found before reaching the border (TODO outColumn)
findNewPosition('end', Board, Row, Column, OutRow, OutColumn) :-
    OutRow is Row,
    OutColumn is Column.

%Gets the new position for a piece moving 'north'
findNewPosition(1, Board, Row, Column, OutRow, OutColumn) :-
    NewRow is Row - 1, 
    (
        isfindNewPositionValid(Board, NewRow, Column) -> 
            findNewPosition(1, Board, NewRow, Column, OutRow, OutColumn);
        findNewPosition('end', Board, Row, Column, OutRow, OutColumn)
    ).

%Gets the new position for a piece moving 'west'
findNewPosition(2, Board, Row, Column, OutRow, OutColumn) :-
    NewColumn is Column - 1, 
    (
        isfindNewPositionValid(Board, Row, NewColumn) -> 
            findNewPosition(2, Board, Row, NewColumn, OutRow, OutColumn);
        findNewPosition('end', Board, Row, Column, OutRow, OutColumn)
    ).

%Gets the new position for a piece moving 'east'
findNewPosition(3, Board, Row, Column, OutRow, OutColumn) :-
    NewColumn is Column + 1, 
    (
        isfindNewPositionValid(Board, Row, NewColumn) -> 
            findNewPosition(3, Board, Row, NewColumn, OutRow, OutColumn);
        findNewPosition('end', Board, Row, Column, OutRow, OutColumn)
    ).    

%Gets the new position for a piece moving 'south'
findNewPosition(4, Board, Row, Column, OutRow, OutColumn) :-
    NewRow is Row + 1, 
    (
        isfindNewPositionValid(Board, NewRow, Column) -> 
            findNewPosition(4, Board, NewRow, Column, OutRow, OutColumn);
        findNewPosition('end', Board, Row, Column, OutRow, OutColumn)
    ).
    
%Gets the new position for a piece moving 'northeast'
findNewPosition(5, Board, Row, Column, OutRow, OutColumn) :-
    NewRow is Row - 1, 
    NewColumn is Column + 1, 
    (
        isfindNewPositionValid(Board, NewRow, NewColumn) -> 
            findNewPosition(5, Board, NewRow, NewColumn, OutRow, OutColumn);
        findNewPosition('end', Board, Row, Column, OutRow, OutColumn)
    ).

%Gets the new position for a piece moving 'northwest'
findNewPosition(6, Board, Row, Column, OutRow, OutColumn) :-
    NewRow is Row - 1, 
    NewColumn is Column - 1, 
    (
        isfindNewPositionValid(Board, NewRow, NewColumn) -> 
            findNewPosition(6, Board, NewRow, NewColumn, OutRow, OutColumn);
        findNewPosition('end', Board, Row, Column, OutRow, OutColumn)
    ).

%Gets the new position for a piece moving 'southeast'
findNewPosition(7, Board, Row, Column, OutRow, OutColumn) :-
    NewRow is Row + 1, 
    NewColumn is Column + 1, 
    (
        isfindNewPositionValid(Board, NewRow, NewColumn) -> 
            findNewPosition(7, Board, NewRow, NewColumn, OutRow, OutColumn);
        findNewPosition('end', Board, Row, Column, OutRow, OutColumn)
    ).

%Gets the new position for a piece moving 'southwest'
findNewPosition(8, Board, Row, Column, OutRow, OutColumn) :-
    NewRow is Row + 1, 
    NewColumn is Column - 1, 
    (
        isfindNewPositionValid(Board, NewRow, NewColumn) -> 
            findNewPosition(8, Board, NewRow, NewColumn, OutRow, OutColumn);
        findNewPosition('end', Board, Row, Column, OutRow, OutColumn)
    ).

valid_moves(Board, Player, ListOfMoves) :-
    getNthPiecePos(Board, Player, Row, Column, 1),
    RowDown is Row + 1,
    RowUp is Row - 1,
    ColumnRight is Column + 1,
    ColumnLeft is Column - 1,
    isfindNewPositionValid(Board, RowUp, Column, 'North', [], OutList1),
    isfindNewPositionValid(Board, Row, ColumnLeft, 'West', OutList1, OutList2),
    isfindNewPositionValid(Board, Row, ColumnRight, 'East', OutList2, OutList3),
    isfindNewPositionValid(Board, RowDown, Column, 'South', OutList3, OutList4),
    isfindNewPositionValid(Board, RowUp, ColumnRight, 'Northeast', OutList4, OutList5),
    isfindNewPositionValid(Board, RowUp, ColumnLeft, 'Northwest', OutList5, OutList6),
    isfindNewPositionValid(Board, RowDown, ColumnRight, 'Southeast', OutList6, OutList7),
    isfindNewPositionValid(Board, RowDown, ColumnLeft, 'Southwest', OutList7, ListOffindNewPositions),
    printList(ListOffindNewPositions).

%Checks if position (Row, Column) is free
isfindNewPositionValid(Board, Row, Column) :-
    getPiece(Row, Column, Board, Piece),
    Piece = 'x'.

isfindNewPositionValid(Board, Row, Column, Dir, InList, OutList) :-
    getPiece(Row, Column, Board, Piece),
    Piece = 'x'  -> append(InList, [Dir], OutList);
    append(InList, [], OutList).

% Checks all conditions that end the game
checkIfWin(Board, Player) :-
    (
       (checkRow(Board, Player),  write('Row\n'));
       (checkColumn(Board, Player), write('Column\n'));
       (checkDiagonalNWSE(Board, Player), write('Diagonal NW-SE\n'));
       (checkDiagonalNESW(Board, Player), write('Diagonal NE-SW\n'));
       (Player = 'w' -> checkDraw(Board), write('Empate'))          
    ).

% Checks if any row has a game ending condition
checkRow([H|T], Player) :-
    sublist([Player, Player, Player], H);
    checkRow(T, Player).

% Checks if any column has a game ending condition
checkColumn(Board, Player) :-
    getNthPiecePos(Board, Player, Nrow, Ncolumn, 1),
    Nrow2 is Nrow + 1, getPiece(Nrow2, Ncolumn, Board, Piece2), Piece2 = Player,
    Nrow3 is Nrow + 2, getPiece(Nrow3, Ncolumn, Board, Piece3), Piece3 = Player.

% Checks if any diagonal has a game ending condition (NW-SE orientation)
checkDiagonalNWSE(Board, Player) :-
    getNthPiecePos(Board, Player, Nrow, Ncolumn, 1),
    Nrow2 is Nrow + 1, Ncolumn2 is Ncolumn + 1, getPiece(Nrow2, Ncolumn2, Board, Piece2), Piece2 = Player, 
    Nrow3 is Nrow + 2, Ncolumn3 is Ncolumn + 2, getPiece(Nrow3, Ncolumn3, Board, Piece3), Piece3 = Player.

% Checks if any diagonal has a game ending condition (NE-SW orientation)
checkDiagonalNESW(Board, Player) :-
    getNthPiecePos(Board, Player, Nrow, Ncolumn, 1),
    Nrow2 is Nrow + 1, Ncolumn2 is Ncolumn - 1, getPiece(Nrow2, Ncolumn2, Board, Piece2), Piece2 = Player, 
    Nrow3 is Nrow + 2, Ncolumn3 is Ncolumn - 2, getPiece(Nrow3, Ncolumn3, Board, Piece3), Piece3 = Player.

% Checks if the same position occurred for the third time. If it did, then the game is declared a draw. 
checkDraw(Board) :-
    previousBoards(N1, Board),
    previousBoards(N2, Board),
    N1 \= N2.