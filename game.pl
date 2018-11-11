% Starts a game with the start board 
initGame(Player1, Player2) :-
    startBoard(Board),
    playTurn(Board, 1).

% Game loop
playTurn(Board, Njogada):-
    %valid_moves(Board, 'b', ListOfMoves).
    %getNthPiecePos(Board, 'b', Nrow, Ncolumn, 3).
    blackTurn(Board, IntBoard),
    (
        checkIfWin(IntBoard, 'b');
        (
            %guardaTabuleiro(Njogada, IntBoard), 
            whiteTurn(IntBoard, FinalBoard),
            (
                checkIfWin(FinalBoard, 'w');
                playTurn(FinalBoard, Njogada)
            )
        )
    ).

% Proccesses black turn
blackTurn(InBoard, OutBoard) :-
    display_game(InBoard, 'b'),
    write('\nNow playing: BLACK\n\n'),
    getMovingPiece(InBoard, Row, Column, 'b'),
    readDirection(Direction),
    findNewPosition(Direction, InBoard, Row, Column, 'b', OutBoard).

    
% Proccesses white turn
whiteTurn(InBoard, OutBoard) :-
    display_game(InBoard, 'w'),
    write('\nNow playing: WHITE\n\n'),
    getMovingPiece(InBoard, Row, Column, 'w'),
    readDirection(Direction),
    findNewPosition(Direction, InBoard, Row, Column, 'w', OutBoard).    

%Finds the position to where the piece is going to move and updates board
findNewPosition(Direction, Board, Row, Column, Player, OutBoard) :-
    move(Direction, Board, Row, Column, OutRow, OutColumn),
    changePiece(Board, Column, Row, 'x', IntBoard),
    changePiece(IntBoard, OutColumn, OutRow, Player, OutBoard).

%When a move function was prematurely ended because a piece was found before reaching the border (TODO outColumn)
move('end', Board, Row, Column, OutRow, OutColumn) :-
    OutRow is Row,
    OutColumn is Column.

%Gets the new position for a piece moving 'north'
move(1, Board, Row, Column, OutRow, OutColumn) :-
    NewRow is Row - 1, 
    (
        isMoveValid(Board, NewRow, Column) -> 
            move(1, Board, NewRow, Column, OutRow, OutColumn);
        move('end', Board, Row, Column, OutRow, OutColumn)
    ).

%Gets the new position for a piece moving 'west'
move(2, Board, Row, Column, OutRow, OutColumn) :-
    NewColumn is Column - 1, 
    (
        isMoveValid(Board, Row, NewColumn) -> 
            move(2, Board, Row, NewColumn, OutRow, OutColumn);
        move('end', Board, Row, Column, OutRow, OutColumn)
    ).

%Gets the new position for a piece moving 'east'
move(3, Board, Row, Column, OutRow, OutColumn) :-
    NewColumn is Column + 1, 
    (
        isMoveValid(Board, Row, NewColumn) -> 
            move(3, Board, Row, NewColumn, OutRow, OutColumn);
        move('end', Board, Row, Column, OutRow, OutColumn)
    ).    

%Gets the new position for a piece moving 'south'
move(4, Board, Row, Column, OutRow, OutColumn) :-
    NewRow is Row + 1, 
    (
        isMoveValid(Board, NewRow, Column) -> 
            move(4, Board, NewRow, Column, OutRow, OutColumn);
        move('end', Board, Row, Column, OutRow, OutColumn)
    ).
    
%Gets the new position for a piece moving 'northeast'
move(5, Board, Row, Column, OutRow, OutColumn) :-
    NewRow is Row - 1, 
    NewColumn is Column + 1, 
    (
        isMoveValid(Board, NewRow, NewColumn) -> 
            move(5, Board, NewRow, NewColumn, OutRow, OutColumn);
        move('end', Board, Row, Column, OutRow, OutColumn)
    ).

%Gets the new position for a piece moving 'northwest'
move(6, Board, Row, Column, OutRow, OutColumn) :-
    NewRow is Row - 1, 
    NewColumn is Column - 1, 
    (
        isMoveValid(Board, NewRow, NewColumn) -> 
            move(6, Board, NewRow, NewColumn, OutRow, OutColumn);
        move('end', Board, Row, Column, OutRow, OutColumn)
    ).

%Gets the new position for a piece moving 'southeast'
move(7, Board, Row, Column, OutRow, OutColumn) :-
    NewRow is Row + 1, 
    NewColumn is Column + 1, 
    (
        isMoveValid(Board, NewRow, NewColumn) -> 
            move(7, Board, NewRow, NewColumn, OutRow, OutColumn);
        move('end', Board, Row, Column, OutRow, OutColumn)
    ).

%Gets the new position for a piece moving 'southwest'
move(8, Board, Row, Column, OutRow, OutColumn) :-
    NewRow is Row + 1, 
    NewColumn is Column - 1, 
    (
        isMoveValid(Board, NewRow, NewColumn) -> 
            move(8, Board, NewRow, NewColumn, OutRow, OutColumn);
        move('end', Board, Row, Column, OutRow, OutColumn)
    ).

valid_moves(Board, Player, ListOfMoves) :-
    getFirstPiecePos(Board, Player, Row, Column),
    RowDown is Row + 1,
    RowUp is Row - 1,
    ColumnRight is Column + 1,
    ColumnLeft is Column - 1,
    isMoveValid(Board, RowUp, Column, 'North', [], OutList1),
    isMoveValid(Board, Row, ColumnLeft, 'West', OutList1, OutList2),
    isMoveValid(Board, Row, ColumnRight, 'East', OutList2, OutList3),
    isMoveValid(Board, RowDown, Column, 'South', OutList3, OutList4),
    isMoveValid(Board, RowUp, ColumnRight, 'Northeast', OutList4, OutList5),
    isMoveValid(Board, RowUp, ColumnLeft, 'Northwest', OutList5, OutList6),
    isMoveValid(Board, RowDown, ColumnRight, 'Southeast', OutList6, OutList7),
    isMoveValid(Board, RowDown, ColumnLeft, 'Southwest', OutList7, ListOfMoves),
    printList(ListOfMoves).


printList([]).

printList([H|T]) :-
    format('~w \n', H),
    printList(T).

%Checks if position (Row, Column) is free
isMoveValid(Board, Row, Column) :-
    getPiece(Row, Column, Board, Piece),
    Piece = 'x'.

isMoveValid(Board, Row, Column, Dir, InList, OutList) :-
    getPiece(Row, Column, Board, Piece),
    Piece = 'x'  -> append(InList, [Dir], OutList);
    append(InList, [], OutList).

% Checks all conditions that end the game
checkIfWin(Board, Player) :-
    (
       (checkRow(Board, Player),  write('Row\n'));
       (checkColumn(Board, Player), write('Column\n'));
       (checkDiagonalNWSE(Board, Player), write('Diagonal NW-SE\n'));
       (checkDiagonalNESW(Board, Player), write('Diagonal NE-SW\n'))          
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