% Starts a game with the start board 
initGame(1) :-
    startBoard(Board),
    playTurn(Board, 1).

initGame(2, Difficulty) :-
    startBoard(Board),
    playTurnVSBot(Board, 1, Difficulty).

initGame(3, Difficulty) :-
    startBoard(Board),
    playTurnBotVSBot(Board, 1, Difficulty).

% Game loop
playTurn(Board, N):-
    blackTurn(Board, IntBoard),
    (
        game_over(IntBoard, 'b');
        (
            whiteTurn(IntBoard, FinalBoard),
            (
                game_over(FinalBoard, 'w');
                (
                    saveBoard(N, Board),
                    NewN is N + 1,
                    playTurn(FinalBoard, NewN)
                )
            )
        )
    ).

% Game loop player vs bot
playTurnVSBot(Board, N, Dif):-
    blackTurn(Board, IntBoard),
    (
        game_over(IntBoard, 'b');
        (
            botTurn(IntBoard, FinalBoard, Dif, 'w'),
            (
                game_over(FinalBoard, 'w');
                (
                    saveBoard(N, Board),
                    NewN is N + 1,
                    playTurnVSBot(FinalBoard, NewN, Dif)
                )
            )
        )
    ).

% Game loop bot vs bot
playTurnBotVSBot(Board, N, Dif):-
    botTurn(Board, IntBoard, Dif, 'b'),
    (
        game_over(IntBoard, 'b');
        (
            botTurn(IntBoard, FinalBoard, Dif, 'w'),
            (
                game_over(FinalBoard, 'w');
                (
                    saveBoard(N, Board),
                    NewN is N + 1,
                    playTurnBotVSBot(FinalBoard, NewN, Dif)
                )
            )
        )
    ).

% Processes black turn
blackTurn(InBoard, OutBoard) :-
    display_game(InBoard, 'b'),
    write('\nNow playing: BLACK\n\n'),
    move(Direction, InBoard, 'b', OutBoard).

% Processes white turn
whiteTurn(InBoard, OutBoard) :-
    display_game(InBoard, 'w'),
    write('\nNow playing: WHITE\n\n'),
    move(Direction, InBoard, 'w', OutBoard).  

% Processes easy bot turn
botTurn(InBoard, OutBoard, 1, Color) :-
    display_game(InBoard, Color),
    write('\n'),
    random(1, 4, Piece),
    getNthPiecePos(InBoard, Color, Nrow, Ncolumn, Piece),
    valid_moves(InBoard, Nrow, Ncolumn, Color, ListOfMoves),
    randomDirection(ListOfMoves, Direction),
    findNewPosition(Direction, InBoard, Nrow, Ncolumn, OutRow, OutColumn),
    changePiece(InBoard, Ncolumn, Nrow, 'x', IntBoard),
    changePiece(IntBoard, OutColumn, OutRow, Color, OutBoard).

% Processes hard bot turn
botTurn(InBoard, OutBoard, 2, Color) :-
    write('hi\n'),
    getBestPlay(1, InBoard, Color, Row1, Column1, Direction1, Value1), write('pass1\n'),
    getBestPlay(2, InBoard, Color, Row2, Column2, Direction2, Value2), write('pass2\n'),
    getBestPlay(3, InBoard, Color, Row3, Column3, Direction3, Value3), write('pass3\n'),
    checkBestPiece(Value1, Value2, Value3, Piece),
    parse(Piece, Row, Row1, Row2, Row3, Column, Column1, Column2, Column3, Direction, Direction1, Direction2, Direction3),
    findNewPosition(Direction, InBoard, Row, Column, OutRow, OutColumn),
    changePiece(InBoard, Column, Row, 'x', IntBoard),
    changePiece(IntBoard, OutColumn, OutRow, Color, OutBoard).

% Parses values for the chosen piece
parse(Piece, Row, Row1, Row2, Row3, Column, Column1, Column2, Column3, Direction, Direction1, Direction2, Direction3) :-
    (
        (Piece = 1, Direction is Direction1, Row is Row1, Column is Column1);
        (Piece = 2, Direction is Direction2, Row is Row2, Column is Column2);
        (Piece = 3, Direction is Direction3, Row is Row3, Column is Column3)
    ), !.

% Gets piece with highest value move
checkBestPiece(Value1, Value2, Value3, Piece) :-
    (
        (Value1 > Value2, Value1 > Value3, Piece is 1);
        (Value2 > Value1, Value2 > Value3, Piece is 2);
        (Value3 > Value1, Value3 > Value2, Piece is 3);
        (Value1 = Value2, random(1, 3, Piece));
        (Value2 = Value3, random(2, 4, Piece));
        (Value1 = Value3, random(1, 3, Choice), ((Choice = 1, Piece is 1);(Choice = 2, Piece is 3)));
        random(1, 4, Piece)
    ), !.

% Gets best play for the piece
getBestPlay(N, Board, Color, Row, Column, Direction, Value) :-
    getNthPiecePos(Board, Color, Row, Column, N),
    valid_moves(Board, Row, Column, Color, Moves),
    getBestDirection(8, Board, Color, Row, Column, -1, -1, Moves, Direction, Value), write('passini\n').

% Gets best direction for the play
getBestDirection(0, Board, Color, Row, Column, TempDir, TempValue, Moves, Direction, Value) :-
    write('HENLO\n'),
    Value is TempValue,
    Direction is TempDir.

getBestDirection(Dir, Board, Color, Row, Column, TempDir, TempValue, Moves, Direction, Value) :-
    Next is Dir - 1, format('~w\n', Dir),
    ((sublist([Dir], Moves), % check is Dir is valid 
        findNewPosition(Dir, Board, Row, Column, OutRow, OutColumn),
        changePiece(Board, Column, Row, 'x', IntBoard),
        changePiece(IntBoard, OutColumn, OutRow, Color, OutBoard),
        value(OutBoard, Color, Val), !,(
        (Val > TempValue, write('>\n'),% check if value is superior to the one stored
            getBestDirection(Next, Board, Color, Row, Column, Dir, Val, Moves, Direction, Value)
        );((Val = TempValue, write('=\n'),% check if value is equal to the one stored
            random(1, 3, Check),
            Check = 1, write('==\n'), % check if the direction is changed based on a random number
            getBestDirection(Next, Board, Color, Row, Column, Dir, Val, Moves, Direction, Value), !)
            ;getBestDirection(Next, Board, Color, Row, Column, TempDir, TempValue, Moves, Direction, Value)
        ))
    );( 
    getBestDirection(Next, Board, Color, Row, Column, TempDir, TempValue, Moves, Direction, Value))).

%Evaluates board state
value(Board, Color, Value) :-
    Value is 1. %TODO

/*
% Processes hard bot turn
botTurn(InBoard, OutBoard, 2, Color) :-
    getNthPiecePos(InBoard, Color, Nrow, Ncolumn, 1),
    valid_moves(InBoard, Nrow, Ncolumn, Color, Moves),
    evaluate_and_choose(InBoard, Moves, Color, Nrow, Ncolumn, (nil, -100), Move).  

%Evaluates every valid move and chooses the best one
evaluate_and_choose(Board, [Move|Moves], Player, Row, Column, Record, BestMove) :-
    findNewPosition(Move, Board, Row, Column, NewRow, NewColumn),
    value(Board, Player, Value),
    update(Move, Value, Record, NewRecord),
    evaluate_and_choose(Board, Moves, Player, Row, Column, NewRecord, BestMove).

evaluate_and_choose(Board, [], Player, Row, Column, (Move, Value), Move) :-
    format('Value: ~w \n', Value).

%Evaluates board state
value(Board, Player, Value) :-
    Value is 1. %TODO

%If value is bigger than the value stored in Record then 
update(Move, Value, (Move1, Value1), (Move, Value)) :-
    Value > Value1.

update(Move, Value, (Move1, Value1), (Move1, Value1)) :-
    Value =< Value1.
*/

%Finds the position to where the piece is going to move and updates board
move(Direction, InBoard, Player, OutBoard) :-
    getMovingPiece(InBoard, Row, Column, Player),
    valid_moves(InBoard, Row, Column, Player, ListOfMoves),
    readDirection(ListOfMoves, Direction), 
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
    ((isMoveValid(Board, NewRow, Column),
        findNewPosition(1, Board, NewRow, Column, OutRow, OutColumn));
        findNewPosition('end', Board, Row, Column, OutRow, OutColumn)).

%Gets the new position for a piece moving 'west'
findNewPosition(2, Board, Row, Column, OutRow, OutColumn) :-
    NewColumn is Column - 1, 
    ((isMoveValid(Board, Row, NewColumn),
        findNewPosition(2, Board, Row, NewColumn, OutRow, OutColumn));
        findNewPosition('end', Board, Row, Column, OutRow, OutColumn)).

%Gets the new position for a piece moving 'east'
findNewPosition(3, Board, Row, Column, OutRow, OutColumn) :-
    NewColumn is Column + 1, 
    ((isMoveValid(Board, Row, NewColumn),
        findNewPosition(3, Board, Row, NewColumn, OutRow, OutColumn));
        findNewPosition('end', Board, Row, Column, OutRow, OutColumn)).   

%Gets the new position for a piece moving 'south'
findNewPosition(4, Board, Row, Column, OutRow, OutColumn) :-
    NewRow is Row + 1, 
    ((isMoveValid(Board, NewRow, Column),
        findNewPosition(4, Board, NewRow, Column, OutRow, OutColumn));
        findNewPosition('end', Board, Row, Column, OutRow, OutColumn)).
    
%Gets the new position for a piece moving 'northeast'
findNewPosition(5, Board, Row, Column, OutRow, OutColumn) :-
    NewRow is Row - 1, 
    NewColumn is Column + 1, 
    ((isMoveValid(Board, NewRow, NewColumn),
        findNewPosition(5, Board, NewRow, NewColumn, OutRow, OutColumn));
        findNewPosition('end', Board, Row, Column, OutRow, OutColumn)).

%Gets the new position for a piece moving 'northwest'
findNewPosition(6, Board, Row, Column, OutRow, OutColumn) :-
    NewRow is Row - 1, 
    NewColumn is Column - 1, 
    ((isMoveValid(Board, NewRow, NewColumn),
        findNewPosition(6, Board, NewRow, NewColumn, OutRow, OutColumn));
        findNewPosition('end', Board, Row, Column, OutRow, OutColumn)).

%Gets the new position for a piece moving 'southeast'
findNewPosition(7, Board, Row, Column, OutRow, OutColumn) :-
    NewRow is Row + 1, 
    NewColumn is Column + 1, 
    ((isMoveValid(Board, NewRow, NewColumn),
        findNewPosition(7, Board, NewRow, NewColumn, OutRow, OutColumn));
        findNewPosition('end', Board, Row, Column, OutRow, OutColumn)).

%Gets the new position for a piece moving 'southwest'
findNewPosition(8, Board, Row, Column, OutRow, OutColumn) :-
    NewRow is Row + 1, 
    NewColumn is Column - 1, 
    ((isMoveValid(Board, NewRow, NewColumn),
        findNewPosition(8, Board, NewRow, NewColumn, OutRow, OutColumn));
        findNewPosition('end', Board, Row, Column, OutRow, OutColumn)).

%Gets the possible moves
valid_moves(Board, Row, Column, Player, ListOfMoves) :-
    RowDown is Row + 1,
    RowUp is Row - 1,
    ColumnRight is Column + 1,
    ColumnLeft is Column - 1,
    isMoveValid(Board, RowUp, Column, 1, [], OutList1),
    isMoveValid(Board, Row, ColumnLeft, 2, OutList1, OutList2),
    isMoveValid(Board, Row, ColumnRight, 3, OutList2, OutList3),
    isMoveValid(Board, RowDown, Column, 4, OutList3, OutList4),
    isMoveValid(Board, RowUp, ColumnRight, 5, OutList4, OutList5),
    isMoveValid(Board, RowUp, ColumnLeft, 6, OutList5, OutList6),
    isMoveValid(Board, RowDown, ColumnRight, 7, OutList6, OutList7),
    isMoveValid(Board, RowDown, ColumnLeft, 8, OutList7, ListOfMoves).

%Checks if position (Row, Column) is valid
isMoveValid(Board, Row, Column) :-
    getPiece(Row, Column, Board, Piece),
    Piece = 'x'.

%If the movement in a certain direction is valid, that direction is added to the list fo valid moves
isMoveValid(Board, Row, Column, Dir, InList, OutList) :-
    (getPiece(Row, Column, Board, Piece),
    Piece = 'x', append(InList, [Dir], OutList));
    append(InList, [], OutList).

% Checks all conditions that end the game
game_over(Board, Player) :-
    (checkRow(Board, Player), display_game(Board, Player), write('Row Win\n'));
    (checkColumn(Board, Player), display_game(Board, Player), write('Column Win\n'));
    (checkDiagonalNWSE(Board, Player), display_game(Board, Player), write('Diagonal NW-SE Win\n'));
    (checkDiagonalNESW(Board, Player), display_game(Board, Player), write('Diagonal NE-SW Win\n'));
    (Player = 'w', checkDraw(Board), display_game(Board, Player), write('Draw')).

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