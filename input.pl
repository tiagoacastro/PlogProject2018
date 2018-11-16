%Gets coordinates from piece that is going to be moved from user , and validates them.
getMovingPiece(Board, Row, Column, Player) :-
    readRow(Row),
    readColumn(Column),
    getPiece(Row, Column, Board, Piece),
    (Piece = Player, write('\nPiece selected\n\n'));
    (write('\nPiece selected is invalid. Try again.\n\n'), 
    getMovingPiece(Board, Row, Column, Player)).
        
%Converts number(1-5) to respective row and checks if row is valid
readRow(Row) :-
    repeat,
    write('Enter the row of the piece you want to move (1-5): '),  
    get_code(Input),
    skip_line,  
    (
        Input = 49, Row is 1;
        Input = 50, Row is 2;
        Input = 51, Row is 3;
        Input = 52, Row is 4;
        Input = 53, Row is 5;
        write('\nRow is invalid. Try again.\n'),
        fail
    ), !.

%Converts letter(a-e) to respective row and checks if row is valid
readColumn(Column) :-
    repeat,
    write('Enter the column of the piece you want to move (a-e): '),
    get_code(Input),
    skip_line,  
    (
        Input = 97, Column is 1;
        Input = 98, Column is 2;
        Input = 99, Column is 3;
        Input = 100, Column is 4;
        Input = 101, Column is 5;
        write('\nColumn is invalid. Try again.\n'),
        fail
    ), !.

%Gets direction of movement from user and validates it
readDirection(ListOfMoves, Direction) :-
    printDirections(ListOfMoves),
    repeat,
    write('Enter the desired direction: '),
    get_code(Input),
    skip_line,  
    (
        Input = 49, sublist([1], ListOfMoves), Direction is 1;
        Input = 50, sublist([2], ListOfMoves), Direction is 2;
        Input = 51, sublist([3], ListOfMoves), Direction is 3;
        Input = 52, sublist([4], ListOfMoves), Direction is 4;
        Input = 53, sublist([5], ListOfMoves), Direction is 5;
        Input = 54, sublist([6], ListOfMoves), Direction is 6;
        Input = 55, sublist([7], ListOfMoves), Direction is 7;
        Input = 56, sublist([8], ListOfMoves), Direction is 8;
        write('\nDirection is invalid. Try again.\n'),
        fail
    ), !.

%Only the valid directions are displayed to the user
printDirections(ListOfMoves) :-
    write('DIRECTION\n\n'),
    printDir(1, 'North', ListOfMoves),
    printDir(2, 'West', ListOfMoves),
    printDir(3, 'East', ListOfMoves),
    printDir(4, 'South', ListOfMoves),
    printDir(5, 'Northeast', ListOfMoves),
    printDir(6, 'Northwest', ListOfMoves),
    printDir(7, 'Southeast', ListOfMoves),
    printDir(8, 'Southwest', ListOfMoves),
    write('\n').

%If a direction is in the list of valid moves, then it is displayed as valid, otherwise it is not valid
printDir(Number, Direction, ListOfMoves) :-
    (sublist([Number], ListOfMoves),
    format('~w - ', Number),
    format('~w\n', Direction));
    (format('~w not valid\n', Direction)).