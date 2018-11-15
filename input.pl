%Gets coordinates from piece that is going to be moved from user , and validates them.
getMovingPiece(Board, Row, Column, Player) :-
    readRow(Row),
    readColumn(Column),
    format('Row: ~w ', Row), format('Col: ~w\n', Column),
    getPiece(Row, Column, Board, Piece),
    (Piece = Player, write('\nPiece selected\n'));
    (write('\nPiece selected is invalid. Try again.\n\n'), 
    getMovingPiece(Board, Row, Column, Player)).
        
%Converts number(1-5) to respective row and checks if row is valid
readRow(Row) :-
    write('Enter the row of the piece you want to move (1-5)'),  
    skip_line,  
    get_code(Input),
    (
        Input = 49, Row is 1;
        Input = 50, Row is 2;
        Input = 51, Row is 3;
        Input = 52, Row is 4;
        Input = 53, Row is 5;
        (write('\nRow is invalid. Try again.\n'),
        readRow(Row))
    ), !.

%Converts letter(a-e) to respective row and checks if row is valid
readColumn(Column) :-
    write('Enter the column of the piece you want to move (a-e)'),
    skip_line,  
    get_code(Input),
    (
        Input = 97, Column is 1;
        Input = 98, Column is 2;
        Input = 99, Column is 3;
        Input = 100, Column is 4;
        Input = 101, Column is 5;
        (write('\nColumn is invalid. Try again.\n'),
        readColumn(Column))
    ), !.

%Gets direction of movement from user and validates it
readDirection(ListOfMoves, Direction) :-
    printDirections(ListOfMoves),
    write('Enter the desired direction'),
    skip_line,  
    get_code(Input),
    (
        Input = 49, sublist(['North'], ListOfMoves), Direction is 1;
        Input = 50, sublist(['West'], ListOfMoves), Direction is 2;
        Input = 51, sublist(['East'], ListOfMoves), Direction is 3;
        Input = 52, sublist(['South'], ListOfMoves), Direction is 4;
        Input = 53, sublist(['Northeast'], ListOfMoves), Direction is 5;
        Input = 54, sublist(['Northwest'], ListOfMoves), Direction is 6;
        Input = 55, sublist(['Southeast'], ListOfMoves), Direction is 7;
        Input = 56, sublist(['Southwest'], ListOfMoves), Direction is 8;
        write('\nDirection is invalid. Try again.\n'),
        readDirection(ListOfMoves, Direction) 
    ), !.

%Only the valid directions are displayed to the user
printDirections(ListOfMoves) :-
    printDir(1, 'North', ListOfMoves),
    printDir(2, 'West', ListOfMoves),
    printDir(3, 'East', ListOfMoves),
    printDir(4, 'South', ListOfMoves),
    printDir(5, 'Northeast', ListOfMoves),
    printDir(6, 'Northwest', ListOfMoves),
    printDir(7, 'Southeast', ListOfMoves),
    printDir(8, 'Southwest', ListOfMoves).


printDir(Number, Direction, ListOfMoves) :-
    (sublist([Direction], ListOfMoves),
    format('~w - ', Number),
    format('~w\n', Direction));
    (format('~w not valid\n', Direction)).