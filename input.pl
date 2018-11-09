
%Gets coordinates from piece that is going to be moved from user , and validates them.
getMovingPiece(Board, Row, Column, Player) :-
    readRow(Row),
    readColumn(Column),
    format('Row: ~w', Row), format('Col: ~w\n', Column),
    getPiece(Row, Column, Board, Piece),
    (
        Piece = Player -> write('\nPiece selected\n');
        write('\nPiece selected is invalid. Try again.\n\n'), 
        getMovingPiece(Board, Row, Column, Player)   
    ).
        
%Converts letter(a-e) to respective row and checks if row is valid
readRow(Row) :-
    write('Enter the row of the piece you want to move (a-e)'),    
    read(Input),
    (
            Input = 'a' -> Row is 1;
            Input = 'b' -> Row is 2;
            Input = 'c' -> Row is 3;
            Input = 'd' -> Row is 4;
            Input = 'e' -> Row is 5;
            write('\nRow is invalid. Try again.\n'),
            readRow(Row) 
    ).

%Checks if column is valid
readColumn(Column) :-
    write('Enter the column of the piece you want to move (1-5)'),
    read(Input),
    (
        Input = 1 -> Column is 1;
        Input = 2 -> Column is 2;
        Input = 3 -> Column is 3;
        Input = 4 -> Column is 4;
        Input = 5 -> Column is 5;
        write('\nColumn is invalid. Try again.\n'),
        readColumn(Column) 
    ).

%Gets direction of movement from user and validates it
readDirection(Direction) :-
    write('1 - North   2 - West   3 - East   4 - South \n'),
    write('5 - Northeast   6 - Northwest   7 - Southeast   8 - Southwest \n'),
    write('Enter the desired direction'),
    read(Input),
    (
            Input = 1 -> Direction is 1;
            Input = 2 -> Direction is 2;
            Input = 3 -> Direction is 3;
            Input = 4 -> Direction is 4;
            Input = 5 -> Direction is 5;
            Input = 6 -> Direction is 6;
            Input = 7 -> Direction is 7;
            Input = 8 -> Direction is 8;
            write('Direction is invalid. Try again.\n'),
            readDirection(Direction) 
    ).
