
%Gets coordinates from piece that is going to be moved from user , and validates them.
getMovingPiece(Board, Row, Column, Player) :-
    readRow(Row),
    readColumn(Column),
    getPiece(Row, Column, Board,Piece).
%    validatePiece(Board, Row, Column, Player, Piece).
        
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
            write('Row is invalid. Try again\n'),
            readRow(Row) 
    ).
/*
validateRow('a', Row) :-
    Row is 1.

validateRow('b', Row) :-
    Row is 2.

validateRow('c', Row) :-
    Row is 3.

validateRow('d', Row) :-
    Row is 4.

validateRow('e', Row) :-
    Row is 5.

%If row is invalid
validateRow(_Row, Row) :-
    write('Row is invalid. Try again\n'),
    readRow(Input).
*/

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
        write('Column is invalid. Try again\n'),
        readColumn(Column) 
    ).

/*validateColumn(1, Column) :-    
    Column is 1.

validateColumn(2, Column) :-    
    Column is 2.

validateColumn(3, Column) :-    
    Column is 3.

validateColumn(4, Column) :-    
    Column is 4.

validateColumn(5, Column) :-    
    Column is 5.

%If column is invalid
validateColumn(_Column, Column) :-
    write('Column is invalid. Try again\n'),
    readColumn(Column).*/   

%If piece is the same as the player('b' or 'w'), return true.
validatePiece(Board, Row, Column, P, P).

%If piece is different from the player('b' or 'w'), return false.
validatePiece(Board, Row, Column, Player, Piece) :-
    write('Piece selected is invalid. Try again\n'), 
    getMovingPiece(Board, Row, Column, Player).

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
            write('Direction is invalid. Try again\n'),
            readDirection(Direction) 
    ).

/*validateDirection(1, Direction) :-
    Direction is 1.

validateDirection(2, Direction) :-
    Direction is 2.

validateDirection(3, Direction) :-
    Direction is 3.

validateDirection(4, Direction) :-
    Direction is 4.

validateDirection(5, Direction) :-
    Direction is 5.

validateDirection(6, Direction) :-
    Direction is 6.

validateDirection(7, Direction) :-
    Direction is 7.

validateDirection(8, Direction) :-
    Direction is 8.*/

%If direction is not valid
/*validateDirection(_Direction, Direction) :-
    write('Direction is invalid. Try again\n'),
    readDirection(Direction).*/
