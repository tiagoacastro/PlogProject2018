%Gets coordinates from piece that is going to be moved and validates them.

getMovingPiece(Board, Row, Column, Player) :-
    repeat,
    readRow(Row),
    readColumn(Column),
    getPiece(Row, Column, Board, Piece),
    validatePiece(Piece, Player).
/*    (Piece = Player, write('\nPiece selected\n\n'));
    (write('\nPiece selected is invalid. Try again.\n\n'), 
    fail).*/
        
%Gets row from user, converts it to 1-5 and checks if it is valid

readRow(Row) :-
    repeat,
    write('Enter the row of the piece you want to move (1-5): '),  
    get_code(Input),
    skip_line,  
    validateRow(Input, Row) , !.

%Input = 1
validateRow(49, 1) :- !.

%Input = 2
validateRow(50, 2) :- !.

%Input = 3
validateRow(51, 3) :- !.

%Input = 4
validateRow(52, 4) :- !.

%Input = 5
validateRow(53, 5) :- !.

%Input is invalid
validateRow(_,Row) :-
    write('\nRow is invalid. Try again.\n'),
    fail.

%Gets column from user, converts it to number (1-5) and checks if it is valid

readColumn(Column) :-
    repeat,
    write('Enter the column of the piece you want to move (a-e): '),
    get_code(Input),
    skip_line,
    validateColumn(Input, Column), !.  

%Input = a
validateColumn(97, 1) :- !.

%Input = b
validateColumn(98, 2) :- !.

%Input = c
validateColumn(99, 3) :- !.

%Input = d
validateColumn(100, 4) :- !.

%Input = e
validateColumn(101, 5) :- !.

%Input is invalid
validateColumn(_, Column) :-
    write('\nColumn is invalid. Try again.\n'),
    fail.

%Checks if piece belong to current player

%Piece selected is valid
validatePiece(Piece, Player) :-
    Piece = Player,
    write('\nPiece selected\n\n').

%Piece selected is invalid
validatePiece(Piece, Player) :-
    Piece \= Player,
    write('\nPiece selected is invalid. Try again.\n\n'),
    fail.

%----------------------d-------------------------------------

%Gets direction of movement from user and validates it

readDirection(ListOfMoves, Direction) :-
    printDirections(ListOfMoves),
    repeat,
    write('Enter the desired direction: '),
    get_code(Input),
    skip_line,  
    (
        (Input = 49, sublist([1], ListOfMoves), Direction is 1);
        (Input = 50, sublist([2], ListOfMoves), Direction is 2);
        (Input = 51, sublist([3], ListOfMoves), Direction is 3);
        (Input = 52, sublist([4], ListOfMoves), Direction is 4);
        (Input = 53, sublist([5], ListOfMoves), Direction is 5);
        (Input = 54, sublist([6], ListOfMoves), Direction is 6);
        (Input = 55, sublist([7], ListOfMoves), Direction is 7);
        (Input = 56, sublist([8], ListOfMoves), Direction is 8);
        (write('\nDirection is invalid. Try again.\n'),
        fail)
    ), !.

%Displays directions. 

printDirections(ListOfMoves) :-
    write('DIRECTION\n\n'),
    printDir(1, 'North', ListOfMoves),
    printDir(2, 'West', ListOfMoves),
    printDir(3, 'East', ListOfMoves),
    printDir(4, 'South', ListOfMoves),
    write('\n'),
    printDir(5, 'Northeast', ListOfMoves),
    printDir(6, 'Northwest', ListOfMoves),
    printDir(7, 'Southeast', ListOfMoves),
    printDir(8, 'Southwest', ListOfMoves),
    write('\n').

%If a direction is in the list of valid moves, then it is displayed as valid, otherwise it is not valid

printDir(Number, Direction, ListOfMoves) :-
    (sublist([Number], ListOfMoves),
    format('~w - ', Number),
    format('~w   ', Direction));
    (format('~w not valid   ', Direction)).

%-----------------------------------------------------------
