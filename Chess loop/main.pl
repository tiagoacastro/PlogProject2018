:- use_module(library(clpfd)).
:- include('utilities.pl').

%Solves the position of the pieces
solve(Npieces, Nrows, Ncols, Type1, Type2, Types, Rows, Cols) :-
    length(Types, Npieces),
    length(Rows, Npieces),
    domain(Rows, 1, Nrows),
    length(Cols, Npieces),
    domain(Cols, 1, Ncols),
    length(Res, Npieces),
    prepare(Ncols, Rows, Cols, Res),
    all_distinct(Res),
    set_types(Types, Type1, Type2, Npieces),
    get_min(Nrows, Ncols, Min),
    setup(Types, Rows, Cols, Min),
    labeling([ff], Rows),
    labeling([ff], Cols),
    display_solution(Nrows, Ncols, Types, Rows, Cols).

%prepare base case
prepare(Ncols, [], [], []).

%makes a list of the positions in order to later call all_distinct on it
prepare(Ncols, [R|Rr], [C|Cr], [P|Pr]):-
    P #= R * Ncols + C,
    prepare(Ncols, Rr, Cr, Pr).

%Sets up the iteration function
setup(Types, [R1|Rr], [C1|Cr], Min):-
    iterate(Types, [R1|Rr], [C1|Cr], R1, C1, Min).
    
%Base case for the iteration of the list, last pieces eats the first one
iterate([H|[]], [R1|[]], [C1|[]], Fr, Fc, Min):-
    eat(H, R1, Fr, C1, Fc, Min).
    
%General case for the iteration of the list, a piece eats the next one
iterate([H|Tr], [R1,R2|Rr], [C1,C2|Cr], Fr, Fc, Min):-
    eat(H, R1, R2, C1, C2, Min),
    iterate(Tr, [R2|Rr], [C2|Cr], Fr, Fc, Min).
    
%King move
eat(1, R1, R2, C1, C2, Min):-
    ((R2 #= R1+1 #/\ (C2 #= C1 #\/ (C2 #= C1+1 #\/ C2 #= C1-1))) #\/
    (R2 #= R1 #/\ (C2 #= C1+1 #\/ C2 #= C1-1)) #\/
    (R2 #= R1-1 #/\ (C2 #= C1 #\/ (C2 #= C1+1 #\/ C2 #= C1-1)))).

%Queen move
eat(2, R1, R2, C1, C2, Min):-
    domain([X],1,Min),
    ((R2 #= R1+X #/\ C2 #= C1+X) #\/ 
    (R2 #= R1+X #/\ C2 #= C1-X) #\/ 
    (R2 #= R1-X #/\ C2 #= C1+X) #\/ 
    (R2 #= R1-X #/\ C2 #= C1-X) #\/ 
    (R2 #= R1 #/\ C2 #\= C1) #\/ 
    (R2 #\= R1 #/\ C2 #= C1)).

%Rook move
eat(3, R1, R2, C1, C2, Min):-
    ((R2 #= R1 #/\ C2 #\= C1) #\/ 
    (R2 #\= R1 #/\ C2 #= C1)).
    
%Bishop move
eat(4, R1, R2, C1, C2, Min):-
    domain([X],1,Min),
    ((R2 #= R1+X #/\ C2 #= C1+X) #\/ 
    (R2 #= R1+X #/\ C2 #= C1-X) #\/ 
    (R2 #= R1-X #/\ C2 #= C1+X) #\/ 
    (R2 #= R1-X #/\ C2 #= C1-X)).

%Knight move
eat(5, R1, R2, C1, C2, Min):-
    ((R2 #= R1+2 #/\ (C2 #= C1+1 #\/ C2 #= C1-1)) #\/ 
    (R2 #= R1-2 #/\ (C2 #= C1+1 #\/ C2 #= C1-1)) #\/ 
    (C2 #= C1+2 #/\ (R2 #= R1+1 #\/ R2 #= R1-1)) #\/ 
    (C2 #= C1-2 #/\ (R2 #= R1+1 #\/ R2 #= R1-1))).

%set_types base case
set_types([], _, _, 0).

%Alternates the types of the ordered pieces, so that after the move of a type1 piece, a type2 move follows and vice-versa.
set_types([H|R], Type1, Type2, Npieces):-
    Type is Npieces mod 2,
    Next is Npieces - 1,
    give_type(H, Type1, Type2, Type),
    set_types(R, Type1, Type2, Next).

%Sets the type1 for the even pieces
give_type(Type1, Type1, Type2, 0).

%Sets the type2 for the odd pieces
give_type(Type2, Type1, Type2, 1).

display_solution(Nrows, Ncols, Types, Rows, Cols) :-
    length(IntBoard, Nrows),
    init_board(IntBoard, Ncols),
    fill_board(IntBoard, Types, Rows, Cols, Board),
    display_board(Board, Ncols).

init_board([], _).

init_board([Row|T], Ncols) :-
    length(Row, Ncols),
    fillRow(Row),
    init_board(T, Ncols).

fillRow([]).

fillRow([' '|T]) :-
    fillRow(T).

fill_board(Board, [], [], [], Board).

fill_board(Board, [Htypes|Ttypes], [Hrows|Trows], [Hcols|Tcols], BoardOut) :-
    changePosition(Board, Hrows, Hcols, Htypes, NewBoard),
    fill_board(NewBoard, Ttypes, Trows, Tcols, BoardOut).

