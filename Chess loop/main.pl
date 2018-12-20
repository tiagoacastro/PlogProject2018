:- use_module(library(clpfd)).
:- use_module(library(random)).
:- include('utilities.pl').


random_problem(Nrows, Ncols, Type1, Type2, Npieces, Pos) :-
    random(1, 6, T1),
    random(1, 6, T2),
    check_types(T1, T2, Type1, Type2),
    random(2, 10, Nrows),
    random(2, 10, Ncols),
    random(2, 5, Npieces),
    solve(Npieces, Nrows, Ncols, Type1, Type2, Pos).

check_types(T1, T2, T1, T2) :- 
    T1 \= T2.

check_types(T1, T2, Type1, Type2) :- 
    T1 = T2,
    random(1, 3, NewT1),
    random(1, 3, NewT2),
    check_types(NewT1, NewT2, Type1, Type2).

%Solves the position of the pieces
solve(N, Nrows, Ncols, Type1, Type2, Res) :-
    Npieces is N * 2,
    length(Types, Npieces), length(Rows, Npieces), length(Cols, Npieces), length(Res, Npieces),
    domain(Rows, 1, Nrows), domain(Cols, 1, Ncols),
    prepare(Ncols, Rows, Cols, Res),
    set_types(Types, Type1, Type2, Npieces),
    get_min(Nrows, Ncols, Min),
    setup(Types, Rows, Cols, Min),
    labeling([ff], Res),
    display_solution(Nrows, Ncols, Types, Rows, Cols).

%Prepare base case
prepare(_, [], [], []).

%Makes a list of the absolute positions
prepare(Ncols, [R|Rr], [C|Cr], [P|Pr]):-
    P #= R * Ncols + C - 4,
    prepare(Ncols, Rr, Cr, Pr).

%Sets up the iteration function
setup(Types, [R1|Rr], [C1|Cr], Min):-
    iterate(Types, [R1|Rr], [C1|Cr], R1, C1, Min, [R1|Rr], [C1|Cr], 1).
    
%Base case for the iteration of the list, last pieces eats the first one
iterate([H|[]], [R1|[]], [C1|[]], Fr, Fc, Min, Rows, Columns, Index):-
    eat(H, R1, Fr, C1, Fc, Min),
    Index2 is 1,
    restrict(H, R1, C1, Rows, Columns, Index, Index2, Min, 1, Fr, Fc).
    
%General case for the iteration of the list, a piece eats the next one
iterate([H|Tr], [R1,R2|Rr], [C1,C2|Cr], Fr, Fc, Min, Rows, Columns, Index):-
    eat(H, R1, R2, C1, C2, Min),
    Index2 is Index + 1,
    restrict(H, R1, C1, Rows, Columns, Index, Index2, Min, 1, R2, C2),
    iterate(Tr, [R2|Rr], [C2|Cr], Fr, Fc, Min, Rows, Columns, Index2).

%Restrict base case
restrict(_, _, _, [], [], _, _, _, _, _, _).

%Restrict when the analyzed piece is the one attacking (do nothing)
restrict(H, R1, C1, [_|R], [_|C], Index1, Index2, Min, N, _, _):-
    N = Index1,
    New is N + 1,
    restrict(H, R1, C1, R, C, Index1, Index2, Min, New, _, _), !.

%Restrict when the analyzed piece is the one being attacked (do nothing)
restrict(H, R1, C1, [_|R], [_|C], Index1, Index2, Min, N, _, _):-
    N = Index2,
    New is N + 1,
    restrict(H, R1, C1, R, C, Index1, Index2, Min, New, _, _), !.

%Restrict when the analyzed piece is foreign (not involved in the current play)
%Makes so the foreign piece can't be in positions attackable by the current atacker
restrict(H, R1, C1, [R2|R], [C2|C], Index1, Index2, Min, N, Pr, Pc):-
    dont_eat(H, R1, R2, C1, C2, Min, Pr, Pc),
    New is N + 1,
    restrict(H, R1, C1, R, C, Index1, Index2, Min, New, Pr, Pc).
    
%King move
eat(1, R1, R2, C1, C2, _):-
    (R2 #= R1+1 #/\ (C2 #= C1 #\/ (C2 #= C1+1 #\/ C2 #= C1-1))) #\/
    (R2 #= R1 #/\ (C2 #= C1+1 #\/ C2 #= C1-1)) #\/
    (R2 #= R1-1 #/\ (C2 #= C1 #\/ (C2 #= C1+1 #\/ C2 #= C1-1))).

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
eat(3, R1, R2, C1, C2, _):-
    (R2 #= R1 #/\ C2 #\= C1) #\/ 
    (R2 #\= R1 #/\ C2 #= C1).

%Bishop move
eat(4, R1, R2, C1, C2, Min):-
    domain([X],1,Min),
    ((R2 #= R1+X #/\ C2 #= C1+X) #\/ 
    (R2 #= R1+X #/\ C2 #= C1-X) #\/ 
    (R2 #= R1-X #/\ C2 #= C1+X) #\/ 
    (R2 #= R1-X #/\ C2 #= C1-X)).

%Knight move
eat(5, R1, R2, C1, C2, _):-
    (R2 #= R1+2 #/\ (C2 #= C1+1 #\/ C2 #= C1-1)) #\/ 
    (R2 #= R1-2 #/\ (C2 #= C1+1 #\/ C2 #= C1-1)) #\/ 
    (C2 #= C1+2 #/\ (R2 #= R1+1 #\/ R2 #= R1-1)) #\/ 
    (C2 #= C1-2 #/\ (R2 #= R1+1 #\/ R2 #= R1-1)). 

%Restrictions applied to the foreign pieces when the attack is done by a King
dont_eat(1, R1, R2, C1, C2, _, _, _):-
    R2 #> R1+1 #\/
    R2 #< R1-1 #\/
    C2 #> C1+1 #\/
    C2 #< C1-1.

%Restrictions applied to the foreign pieces when the attack is done by a Queen
dont_eat(2, R1, R2, C1, C2, Min, R, C):-
    R2 #\= R1, 
    C2 #\= C1,
    V #= 0,
    restrict_NE(R1, R2, C1, C2, 1, Min, R, C, V),
    restrict_SE(R1, R2, C1, C2, 1, Min, R, C, V),
    restrict_SW(R1, R2, C1, C2, 1, Min, R, C, V),
    restrict_NW(R1, R2, C1, C2, 1, Min, R, C, V).

%Restrictions applied to the foreign pieces when the attack is done by a Rook
dont_eat(3, R1, R2, C1, C2, _, _, _):-
    R2 #\= R1,
    C2 #\= C1.

%Restrictions applied to the foreign pieces when the attack is done by a Bishop
dont_eat(4, R1, R2, C1, C2, Min, R, C):-
    (R2 #= R1 #/\ C2 #= C1) #<=> 0,
    V #= 0,
    restrict_NE(R1, R2, C1, C2, 1, Min, R, C, V),
    restrict_SE(R1, R2, C1, C2, 1, Min, R, C, V),
    restrict_SW(R1, R2, C1, C2, 1, Min, R, C, V),
    restrict_NW(R1, R2, C1, C2, 1, Min, R, C, V).   

%Restrictions applied to the foreign pieces when the attack is done by a Knight
dont_eat(5, R1, R2, C1, C2, _, _, _):-
    (R2 #= R1 #/\ C2 #= C1) #<=> 0,
    ((R2 #= R1+2 #/\ (C2 #= C1+1 #\/ C2 #= C1-1)) #\/ 
    (R2 #= R1-2 #/\ (C2 #= C1+1 #\/ C2 #= C1-1)) #\/ 
    (C2 #= C1+2 #/\ (R2 #= R1+1 #\/ R2 #= R1-1)) #\/ 
    (C2 #= C1-2 #/\ (R2 #= R1+1 #\/ R2 #= R1-1))) #<=> 0.

%Restrict_NE base case
restrict_NE(_, _, _, _, Min, Min, _, _, _).

%Restricts NE diagonal of the attacker as forbidden for all foreign pieces
restrict_NE(R1, R2, C1, C2, N, Min, R, C, V):-
    N < Min,
    ((V #= 0) #=> (((R2 #= R1-N #/\ C2 #= C1+N) #<=> 0) #/\ ((R #= R1-N #/\ C #= C1+N) #<=> X))),
    ((V #= 1) #=> (X #= 1)),
    Next is N + 1,
    restrict_NE(R1, R2, C1, C2, Next, Min, R, C, X).

%Restrict_SE base case
restrict_SE(_, _, _, _, Min, Min, _, _, _).

%Restricts SE diagonal of the attacker as forbidden for all foreign pieces
restrict_SE(R1, R2, C1, C2, N, Min, R, C, V):-
    N < Min,
    ((V #= 0) #=> (((R2 #= R1+N #/\ C2 #= C1+N) #<=> 0) #/\ ((R #= R1+N #/\ C #= C1+N) #<=> X))),
    ((V #= 1) #=> (X #= 1)),
    Next is N + 1,
    restrict_SE(R1, R2, C1, C2, Next, Min, R, C, X).

%Restrict_SW base case
restrict_SW(_, _, _, _, Min, Min, _, _, _).

%Restricts SW diagonal of the attacker as forbidden for all foreign pieces
restrict_SW(R1, R2, C1, C2, N, Min, R, C, V):-
    N < Min,
    ((V #= 0) #=> (((R2 #= R1+N #/\ C2 #= C1-N) #<=> 0) #/\ ((R #= R1+N #/\ C #= C1-N) #<=> X))),
    ((V #= 1) #=> (X #= 1)),
    Next is N + 1,
    restrict_SW(R1, R2, C1, C2, Next, Min, R, C, X).

%Restrict_NW base case
restrict_NW(_, _, _, _, Min, Min, _, _, _).

%Restricts NW diagonal of the attacker as forbidden for all foreign pieces
restrict_NW(R1, R2, C1, C2, N, Min, R, C, V):-
    N < Min,
    ((V #= 0) #=> (((R2 #= R1-N #/\ C2 #= C1-N) #<=> 0) #/\ ((R #= R1-N #/\ C #= C1-N) #<=> X))),
    ((V #= 1) #=> (X #= 1)),
    Next is N + 1,
    restrict_NW(R1, R2, C1, C2, Next, Min, R, C, X).

%Set_types base case
set_types([], _, _, 0).

%Alternates the types of the ordered pieces, so that after the move of a type1 piece, a type2 move follows and vice-versa.
set_types([H|R], Type1, Type2, Npieces):-
    Type is Npieces mod 2,
    Next is Npieces - 1,
    give_type(H, Type1, Type2, Type),
    set_types(R, Type1, Type2, Next).

%Sets the type1 for the even pieces
give_type(Type1, Type1, _, 0).

%Sets the type2 for the odd pieces
give_type(Type2, _, Type2, 1).

%Creates empty board and fills it with pieces in the correct positions
display_solution(Nrows, Ncols, Types, Rows, Cols) :-
    length(IntBoard, Nrows),
    init_board(IntBoard, Ncols),
    fill_board(IntBoard, Types, Rows, Cols, Board),
    display_board(Board, Ncols).

%Base case
init_board([], _).

%For each row, creates an array with length = number of columns
init_board([Row|T], Ncols) :-
    length(Row, Ncols),
    fillRow(Row),
    init_board(T, Ncols).

%Base vase
fillRow([]).

%Fills row with empty spaces
fillRow([' '|T]) :-
    fillRow(T).

%Base case
fill_board(Board, [], [], [], Board).

%Fills board with pieces in their respective positions
fill_board(Board, [Htypes|Ttypes], [Hrows|Trows], [Hcols|Tcols], BoardOut) :-
    changePosition(Board, Hrows, Hcols, Htypes, NewBoard),
    fill_board(NewBoard, Ttypes, Trows, Tcols, BoardOut).