:- use_module(library(clpfd)).
:- use_module(library(random)).
:- include('utilities.pl').

stats(Selection, Choice, Direction) :-
    write('\33\[2J'),

    statistics(walltime, [Start1,_]),
    solve_stats(2, 2, 4, 1, 3, Selection, Choice, Direction),
    statistics(walltime, [End1,_]),

    statistics(walltime, [Start2,_]),
    solve_stats(3, 4, 5, 1, 3, Selection, Choice, Direction), 
    statistics(walltime, [End2,_]),

    statistics(walltime, [Start3,_]),
    solve_stats(4, 4, 6, 1, 4, Selection, Choice, Direction), 
    statistics(walltime, [End3,_]),

    statistics(walltime, [Start4,_]),
    solve_stats(4, 4, 7, 2, 5, Selection, Choice, Direction), 
    statistics(walltime, [End4,_]),

    Time1 is End1 - Start1, Time2 is End2 -  Start2, Time3 is End3 - Start3, Time4 is End4 - Start4,  
    format('Options: ~w, ', Selection),
    format('~w, ', Choice),
    format('~w\n', Direction),
    format('2 rooks and kings on a 2x4: ~4d s\n', [Time1]),
    format('3 rooks and kings on a 4x5: ~4d s\n', [Time2]),
    format('4 bischop and kings on a 4x6: ~4d s\n', [Time3]),
    format('5 knights and rooks on 3x8: ~4d s\n', [Time4]).


random_problem(Nrows, Ncols, Type1String, Type2String, Npieces) :-
    repeat,
    random(1, 6, T1), random(1, 6, T2),
    check_types(T1, T2, Type1, Type2),
    convert_to_string(Type1, Type1String), convert_to_string(Type2, Type2String),
    random(2, 6, Nrows), random(2, 6, Ncols), random(2, 4, Npieces),
    findall(Board, aux(Npieces, Nrows, Ncols, Type1, Type2, Board), X1), 
    erase_duplicates(X1, Fixed, 1, Npieces),
    length(Fixed, 1),
    get_first_board(X1, FirstBoard),
    display_board(FirstBoard, Ncols).

erase_duplicates(Out, Out, A, A).

erase_duplicates(In, Out, N, A):-
    erase_simetrics(In, Temp),
    offset(Temp, List),
    Next is N+1,
    erase_duplicates(List, Out, Next, A).

erase_simetrics([H|T], Out):-
    reverse_matrix(H, [], Rev),
    delete(T, Rev, T1),
    transpose(H, Trans),
    delete(T1, Trans, T2),
    transpose(Rev, [], RevTrans),
    delete(T2, RevTrans, Out).

get_first_board([H|[]], H).

check_types(T1, T2, T1, T2) :- 
    T1 \= T2.

check_types(T1, T2, Type1, Type2) :- 
    T1 = T2,
    random(1, 3, NewT1),
    random(1, 3, NewT2),
    check_types(NewT1, NewT2, Type1, Type2).

convert_to_string(1, 'King').
convert_to_string(2, 'Queen').
convert_to_string(3, 'Rook').
convert_to_string(4, 'Bishop').
convert_to_string(5, 'Knight').

%Solves the position of the pieces
solve(N, Nrows, Ncols, Type1, Type2, Res) :-
    Npieces is N * 2,
    length(Types, Npieces), length(Rows, Npieces), length(Cols, Npieces), length(Res, Npieces),
    domain(Rows, 1, Nrows), domain(Cols, 1, Ncols),
    prepare(Ncols, Rows, Cols, Res),
    set_types(Types, Type1, Type2, Npieces),
    get_min_max(Nrows, Ncols, Min, Max),
    setup(Types, Rows, Cols, Min, Max),
    once(labeling([ff], Res)),
    display_solution(Nrows, Ncols, Types, Rows, Cols).

%Solve used when generating a random problem. Initializes board but does not displays it.
aux(N, Nrows, Ncols, Type1, Type2, Board) :-
    Npieces #= N * 2,
    length(Types, Npieces), length(Rows, Npieces), length(Cols, Npieces), length(Res, Npieces),
    domain(Rows, 1, Nrows), domain(Cols, 1, Ncols),
    prepare(Ncols, Rows, Cols, Res),
    set_types(Types, Type1, Type2, Npieces),
    get_min(Nrows, Ncols, Min),
    get_max(Nrows, Ncols, Max),
    setup(Types, Rows, Cols, Min, Max),
    labeling([ff], Res),
    length(IntBoard, Nrows),
    init_board(IntBoard, Ncols),
    fill_board(IntBoard, Types, Rows, Cols, Board).

%Solve used when getting execution times. Does not initializes board.
solve_stats(N, Nrows, Ncols, Type1, Type2, Selection, Choice, Direction) :-
    Npieces is N * 2,
    length(Types, Npieces), length(Rows, Npieces), length(Cols, Npieces), length(Res, Npieces),
    domain(Rows, 1, Nrows), domain(Cols, 1, Ncols),
    prepare(Ncols, Rows, Cols, Res),
    set_types(Types, Type1, Type2, Npieces),
    get_min_max(Nrows, Ncols, Min, Max),
    setup(Types, Rows, Cols, Min, Max),
    once(labeling([Selection, Choice, Direction], Res)).

%Prepare base case
prepare(_, [], [], []).

%Makes a list of the absolute positions
prepare(Ncols, [R|Rr], [C|Cr], [P|Pr]):-
    P #= (R-1) * Ncols + C,
    prepare(Ncols, Rr, Cr, Pr).

%Sets up the iteration function
setup(Types, [R1|Rr], [C1|Cr], Min, Max):-
    iterate(Types, [R1|Rr], [C1|Cr], R1, C1, Min, Max, [R1|Rr], [C1|Cr], 1).
    
%Base case for the iteration of the list, last pieces eats the first one
iterate([H|[]], [R1|[]], [C1|[]], Fr, Fc, Min, Max, Rows, Columns, Index):-
    eat(H, R1, Fr, C1, Fc, Min, Max),
    Index2 is 1,
    restrict(H, R1, C1, Rows, Columns, Index, Index2, Min, Max, 1, Fr, Fc).
    
%General case for the iteration of the list, a piece eats the next one
iterate([H|Tr], [R1,R2|Rr], [C1,C2|Cr], Fr, Fc, Min, Max, Rows, Columns, Index):-
    eat(H, R1, R2, C1, C2, Min, Max),
    Index2 is Index + 1,
    restrict(H, R1, C1, Rows, Columns, Index, Index2, Min, Max, 1, R2, C2),
    iterate(Tr, [R2|Rr], [C2|Cr], Fr, Fc, Min, Max, Rows, Columns, Index2).

%Restrict base case
restrict(_, _, _, [], [], _, _, _, _, _, _, _).

%Restrict when the analyzed piece is the one attacking (do nothing)
restrict(H, R1, C1, [_|R], [_|C], Index1, Index2, Min, Max, N, Pr, Pc):-
    N = Index1,
    New is N + 1,
    restrict(H, R1, C1, R, C, Index1, Index2, Min, Max, New, Pr, Pc), !.

%Restrict when the analyzed piece is the one being attacked (do nothing)
restrict(H, R1, C1, [_|R], [_|C], Index1, Index2, Min, Max, N, Pr, Pc):-
    N = Index2,
    New is N + 1,
    restrict(H, R1, C1, R, C, Index1, Index2, Min, Max, New, Pr, Pc), !.

%Restrict when the analyzed piece is foreign (not involved in the current play)
%Makes so the foreign piece can't be in positions attackable by the current atacker
restrict(H, R1, C1, [R2|R], [C2|C], Index1, Index2, Min, Max, N, Pr, Pc):-
    dont_eat(H, R1, R2, C1, C2, Min, Max, Pr, Pc),
    New is N + 1,
    restrict(H, R1, C1, R, C, Index1, Index2, Min, Max, New, Pr, Pc).
    
%King move
eat(1, R1, R2, C1, C2, _, _):-
    (R2 #= R1+1 #/\ (C2 #= C1 #\/ (C2 #= C1+1 #\/ C2 #= C1-1))) #\/
    (R2 #= R1 #/\ (C2 #= C1+1 #\/ C2 #= C1-1)) #\/
    (R2 #= R1-1 #/\ (C2 #= C1 #\/ (C2 #= C1+1 #\/ C2 #= C1-1))).

%Queen move
eat(2, R1, R2, C1, C2, Min, Max):-
    domain([X1],1,Max),
    domain([X2],1,Min),
    ((R2 #= R1 #/\ C2 #= C1+X1) #\/ 
    (R2 #= R1 #/\ C2 #= C1-X1) #\/ 
    (R2 #= R1+X1 #/\ C2 #= C1) #\/ 
    (R2 #= R1-X1 #/\ C2 #= C1) #\/
    (R2 #= R1+X2 #/\ C2 #= C1+X2) #\/ 
    (R2 #= R1+X2 #/\ C2 #= C1-X2) #\/ 
    (R2 #= R1-X2 #/\ C2 #= C1+X2) #\/ 
    (R2 #= R1-X2 #/\ C2 #= C1-X2)).

%Rook move
eat(3, R1, R2, C1, C2, _, Max):-
    domain([X],1,Max),
    ((R2 #= R1 #/\ C2 #= C1+X) #\/ 
    (R2 #= R1 #/\ C2 #= C1-X) #\/ 
    (R2 #= R1+X #/\ C2 #= C1) #\/ 
    (R2 #= R1-X #/\ C2 #= C1)).

%Bishop move
eat(4, R1, R2, C1, C2, Min, _):-
    domain([X],1,Min),
    ((R2 #= R1+X #/\ C2 #= C1+X) #\/ 
    (R2 #= R1+X #/\ C2 #= C1-X) #\/ 
    (R2 #= R1-X #/\ C2 #= C1+X) #\/ 
    (R2 #= R1-X #/\ C2 #= C1-X)).

%Knight move
eat(5, R1, R2, C1, C2, _, _):-
    (R2 #= R1+2 #/\ (C2 #= C1+1 #\/ C2 #= C1-1)) #\/ 
    (R2 #= R1-2 #/\ (C2 #= C1+1 #\/ C2 #= C1-1)) #\/ 
    (C2 #= C1+2 #/\ (R2 #= R1+1 #\/ R2 #= R1-1)) #\/ 
    (C2 #= C1-2 #/\ (R2 #= R1+1 #\/ R2 #= R1-1)). 

%Restrictions applied to the foreign pieces when the attack is done by a King
dont_eat(1, R1, R2, C1, C2, _, _, _, _):-
    R2 #> R1+1 #\/
    R2 #< R1-1 #\/
    C2 #> C1+1 #\/
    C2 #< C1-1.

%Restrictions applied to the foreign pieces when the attack is done by a Queen
dont_eat(2, R1, R2, C1, C2, Min, Max, R, C):-
    (R2 #= R1 #/\ C2 #= C1) #<=> 0,
    restrict_N(R1, R2, C1, C2, 1, Max, R, C),
    restrict_E(R1, R2, C1, C2, 1, Max, R, C),
    restrict_S(R1, R2, C1, C2, 1, Max, R, C),
    restrict_W(R1, R2, C1, C2, 1, Max, R, C),
    restrict_NE(R1, R2, C1, C2, 1, Min, R, C),
    restrict_SE(R1, R2, C1, C2, 1, Min, R, C),
    restrict_SW(R1, R2, C1, C2, 1, Min, R, C),
    restrict_NW(R1, R2, C1, C2, 1, Min, R, C).

%Restrictions applied to the foreign pieces when the attack is done by a Rook
dont_eat(3, R1, R2, C1, C2, _, Max, R, C):-
    (R2 #= R1 #/\ C2 #= C1) #<=> 0,
    restrict_N(R1, R2, C1, C2, 1, Max, R, C),
    restrict_E(R1, R2, C1, C2, 1, Max, R, C),
    restrict_S(R1, R2, C1, C2, 1, Max, R, C),
    restrict_W(R1, R2, C1, C2, 1, Max, R, C).

%Restrictions applied to the foreign pieces when the attack is done by a Bishop
dont_eat(4, R1, R2, C1, C2, Min, _, R, C):-
    (R2 #= R1 #/\ C2 #= C1) #<=> 0,
    restrict_NE(R1, R2, C1, C2, 1, Min, R, C),
    restrict_SE(R1, R2, C1, C2, 1, Min, R, C),
    restrict_SW(R1, R2, C1, C2, 1, Min, R, C),
    restrict_NW(R1, R2, C1, C2, 1, Min, R, C).   

%Restrictions applied to the foreign pieces when the attack is done by a Knight
dont_eat(5, R1, R2, C1, C2, _, _, _, _):-
    (R2 #= R1 #/\ C2 #= C1) #<=> 0,
    ((R2 #= R1+2 #/\ (C2 #= C1+1 #\/ C2 #= C1-1)) #\/ 
    (R2 #= R1-2 #/\ (C2 #= C1+1 #\/ C2 #= C1-1)) #\/ 
    (C2 #= C1+2 #/\ (R2 #= R1+1 #\/ R2 #= R1-1)) #\/ 
    (C2 #= C1-2 #/\ (R2 #= R1+1 #\/ R2 #= R1-1))) #<=> 0.

%Restrict_N base case
restrict_N(_, _, _, _, M, M, _, _).

%Restricts N direction of the attacker as forbidden for all foreign pieces
%R1 C1 - atacking piece position
%R2 C2 - avaliated position
%R C - atacked piece position
restrict_N(R1, R2, C1, C2, N, M, R, C):-
    N < M,
    ((#\(C #= C1 #/\ R #< R1 #/\ R1-N #< R)) #=> ((R2 #= R1-N #/\ C2 #= C1) #<=> 0)),
    Next is N + 1,
    restrict_N(R1, R2, C1, C2, Next, M, R, C).

%Restrict_E base case
restrict_E(_, _, _, _, M, M, _, _).

%Restricts E direction of the attacker as forbidden for all foreign pieces
%R1 C1 - atacking piece position
%R2 C2 - avaliated position
%R C - atacked piece position
restrict_E(R1, R2, C1, C2, N, M, R, C):-
    N < M,
    ((#\(R #= R1 #/\ C #> C1 #/\ C1+N #> C)) #=> ((R2 #= R1 #/\ C2 #= C1+N) #<=> 0)),
    Next is N + 1,
    restrict_E(R1, R2, C1, C2, Next, M, R, C).

%Restrict_S base case
restrict_S(_, _, _, _, M, M, _, _).

%Restricts S direction of the attacker as forbidden for all foreign pieces
%R1 C1 - atacking piece position
%R2 C2 - avaliated position
%R C - atacked piece position
restrict_S(R1, R2, C1, C2, N, M, R, C):-
    N < M,
    ((#\(C #= C1 #/\ R #> R1 #/\ R1+N #> R)) #=> ((R2 #= R1+N #/\ C2 #= C1) #<=> 0)),
    Next is N + 1,
    restrict_S(R1, R2, C1, C2, Next, M, R, C).

%Restrict_W base case
restrict_W(_, _, _, _, M, M, _, _).

%Restricts W direction of the attacker as forbidden for all foreign pieces
%R1 C1 - atacking piece position
%R2 C2 - avaliated position
%R C - atacked piece position
restrict_W(R1, R2, C1, C2, N, M, R, C):-
    N < M,
    ((#\(R #= R1 #/\ C #< C1 #/\ C1-N #< C)) #=> ((R2 #= R1 #/\ C2 #= C1-N) #<=> 0)),
    Next is N + 1,
    restrict_W(R1, R2, C1, C2, Next, M, R, C).

%Restrict_NE base case
restrict_NE(_, _, _, _, M, M, _, _).

%Restricts NE diagonal of the attacker as forbidden for all foreign pieces
%R1 C1 - atacking piece position
%R2 C2 - avaliated position
%R C - atacked piece position
restrict_NE(R1, R2, C1, C2, N, M, R, C):-
    N < M,
    ((#\(R #< R1 #/\ C #> C1 #/\ C-C1 #= R1-R #/\ C1+N #> C)) #=> ((R2 #= R1-N #/\ C2 #= C1+N) #<=> 0)),
    Next is N + 1,
    restrict_NE(R1, R2, C1, C2, Next, M, R, C).

%Restrict_SE base case
restrict_SE(_, _, _, _, M, M, _, _).

%Restricts SE diagonal of the attacker as forbidden for all foreign pieces
%R1 C1 - atacking piece position
%R2 C2 - avaliated position
%R C - atacked piece position
restrict_SE(R1, R2, C1, C2, N, M, R, C):-
    N < M,
    ((#\(R #> R1 #/\ C #> C1 #/\ C-C1 #= R-R1 #/\ C1+N #> C)) #=> ((R2 #= R1+N #/\ C2 #= C1+N) #<=> 0)),
    Next is N + 1,
    restrict_SE(R1, R2, C1, C2, Next, M, R, C).

%Restrict_SW base case
restrict_SW(_, _, _, _, M, M, _, _).

%Restricts SW diagonal of the attacker as forbidden for all foreign pieces
%R1 C1 - atacking piece position
%R2 C2 - avaliated position
%R C - atacked piece position
restrict_SW(R1, R2, C1, C2, N, M, R, C):-
    N < M,
    ((#\(R #> R1 #/\ C #< C1 #/\ C1-C #= R-R1 #/\ C1-N #< C)) #=> ((R2 #= R1+N #/\ C2 #= C1-N) #<=> 0)),
    Next is N + 1,
    restrict_SW(R1, R2, C1, C2, Next, M, R, C).

%Restrict_NW base case
restrict_NW(_, _, _, _, M, M, _, _).

%Restricts NW diagonal of the attacker as forbidden for all foreign pieces
%R1 C1 - atacking piece position
%R2 C2 - avaliated position
%R C - atacked piece position
restrict_NW(R1, R2, C1, C2, N, M, R, C):-
    N < M,
    ((#\(R #< R1 #/\ C #< C1 #/\ C1-C #= R1-R #/\ C1-N #< C)) #=> ((R2 #= R1-N #/\ C2 #= C1-N) #<=> 0)),
    Next is N + 1,
    restrict_NW(R1, R2, C1, C2, Next, M, R, C).

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