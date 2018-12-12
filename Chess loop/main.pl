:- use_module(library(clpfd)).
:- include('debug.pl').

/*
nqueens(N,D,Cols):-
    length(Cols,N),
    domain(Cols,1,D),
    constrain(Cols),
    all_distinct(Cols), % Redundante mas diminui o tempo de resolução
    labeling([ff],Cols).

constrain([]).

constrain([H | RCols]):-
    safe(H,RCols,1),
    constrain(RCols).

safe(_,[],_).

safe(X,[Y | T], K) :-
    noattack(X,Y,K),
    K1 is K + 1,
    safe(X,T,K1).

noattack(X,Y,K) :-
    X #\= Y,
    X + K #\= Y,
    X - K #\= Y.
   
nrooks(N, Cols) :-
    length(Cols,N),
    domain(Cols,1,N),
    all_distinct(Cols),
    labeling([ff], Cols).

nbishop(N, Rows, Cols) :-
    length(Cols, N),
    length(Rows, N),
    domain(Cols, 1, N),
    domain(Rows, 1, N).
    constBishop(Cols, Rows).
*/

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
    setup(Types, Rows, Cols),
    labeling([ff], Rows),
    labeling([ff], Cols).   

%prepare base case
prepare(Ncols, [], [], []).

%makes a list of the positions in order to later call all_distinct on it
prepare(Ncols, [R|Rr], [C|Cr], [P|Pr]):-
    P #= R * Ncols + C,
    prepare(Ncols, Rr, Cr, Pr).

%Sets up the iteration function
setup(Types, [R1|Rr], [C1|Cr]):-
    iterate(Types, [R1|Rr], [C1|Cr], R1, C1).
    
%Base case for the iteration of the list, last pieces eats the first one
iterate([H|[]], [R1|[]], [C1|[]], Fr, Fc):-
    eat(H, R1, Fr, C1, Fc).
    
%General case for the iteration of the list, a piece eats the next one
iterate([H|Tr], [R1,R2|Rr], [C1,C2|Cr], Fr, Fc):-
    eat(H, R1, R2, C1, C2),
    iterate(Tr, [R2|Rr], [C2|Cr], Fr, Fc).
    
%King move
eat(1, R1, R2, C1, C2):-
    ((R2 #= R1+1 #/\ (C2 #= C1 #\/ (C2 #= C1+1 #\/ C2 #= C1-1))) #\/
    (R2 #= R1 #/\ (C2 #= C1+1 #\/ C2 #= C1-1)) #\/
    (R2 #= R1-1 #/\ (C2 #= C1 #\/ (C2 #= C1+1 #\/ C2 #= C1-1)))).

%Queen move
eat(2, R1, R2, C1, C2).

%Rook move
eat(3, R1, R2, C1, C2):-
    ((R2 #= R1 #/\ C2 #\= C1) #\/ 
    (R2 #\= R1 #/\ C2 #= C1)).
    
%Bishop move
eat(4, R1, R2, C1, C2). 

%Knight move
eat(5, R1, R2, C1, C2):-
    ((R2 #= R1+2 #/\ (C2 #= C1+1 #\/ C2 #= C1-1)) #\/ 
    (R2 #= R1-2 #/\ (C2 #= C1+1 #\/ C2 #= C1-1)) #\/ 
    (C2 #= C1+2 #/\ (R2 #= R1+1 #\/ R2 #= R1-1)) #\/ 
    (C2 #= C1-2 #/\ (R2 #= R1+1 #\/ R2 #= R1-1))).

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