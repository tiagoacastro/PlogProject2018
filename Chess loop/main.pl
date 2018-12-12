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
solve(Npieces, Nrows, Ncols, Type1, Type2, Types, Pos) :-
    length(Types, Npieces),
    length(Pos, Npieces),
    Npos is Nrows * Ncols, domain(Pos, 1, Npos),
    all_distinct(Pos),
    setTypes(Types, Type1, Type2, Npieces),
    setup(Types, Pos, Ncols),
    labeling([ff], Pos).   

%Sets up the iteration function
setup(Types, [P1|Pr], Ncols):-
    iterate(Types, [P1|Pr], Ncols, P1).
    
%Base case for the iteration of the list, last pieces eats the first one
iterate([H|[]], [P1|[]], Ncols, Fp):-
    R1 #= P1 / Ncols,
    R2 #= Fp / Ncols,
    C1 #= P1 mod Ncols,
    C2 #= Fp mod Ncols,
    eat(H, R1, R2, C1, C2),
    write('\n').
    
%General case for the iteration of the list, a piece eats the next one
iterate([H|Tr], [P1,P2|Pr], Ncols, Fp):-
    R1 #= div(P1, Ncols),
    R2 #= div(P2, Ncols),
    C1 #= mod(P1, Ncols),
    C2 #= mod(P2, Ncols),
    eat(H, R1, R2, C1, C2),
    write('\n'),
    iterate(Tr, [P2|Pr], Ncols, Fp).
    
%King move
eat(1, R1, R2, C1, C2):-
    write('1'),
    ((R2 #= R1+1 #/\ (C2 #= C1 #\/ (C2 #= C1+1 #\/ C2 #= C1-1))) #\/
    (R2 #= R1 #/\ (C2 #= C1+1 #\/ C2 #= C1-1)) #\/
    (R2 #= R1-1 #/\ (C2 #= C1 #\/ (C2 #= C1+1 #\/ C2 #= C1-1)))).

%Queen move
eat(2, R1, R2, C1, C2).

%Rook move
eat(3, R1, R2, C1, C2):-
    write('3'),
    ((R2 #= R1 #/\ C2 #\= C1) #\/ 
    (R2 #\= R1 #/\ C2 #= C1)).
    
%Bishop move
eat(4, R1, R2, C1, C2). 

%Knight move
eat(5, R1, R2, C1, C2):-
    write('5'),
    ((R2 #= R1+2 #/\ (C2 #= C1+1 #\/ C2 #= C1-1)) #\/ 
    (R2 #= R1-2 #/\ (C2 #= C1+1 #\/ C2 #= C1-1)) #\/ 
    (C2 #= C1+2 #/\ (R2 #= R1+1 #\/ R2 #= R1-1)) #\/ 
    (C2 #= C1-2 #/\ (R2 #= R1+1 #\/ R2 #= R1-1))).

setTypes([], _, _, 0).

%Alternates the types of the ordered pieces, so that after the move of a type1 piece, a type2 move follows and vice-versa.
setTypes([H|R], Type1, Type2, Npieces):-
    Type is Npieces mod 2,
    Next is Npieces - 1,
    giveType(H, Type1, Type2, Type),
    setTypes(R, Type1, Type2, Next).

%Sets the type1 for the even pieces
giveType(Type1, Type1, Type2, 0).

%Sets the type2 for the odd pieces
giveType(Type2, Type1, Type2, 1).