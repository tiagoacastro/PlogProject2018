:- use_module(library(clpfd)).

nqueens(N,D,Cols):-
    length(Cols,N),
    domain(Cols,1,D),
    constrain(Cols),
    % all_distinct(Cols), % Redundante mas diminui o tempo de resolução
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

%constBishop([HRow|TRow], [HCol|TCol]) :-
    



