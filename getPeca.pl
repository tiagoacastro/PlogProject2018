board( [
            [1, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0]
        ]).

getPeca(Linha, Coluna, Board, Peca) :-
    getLinha(Linha, Board, Row),
    getColuna(Coluna, Row, Peca).

getLinha(1, [Linha|_], Linha).

getLinha(N, [_|Resto], Linha) :-
    N>1,
    Previous is N-1,
    getLinha(Previous, Resto, Linha).

getColuna(1, [Coluna|_], Coluna).

getColuna(N, [_|Resto], Coluna) :-
    N>1,
    Previous is N-1,
    getColuna(Previous, Resto, Coluna).

/*
    board(Board), getPeca(1,1,Board,Peca).
*/