print_list([]).
print_list([A|B]) :-
    format("~w", A),
    print_list(B).

get_min(Nrows, Ncols, Min):-
    Nrows =< Ncols,
    Min is Nrows.

get_min(Nrows, Ncols, Min):-
    Nrows > Ncols,
    Min is Ncols.