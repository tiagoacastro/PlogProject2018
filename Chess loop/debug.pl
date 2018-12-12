print_list([]).
print_list([A|B]) :-
    format("~w", A),
    print_list(B).