% prolog

:- use_module(library(clpfd)).

list_length([], 0).
list_length([ _ | Tail], N) :-
N #> 0,
N #= N0 + 1,
list_length(Tail, N0).
