% knight's path

move(1,6).
move(1,8).
move(3,4).
move(3,8).
move(6,1).
move(6,7).
move(8,1).
move(8,3).
move(2,7).
move(2,9).
move(4,3).
move(4,9).
move(7,2).
move(7,6).
move(9,2).
move(9,4).

path(X,X,_).
path(X,Z,S) :-
		move(X,Y),
		member(Y,S),
		path(Y,Z,[Y|S]).

