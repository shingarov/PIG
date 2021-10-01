/*
 * The Emden-Chen-Parker Term Rewriter.
 */


:- op(500, xfx, ↠).

fuck(X, Y) :- X ↠ Y.

a ↠ b.


:- op(500, xfx, =>).
e(X,Y) :- e2(X,Y).
e2(X,Z) :- e1(X,Y), e2(Y,Z).
e2(X,X).

/* Substitutivity: */
e1(transfer(A1,B), transfer(A2,B)) :- e1(A1,A2).
e1(transfer(A,B1), transfer(A,B2)) :- e1(B1,B2).

/* Rewrite Axiom */
e1(X,Y) :- (X => Y).

