%
% The van Emden rewriter for combinatory logic.
%

:- op(1000, xfx, →).
:- op(1000, xfx, ↠).

X ↠ Y :- e2(X, Y).
e2(X,Z) :- e1(X,Y), e2(Y,Z).
e2(X,X).



/* Rewrite Axiom */
e1(X,Y) :- (X → Y).




% Combinator definitions
i - X → X.
k - S - _ → S.
s - X - Y - Z → (X-Z)-(Y-Z).

% Secondary
b → s-(k-s)-k.
c → s - (b-b-s) - (k-k).
w → s - s - (k-i).

% Substitutivity:
X-Y → X1-Y :- X→X1. % left application
X-Y → X-Y1 :- Y→Y1. % right application
