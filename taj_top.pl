/*
/usr/bin/swipl -s /home/boris/taj_top.pl -g solve_idioms -t halt
*/

:- consult(equivalence_theory).

emit(AST, Instructions) :-
   e(AST, AbstractInstructions),
   all_gprs(AbstractInstructions,GPRs),
   removeAll(GPRs,[6,7,8,9,10,11,12,13,14,15,19,20,21,22,23,24,25,26,27,28,30,31], Available),
   select_reg(AbstractInstructions, Available, [], Instructions1, _, _),
   flatten(Instructions1, Instructions).

/* Concretize one GPR: select_reg(AbstractR, Available, Bound,
              ConcreteR, StillAvailable, NewBound)  */
select_reg(gpr(N), Avail, Bound,
           gpr(N), StillAvail, Bound) :-
                   remove(N, Avail, StillAvail), !. /* R already concrete */
select_reg(tmp_gpr(T), Avail, Bound, /* T already concretized to be G */
           gpr(G), Avail, Bound) :- memberchk([T|G],Bound), !.
select_reg(tmp_gpr(T), [G|StillAvail], Bound, /* Take one available GPR */
           gpr(G), StillAvail, [[T|G] | Bound]) :- !.
/* No more scratch registers; don't know how to spill */
select_reg(tmp_gpr(_),_,_,_,_,_) :- !, fail.

/* Lists */
select_reg([], A, B, [], A, B) :- !.
select_reg([AH|AT], Available, Bound,
           [CH|CT], StillAvailable, NewBound) :-
                   select_reg(AH, Available, Bound,
                              CH, TmpAvailable, TmpBound),
                   select_reg(AT, TmpAvailable, TmpBound,
                              CT, StillAvailable, NewBound), !.

/* Terms */
select_reg(AbstractTerm, Available, Bound,
           ConcreteTerm, StillAvailable, NewBound) :-
                   AbstractTerm =.. [F|AbstractArgs],
                   select_reg(AbstractArgs, Available, Bound,
                              ConcreteArgs, StillAvailable, NewBound),
                   ConcreteTerm =.. [F|ConcreteArgs], !.

/* Everything else is rewritten verbatim */
select_reg(A, Available, Bound, A, Available, Bound).



/* List L of all concrete GPRs in expression E */
all_gprs([],[]) :-!.
all_gprs(gpr(N),N) :-!.
all_gprs([H|T], X) :- /* HG is list */
        all_gprs(H,HG),
        all_gprs(T,TG),
        concat(HG,TG,X), !.
all_gprs([H|T], [HG|TG]) :- /* HG is not list */
        all_gprs(H,HG),
        all_gprs(T,TG), !.
all_gprs(T,L) :-
        T =.. [_|Args],
        all_gprs(Args,L), !.

/*******************************************************
 * General utility functions                           *
 *******************************************************/

concat([],B,B).
concat([H|T],B,[H|TB]) :- concat(T,B,TB).
/* flatten(List,FlattenedList) */
flatten([],[]) :- !.
flatten([[]|L],F) :- flatten(L,F), !. /* amputate empty head */
flatten([[X|Y]|T],F) :- /* head is a list */
    concat(Y,T,Z),
    flatten([X|Z],F), !.
flatten([X|T],[X|FT]) :- flatten(T,FT), !.
/* If all else fails, X is not a list.
 * Without the following clause, the allocator will insist
 * on more than one instruction. */
flatten(X,[X]).

current_int(_) :- fail.
new_int(I) :- current_int(I), J is I+1, abolish(current_int/1), assert(current_int(J)), !.
new_int(I) :- I=1, abolish(current_int/1), assert(current_int(2)).


/* Test if S is subterm of T; used for allocating distinct registers */
subterm(T,T).
subterm(S,T) :- compound(T), T =.. [S|_].   /* S is functor of T */
subterm(S,T) :- compound(T), T =.. [_|Args], subterm_list(S,Args).
subterm_list(S,[Arg|_]) :- subterm(S,Arg).
subterm_list(S,[_|Args]) :- subterm_list(S,Args).

leaves([T|nil],T) :- atom(T).

/* remove Arg1 from Arg2, producing Arg3 */
remove(_, [], []).
remove(X, [X|L], M) :- remove(X, L, M), !.
remove(X, [Y|L], [Y|Z]) :- remove(X, L, Z).

/* removeAll(All, In, Out) */
removeAll([],X,X).
removeAll([X|R],In,Out) :- remove(X,In,T), removeAll(R,T,Out).


:- consult(transfers).


/* See if arg is grounded to a concrete or temp GPR.
   Fail if arg is not fully grounded. */
bound_GPR(gpr(N)) :- integer(N), memberchk(N, [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31]).
bound_GPR(tmp_gpr(N)) :- integer(N).

/* Succeed if the arg is bound to a concrete or temp GPR.
   If unbound, get a new temp GPR. */
is_GPR(R) :- bound_GPR(R), !.
is_GPR(tmp_gpr(X)) :- new_int(X).

:- consult(idioms).
:- consult(idiom_emission).

