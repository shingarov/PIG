emit_rtl(RTL, Instructions) :- e(RTL, Instructions), !.

emit_idiom(Idiom, Instructions) :-
        idiom_pattern(Idiom, RTL),
        emit_rtl(RTL, InstructionList),
        flatten(InstructionList, Instructions).

write_operand_names([]) :- !.
write_operand_names(X) :- write_operand_names0(X).

write_operand_names0([]) :- write(' | '), nl, !.

write_operand_names0([ H|T ]) :-
        write(' :'), write(H), write_operand_names0(T).

write_instruction_list([]) :- write(' yourself ]'), !.
write_instruction_list([ H|T ]) :-
        write_instruction(H), write_instruction_list(T).


write_operand(Op) :- integer(Op), write(' with: '), write(Op), !.
write_operand(Op) :- var(Op), write(' with: '), write(Op), !.
write_operand(low16(Op)) :- write(' with: (16rFFFF & '), write(Op), write(')'), !.
write_operand(hi16(Op)) :- write(' with: '), write(Op), write('>>16'), !.

write_operands([]) :- write('Array new'), !.
write_operands(OperandList) :- write( '(Array ' ), write_operands0(OperandList), write(')').
write_operands0([]) :- !.
write_operands0( [H|T] ) :-
        write_operand(H), write_operands0(T).

write_instruction(J) :-
        J =.. [ InstrName | Operands ],
        write(' add: ((self instructionAt: #'),
        write(InstrName),
        write(' ) bindSequence: '),
        write_operands(Operands),
        write(' );'),
        nl.

solve_idiom(Idiom) :-
        emit_idiom(Idiom, Instructions),
        functor(Idiom, IdiomName, _),
        write(IdiomName),
        write(' {'),
        nl,
        write('['),
        Idiom =.. [ _ | OperandNames ],
        write_operand_names(OperandNames),
        write('OrderedCollection new'), nl,
        write_instruction_list(Instructions),
        nl,
        write('}'),
        nl, nl.

solve_idioms :-
        bagof(I, solve_idiom(I), _).

