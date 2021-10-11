Class {
	#name : #InstructionSelectionTest,
	#superclass : #TestCaseWithZ3Context,
	#category : #'PIG-Core-Tests'
}

{ #category : #tests }
InstructionSelectionTest >> testInstrSelect [
	| answers state isa |
	state := { 'state_1' toInt. 'state_2' toInt. 'state_3' toInt. 'state_4' toInt. }.
	isa := Dictionary new
		at: 'add1' put: CommandADD1 new;
		at: 'load2' put: CommandLOAD2 new;
		at: 'state' put: state;
		yourself.
	
	answers := EzSMT new solveProgram: '
		fuel(2).
		«(state at: 1) === 1».
		«(state at: M+1) === 3» :- fuel(M).
		
		1 { program_text(I,T) : instr(I) } 1 :- T = 1..M, fuel(M).
		
		instr(add1;load2).
		«I mutates: (state at: T) into: (state at: T+1)» :- instr(I), program_text(I, T), T = 1..M, fuel(M).
		
		'	in: isa.
	
	
	self halt
]
