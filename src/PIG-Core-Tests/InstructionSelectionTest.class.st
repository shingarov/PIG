Class {
	#name : #InstructionSelectionTest,
	#superclass : #TestCaseWithZ3Context,
	#category : #'PIG-Core-Tests'
}

{ #category : #tests }
InstructionSelectionTest >> testInstrSelect [
	| answers e acd state |
	
	state := { 'state_1' toInt. 'state_2' toInt. 'state_3' toInt. 'state_4' toInt. }.
	
	answers := EzSMT new solveProgram: '
		fuel(2).
		«(state at: 1) === 1».
		«(state at: M+1) === 4» :- fuel(M).
		
		1 { program_text(I,T) : instr(I) } 1 :- T = 1..M, fuel(M).
		
		instr(add1).
		«(state at: T+1) === ((state at: T)+1)» :- program_text(add1, T), T = 1..M, fuel(M).
		
		instr(load2).
		«(state at: T+1) === 2» :- program_text(load2, T), T = 1..M, fuel(M).
		
		'	in: thisContext.
	
	
	self halt.
	self assert: answers size equals: 2.
	e := answers detect: [ :ans | ans trueAtoms size = 1 ].
	self assert: e trueAtoms equals: #('e').
	self assert: (e constraintAssignments at: 'x') >= 10.
	acd := answers detect: [ :ans | ans ~= e ].
	self assert: (acd constraintAssignments at: 'x') <= 0.
]
