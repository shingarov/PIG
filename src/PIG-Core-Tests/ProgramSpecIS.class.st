Class {
	#name : #ProgramSpecIS,
	#superclass : #ProgramSpecification,
	#instVars : [
		'state'
	],
	#category : #'PIG-Core-Tests'
}

{ #category : #'as yet unclassified' }
ProgramSpecIS >> _ [
	candidate isNil ifTrue: [
		"within step 1: discover SOME X"
			stateChain := (1 to: self fuel+1) collect: [ :t | Int sort mkFreshConst: ('state_', t printString) ].
			stateChain at: 1 put: state.
			state := stateChain last
	] ifFalse: [
		"we have a program, and trying to verify it"
		(candidate synthesizedProgram collect: [ :instName | isa at: instName ])
			"symbolically run the program"
			do: [ :inst | state := inst work: state ]
	 ]
]

{ #category : #'as yet unclassified' }
ProgramSpecIS >> add2: X [
	self precondition: state === X.
	self _.
	self postcondition: state === (X+2).
]

{ #category : #logic }
ProgramSpecIS >> domain [
	^Dictionary newFromAssociations: {
		#state -> Int sort
	}
]

{ #category : #'as yet unclassified' }
ProgramSpecIS >> fuel [
	^2
]

{ #category : #'as yet unclassified' }
ProgramSpecIS >> makeThree [
	self precondition: state === 1.
	state := state + 1.
	state := state + 1.
	self postcondition: state === 3.
]

{ #category : #'as yet unclassified' }
ProgramSpecIS >> state [
	^state
]

{ #category : #'as yet unclassified' }
ProgramSpecIS >> synthStep1 [
	| X add1 load2 answers |
	X := 'X' toInt.
	self add2: X.
	
	add1 := CommandADD1 new.
	load2 := CommandLOAD2 new.
	isa := Dictionary new
		at: 'add1' put: add1;
		at: 'load2' put: load2;
	 yourself.
	answers := EzSMT new solveProgram: '
		fuel(2).
		«precondition».
		«postcondition».
		
		1 { program_text(I,T) : instr(I) } 1 :- T = 1..M, fuel(M).
		
		instr(add1;load2).
		«I mutates: (stateChain at: T) into: (stateChain at: T+1)» :- instr(I), program_text(I, T), T = 1..M, fuel(M).
		
		'	in: thisContext.
	
	
	self halt.
	
	candidate := answers first.
	self initializeStateSpace.
	self add2: ('X' toInt).
	[self verify. ^candidate]
		on: NotValid 
		do: [ :ex | ^self synthStep2: {ex counterexample constantsASTs at: 'X'.} ]
]

{ #category : #'as yet unclassified' }
ProgramSpecIS >> synthStep2: Xs [
	| add1 load2 answers specificationFormula clauses_chains chains vspec |
self halt.
	clauses_chains := Xs collect: [ :example |
		| spec initState |
		spec := ProgramSpecIS new.
		initState := spec state.
		spec add2: example.
		(spec precondition ==> spec postcondition forall: initState)
			-> spec stateChain 
	].
	specificationFormula := Bool and: (clauses_chains collect: #key).
	chains := clauses_chains collect: #value.
	
	add1 := CommandADD1 new.
	load2 := CommandLOAD2 new.
	isa := Dictionary new
		at: 'add1' put: add1;
		at: 'load2' put: load2;
	 yourself.
	answers := EzSMT new solveProgram: 'nex(', Xs size printString, ').
		fuel(2).
		«specificationFormula».
		
		1 { program_text(I,T) : instr(I) } 1 :- T = 1..M, fuel(M).
		
		instr(add1;load2).
		«I mutates: ((chains at:Ex) at: T) into: ((chains at:Ex) at: T+1)» :- instr(I), program_text(I, T), T = 1..M, fuel(M), Ex = 1..NEx, nex(NEx).
		
		'	in: thisContext.
	
	self halt.
	
	vspec := self class new.
	vspec isa: isa.
	vspec 	candidate: answers anyOne.
	vspec add2: ('X' toInt).
	[vspec verify. ^vspec candidate]
		on: NotValid 
		do: [ :ex | ^self synthStep2: Xs, {ex counterexample constantsASTs at: 'X'.} ]
]

{ #category : #'as yet unclassified' }
ProgramSpecIS >> synthThree [
	self precondition: state === 1.
	self _.
	self postcondition: state === 3.
]
