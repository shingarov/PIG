Class {
	#name : #ProgramSpecification,
	#superclass : #Object,
	#instVars : [
		'precondition',
		'postcondition',
		'stateChain',
		'candidate',
		'isa'
	],
	#category : #'PIG-Core'
}

{ #category : #accessing }
ProgramSpecification >> candidate [
	^candidate
]

{ #category : #accessing }
ProgramSpecification >> candidate: aProgram [
	candidate := aProgram 
]

{ #category : #logic }
ProgramSpecification >> domain [
	self subclassResponsibility
]

{ #category : #initialization }
ProgramSpecification >> initialize [ 
	self initializeStateSpace
]

{ #category : #initialization }
ProgramSpecification >> initializeStateSpace [
	self class instVarNames do: [ :aName |
		self instVarNamed: aName put: ((self domain at: aName) mkFreshConst: aName asString) ]
]

{ #category : #accessing }
ProgramSpecification >> isa: anObject [ 
	isa := anObject 

]

{ #category : #accessing }
ProgramSpecification >> postcondition [
	^ postcondition
]

{ #category : #accessing }
ProgramSpecification >> postcondition: anObject [
	postcondition := anObject
]

{ #category : #accessing }
ProgramSpecification >> precondition [
	^ precondition
]

{ #category : #accessing }
ProgramSpecification >> precondition: anObject [
	precondition := anObject
]

{ #category : #accessing }
ProgramSpecification >> stateChain [
	^stateChain 
]

{ #category : #'as yet unclassified' }
ProgramSpecification >> synthesize [
	^self synthesize: Bool true
]

{ #category : #'as yet unclassified' }
ProgramSpecification >> synthesize: contradictions [
	|  add1 load2 answers |
	add1 := CommandADD1 new.
	load2 := CommandLOAD2 new.
	isa := Dictionary new
		at: 'add1' put: add1;
		at: 'load2' put: load2;
	 yourself.
precondition := precondition and: contradictions.
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
		do: [ :ex | ^self synthesize: ex counterexample blockerContradictions ]
]

{ #category : #logic }
ProgramSpecification >> verify [
	| solver |
	solver := Z3Solver new.
	solver proveValid: self precondition ==> self postcondition.
	solver del
]
