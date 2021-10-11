Class {
	#name : #CEGIS,
	#superclass : #Object,
	#instVars : [
		'availableBlocks'
	],
	#category : #'PIG-Core'
}

{ #category : #'instance creation' }
CEGIS class >> from: availableBlocks [
	^self basicNew
		availableBlocks: availableBlocks;
		yourself 
]

{ #category : #accessing }
CEGIS >> availableBlocks [
	^ availableBlocks
]

{ #category : #accessing }
CEGIS >> availableBlocks: anObject [
	availableBlocks := anObject
]

{ #category : #synthesis }
CEGIS >> synthesize: specClass [
	^self synthesize: specClass given: {Int sort anyOne.}
]

{ #category : #synthesis }
CEGIS >> synthesize: specClass given: exampleInputs [
	| candidate vspec |
	candidate := self 	synthesize: specClass givenExactly: exampleInputs.
	candidate isNil ifTrue: [ ^nil ].
	
	vspec := specClass new.
	vspec isa: availableBlocks.
	vspec 	candidate: candidate.
	vspec state: 'X' toInt.
	vspec perform: vspec scriptSelector.
	[ vspec verify. ^vspec candidate ]
			on: NotValid 
			do: [ :ex | ^self synthesize: specClass given: exampleInputs, {ex counterexample constantsASTs at: 'X'.} ]
]

{ #category : #synthesis }
CEGIS >> synthesize: specClass givenExactly: exampleInputs [
	"Synthesize and answer a program out of the receiver's availableBlocks,
	providing it will work correctly on the given exampleInputs.
	No further verification is performed (in other words, the synthesized
	program may not behave correctly for any other input.
	If a program consisting of <fuel> blocks can not be found, answer nil."
	| answers specificationFormula clauses_chains chains |

	clauses_chains := exampleInputs collect: [ :example |
		| spec |
		spec := specClass new.
		spec state: example.
		spec perform: spec scriptSelector "with: example".
		(spec precondition ==> spec postcondition)
			-> spec stateChain 
	].
	specificationFormula := Bool and: (clauses_chains collect: #key).
	chains := clauses_chains collect: #value.
	
	answers := EzSMT new solveProgram: 'nex(', exampleInputs size printString, ').
		fuel(2).
		«specificationFormula».
		
		1 { program_text(I,T) : instr(I) } 1 :- T = 1..M, fuel(M).
		
		instr(add1;load2).
		«I mutates: ((chains at:Ex) at: T) into: ((chains at:Ex) at: T+1)» :- instr(I), program_text(I, T), T = 1..M, fuel(M), Ex = 1..NEx, nex(NEx).
		
		'	in: (availableBlocks copy 
				at: 'specificationFormula' put: specificationFormula;
				at: 'chains' put: chains;
			yourself).

	answers isEmpty ifTrue: [ ^nil ].
	^BlockProgram text: answers anyOne synthesizedProgram isa: availableBlocks
]
