Class {
	#name : #CEGIS,
	#superclass : #Object,
	#instVars : [
		'specClass'
	],
	#category : #'PIG-Core'
}

{ #category : #'instance creation' }
CEGIS class >> of: aSpecificationClass [
	^self basicNew
		specClass: aSpecificationClass;
		yourself 
]

{ #category : #accessing }
CEGIS >> specClass [
	^ specClass
]

{ #category : #accessing }
CEGIS >> specClass: anObject [
	specClass := anObject
]

{ #category : #synthesis }
CEGIS >> synthesize [
	^self synthesizeFrom: {Int sort anyOne.}
]

{ #category : #'as yet unclassified' }
CEGIS >> synthesizeFrom: exampleInputs [
	| add1 load2 answers specificationFormula clauses_chains chains vspec isa |

	clauses_chains := exampleInputs collect: [ :example |
		| spec initState |
		spec := self specClass new.
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
	answers := EzSMT new solveProgram: 'nex(', exampleInputs size printString, ').
		fuel(2).
		«specificationFormula».
		
		1 { program_text(I,T) : instr(I) } 1 :- T = 1..M, fuel(M).
		
		instr(add1;load2).
		«I mutates: ((chains at:Ex) at: T) into: ((chains at:Ex) at: T+1)» :- instr(I), program_text(I, T), T = 1..M, fuel(M), Ex = 1..NEx, nex(NEx).
		
		'	in: thisContext.
	

	
	vspec := self specClass new.
	vspec isa: isa.
	vspec 	candidate: answers anyOne.
	vspec add2: ('X' toInt).
	[vspec verify. ^vspec candidate]
		on: NotValid 
		do: [ :ex | ^self synthesizeFrom: exampleInputs, {ex counterexample constantsASTs at: 'X'.} ]
]
