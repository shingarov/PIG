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

{ #category : #logic }
ProgramSpecification >> verify [
	| solver |
	solver := Z3Solver new.
	solver proveValid: self precondition ==> self postcondition.
	solver del
]
