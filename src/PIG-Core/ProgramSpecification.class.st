Class {
	#name : #ProgramSpecification,
	#superclass : #Object,
	#instVars : [
		'precondition',
		'postcondition',
		'searchBlock'
	],
	#category : #'PIG-Core'
}

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
		self instVarNamed: aName put: ((self class domain at: aName) mkFreshConst: aName asString) ]
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
ProgramSpecification >> searchBlock: anObject [
	searchBlock := anObject
]

{ #category : #logic }
ProgramSpecification >> verify [
	| solver |
	solver := Z3Solver new.
	solver proveValid: self precondition ==> self postcondition.
	solver del
]
