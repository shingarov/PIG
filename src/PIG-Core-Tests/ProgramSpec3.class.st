Class {
	#name : #ProgramSpec3,
	#superclass : #ProgramSpecification,
	#instVars : [
		'x',
		'y'
	],
	#category : #'PIG-Core-Tests'
}

{ #category : #adding }
ProgramSpec3 >> add256ToY [
	| oldY |
	oldY := y.
	self precondition: Bool true.
	y := y + 256.
	self postcondition: y === oldY

]

{ #category : #adding }
ProgramSpec3 >> addOneTo: X [
	self precondition: x === X.
	x := x + 1.
	self postcondition: x === (X+1).

]

{ #category : #logic }
ProgramSpec3 >> domain [
	^Dictionary newFromAssociations: {
		#x -> Int sort.
		#y -> (BitVector length: 8)
	}
]
