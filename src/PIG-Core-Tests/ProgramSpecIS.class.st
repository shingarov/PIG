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
	^searchBlock value
]

{ #category : #'as yet unclassified' }
ProgramSpecIS >> add2 [
	| oldY |
	oldY := state.
	self precondition: Bool true.
	self _.
	self postcondition: state === (oldY+2).
]

{ #category : #'as yet unclassified' }
ProgramSpecIS >> add2: X [
	| oldY |
	oldY := state.
	self precondition: Bool true.
	self _.
	self postcondition: state === (oldY+5).
]

{ #category : #logic }
ProgramSpecIS >> domain [
	^Dictionary newFromAssociations: {
		#state -> Int sort
	}
]

{ #category : #'as yet unclassified' }
ProgramSpecIS >> makeThree [
	self precondition: state === 1.
	state := state + 1.
	state := state + 1.
	self postcondition: state === 3.
]

{ #category : #'as yet unclassified' }
ProgramSpecIS >> scriptSelector [
	^#add2
]

{ #category : #accessing }
ProgramSpecIS >> state [
	^state
]

{ #category : #accessing }
ProgramSpecIS >> state: s [
	state := s
]

{ #category : #'as yet unclassified' }
ProgramSpecIS >> synthThree [
	self precondition: state === 1.
	self _.
	self postcondition: state === 3.
]
