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
ProgramSpecIS >> scriptSelector [
	^#add2:
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
