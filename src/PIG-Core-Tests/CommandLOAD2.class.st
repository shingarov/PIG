Class {
	#name : #CommandLOAD2,
	#superclass : #HoareCommand,
	#category : #'PIG-Core-Tests'
}

{ #category : #semantics }
CommandLOAD2 >> mutates: oldState into: newState [ 
	^newState === 2
]
