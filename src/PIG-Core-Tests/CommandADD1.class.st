Class {
	#name : #CommandADD1,
	#superclass : #HoareCommand,
	#category : #'PIG-Core-Tests'
}

{ #category : #semantics }
CommandADD1 >> mutates: oldState into: newState [ 
	^newState === (oldState + 1)
]
