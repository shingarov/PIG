Class {
	#name : #CommandADD1,
	#superclass : #HoareCommand,
	#category : #'PIG-Core-Tests'
}

{ #category : #semantics }
CommandADD1 >> work: oldState [
	^oldState + 1
]
