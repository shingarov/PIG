Class {
	#name : #Skip,
	#superclass : #HoareCommand,
	#category : #'PIG-Core'
}

{ #category : #semantics }
Skip >> mutates: oldState into: newState [
	^oldState === newState
]
