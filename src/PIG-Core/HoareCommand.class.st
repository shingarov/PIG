Class {
	#name : #HoareCommand,
	#superclass : #Object,
	#category : #'PIG-Core'
}

{ #category : #copying }
HoareCommand >> , anotherCommand [
	^Catenation c1: self c2: anotherCommand
]

{ #category : #semantics }
HoareCommand >> mutates: oldState into: newState [
]
