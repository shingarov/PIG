Class {
	#name : #BlockProgram,
	#superclass : #Object,
	#instVars : [
		'text',
		'isa'
	],
	#category : #'PIG-Core'
}

{ #category : #'instance creation' }
BlockProgram class >> text: arrayOfBlocks isa: isa [
	^self basicNew 
		text: arrayOfBlocks;
		isa: isa;
		yourself 
]

{ #category : #accessing }
BlockProgram >> isa [
	^ isa
]

{ #category : #accessing }
BlockProgram >> isa: anObject [
	isa := anObject
]

{ #category : #accessing }
BlockProgram >> text [
	^ text
]

{ #category : #accessing }
BlockProgram >> text: anObject [
	text := anObject
]
