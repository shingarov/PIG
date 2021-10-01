Class {
	#name : #Catenation,
	#superclass : #HoareCommand,
	#instVars : [
		'c1',
		'c2'
	],
	#category : #'PIG-Core'
}

{ #category : #'instance creation' }
Catenation class >> c1: c1 c2: c2 [
	^self basicNew
		c1: c1;
		c2: c2;
		yourself
]

{ #category : #'instance creation' }
Catenation class >> new [
	self shouldNotImplement
]

{ #category : #accessing }
Catenation >> c1 [
	^ c1
]

{ #category : #accessing }
Catenation >> c1: anObject [
	c1 := anObject
]

{ #category : #accessing }
Catenation >> c2 [
	^ c2
]

{ #category : #accessing }
Catenation >> c2: anObject [
	c2 := anObject
]
