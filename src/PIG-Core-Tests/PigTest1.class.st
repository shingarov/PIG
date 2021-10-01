Class {
	#name : #PigTest1,
	#superclass : #TestCaseWithZ3Context,
	#category : #'PIG-Core-Tests'
}

{ #category : #tests }
PigTest1 >> test1 [ 
	| X spec |
	X := 'X' toInt.
	spec := ProgramSpec3 new.
	spec addOneTo: X.
	spec verify.
]

{ #category : #tests }
PigTest1 >> test2 [
	| spec |
	spec := ProgramSpec3 new.
	spec add256ToY.
	spec verify.
]
