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

{ #category : #tests }
PigTest1 >> test3 [
	| spec |
	spec := ProgramSpecIS new.
	spec makeThree.
	spec verify.
]

{ #category : #tests }
PigTest1 >> test4 [
	| spec |
	spec := ProgramSpecIS new.
	spec synthThree.
	spec synthesize.
]

{ #category : #tests }
PigTest1 >> testSynthAdd2 [
	| cegis program |
	cegis := CEGIS of: ProgramSpecIS.
	program := cegis synthesize.
	self assert: program synthesizedProgram size equals: 2.
	self assert: program synthesizedProgram first equals: 'add1'.
	self assert: program synthesizedProgram second equals: 'add1'.
]
