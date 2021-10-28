Class {
	#name : #LiquidTest,
	#superclass : #TestCaseWithZ3Context,
	#category : #'PIG-Core-Tests'
}

{ #category : #compiler }
LiquidTest class >> compiler [
	^ super compiler addPlugin: LiquidPrereqOpalPlugin 
]

{ #category : #tests }
LiquidTest >> aaa: x [
	<verify>
	<x: 'Int sort | [ :nu | nu > 0 ]'>
	| y |
	y := x + 1.
	self assert: y > 1.
	
]

{ #category : #tests }
LiquidTest >> test1 [ 
	| l any |
	l := Int sort | [ :x | x > 0 ].
	any := l anyOne.
	self assert: any sort == Int sort.
	self assert: any value > 0
]

{ #category : #tests }
LiquidTest >> test2 [
	| l any |
	l := Int sort asRefinement.
	any := l anyOne.
	self assert: any sort == Int sort. 
]

{ #category : #tests }
LiquidTest >> test3 [ 
	| l any |
	l := Int sort | [ :x | x*3 > 10 ].
	any := l anyOne.
	self assert: any sort == Int sort.
	self assert: any value * 3 > 10
]

{ #category : #tests }
LiquidTest >> testLessThan [
	| l1 l2 |
	l1 := Int sort | [ :x | x > 0 ].
	l2 := Int sort | [ :x | x > 1 ].
	self assert: l2 <= l1.
	self deny:   l1 <= l2.

]
