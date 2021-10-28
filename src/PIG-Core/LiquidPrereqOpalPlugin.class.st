Class {
	#name : #LiquidPrereqOpalPlugin,
	#superclass : #OCCompilerASTPlugin,
	#category : #'PIG-Core'
}

{ #category : #private }
LiquidPrereqOpalPlugin class >> priority [
	^100
]

{ #category : #'as yet unclassified' }
LiquidPrereqOpalPlugin >> addVar: v to: aTyEnvDictionary [
	| prag eSource |
	prag := ast pragmas detect: [ :aPragma | v, #':' = aPragma selector ] ifNone: [ ^self ].
	eSource := prag arguments first value.
	aTyEnvDictionary at: v put: (OpalCompiler new evaluate: eSource)
]

{ #category : #'as yet unclassified' }
LiquidPrereqOpalPlugin >> createTypeEnvironment [
	| tyEnv |
	tyEnv := Dictionary new.
	ast allDefinedVariables do: [ :var |
		self addVar: var to: tyEnv ].
	ast methodPropertyAt: #liquid_tyenv put: tyEnv.
	^ast
	
]

{ #category : #api }
LiquidPrereqOpalPlugin >> transform [
	ast pragmas detect: [ :p | p selector = #verify ] ifNone: [ ^ast ].
	^self createTypeEnvironment 

]
