Extension { #name : #AnswerSet }

{ #category : #'*PIG-Core' }
AnswerSet >> gtInspectorProgramTextIn: composite [
	<gtInspectorPresentationOrder: 100>
	^ composite fastList
		title: 'Synthesized program';
		display: [ self synthesizedProgram ]
]

{ #category : #'*PIG-Core' }
AnswerSet >> synthesizedProgram [
	| programText |
	programText := self trueAtoms
		select: [ :atom | atom beginsWith: 'program_text(' ]
		thenCollect: [ :atom |
			| inst_time |
			inst_time := atom copyFrom: 14 to: atom size-1.
			inst_time := $, split: inst_time.
			inst_time second -> inst_time first
		].
	^(programText sorted: [ :a :b | a key asInteger <= b key asInteger ]) collect: #value
]
