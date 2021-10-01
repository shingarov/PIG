Class {
	#name : #BaselineOfPIG,
	#superclass : #BaselineOf,
	#category : #BaselineOfPIG
}

{ #category : #baselines }
BaselineOfPIG >> baseline: spec [
	<baseline>
	spec for: #pharo
	do: [
		spec
			baseline: 'ArchC' with: [ spec repository: 'github://shingarov/Pharo-ArchC:pure-z3' ];
			baseline: 'PetitASP' with: [ spec repository: 'github://shingarov/PetitASP:thinkery' ].

		spec 
			package: #'PIG-Core' with: [ spec requires: #('ArchC' 'PetitASP') ];
			package: #'PIG-Core-Tests'.
	]
]
