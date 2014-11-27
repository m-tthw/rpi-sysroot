Needs["Macros`"];

Thrower::error = "Exception thrown";

failure[scope_Symbol] := Failure[Thrower, <|"MessageParameters" :> {}, "MessageTemplate" :> Thrower::error, "ScopeStack" -> {scope}|>]

Thrower[] := Module[{},
	DOSOMETHING;
	ThrowFailure[Thrower::error];
	NEVERDOTHAT
];
ReturnFail[] := Module[{},
	DOSOMETHING;
	ToFailure[Message[Thrower::error]]
];
Test[
	ReturnFail[];
	DOSOMETHING
	,
	DOSOMETHING
	,
	TestID->"ToFailure do not trigger Abort and can be used to return a clean failure to the end user"
]

Test[
	f[] := CatchFailure @ Module[{},
		Thrower[];
		RETURNSOMETHING
	];
	f[]
	,
	failure[f]
	,
	TestID->"Catch a ThrowFailure returns a Failure"
]
Test[
	f[] := Module[{},
		res = ReturnFail[];
		RETURNSOMETHING[res]
	];
	Head @ f[]
	,
	RETURNSOMETHING
	,
	TestID->"Function that returns a Failure does not abort execution"
]
Test[
	f[] := CatchFailure @ Module[{},
		res = CheckFailure @ ReturnFail[];
		RETURNSOMETHING[res]
	];
	f[]
	,
	ToFailure[Message[Thrower::error]]
	,
	TestID->"Function that returns a Failure should be enclosed by a CheckFailure so that failure propagates"
]
Test[
	child[] := Module[{},
		DOSOMETHING;
		Thrower[];
		NEVERREACHED
	];
	parent[] := CatchFailure @ Module[{},
		DOSOMETHING;
		child[];
		RETURNSOMETHING;
	]
	,
	failure[child];
	,
	TestID->"Failure propagates througth the call stack until they are caugth."
]