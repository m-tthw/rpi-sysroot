(* ::Package:: *)

Paclet[
	Name -> "Interpreter",
	Version -> "1.2.66.0",
	MathematicaVersion -> "10+",
	Description -> "Interpretation of strings",
	Loading -> Automatic,
	Creator -> "Riccardo Di Virgilio <riccardod@wolfram.com>, Carlo Barbieri <carlob@wolfram.com>, Chiara Basile <chiarab@wolfram.com>",
	Extensions -> {
		{
			"Kernel", 
			Context -> {"InterpreterLoader`", "Interpreter`"}, 
			Symbols -> {
				"System`Interpreter", 
				"System`Restricted", 
				"System`DelimitedSequence", 
				"System`GeoLocation", 
				"System`$InterpreterTypes"
			}
		},
		{"JLink"},
		{"Resource", Root -> ".", Resources -> {"MetaData"}}
	}
]
