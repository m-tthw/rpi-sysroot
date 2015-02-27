(* All loading of the paclet's Wolfram Language code should go through this file. *)

(* Developer maintains this list of symbols.
   autoloadSymbols must agree with the symbols listed in the Kernel extension in the PacletInfo.m file.
*)

Interpreter`Private`autoloadSymbols = {
    "System`Interpreter", 
    "System`Restricted", 
    "System`DelimitedSequence", 
    "System`GeoLocation", 
    "System`$InterpreterTypes"
};

Interpreter`Private`symsToProtect = Hold[
    Select[Names["Interpreter`*"] ~Join~ Names["Interpreter`PackageScope`*"],
        ToExpression[#, InputForm, Function[{sym}, Length[DownValues[sym]] > 0 || Length[SubValues[sym]] > 0, HoldFirst]] &
    ] ~Join~ Interpreter`Private`autoloadSymbols ~ Complement ~ {
        "InterpreterObject", 
        "System`$InterpreterTypes",
        "Interpreter`PackageScope`realDigitsStringExpressionsAssoc",
        "Interpreter`PackageScope`getOperator",
        "Interpreter`PackageScope`loadFramework",
        "Interpreter`PackageScope`$MetaData",
        "Interpreter`PackageScope`$EntityTypes"
    }
];

PacletManager`Package`loadWolframLanguageCode[
    "Interpreter", 
    "Interpreter`", 
    DirectoryName[$InputFileName], 
    "Primitives.m",
    "AutoUpdate" -> True, 
    "ForceMX" -> False, 
    "Lock" -> False,
    "AutoloadSymbols" -> Interpreter`Private`autoloadSymbols, 
    "SymbolsToProtect" -> Interpreter`Private`symsToProtect,
    "HiddenImports" -> {
        (* "DataDropClient`", uncomment this when datadrop type will be inside interpreter *)
        "Security`", 
        "URLUtilities`", 
        "JLink`"
    }
]