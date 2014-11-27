(* All loading of the paclet's Wolfram Language code should go through this file. *)

(* Developer maintains this list of symbols.
   autoloadSymbols must agree with the symbols listed in the Kernel extension in the PacletInfo.m file.
*)

Templating`Private`autoloadSymbols = {
    "System`CombinerFunction", 
    "System`InsertionFunction", 
    "System`StringTemplate", 
    "System`DefaultValue",
    "System`FileTemplate",
    "System`FileTemplateApply", 
    "System`TemplateApply", 
    "System`TemplateObject",
    "System`TemplateIf", 
    "System`TemplateSequence", 
    "System`TemplateSlot", 
    "System`TemplateExpression", 
    "System`TemplateWith", 
    "System`XMLTemplate",
    "System`Pluralize",
    "System`$HTMLExportRules",
    "System`$TemplatePath",
    "Templating`ExportHTML"
};

Templating`Private`symsToProtect = Hold[
    Select[Names["Templating`*"] ~Join~ Names["Templating`PackageScope`*"],
        ToExpression[#, InputForm, Function[{sym}, Length[DownValues[sym]] > 0 || Length[SubValues[sym]] > 0, HoldFirst]] &
    ] ~Join~ (Templating`Private`autoloadSymbols ~Complement~ {
        "System`$HTMLExportRules",
        "System`$TemplatePath",
        "Templating`ExportHTML",
        "Templating`HTML`PackagePrivate`$HTMLDefaultTemplate"
    })
];

PacletManager`Package`loadWolframLanguageCode[
    "Templating", 
    "Templating`", 
    DirectoryName[$InputFileName], 
    "Primitives.m",
    "AutoUpdate" -> True, 
    "ForceMX" -> False, 
    "Lock" -> False,
    "AutoloadSymbols" -> Templating`Private`autoloadSymbols,
    "SymbolsToProtect" -> Templating`Private`symsToProtect, 
    "HiddenImports" -> {"GeneralUtilities`"}
]