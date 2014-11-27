BeginPackage["CloudObject`"]

System`CloudDeploy;

Begin["`Private`"]

(* Dependencies *)
System`APIFunction;
System`FormFunction;
System`HTMLData;
System`EmbeddedHTML;
System`Grammar;
System`$CloudEvaluation;

Attributes[headDeployFormat] = {HoldAll};
headDeployFormat[APIFunction] = "API";
headDeployFormat[Delayed] = "Computation";
headDeployFormat[Dynamic] = "Computation";
headDeployFormat[FormFunction] = "Form";
headDeployFormat[ScheduledTask] = "Task";
headDeployFormat[Grammar] = "Grammar";
headDeployFormat[_] = None;

Attributes[headDeployFormatQ] = {HoldAll};
headDeployFormatQ[expr_] := headDeployFormat[expr] =!= None;

Attributes[headMimeType] = {HoldAll};
headMimeType[head_Symbol] := Module[{format = headDeployFormat[head]},
    If[format =!= None, formatToMimeType[format], None]
]
headMimeType[_] = None;

Attributes[expressionMimeType] = {HoldAll};
expressionMimeType[head_[args___]] := headMimeType[head]
expressionMimeType[_] := None

Attributes[deployAsSpecialExpressionQ] = {HoldAll};
deployAsSpecialExpressionQ[expr_] := expressionMimeType[expr] =!= None

Unprotect[CloudDeploy];

Options[CloudDeploy] = {Permissions->Automatic, IconRules->Automatic, MetaInformation->{}};

CloudDeploy[bundle:ExternalBundle[bundleElements_List], dest_CloudObject, opts:OptionsPattern[]] :=
    Module[{elementObjects, bundleexpr},
        (* Step 1 of 3. Ensure the bundle directory exists *)
        Quiet[createBundle[dest], CloudObject::notparam] /. {
        	HTTPError[_] :> Return[$Failed]
        };

        (* Step 2 of 3. deploy the individual elements *)
        elementObjects = $lastBundleDeployResult = Map[
            deployBundleElement[dest, #, opts]&,
            bundleElements
        ];
        If[Position[elementObjects, $Failed, Infinity, 1] =!= {},
			Return[$Failed]
        ];

        (* Step 3 of 3. deploy the ExternalBundle content *)
        bundleexpr = ExternalBundle[elementObjects];
        CloudPut[bundleexpr, FileNameJoin[{dest, ".bundle"}], opts];

        dest
    ];

assocToList[assoc_Association] := Map[assoc[#]&, Keys[assoc]] (* workaround for certain Mathematica builds where Normal[_Association] normalizes deeply *)

deployBundleElement[dir_CloudObject, name_String -> elements_List, opts:OptionsPattern[]] :=
	CreateDirectory[FileNameJoin[{dir, name}]] /. {
		subdir_CloudObject :>
			name -> Map[deployBundleElement[subdir, #, opts]&, elements],
		_ :> $Failed
	}

deployBundleElement[dir_CloudObject, name_String -> direlements_Association, opts:OptionsPattern[]] :=
	deployBundleElement[dir, name -> assocToList[direlements], opts]

deployBundleElement[dir_CloudObject, name_String -> expr_, opts:OptionsPattern[]] :=
	CloudDeploy[expr, FileNameJoin[{dir, name}], opts] /. {
		obj_CloudObject :> name -> obj,
		_ :> $Failed
	}

CloudDeploy[ExternalBundle[elements_Association], dest_CloudObject, opts:OptionsPattern[]] :=
	CloudDeploy[ExternalBundle[assocToList[elements]], dest, opts]

CloudDeploy[apigroup_APIFunctionGroup, obj_CloudObject, opts:OptionsPattern[]] :=
    deployAPIFunctionGroup[obj, apigroup, opts]

cd:CloudDeploy[grammar_Grammar, obj_CloudObject, opts:OptionsPattern[]] :=
    If[ TrueQ[$CloudEvaluation],
        Semantic`PLIDump`iGrammarDeploy[grammar, obj, opts]
        ,
        CloudEvaluate[cd]
    ];

CloudDeploy[expr_?deployAsSpecialExpressionQ, obj_CloudObject, opts:OptionsPattern[]] :=
    iCloudPut[Unevaluated[expr], obj, expressionMimeType[expr], SaveDefinitions -> True, opts]

CloudDeploy[ExportForm[expr_, format_, rest___], obj_CloudObject, opts:OptionsPattern[]] :=
    CloudExport[Unevaluated[expr], format, obj, rest, opts]

CloudDeploy[HTMLData[html_], obj_CloudObject, opts:OptionsPattern[]] :=
    CloudExport[html, "HTML", obj, opts]

CloudDeploy[expr_, obj_CloudObject, opts:OptionsPattern[]] :=
    CloudDeploy[ExportForm[expr], obj, opts]

CloudDeploy[expr_, uri_String, opts:OptionsPattern[]] :=
    CloudDeploy[Unevaluated[expr], CloudObject[uri], opts]

CloudDeploy[expr_, opts:OptionsPattern[]] :=
    CloudDeploy[Unevaluated[expr], CloudObject[], opts]

CloudDeploy[args___] := (ArgumentCountQ[CloudDeploy,Length[DeleteCases[{args},_Rule,Infinity]],1,2];Null/;False)

SetAttributes[CloudDeploy, {ReadProtected}];
Protect[CloudDeploy];

Options[deployAPIFunctionGroup] = Options[CloudDeploy];
deployAPIFunctionGroup[dest:CloudObject[uri_, objopts:OptionsPattern[CloudObject]],
        APIFunctionGroup[apifunctions : {Rule[_String, _APIFunction] ...}, groupOptions___?OptionsQ],
        opts:OptionsPattern[]] :=
    Module[{apifunctionObjects, apigroup,
            permissionsOptionValue = OptionValue[CloudDeploy, {opts, objopts}, Permissions]},

        (* Step 1 of 3. Ensure the APIFunctionGroup bundle directory exists *)
        Quiet[createBundle[dest, ".api"], CloudObject::notparam];

        (* Step 2 of 3. deploy the individual functions *)
        apifunctionObjects = $lastAPIResult = Map[
            CloudDeploy[#[[2]], FileNameJoin[{dest, #[[1]]}],
                Permissions -> permissionsOptionValue,
                IconRules -> OptionValue[IconRules]]&,
            apifunctions
        ];
        (* TODO check if any apifunctionObjects failed *)

        (* Step 3 of 3. deploy the APIFunctionGroup content *)
        apigroup = APIFunctionGroup[
            Map[Apply[Rule, #]&,
                Transpose[{Map[First, apifunctions], apifunctionObjects}]
            ],
            groupOptions];

        CloudPut[apigroup, FileNameJoin[{dest, ".bundle"}],
            Permissions -> permissionsOptionValue,
            IconRules -> OptionValue[IconRules]];

        dest
    ];

createBundle[dest_CloudObject, mimeTypeExtension_String:""] :=
    responseCheck[execute[dest, Automatic, UseUUID -> False,
        Type -> "application/vnd.wolfram.bundle"<>mimeTypeExtension], CloudDeploy, dest];

End[]

EndPackage[]
