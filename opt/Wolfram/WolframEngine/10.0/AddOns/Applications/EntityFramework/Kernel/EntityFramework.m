(*System`Quantity;*)
System`Entity;
System`EntityValue;
System`EntityProperty;
System`EntityProperties;
System`CommonName;
System`CanonicalName;
System`EntityTypeName;
System`EntityClass;
System`EntityPropertyClass;
System`EntityList;
System`EntityClassList;
Experimental`FindEntities;
Internal`DisableEntityFramework;
Internal`AddToEntityNameCache;
Internal`PreloadEntityNameCache;
Internal`BulkFetchEntityNames;

EntityFramework`MakeEntityFrameworkBoxes;

Begin["EntityFramework`Private`"];

(*symbols to be protected/read protected (except option names FormatName/Qualifiers/SourceEntityType)*)
$readProtectedSymbols={Entity, EntityValue, EntityClass, EntityProperty, EntityPropertyClass, 
	Experimental`FindEntities, CanonicalName, CommonName, EntityList, EntityClassList,
	EntityProperties,EntityTypeName};
	
Unprotect@@$readProtectedSymbols;

$tag = "EntityFrameworkCatchThrowTag";
(*** Formatting code ***)
Needs["WolframAlphaClient`"];(*initialize WAClient*)
(* this should only have to live in TypesetInit.m, but for some reason gets clobbered when this is loaded... *)
Unprotect[System`Entity, System`EntityProperty, EntityFramework`MakeEntityFrameworkBoxes];

Options[EntityPropertyClass] = Options[EntityProperty] = {UnitSystem :> $UnitSystem}

SetAttributes[EntityFramework`MakeEntityFrameworkBoxes, HoldAllComplete];

Entity /: MakeBoxes[x_Entity, fmt_] := (Entity;(*trigger auto-loader*)
  With[{boxes = EntityFramework`MakeEntityFrameworkBoxes[x, fmt]}, 
   boxes /; boxes =!= $Failed])

EntityProperty /: MakeBoxes[x_EntityProperty, fmt_] := (Entity;(*trigger auto-loader*)
  With[{boxes = EntityFramework`MakeEntityFrameworkBoxes[x, fmt]}, boxes /; boxes =!= $Failed])

EntityValue /: MakeBoxes[x_EntityValue, fmt_] := (Entity;(*trigger auto-loader*)
  With[{boxes = EntityFramework`MakeEntityFrameworkBoxes[x, fmt]}, boxes /; boxes =!= $Failed])
  
EntityClass /: MakeBoxes[x_EntityClass, fmt_] := (Entity;(*trigger auto-loader*)
  With[{boxes = EntityFramework`MakeEntityFrameworkBoxes[x, fmt]}, boxes /; boxes =!= $Failed])

EntityPropertyClass /: MakeBoxes[x_EntityPropertyClass, fmt_] := (Entity;(*trigger auto-loader*)
  With[{boxes = EntityFramework`MakeEntityFrameworkBoxes[x, fmt]}, boxes /; boxes =!= $Failed])

(* MakeExpression rules now live in TypesetInit.m *)

$entitynametimeout = 0.8; (* Seconds timeout for single entity formatting *)

getetypeLabel[etype_] := If[MatchQ[entityLabelRules, {_Rule..}],
	etype /. entityLabelRules,
	etype
]

geteclasstypeLabel[etype_] := If[MatchQ[entityClassLabelRules, {_Rule..}],
	etype /. entityClassLabelRules,
	etype
]

makeTooltip[e_] := With[{str=ToString[e, InputForm]}, 
   MakeBoxes[str, StandardForm]
]

MakeTypesetBoxes = Function[Null, Block[{BoxForm`UseTextFormattingQ = False}, MakeBoxes[##]], HoldAllComplete];


EntityFramework`MakeEntityFrameworkBoxes[__] = $Failed;

(* Entity[] formatting *)

EntityFramework`MakeEntityFrameworkBoxes[e:Entity[etype_String, ename:(_String|_List|_Integer), opts___], StandardForm]/;!TrueQ[$dontFormatEntity] := 
  With[{fname = GetEntityName[e, $entitynametimeout]},
    With[{tooltip = makeTooltip[e],
      label = ToBoxes[If[StringQ[Unevaluated[etype]], getetypeLabel[etype], "unknown"], StandardForm],
      boxes = Block[{$dontFormatEntity=True}, 
        With[{strip=e}, MakeBoxes[strip, StandardForm]]]
      },
      TemplateBox[{MakeTypesetBoxes[fname,StandardForm], boxes, tooltip, label}, "Entity"
      ]
    ]/; OKEntityNameQ[fname]
  ]

EntityFramework`MakeEntityFrameworkBoxes[e:Entity[etype_String, ename:(_String|_List|_Integer), opts___], TraditionalForm] := 
  With[{fname = GetEntityName[e, $entitynametimeout]},
    With[{boxes = MakeTypesetBoxes[fname, TraditionalForm]},
      InterpretationBox[boxes, e, BaseStyle -> "Entity"]
    ]/;OKEntityNameQ[fname]
  ]

(* EntityProperty[] formatting *)

EntityFramework`MakeEntityFrameworkBoxes[e:EntityProperty[etype_String, pname:(_String|_List), opts:OptionsPattern[]], StandardForm]/;!TrueQ[$dontFormatEntity] := 
  With[{fname = GetEntityName[e, $entitynametimeout]},
    With[{tooltip = makeTooltip[e],
      boxes = Block[{$dontFormatEntity=True}, MakeBoxes[e, StandardForm]]},
      TemplateBox[{MakeTypesetBoxes[fname,StandardForm], boxes, tooltip}, "EntityProperty"]
    ]/; OKEntityNameQ[fname]
  ]
  
EntityFramework`MakeEntityFrameworkBoxes[e:EntityProperty[etype_String,pname:(_String|_List), opts:OptionsPattern[]], TraditionalForm] := 
  With[{fname = GetEntityName[e, $entitynametimeout]},
    With[{boxes = MakeTypesetBoxes[fname, TraditionalForm]},
      InterpretationBox[boxes, e, BaseStyle -> "EntityProperty"]
    ]/;OKEntityNameQ[fname]
  ]

(* EntityValue[] formatting *)

EntityFramework`MakeEntityFrameworkBoxes[EntityValue[e:(_Entity | _EntityClass), prop_EntityProperty], fmt:StandardForm] := 
Module[{eboxes = MakeBoxes[e, fmt], propboxes = MakeBoxes[prop, fmt]},
	If[ TrueQ[BoxForm`$UseTextFormattingForEntityValue] || Head[eboxes] =!= TemplateBox || Head[propboxes] =!= TemplateBox,
		RowBox[{"EntityValue", "[", RowBox[{eboxes, ",", propboxes}], "]"}],
		TemplateBox[{eboxes, propboxes}, "EntityValue"]
	]
]

(* EntityClass[] formatting *)

EntityFramework`MakeEntityFrameworkBoxes[e:EntityClass[etype_String, ename:(_String|_List|All), opts___], StandardForm]/;!TrueQ[$dontFormatEntity] := 
  With[{fname = GetEntityName[e, $entitynametimeout]},
    With[{tooltip = makeTooltip[e],
      label = ToBoxes[If[StringQ[Unevaluated[etype]], geteclasstypeLabel[etype], "unknown"], StandardForm],
      boxes = Block[{$dontFormatEntity=True}, 
        MakeBoxes[e, StandardForm]]
      },
      TemplateBox[{MakeTypesetBoxes[fname,StandardForm], boxes, tooltip, label}, "EntityClass"]
    ]/; OKEntityNameQ[fname]
  ]

EntityFramework`MakeEntityFrameworkBoxes[e:EntityClass[etype_String, ename:(_String|_List|All), opts___], TraditionalForm] := 
  With[{fname = GetEntityName[e, $entitynametimeout]},
    With[{boxes = MakeTypesetBoxes[fname, TraditionalForm]},
      InterpretationBox[boxes, e, BaseStyle -> "EntityClass"]
    ]/;OKEntityNameQ[fname]
  ]

(* EntityPropertyClass[] formatting *)

EntityFramework`MakeEntityFrameworkBoxes[e:EntityPropertyClass[ptype_String, pname:(_String|_List), opts:OptionsPattern[]], StandardForm]/;!TrueQ[$dontFormatEntity] := 
  With[{fname = GetEntityName[e, $entitynametimeout]},
    With[{tooltip = makeTooltip[e],
      boxes = Block[{$dontFormatEntity=True}, MakeBoxes[e, StandardForm]]},
      TemplateBox[{MakeTypesetBoxes[fname,StandardForm], boxes, tooltip}, "EntityPropertyClass"]
    ]/; OKEntityNameQ[fname]
  ]
  
EntityFramework`MakeEntityFrameworkBoxes[e:EntityPropertyClass[ptype_String, pname:(_String|_List), opts:OptionsPattern[]], TraditionalForm] := 
  With[{fname = GetEntityName[e, $entitynametimeout]},
    With[{boxes = MakeTypesetBoxes[fname, TraditionalForm]},
      InterpretationBox[boxes, e, BaseStyle -> "EntityPropertyClass"]
    ]/;OKEntityNameQ[fname]
  ]

(*** Entity Name Cache ***)
(*** Very basic down values based, partly because the plan is to cache it on disk as well ***)
$SystemArchitecture := Switch[$ProcessorType,
	"x86-64", "64Bit",
	"x86", "32Bit",
	_, "32Bit"]
$EntityNamesMXFile = FileNameJoin[{DirectoryName[DirectoryName[$InputFileName]],"Resources",$SystemArchitecture,"EntityNames.mx"}];
$EntityNameCacheDirectory = FileNameJoin[{DirectoryName[DirectoryName[$InputFileName]],"Resources",$SystemArchitecture}]
$MaxEntityCacheSizeTotal = 4*10^6;

$specialCacheEntityTypes = {"City", "Chemical", "WeatherStation"};

initSpecialCacheCases[]:=Module[{},
	Set[specialCacheNotLoadedQ[#],True]&/@$specialCacheEntityTypes
];

markCacheLoaded["City"] := Set[specialCacheNotLoadedQ["City"],False]
markCacheLoaded["Chemical"] := Set[specialCacheNotLoadedQ["Chemical"],False]
markCacheLoaded["WeatherStation"] := Set[specialCacheNotLoadedQ["WeatherStation"],False]

EntityNameCacheFileLookup["City"] = FileNameJoin[{$EntityNameCacheDirectory,"CityEntityNames.mx"}]
EntityNameCacheFileLookup["Chemical"] = FileNameJoin[{$EntityNameCacheDirectory,"ChemicalEntityNames.mx"}]
EntityNameCacheFileLookup["WeatherStation"] = FileNameJoin[{$EntityNameCacheDirectory,"WeatherStationEntityNames.mx"}]

loadMXFile[file_String] := Module[{},
	If[FileExistsQ[file],
		Get[file]
	]
];

initEntityNameCache[] := Module[{},
  Clear[rawENC,rawENClass,rawEPC,rawEPClass,rawEPQ];
  rawENC[__] = None;
  rawENClass[__] = None;
  rawEPC[__] = None;
  rawEPClass[__] = None;
  rawEPQ[__] = None;
  loadMXFile[$EntityNamesMXFile];(*initialize with shipping EntityNames if available*)
  $rawENCSize = 0;(*build-in entities don't count against cache size*)
  specialCacheNotLoadedQ[__]=False;(*reset loading flags for special data-paclet caches*)
  initSpecialCacheCases[];
  EPDC[__] = False;
];
initEntityNameCache[];


EntityNameCacheFetch[Entity[type_, name_]]/;specialCacheNotLoadedQ[type] := Module[{},
	markCacheLoaded[type];
	loadMXFile[EntityNameCacheFileLookup[type]];
	EntityNameCacheFetch[Entity[type,name]]
]
EntityNameCacheFetch[Entity[type_, name_]] := rawENC[{type, name}]  (* add file loading later *)
EntityNameCacheFetch[EntityClass[type_, name_]] := rawENClass[{type, name}]
EntityNameCacheFetch[EntityProperty[type_, name_]] := rawEPC[{type, name}]
EntityNameCacheFetch[EntityPropertyClass[type_, name_]] := rawEPClass[{type,name}]
EntityNameCacheFetch[EntityPropertyQualifier[type_, prop_,name_]] := rawEPQ[{type,prop,name}]
EntityNameCacheFetch[EntityPropertyQualifier[type_, prop_, qualname_, name_]] := With[{first=rawEPQ[{type,prop,qualname,name}]},
	If[UnsameQ[first,None],first,EntityNameCacheFetch[EntityPropertyQualifier[type, prop,name]]]](*TODO: remove once this is finalized*)

cachableNameQ[_?OKEntityNameQ]:=True
cachableNameQ[_Missing] := True
cachableNameQ[_] := False

EntityNameCacheAdd[h_[type_, name_,extra___], fname_?cachableNameQ] := Catch[With[
	{container=Switch[h,
		Entity,rawENC,
		EntityClass,rawENClass,
		EntityProperty,rawEPC,
		EntityPropertyClass,rawEPClass,
		EntityPropertyQualifier, rawEPQ,
		_,Throw[$Failed,$tag]]},
  If[$rawENCSize > $MaxEntityCacheSizeTotal, initEntityNameCache[]];
  If[!OKEntityNameQ[container[{type, name, extra}]], $rawENCSize++];
  container[{type, name, extra}] = fname
],$tag]

EntityPropertyDataCachingAdd[prop:{_String,_String},True] := Set[EPDC[prop],True]

EntityNameCacheClear[] := initEntityNameCache[]

EntityNameCacheSize[] := $rawENCSize;

Internal`AddToEntityNameCache[x_List] := EntityNameCacheAdd[Apply[Entity,#,{1}]]&/@x
Internal`AddToEntityNameCache[entity_List,name_] := EntityNameCacheAdd[Entity@@entity,name]
Internal`AddToEntityNameCache[x__] := EntityNameCacheAdd[x];  (* externally exposed function *)

Internal`PreloadEntityNameCache[expr_]:= Module[{res = {}, entities=Cases[Hold[expr], Entity[type_,name_]/;rawENC[{type,name}]===None, Infinity]},
	warmUpEntityValue[];
	If[entities=!={}, res = Internal`MWACompute["MWAEntityNames",{DeleteDuplicates[entities]}]];
	If[MatchQ[res, (HoldComplete|Hold)[{__Rule}]],
		EntityNameCacheAdd[Entity@@#[[1]], #[[2]]] &/@ ReleaseHold[res];
		Length[res]
		,
		0
	]
]

(* used by Dataset component for faster formatting -- taliesinb *)
Internal`BulkFetchEntityNames[list_] := Module[
	{res = {}, entities= DeleteCases[list, Entity[type_,name_]/;rawENC[{type,name}] =!= None]},
	warmUpEntityValue[];
	If[entities=!={}, 
		 entities = DeleteDuplicates[entities];
		 If[Length[entities] > 32,
		 	GeneralUtilities`TemporaryInformation["Retrieving Entity names"]; (* if GU isn't loaded, well, nothing happens *)
		 ];
		res = Internal`MWACompute["MWAEntityNames",{DeleteDuplicates[entities]}];
	];
	If[MatchQ[res, (HoldComplete|Hold)[{__Rule}]],
		EntityNameCacheAdd[Entity@@#[[1]], #[[2]]] &/@ ReleaseHold[res];
	];		
	Replace[rawENC[{#[[1]],#[[2]]}]& /@ list, None -> $Failed, {1}]
]

(*** End Entity Name Cache code ***)
OKEntityNameQ[_String]:=True
OKEntityNameQ[Null]:=False
OKEntityNameQ[$Failed]:=False
OKEntityNameQ[_$Failed]:=False
OKEntityNameQ[_rawENC]:=False
OKEntityNameQ[_rawENClass]:=False
OKEntityNameQ[_rawEPC]:=False
OKEntityNameQ[_rawEPClass]:=False
OKEntityNameQ[_rawEPQ]:=False
OKEntityNameQ[_Symbol]:=False
OKEntityNameQ[_Missing]:=False
OKEntityNameQ[_Rule]:=False
OKEntityNameQ[_List]:=False
OKEntityNameQ[other_]:=With[{n=ToString[Head[other]]},Catch[(*TODO: put something more robust here*)
	MemberQ[$ContextPath,Quiet[Check[Context[n],Throw[False,OKEntityNameQ]]]],
	OKEntityNameQ
]]
(*TODO:Migrate to MWANames once it's working*)
getEntityNameServer[Entity[type_String,name_]]:=Module[{apires},
  warmUpEntityValue[];
  apires = Internal`MWACompute["MWAEntityNames",{Entity[type, name]}];
  apires = ReleaseHold[apires];
  Which[
  	StringQ[apires], apires,
  	MatchQ[apires,{_Rule}], Last[First[apires]],
  	OKEntityNameQ[apires], apires,
  	True, $Failed
  ]
]

getEntityNameServer[EntityClass[type_String,name_]]:=Module[{apires},
  warmUpEntityValue[];
  apires = Internal`MWACompute["MWANames",{EntityClass[type,name]}];
  apires = ReleaseHold[apires];
  If[!MatchQ[apires,{___,"EntityClassNameRules" -> {_Rule},___}], $Failed, 
  	{type,name}/.("EntityClassNameRules"/.apires)]
]

getEntityNameServer[EntityProperty[type_String,name_]]:=Module[{apires},
  warmUpEntityValue[];
  apires = Internal`MWACompute["MWANames",{EntityProperty[type,name]}];
  apires = ReleaseHold[apires];
  If[!MatchQ[apires,{___,"PropertyNameRules" -> {_Rule},___}], $Failed, 
  	{type,name}/.("PropertyNameRules"/.apires)]
]

getEntityNameServer[EntityProperty[type_String,name_,qualifiers_List]]:=Module[{apires,qual},
  warmUpEntityValue[];
  apires = Internal`MWACompute["MWANames",{EntityProperty[type,name,qualifiers]}];
  apires = ReleaseHold[apires];
  If[!MatchQ[apires,{___,"PropertyNameRules" -> {_Rule},___}], $Failed, 
  	qual=replaceWithDefault["QualifierValueNameRules",apires,{}];
  	qual=({type,name,Sequence@@#}&/@qualifiers)/.qual;
  	qual = (qual /. {type,name,_,x_} :> {type,name,x})/.replaceWithDefault["QualifierValueNameRules",apires,{}];
  	qual = qual /. {type,name,x_} :> x;
  	qual = (qual /. Interval[i_List]:>Row[i,"\"to\""]) /. date_DateObject :> DateString[date];
  	{
  		{type,name}/.replaceWithDefault["PropertyNameRules",apires,{}],
  		qual
  		}]
]

getEntityNameServer[EntityPropertyClass[type_String,name_]]:=Module[{apires},
  warmUpEntityValue[];
  apires = Internal`MWACompute["MWANames",{EntityPropertyClass[type,name]}];
  apires = ReleaseHold[apires];
  If[!MatchQ[apires,{___,"PropertyClassNameRules" -> {_Rule},___}], $Failed, 
  	{type,name}/.("PropertyClassNameRules"/.apires)]
]

getEntityNameServer[other___] := $Failed

makeEPQualifierName[None,___] := None
makeEPQualifierName[propName_, qualNames_List] := Row[
	{propName, "\[ThinSpace]|\[ThinSpace]", Row[
	DeleteCases[qualNames,None],
	";"]}
]


(*special case for EntityProperty with Qualifiers*)
GetEntityName[EntityProperty[etype_,ename_,qualifiers:{_Rule..}], timeout_] :=
Module[{propName = None, qualNames = None},
	propName = EntityNameCacheFetch[EntityProperty[etype, ename]];
	qualNames = Map[EntityNameCacheFetch[EntityPropertyQualifier[etype,ename,Sequence@@#]]&,qualifiers];
	If[Not[FreeQ[{propName,qualNames},None]] && timeout >0 ,
		If[Length[#]===2,{propName,qualNames}=#]&[TimeConstrained[getEntityNameServer[EntityProperty[etype, ename,qualifiers]], timeout, $Failed]];
		EntityNameCacheAdd[EntityProperty[etype, ename],propName];
		Thread[EntityNameCacheAdd[Map[EntityPropertyQualifier[etype, ename, Sequence@@#] &, qualifiers], qualNames]]
	];
	makeEPQualifierName[propName,qualNames]
]

GetEntityName[h_[etype_, ename_, opts___], timeout_] :=
Module[{res = None},
  res = EntityNameCacheFetch[h[etype, ename]];
  If[res === None && timeout > 0,
    res = TimeConstrained[getEntityNameServer@@List[h[etype, ename]], timeout, $Failed];
    EntityNameCacheAdd[h[etype, ename], res]  (* only fires if res is _String *)
  ];
  res
]


(* takes list of message data from MWACompute and issues the messages *)
issueMWAMessages[msg_]:=Cases[
  msg/.{CalculateUtilities`ConvertMWAUtilities`Private`EntityProperty->EntityProperty}, (* temporary context confusion *)
  {{s_Symbol,tag_String},args___}:>Message[MessageName[s,tag],args],Infinity]


Unprotect[EntityProperty,EntityPropertyClass];

(**  Core EntityValue  code **)

ConvertTemporaryMWASymbols[x_, newcontext_String:"Global`"] := Module[{names, new, warning},
	names = Names["Internal`MWASymbols`Temporary`*"];
	If[names==={}, Return[x]];
	new = StringReplace[names,__~~"`"->newcontext];
	warning = Quiet[Select[new, Names[#] =!= {} && Check[ToExpression[#, InputForm, OwnValues], {}] =!= {}&]];
	If[warning =!= {}, Message[EntityValue::val, ToExpression[#,InputForm,HoldForm]&/@warning]];
	x/.((ToExpression[#,InputForm,HoldPattern]:>With[{tmp=Symbol[StringReplace[#,__~~"`"->newcontext]]}, tmp/;True])&/@names)

]

Internal`CacheEntityNames[res_] := Module[{rules,apires=ReleaseHold[res]},
	If[And[!MatchQ[apires,{_Rule..}],MatchQ[apires,{_,_Rule..}]], apires = Rest[apires]];
	If[MatchQ[apires,{_Rule..}],
		rules = replaceWithDefault["EntityNameRules",apires, {}];
	    EntityNameCacheAdd[Entity@@#[[1]], #[[2]]] &/@ rules;
	    rules = replaceWithDefault["EntityClassNameRules",apires, {}];
	    EntityNameCacheAdd[EntityClass@@#[[1]], #[[2]]] &/@ rules;
	    rules = replaceWithDefault["PropertyNameRules",apires, {}];
	    EntityNameCacheAdd[EntityProperty@@#[[1]], #[[2]]] &/@ rules;
	    rules = replaceWithDefault["PropertyClassNameRules",apires, {}];
	    EntityNameCacheAdd[EntityPropertyClass@@#[[1]], #[[2]]] &/@ rules;
	    rules = replaceWithDefault["QualifierValueNameRules", apires, {}];
	    EntityNameCacheAdd[EntityPropertyQualifier@@#[[1]], #[[2]]] &/@ rules;
	    rules = replaceWithDefault["CachingRules", apires, {}];
	    EntityPropertyDataCachingAdd@@#& /@ rules;
	]
]

ReleaseMWAComputeHold=ReleaseHold;   (* possibly add security features here *)
SetAttributes[Internal`MWASymbols`MWAData, HoldAll];(*see 268678*)

replaceWithDefault[expr_,rules_,default_,level_:{0}]:=Replace[expr,Flatten[{rules,_->default}],level]
Unprotect[EntityValue]; (*deal with weird edge-case; sometimes get re-protected during loading*)
$GetEntitiesInBatches = True;(*on by default*)
$CacheEntityMembers = True; (*on by default*)
$CacheEntityData = False; (*off by default*)
$GetEntityDataInBatches = True;(*on by default*)
$GetEntityClassDataInBatches = True;(*on by default*)

$SpecialEVStrings = {
	"Entities","EntityClasses","EntityCount","Properties",
	"EntityCanonicalNames", "EntityClassCanonicalNames","RandomEntities",
	"PropertyCanonicalNames","SampleEntities","SampleEntityClasses",
	"EntityClassCount","PropertyClasses","PropertyClassCanonicalNames",
	"RandomEntityClasses"};
$SpecialEVRanges = {
	{"RandomEntities", _} , {"Entities", _} , {"EntityCanonicalNames", _} , 
	{"EntityClasses", _} , {"RandomEntityClasses", _} , {"Properties", _}, 
	{"PropertyClasses", _}, {"EntityClassCanonicalNames", _}, 
	{"PropertyCanonicalNames", _}, {"PropertyClassCanonicalNames", _}};
$SpecialEVPatterns = Alternatives@@Join[$SpecialEVStrings,$SpecialEVRanges];
$BatchedQualifiers = {"EntityAssociation", "Date", "Note"};
$UnbatchedQualifierPatterns = With[{alts=Alternatives@@$BatchedQualifiers},{_,Except[alts]}];

getQueryID[]:=ExportString[Hash[{$MachineID,$LicenseID,AbsoluteTime[]}],"Base64"](*TODO: replace with CreateUUID if doesn't req loading CloudObject*)
$EVQIDF = True;

EntityValue[args___]/;TrueQ[$EVQIDF]:=Block[{Internal`$QueryID=getQueryID[],$EVQIDF=False,res},
		res = iEntityValue[args];
		res /; res =!= $Failed
	]
EntityValue[args___] := With[{res=iEntityValue[args]},
	res /; res =!= $Failed
]

iEntityValue[group:(_Entity|_String|_EntityClass),type:("Entities"|"EntityCanonicalNames")]/;TrueQ[$GetEntitiesInBatches] := GetAllEntities[group,type]
iEntityValue[type:(_Entity|_String), classtype:("EntityClasses"|"EntityClassCanonicalNames")]/;TrueQ[$GetEntitiesInBatches] := GetAllEntityClasses[type, classtype]
iEntityValue[Entity[type_,pat___,Span[start_Integer,end_Integer]],"Entities"]/;TrueQ[$CacheEntityMembers] := Block[
	{$CacheEntityMembers=False},
EntityFramework`Caching`cacheEntityMembers[{type,pat}, {start, end}]
]
iEntityValue[entity:(_Entity|{_Entity..}),property:(_String|_EntityProperty)]/;TrueQ[$CacheEntityData] := Block[{$CacheEntityData},
	EntityFramework`Caching`cacheEntityData[entity,property]]
	
iEntityValue[input_,props_List,arg___]/;Not[TrueQ[$EPLF]] := Block[{
	$EPLF = True, 
	$EntityValueListThresholdValue = Max[1,Floor[2*$EntityValueListThresholdValue/Max[Length[props],1]]]},
	iEntityValue[input,props,arg]
]
iEntityValue[list:{(_Entity|_Missing)..},args___]/;TrueQ[Length[list]>$EntityValueListThresholdValue]:= GetEntityValueInChunks[list,args]
iEntityValue[type:(_String|Entity[_String]),args__] /; TrueQ[$GetEntityDataInBatches] := Block[{$GetEntityDataInBatches=False},
	If[MatchQ[{args},{$SpecialEVPatterns}|$UnbatchedQualifierPatterns],
		EntityValue[type,args],
		GetAllEntityValues[type,args]
	]
]
iEntityValue[class_EntityClass,args__]/;TrueQ[$GetEntityClassDataInBatches] := Block[{$GetEntityClassDataInBatches=False},
	If[MatchQ[{args},{$SpecialEVPatterns}|$UnbatchedQualifierPatterns],
		EntityValue[class,args],
		GetAllEntityClassValues[class,args]
	]
]
iEntityValue[list:{_EntityClass..}, args___] /;And[!TrueQ[$galf], Length[list]>1]:= Block[{$galf=True},With[{groups=Table[{i, i}, {i, Length[list]}]},
	EntityFramework`Dialog`interruptableDataDownloadManager[groups,list,args]]]
iEntityValue[e___]:= callMWACompute[e]

callMWACompute[args___] := With[{e = optionsToQualifier[args]},warmUpEntityValue[];
Module[{res,apires, msg},Block[
	{WolframAlphaClient`Private`$AlphaQueryMMode=If[
		MemberQ[$EVMMODES,WolframAlphaClient`Private`$AlphaQueryMMode],
		WolframAlphaClient`Private`$AlphaQueryMMode,
		"entity"], usys = OptionValue[EntityProperty,UnitSystem]},
    apires=Internal`MWACompute["MWACalculateData",Internal`MWASymbols`MWAData[e, "Version"->0.1	], "UnitSystem"->usys];
    apires=ReleaseMWAComputeHold[apires];
    If[SameQ[apires,$Failed["ComputationTimeout"]],Message[EntityValue::timeout,EntityValue]];
    If[!OptionQ[apires], res = $Failed];
    If[res =!= $Failed, 
	    res=replaceWithDefault["Result",apires,$Failed];
	    msg = replaceWithDefault["Messages", apires, {}];
	    If[msg =!= {}, issueMWAMessages[msg]];
		res = ConvertTemporaryMWASymbols[res]/.$Failed[_]->$Failed;
    ];
	res]
]]

optionsToQualifier[input___] := Sequence@@({input}/.EntityProperty[type_,prop_,opts__]:>EntityProperty[type,prop,fixQualifiers[opts]])
fixQualifiers[l_List] := l
fixQualifiers[r:Rule[UnitSystem,_]] := {r}
(*fixQualifiers[l:(_List..)] := Join[l]*)
fixQualifiers[l_List,r:Rule[UnitSystem,_]] := Append[l,r]
fixQualifiers[r:Rule[UnitSystem,_],l_List] := Prepend[l,r]
fixQualifiers[other__]:=other

$WarmUp = True;

warmUpEntityValue[] /; TrueQ[$WarmUp] := AbortProtect[Module[{cell},
  cell = If[TrueQ[$Notebooks], PrintTemporary[EntityFramework`Dialog`loadingPanel[#]]&, Print][
   "Initializing Wolfram Knowledgebase connection ...."];
  Needs["WolframAlphaClient`"];
  $UnitSystem(*calls WA via MWACompute and sets $UnitSystem*);
  Set[$WarmUp, False];
  If[UnsameQ[cell,Null],Quiet[NotebookDelete[cell]]];
  ]
]


Experimental`FindEntities[s_String,filter_:Automatic]:=Module[{res,apires, rules},
  warmUpEntityValue[];
  apires=Internal`MWACompute["MWAFindEntities",{s,filter}];
  apires=ReleaseMWAComputeHold[apires];
  If[!OptionQ[apires], Message[Internal`FindEntities::TODO]; Return[$Failed]];
  res=replaceWithDefault["Result",apires,$Failed];
  If[MatchQ["Messages"/.apires,{__}],Message[Internal`FindEntities::TODO]];   
  rules = replaceWithDefault["EntityNameRules",apires, {}];
  EntityNameCacheAdd[Entity@@#[[1]], #[[2]]] &/@ rules;
  ConvertTemporaryMWASymbols[res]
]

GetEntityNames[expr_, n_] := Catch[
 Module[{apires}, warmUpEntityValue[];
  apires = TimeConstrained[
  	Internal`MWACompute["MWANames", {expr}],
  	n,
  	Throw[Table[Missing["NotAvailable"],{i,Length[expr]}],$tag]];(*populate with missings if timed out*)
  apires = ReleaseHold[apires];
  cleanUpFormatNameResults[EntityNameCacheFetch/@expr]
],$tag]
  
cleanUpFormatNameResults[res_List] := Map[
	If[OKEntityNameQ[#],#,Missing["NotAvailable"]]&,
	res]
cleanUpFormatNameResults[other_] := other
  
bulkFetchCommonNames[entities_List, n_] := Module[{res = 
   Reap[Map[
     With[{r = EntityNameCacheFetch[#]}, 
       If[cachableNameQ[r], r, Sow[#]; sowPlaceholder[#]]] &, entities]], ask, temp}, 
    ask = Last[res];
    If[UnsameQ[ask, {}], 
    	ask = First[ask];(*remove outer list*)
    	temp = GetEntityNames[ask, n];
    	ask = Thread[sowPlaceholder /@ ask -> temp];
    	res = First[res] /. ask,(*TODO:handle errors*)
    res = First[res]];
  res]

Entity/:EntityProperty[ep__][Entity[e__]] := EntityValue[Entity[e],EntityProperty[ep]]
Entity/:EntityPropertyClass[epc__][Entity[e__]] := EntityValue[Entity[e],EntityPropertyClass[epc]]
Entity/:Entity[e__][args__] := EntityValue[Entity[e],args]

EntityClass/:EntityProperty[ep__][EntityClass[ec__]] := EntityValue[EntityClass[ec],EntityProperty[ep]]
EntityClass/:EntityPropertyClass[epc__][EntityClass[ec__]] := EntityValue[EntityClass[ec],EntityPropertyClass[epc]]
EntityClass/:EntityClass[e__][args__] := EntityValue[EntityClass[e],args]

EntityProperty/:Entity[e__][EntityProperty[ep__]] := EntityValue[Entity[e],EntityProperty[ep]]
EntityProperty/:EntityClass[ec__][EntityProperty[ep__]] := EntityValue[EntityClass[ec],EntityProperty[ep]]

EntityPropertyClass/:Entity[e__][EntityPropertyClass[epc__]] := EntityValue[Entity[e],EntityPropertyClass[epc]]
EntityPropertyClass/:EntityClass[ec__][EntityPropertyClass[epc__]] := EntityValue[EntityClass[ec],EntityPropertyClass[epc]]

EntityProperties[e_] := With[{res=EntityValue[e, "Properties"]},res/;!MatchQ[res,_EntityValue]]
EntityProperties[args___] := (ArgumentCountQ[EntityProperties,Length[{args}],1,1];Null/;False)

EntityTypeName[e:(Entity|EntityClass|EntityProperty|EntityPropertyClass)[type_,___]]:= type
EntityTypeName[l_List] := Map[EntityTypeName,l]
EntityTypeName[args___]:=(ArgumentCountQ[EntityTypeName,Length[{args}],1,1];Null/;False)
EntityTypeName[arg:Except[_Entity|_String|_EntityClass|_EntityProperty|_EntityPropertyClass|_List]] := (Message[EntityTypeName::noent,arg];Null/;False)

CommonName[e:(Entity|EntityClass|EntityProperty|EntityPropertyClass)[type_,name_,___]]:= With[{res=GetEntityName[e, 10]},If[OKEntityNameQ[res],res,Missing["NotAvailable"]]]
CommonName[entities:{(_Entity|_EntityClass|_EntityProperty|_EntityPropertyClass|_Missing)..}] := bulkFetchCommonNames[entities,10]
CommonName[l_List] := Map[CommonName,l]
CommonName[args___]:=(ArgumentCountQ[CommonName,Length[{args}],1,1];Null/;False)
CommonName[arg:Except[_Entity|_EntityProperty|_EntityClass|_EntityPropertyClass|_List]] := (Message[CommonName::noent,arg];Null/;False)

CanonicalName[e:(Entity|EntityClass|EntityProperty|EntityPropertyClass)[type_, name_, ___]] := name
CanonicalName[l_List] := Map[CanonicalName,l]
CanonicalName[args___]:=(ArgumentCountQ[CanonicalName,Length[{args}],1,1];Null/;False)
CanonicalName[arg:Except[_Entity|_EntityProperty|_EntityClass|_EntityPropertyClass|_List]] := (Message[CanonicalName::noent,arg];Null/;False)

EntityList[type:(_String|_Entity|_EntityClass)] := With[{res=EntityValue[type,"Entities"]},res/;MatchQ[res,_List]]
EntityList[args___]:=(ArgumentCountQ[EntityList,Length[{args}],1,1];Null/;False)
EntityList[arg:Except[_Entity|_String|_EntityClass]] := (Message[EntityList::noent,arg];Null/;False)

EntityClassList[type:(_Entity|_String)] := With[{res=EntityValue[type,"EntityClasses"]},res/;MatchQ[res,_List]]
EntityClassList[args___]:=(ArgumentCountQ[EntityClassList,Length[{args}],1,1];Null/;False)
EntityClassList[arg:Except[_Entity|_String]] := (Message[EntityClassList::noent,arg];Null/;False)

(*** Wrappers for *Data functions ***)
$EVDataPacletHeads = Hold[System`AdministrativeDivisionData, System`AircraftData,
System`AirportData, 
System`BridgeData, System`BroadcastStationData, System`BuildingData, System`CometData,
System`CompanyData, System`ConstellationData, System`DamData, 
System`DeepSpaceProbeData, System`EarthImpactData, 
System`ExoplanetData, System`GalaxyData,
System`GeologicalPeriodData, System`HistoricalPeriodData, 
System`IslandData, System`LanguageData, System`LakeData, 
System`LaminaData, System`MannedSpaceMissionData, 
System`MedicalTestData, System`MeteorShowerData, 
System`MineralData, System`MinorPlanetData, System`MountainData, 
System`MovieData, System`NebulaData,
System`NeighborhoodData, System`NuclearExplosionData, 
System`NuclearReactorData, System`OceanData, System`ParkData, 
System`ParticleAcceleratorData, 
System`PersonData, System`PhysicalSystemData, System`PlaneCurveData, 
System`PlanetData, System`PlanetaryMoonData, System`PlantData, 
System`PulsarData, System`SatelliteData, 
System`SolarSystemFeatureData, System`SolidData, System`SpaceCurveData, 
System`SpeciesData, System`StarData, System`StarClusterData, System`SupernovaData, 
System`SurfaceData, System`TropicalStormData, 
System`TunnelData, System`UnderseaFeatureData, 
System`UniversityData, System`VolcanoData, System`ZIPCodeData];

$entityStandardNamePattern[dp_] = _String;
$entityStandardNamePattern["Acronym" | "AdministrativeDivision" | "City" | "GivenName"] = {__String};
$entityStandardNamePattern["AlgebraicCode"] = {_String, {__Integer}};
$entityStandardNamePattern["AreaCode"] = _Integer;
$entityStandardNamePattern["CrystallographicSpaceGroup"] = {_String, _Integer};
$entityStandardNamePattern["Gene"] = {_String, {"Species" -> _String}};

$dataHeadToEntityType = (# -> StringReplace[SymbolName[#], RegularExpression["Data$"] -> ""]) & /@ 
	List @@ EntityFramework`Private`$EVDataPacletHeads;

(dataHeadToEntityTypeLookup[#[[1]]] = #[[2]]) & /@ $dataHeadToEntityType;

dataHeadToEntityTypeLookup[_] = None;

Unprotect@@$EVDataPacletHeads;

Clear@@$EVDataPacletHeads;

(#[args___] := With[{res=EVDataPacletDispatch[#, {args}]}, res/;res=!=$Failed]) & /@ (List@@$EVDataPacletHeads);

With[{heads=List@@$EVDataPacletHeads}, SetAttributes[heads,ReadProtected]];
Protect@@$EVDataPacletHeads;

(* TODO: add flag to indicate entity or entity class when not specified *)
Clear[EVDataPacletDispatch];
EVDataPacletDispatch[head_, args_] := Module[{etype, res},
  etype = dataHeadToEntityTypeLookup[head];
  If[etype === None, Return[$Failed] (*shouldn't happen*)]; Block[{WolframAlphaClient`Private`$AlphaQueryMMode="paclet"},
  res = Switch[args,
   {} | {All | "Entities"}, 
       EntityValue[Entity[etype], "Entities"],
   {"Classes"|"EntityClasses"}, 
       EntityValue[Entity[etype], "EntityClasses"],
   {"Properties" | "PropertyCanonicalNames" | "SampleEntities" | "SampleEntityClasses"| 
   	"EntityCanonicalNames" | "EntityCount" | "EntityClassCount" | "EntityClassCanonicalNames"|
   	"RandomEntityClasses"|"PropertyClassCanonicalNames"|"PropertyClasses"|"RandomEntities"},
       EntityValue[etype, args[[1]]],
   {Alternatives@@$SpecialEVRanges},
   	   EntityValue[etype, args[[1]]],
   {_, _, _, __},(*too many args*)
   	   ArgumentCountQ[head,Length[args],0,3];$Failed,
   {$entityStandardNamePattern[etype], ___},
       If[ValidArgsForEtypeQ[head, etype, args],EntityValue[Entity[etype, args[[1]]], Sequence @@ Rest[args]],$Failed],
   {{($entityStandardNamePattern[etype] | _Entity) ..}, ___},
       If[ValidArgsForEtypeQ[head, etype, args],EntityValue[If[MatchQ[#, _Entity], #, Entity[etype, #]] & /@ args[[1]], Sequence @@ Rest[args]],$Failed],
   {Entity[etype,___], ___},
       If[ValidArgsForEtypeQ[head, etype, args],EntityValue @@ args,$Failed],
   {EntityClass[etype,___]},
   	   If[ValidArgsForEtypeQ[head, etype, args],EntityValue[First[args],"Entities"],$Failed],
   {EntityClass[etype,___],___},
   	   If[ValidArgsForEtypeQ[head, etype, args],EntityValue @@ args,$Failed],
   {EntityPropertyClass[etype,___] ___},
       If[ValidArgsForEtypeQ[head, etype, args],EntityValue @@ args,$Failed],
   _,
       With[{arg=If[ListQ[args]&&Length[args]>0,First[args],Null]},(*safety valve in case we have bad arguments; shouldn't actually need this...*)
       	Message[head::notent,arg,head];$Failed]
   ]];
   If[MatchQ[res, $Failed|_EntityValue], res = $Failed];
   res
  ]
  
ValidArgsForEtypeQ[head_,etype_,args_List] := Switch[args,
	{},True,
	{_}, True,
	{_,_String,___},True,
	{_,(EntityProperty|EntityPropertyClass)[etype,__],___},True,
	{_,_List,___},True,(*TODO: fine-tune this; need to support things like {"RandomEntities",8} on top of _EntityProperty.. and _EntityPropertyClass..*)
	{_,_,___},With[{prop=Part[args,2]},Message[head::notprop,prop,head];False],
	_, False
]
  
(*** End code for wrappers ***)
$EVMMODES= {"utility", "paclet", "entity", "semantic"};

entityCount[class:(_String|_Entity|_EntityClass)] := Block[{WolframAlphaClient`Private`$AlphaQueryMMode="utility"},
	With[{r = EntityValue[class, "EntityCount"]}, 
		If[MatchQ[r,_Missing], Throw[r,$tag]];
		Set[entityCount[class], r]/;IntegerQ[r]]]
entityCount[___] := Throw[$Failed, $tag]

entityClassCount[class:(_String|_Entity)] := Block[{WolframAlphaClient`Private`$AlphaQueryMMode="utility"},
	With[{r = EntityValue[class, "EntityClassCount"]}, Set[entityClassCount[class], r]/;IntegerQ[r]]]
entityClassCount[___] := Throw[$Failed, $tag]

$EntityBatchThreshold = 2500;
$EntityValueListThresholdValue = 128;

batchEntitiesQ[class:(_String|_Entity|_EntityClass)] := TimeConstrained[
	TrueQ[entityCount[class] > $EntityBatchThreshold],
	10,False]
batchEntitiesQ[__] := False

batchEntityClassesQ[class_] := TimeConstrained[
	TrueQ[entityClassCount[class] > $EntityBatchThreshold],
	10,False]
batchEntityClassesQ[__] := False

divideEntityCountIntoBatches[0] := Throw[{},$tag]
divideEntityCountIntoBatches[n_Integer] /; n > $EntityBatchThreshold := Block[
	{steps = Range[1, n, $EntityBatchThreshold], pairs},
	pairs = {#, # + $EntityBatchThreshold - 1} & /@ steps;
	ReplacePart[pairs, -1 -> {pairs[[-1, 1]], n}]]
	
divideEntityCountIntoBatches[n_Integer] /; n > 0 := {{1, n}}
divideEntityCountIntoBatches[___] := Throw[$Failed, $tag]

wrapWithAppropriateHead[etype_String,{start_,stop_},type_String] := wrapWithAppropriateHead[etype,_,{start,stop},type]
wrapWithAppropriateHead[etype_String,pat_,{start_,stop_},type_]:= Switch[type,
	"Entities"|"EntityCanonicalNames",Entity[etype,pat,Span[start,stop]],
	_, EntityClass[etype,pat,Span[start,stop]]
]

wrapWithAppropriateHead[EntityClass[etype_],{start_,stop_},type_String] := wrapWithAppropriateHead[EntityClass[etype,_],{start,stop},type]
wrapWithAppropriateHead[class:EntityClass[etype_, pat_],{start_,stop_},type_String] := Switch[type,
	"Entities"|"EntityCanonicalNames",Entity[class,_,Span[start,stop]],
	_,EntityClass[etype,pat,Span[start,stop]]
]

downloadEntityBatch[etype_String,{start_Integer,stop_Integer},type_String] := Module[{},
	If[ListQ[#],#,Message[EntityValue::batmis,start,stop];{}]&[EntityValue[wrapWithAppropriateHead[etype,{start,stop},type], type]]
]
downloadEntityBatch[Entity[etype_],args__] := downloadEntityBatch[Entity[etype,_],args]
downloadEntityBatch[Entity[etype_,pat_,___],{start_Integer,stop_Integer},type_String] := Module[{},
	If[ListQ[#],#,Message[EntityValue::batmis,start,stop];{}]&[EntityValue[wrapWithAppropriateHead[etype,pat,{start,stop},type], type]]
]
downloadEntityBatch[class_EntityClass, {start_Integer, stop_Integer}, type_String] := Module[{},
 If[ListQ[#],#,Message[EntityValue::batmis,start,stop];{}]&[EntityValue[wrapWithAppropriateHead[class,{start,stop},type], type]]
]
downloadEntityBatch[___] := Throw[$Failed, $tag]

GetAllEntities[class:(_Entity|_String|_EntityClass), type_String] := Block[{$GetEntitiesInBatches=False},Catch[
  With[{batches = divideEntityCountIntoBatches[entityCount[class]]},
    EntityFramework`Dialog`interruptableEntityDownloadManager[class, batches, type]], $tag]
]

GetAllEntityValues[type_, args__] := Block[{$GetEntityDataInBatches = False,$EntityBatchThreshold=$EntityValueListThresholdValue},Catch[
  If[batchEntitiesQ[type],
   With[{batches = divideEntityCountIntoBatches[entityCount[type]]},
    EntityFramework`Dialog`interruptableDataDownloadManager[type, batches, {args}]],
   EntityValue[type, args]
   ], $tag]
	
]

GetAllEntityClassValues[class_EntityClass, args__] := Block[{
	$GetEntityClassDataInBatches = False,$EntityBatchThreshold=$EntityValueListThresholdValue},Catch[
  If[batchEntitiesQ[class],
   With[{batches = divideEntityCountIntoBatches[entityCount[class]]},
    EntityFramework`Dialog`interruptableDataDownloadManager[class, batches, {args}]],
   EntityValue[class, args]
   ], $tag]
	
]

GetAllEntityClasses[class_, type_String] := Block[{$GetEntitiesInBatches=False},Catch[
  If[batchEntityClassesQ[class],
   With[{batches = divideEntityCountIntoBatches[entityClassCount[class]]},
    EntityFramework`Dialog`interruptableEntityDownloadManager[class, batches, type]],
   EntityValue[class, type]
   ], $tag]
]

GetEntityValueInChunks[entities_,args___] := Block[{$EntityBatchThreshold=$EntityValueListThresholdValue,groups},
	groups=divideEntityCountIntoBatches[Length[entities]];
	EntityFramework`Dialog`interruptableDataDownloadManager[groups,entities,args]
]

(*keep various flags & symbols from triggering Dynamic updating*)
Internal`SetValueNoTrack[{$dontFormatEntity,$EVQIDF,Internal`$QueryID,$EPLF}, True];

With[{s=$readProtectedSymbols},SetAttributes[s,{ReadProtected}]];
Protect@@$readProtectedSymbols;

If[FileExistsQ[#],Get[#]]&[FileNameJoin[{DirectoryName[$InputFileName],"Caching.m"}]];
If[FileExistsQ[#],Get[#]]&[FileNameJoin[{DirectoryName[$InputFileName],"Dialog.m"}]];
If[FileExistsQ[#],Get[#]]&[FileNameJoin[{DirectoryName[$InputFileName],"ToFromEntity.m"}]];
   
End[];

