
System`Entity;
System`EntityValue;
System`EntityProperty;
System`EntityProperties;
System`CommonName;
System`CanonicalName;
System`TypeName;
Experimental`FindEntities;
System`FormatName;
System`Qualifiers;
System`SourceEntityType;
System`StripWrappers;
Internal`DisableEntityFramework;
Internal`AddToEntityNameCache;
Internal`PreloadEntityNameCache;

EntityFramework`MakeEntityFrameworkBoxes;

Begin["EntityFramework`Private`"];

(*symbols to be protected/read protected (except option names FormatName/Qualifiers/SourceEntityType)*)
$readProtectedSymbols={Entity, EntityValue, EntityProperty, Experimental`FindEntities};
$protectedSymbols = {FormatName, Qualifiers, SourceEntityType};
	
Unprotect@@$readProtectedSymbols;
Unprotect@@$protectedSymbols;



(*** temporary style options until stylesheet is in place ***)

$entityStyleBoxOptions = If[Quiet[!MatchQ[CurrentValue[{StyleDefinitions,"Entity"}], {__}]],
 { 
  ShowStringCharacters -> False, 
  TemplateBoxOptions->{
    Editable -> False,  
    DisplayFunction -> (
      TooltipBox[
        FrameBox[StyleBox[#1, FontFamily -> "Helvetica", FontColor -> RGBColor[0.395437, 0.20595, 0.061158]]], 
        #3
      ]&), 
    InterpretationFunction -> (#2 & )
  },
  FrameBoxOptions -> {
    FrameStyle -> RGBColor[1., 0.504768, 0.], 
    Background -> RGBColor[1., 0.980392, 0.921569], 
    FrameMargins -> {{5, 5}, {1.5, 1.5}}, 
    ImageMargins -> {{2, 2}, {2, 2}}, 
    BaselinePosition -> Scaled[0.3], 
  RoundingRadius -> 4
  }
 }
 ,
 {}
];

$entityPropertyStyleBoxOptions = If[Quiet[!MatchQ[CurrentValue[{StyleDefinitions,"EntityProperty"}], {__}]],
 {
  ShowStringCharacters -> False, 
  TemplateBoxOptions->{
    Editable -> False,  
    DisplayFunction -> (
      TooltipBox[
        FrameBox[StyleBox[#1, FontSlant -> Italic, FontWeight -> Plain, FontFamily -> "Helvetica", FontColor -> RGBColor[0.395437, 0.20595, 0.061158]]], 
        #3
      ]&), 
   InterpretationFunction -> (#2 & )
  },
  FrameBoxOptions -> {
    FrameStyle -> RGBColor[0.94227,0.703639,0.033387], 
    Background -> RGBColor[1., 0.980392, 0.921569], 
    FrameMargins -> {{5, 5}, {1.5, 1.5}}, 
    ImageMargins -> {{2, 2}, {2, 2}}, 
    BaselinePosition -> Scaled[0.3], 
    RoundingRadius -> 4
  }
 }
 ,
 {}
];


(*** Formatting code ***)

(* This first part will move to StartUp/Typeset/TypesetInit.m when I'm sure
   EntityFramework`MakeEntityFrameworkBoxes in in the builds *)
(*protected = Unprotect[System`Entity, System`EntityProperty, EntityFramework`MakeEntityFrameworkBoxes];*)

SetAttributes[EntityFramework`MakeEntityFrameworkBoxes, HoldAllComplete];

Entity /: MakeBoxes[x_Entity, fmt_] := (Entity;(*trigger auto-loader*)
  With[{boxes = EntityFramework`MakeEntityFrameworkBoxes[x, fmt]}, 
   boxes /; boxes =!= $Failed])

EntityProperty /: MakeBoxes[x_EntityProperty, fmt_] := (Entity;(*trigger auto-loader*)
  With[{boxes = EntityFramework`MakeEntityFrameworkBoxes[x, fmt]}, boxes /; boxes =!= $Failed])

(*Protect @@ protected;*)

(* MakeExpression rules are needed when no front end is present *)

MakeExpression[TemplateBox[{_, interp_, ___}, "Entity", ___], fmt_] := MakeExpression[interp, fmt];

MakeExpression[TemplateBox[{_, interp_, ___}, "EntityProperty", ___], fmt_] := MakeExpression[interp, fmt];

$entitynametimeout = 0.8; (* Seconds timeout for single entity formatting *)

makeTooltip[e_] := With[{str=ToString[DeleteCases[e, FormatName|SourceEntityType -> _, Infinity], InputForm]}, 
   MakeBoxes[str, StandardForm]
]

entityLabelRules = {
  "AdministrativeDivision"->"administrative division",
  "Aircraft"->"aircraft",
  "Airline"->"airline",
  "Airport"->"airport",
  "AmusementPark"->"amusement park",
  "AmusementParkRide"->"amusement park ride",
  "AmusementParkType"->"amusement park type",
  "Artwork"->"artwork",
  "Astronomical"->"astronomical entity",
  "AstronomicalObservatory"->"astronomical observatory",
  "AtmosphericLayer"->"atmospheric layer",
  "Battery"->"battery",
  "Biomolecule"->"biomolecule",
  "BoardGame"->"board game",
  "Book"->"book",
  "Bridge"->"bridge",
  "BroadcastStation"->"broadcast station",
  "BroadcastStationClassification"->"broadcast station classification",
  "Building"->"building",
  "Castle"->"castle",
  "CatBreed"->"cat breed",
  "Character"->"character",
  "Chemical"->"chemical",
  "City"->"city",
  "ClimateType"->"climate type",
  "Cloud"->"cloud",
  "Color"->"color",
  "CommonMaterial"->"common material",
  "Company"->"company",
  "ComputationalComplexityClass"->"computational complexity class",
  "Constellation"->"constellation",
  "ContinuedFraction"->"continued fraction",
  "ContinuedFractionSource"->"continued fraction source",
  "Country"->"country",
  "CrystalFamily"->"crystal family",
  "CrystallographicSpaceGroup"->"crystallographic space group",
  "CrystalSystem"->"crystal system",
  "Dam"->"dam",
  "DeepSpaceProbe"->"deep space probe",
  "Desert"->"desert",
  "Dinosaur"->"dinosaur",
  "Disease"->"disease",
  "DisplayFormat"->"display format",
  "DistrictCourt"->"district court",
  "DogBreed"->"dogbreed",
  "DrugChemical"->"drugchemical",
  "EarthImpact"->"earthimpact",
  "EclipseType"->"eclipsetype",
  "Element"->"element",
  "EthnicGroup"->"ethnic group",
  "FamousAlgorithm"->"algorithm",
  "FamousChemistryProblem"->"chemistry problem",
  "FamousGem"->"gem",
  "FamousMathGame"->"math game",
  "FamousMathProblem"->"math problem",
  "FamousPhysicsProblem"->"physics problem",
  "FictionalCharacter"->"fictional character",
  "FileFormat"->"file format",
  "Financial"->"financial entity",
  "FiniteGroup"->"finite group",
  "FrequencyAllocation"->"frequency allocation range",
  "Gene"->"gene",
  "Geodesy"->"geodetic entity",
  "GeologicalLayer"->"geological layer",
  "GeologicalPeriod"->"geological period",
  "Geometry"->"geometry",
  "GivenName"->"given name",
  "Glacier"->"glacier",
  "Graph"->"graph",
  "HistoricalCountry"->"historical country",
  "HistoricalEvent"->"historical event",
  "HistoricalPeriod"->"historical period",
  "HistoricalSite"->"historic site",
  "Hospital"->"hospital",
  "IceAge"->"iceage",
  "IntegerSequence"->"integer sequence",
  "Invention"->"invention",
  "Island"->"island",
  "Isotope"->"isotope",
  "Knot"->"knot",
  "Lake"->"lake",
  "Lamina"->"lamina",
  "Language"->"language",
  "Lattice"->"lattice",
  "LatticeSystem"->"lattice system",
  "Library"->"library",
  "MannedSpaceMission"->"mannedspacemission",
  "MathematicalFunctionIdentity"->"mathematicalfunctionidentity",
  "MathWorld"->"Math World topic",
  "MatterPhase"->"matter phase",
  "MedicalTest"->"medical test",
  "MeteorShower"->"meteor shower",
  "Mine"->"mine",
  "Mineral"->"mineral",
  "MoonPhase"->"moonphase",
  "Mountain"->"mountain",
  "Movie"->"movie",
  "Museum"->"museum",
  "MusicAct"->"music act",
  "MusicAlbum"->"music album",
  "MusicAlbumRelease"->"music album release",
  "MusicalInstrument"->"musical instrument",
  "MusicWork"->"music work",
  "MusicWorkRecording"->"music work recording",
  "Mythology"->"mythological figure",
  "NaturalHazard"->"natural hazard",
  "NaturalResource"->"natural resource",
  "NotableComputer"->"computer",
  "NuclearExplosion"->"nuclear explosion",
  "NuclearReactor"->"nuclear reactor",
  "ObjectStatus"->"object status",
  "Ocean"->"ocean",
  "Park"->"park",
  "Particle"->"particle",
  "ParticleAccelerator"->"particle accelerator",
  "Periodical"->"periodical",
  "PeriodicTiling"->"periodic tiling",
  "Person"->"person",
  "PhysicalSystem"->"physical system",
  "PlaneCurve"->"plane curve",
  "Pokemon"->"Pokemon",
  "PokemonAbilities"->"Pokemon ability",
  "PokemonGeneration"->"Pokemon generation",
  "PokemonPokedexColor"->"Pokedex color",
  "PokemonType"->"Pokemon type",
  "Polyhedron"->"polyhedron",
  "PopularCurve"->"popular curve",
  "PreservationStatus"->"historic preservation status",
  "Protein"->"protein",
  "Religion"->"religion",
  "River"->"river",
  "Rocket"->"rocket",
  "RocketFunction"->"rocket function",
  "Satellite"->"satellite",
  "Ship"->"ship",
  "SNP"->"SNP",
  "SolarSystemFeature"->"solar system feature",
  "SpaceCurve"->"space curve",
  "Species"->"species specification",
  "SportObject"->"sport object",
  "Stadium"->"stadium",
  "StormType"->"storm type",
  "Surface"->"surface",
  "Surname"->"surname",
  "TerrainType"->"terrain type",
  "TropicalStorm"->"tropical storm",
  "Tunnel"->"tunnel",
  "UnderseaFeature"->"undersea feature",
  "University"->"university",
  "USPublicSchool"->"public school",
  "USSchoolDistrict"->"school district",
  "VisualArtsArtForm"->"art form",
  "VisualArtsArtGenre"->"art genre",
  "Volcano"->"volcano",
  "WallpaperGroup"->"wallpaper group",
  "Waterfall"->"waterfall",
  "Wood"->"wood",
  "Word"->"word",
  "WritingSystem"->"writing system",
  "ZIPCode"->"ZIP code"
};

EntityFramework`MakeEntityFrameworkBoxes[__] = $Failed;

EntityFramework`MakeEntityFrameworkBoxes[e:Entity[etype_, ename_, opts___], StandardForm]/;!TrueQ[$dontFormatEntity] := 
  With[{fname = GetEntityName[e, $entitynametimeout]},
    With[{tooltip = makeTooltip[e],
      label = ToBoxes[If[StringQ[Unevaluated[etype]], etype /. entityLabelRules, "unknown"], StandardForm],
      boxes = Block[{$dontFormatEntity=True}, 
        With[{strip=DeleteCases[e, "InternalSetFormatName"->_, Infinity]}, MakeBoxes[strip, StandardForm]]]
      },
      TemplateBox[{MakeBoxes[fname,StandardForm], boxes, tooltip, label}, "Entity",
          Sequence @@ If[$entityStyleBoxOptions === {}, {}, {InterpretationFunction -> (#2&), BaseStyle -> $entityStyleBoxOptions}]
      ]
    ]/; StringQ[fname]
  ]

EntityFramework`MakeEntityFrameworkBoxes[e:Entity[etype_, ename_, opts___], TraditionalForm] := 
  With[{fname = GetEntityName[e, $entitynametimeout]}, InterpretationBox[fname, e]/;StringQ[fname]]

EntityFramework`MakeEntityFrameworkBoxes[e:EntityProperty[pname_, opts:OptionsPattern[]], StandardForm]/;!TrueQ[$dontFormatEntity] := 
  With[{fname = OptionValue[EntityProperty,{opts},FormatName]},
    With[{tooltip = makeTooltip[e],
      boxes = Block[{$dontFormatEntity=True}, MakeBoxes[e, StandardForm]]},
      TemplateBox[{MakeBoxes[fname,StandardForm], boxes, tooltip}, "EntityProperty",
          Sequence @@ If[$entityPropertyStyleBoxOptions === {}, {}, {InterpretationFunction -> (#2&), BaseStyle -> $entityPropertyStyleBoxOptions}]]
    ]/; StringQ[fname]
  ]
  
EntityFramework`MakeEntityFrameworkBoxes[e:EntityProperty[pname_, opts:OptionsPattern[]], TraditionalForm] := 
  With[{fname = OptionValue[EntityProperty,{opts},FormatName]}, InterpretationBox[fname, e]/;StringQ[fname]]



(*** Entity Name Cache ***)
(*** Very basic down values based, partly because the plan is to cache it on disk as well ***)
$EntityNamesMXFile = FileNameJoin[{DirectoryName[DirectoryName[$InputFileName]],"Resources",$SystemID,"EntityNames.mx"}];
$MaxEntityCacheSizeTotal = 10^6;

loadEntityNamesMX[file_String] := Module[{},
	If[FileExistsQ[file],
		Get[file](*TODO: add any other special error checking incase MX file is garbled*)
	]
];

initEntityNameCache[] := Module[{},
  Clear[rawENC];
  rawENC[__] = None;
  loadEntityNamesMX[$EntityNamesMXFile];(*initialize with shipping EntityNames if available*)
  $rawENCSize = 0;(*build-in entities don't count against cache size*)
];
initEntityNameCache[];

EntityNameCacheFetch[{type_, name_}] := rawENC[{type, name}]  (* add file loading later *)
EntityNameCacheAdd[{type_, name_}, fname_String] := Module[{},
  If[$rawENCSize > $MaxEntityCacheSizeTotal, initEntityNameCache[]];
  If[!StringQ[rawENC[{type, name}]], $rawENCSize++];
  rawENC[{type, name}] = fname
]
EntityNameCacheClear[] := initEntityNameCache[]
EntityNameCacheSize[] := $rawENCSize;
Internal`AddToEntityNameCache[x_List] := EntityNameCacheAdd/@x
Internal`AddToEntityNameCache[x__] = EntityNameCacheAdd[x];  (* externally exposed function *)

Internal`PreloadEntityNameCache[expr_]:= Module[{res = {}, entities=Cases[Hold[expr], Entity[type_,name_]/;rawENC[{type,name}]=!=None, Infinity]},
	If[entities=!={}, res = Internal`MWACompute["MWAEntityNames",{DeleteDuplicates[entities]}]];
	If[MatchQ[res, {__Rule}],
		EntityNameCacheAdd[#[[1]], #[[2]]] &/@ res;
		Length[res]
		,
		0
	]
]

(*** End Entity Name Cache code ***)


getEntityNameServer[type_String,name_]:=Module[{apires},
  apires = Internal`MWACompute["MWAEntityNames",{{type, name}}];
  apires = ReleaseHold[apires];
  If[!StringQ[apires], $Failed, apires]
]

Attributes[GetEntityName] = {HoldFirst}; (* to prevent auto-stripping of InternalSetFormatName option *)
GetEntityName[Entity[etype_, ename_, "InternalSetFormatName" -> fname_, ___], _] := Module[{},
  EntityNameCacheAdd[{etype, ename}, fname];
  fname
]
GetEntityName[Entity[etype_, ename_, opts___], timeout_] :=
Module[{res = None},
  res = Cases[Flatten[{opts}], ("FormatName"|FormatName->x_):>x];
  If[res =!= {}, Return[res[[1]]]];
  res = EntityNameCacheFetch[{etype, ename}];
  If[res === None && timeout > 0,
    res = TimeConstrained[getEntityNameServer[etype, ename], timeout, $Failed];
    EntityNameCacheAdd[{etype, ename}, res]  (* only fires if res is _String *)
  ];
  res
]



(* takes list of message data from MWACompute and issues the messages *)
issueMWAMessages[msg_]:=Cases[
  msg/.{CalculateUtilities`ConvertMWAUtilities`Private`EntityProperty->EntityProperty}, (* temporary context confusion *)
  {{s_Symbol,tag_String},args___}:>Message[MessageName[s,tag],args],Infinity]



Options[EntityProperty]={FormatName->None, Qualifiers->{}, SourceEntityType->{}};

(* Experimental stripping of "InternalSetFormatName", probably will not use this *)
(*
Entity[etype_, ename_, "InternalSetFormatName" -> fname_, rest___] := Module[{},
  EntityNameCacheAdd[{etype, ename}, fname];
  Entity[etype, ename, rest]
]
*)


(**  Core EntityValue  code **)

ConvertTemporaryMWASymbols[x_, newcontext_String:"Global`"] := Module[{names, new, warning},
	names = Names["Internal`MWASymbols`Temporary`*"];
	If[names==={}, Return[x]];
	new = StringReplace[names,__~~"`"->newcontext];
	warning = Quiet[Select[new, Names[#] =!= {} && Check[ToExpression[#, InputForm, OwnValues], {}] =!= {}&]];
	If[warning =!= {}, Message[EntityValue::val, ToExpression[#,InputForm,HoldForm]&/@warning]];
	x/.((ToExpression[#,InputForm,HoldPattern]:>With[{tmp=Symbol[StringReplace[#,__~~"`"->newcontext]]}, tmp/;True])&/@names)

]

ReleaseMWAComputeHold=ReleaseHold;   (* possibly add security features here *)

replaceWithDefault[expr_,rules_,default_,level_:{0}]:=Replace[expr,Flatten[{rules,_->default}],level]

Options[EntityValue] = {StripWrappers -> False};
(* This needs argument checking added *)
$GetEntitiesInBatches = False;(*off by default*)
EntityValue[class:(_Entity|_String),"Entities"]/;TrueQ[$GetEntitiesInBatches] := GetAllEntities[class]
EntityValue[e___]:=Module[{res,apires, rules, msg},
    apires=Internal`MWACompute["MWACalculateData",Internal`MWASymbols`MWAData[e, "Version"->0.1	]];
    apires=ReleaseMWAComputeHold[apires];
    If[!OptionQ[apires], res = $Failed];
    If[res =!= $Failed, 
	    res=replaceWithDefault["Result",apires,$Failed];
	    msg = replaceWithDefault["Messages", apires, {}];
	    If[msg =!= {}, issueMWAMessages[msg]];
	    rules = replaceWithDefault["EntityNameRules",apires, {}];
	    EntityNameCacheAdd[#[[1]], #[[2]]] &/@ rules;
		res = ConvertTemporaryMWASymbols[res]/.$Failed[_]->$Failed;
    ];
	res/;res =!= $Failed
]

Experimental`FindEntities[s_String,filter_:Automatic]:=Module[{res,apires, rules},
  apires=Internal`MWACompute["MWAFindEntities",{s,filter}];
  apires=ReleaseMWAComputeHold[apires];
  If[!OptionQ[apires], Message[Internal`FindEntities::TODO]; Return[$Failed]];
  res=replaceWithDefault["Result",apires,$Failed];
  If[MatchQ["Messages"/.apires,{__}],Message[Internal`FindEntities::TODO]];   
  rules = replaceWithDefault["EntityNameRules",apires, {}];
  EntityNameCacheAdd[#[[1]], #[[2]]] &/@ rules;
  ConvertTemporaryMWASymbols[res]
]

EntityProperties[e_] := EntityValue[e, "Properties"]

TypeName[Entity[type_,___]]:= type
CommonName[e:Entity[type_,name_,___]]:= GetEntityName[e, 10]
CanonicalName[Entity[type_, name_, ___]] := name
CommonName[ep:EntityProperty[_, opts:OptionsPattern[]]] := With[{res=OptionValue[EntityProperty,{opts},FormatName]}, res/;res=!= None]
CanonicalName[ep:EntityProperty[name_, opts:OptionsPattern[]]] := name
(* TODO: Add error messages for CommonName/CanonicalName/EntityProperties *)
TypeName[arg:Except[_Entity]] := (Message[TypeName::noent,arg];Null/;False)
TypeName[args___]:=(ArgumentCountQ[TypeName,Length[{args}],1,1];Null/;False)
CommonName[args___]:=(ArgumentCountQ[CommonName,Length[{args}],1,1];Null/;False)
CommonName[arg:Except[_Entity|EntityProperty]] := (Message[CommonName::noent,arg];Null/;False)
CanonicalName[args___]:=(ArgumentCountQ[CanonicalName,Length[{args}],1,1];Null/;False)
CanonicalName[arg:Except[_Entity|EntityProperty]] := (Message[CanonicalName::noent,arg];Null/;False)
(* TODO: Make most of these "listable", use single server call for CommonName *)

(*** Wrappers for *Data functions ***)
(* The following list should match the similar list in sysinit.m *)
If[!ListQ[$EVDataPacletHeads],
	$EVDataPacletHeads = Hold[System`AdministrativeDivisionData, System`AircraftData,
System`AirportData, 
System`BridgeData, System`BroadcastStationData, System`BuildingData, 
System`CompanyData, System`ConstellationData, System`DamData, 
System`DeepSpaceProbeData, System`EarthImpactData, 
System`FiniteGroupData, System`GeneData, System`HistoricalPeriodData, 
System`IslandData, System`IsotopeData, System`LakeData, 
System`LaminaData, System`MannedSpaceMissionData, 
System`MedicalTestData, System`MeteorShowerData, System`MFIDData, 
System`MineralData, System`MountainData, System`MovieData, 
System`NeighborhoodData, System`NuclearExplosionData, 
System`NuclearReactorData, System`OceanData, System`ParkData, 
System`ParticleAcceleratorData, System`PeriodicTilingData, 
System`PersonData, System`PhysicalSystemData, System`PlaneCurveData, 
System`PlantData, System`PopularCurveData, System`SatelliteData, 
System`SNPData, System`SolarSystemFeatureData, System`SpaceCurveData, 
System`SpeciesData, System`SurfaceData, System`TropicalStormData, 
System`TunnelData, System`UnderseaFeatureData, System`VolcanoData, 
System`WaterfallData, System`ZIPCodeData]
];

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

Protect@@$EVDataPacletHeads;

(* TODO: head-specific messages *)
(* TODO: add flag to indicate entity or entity class when not specified *)
Clear[EVDataPacletDispatch];
EVDataPacletDispatch[head_, args_] := Module[{etype, res},
  etype = dataHeadToEntityTypeLookup[head];
  If[etype === None, Return[$Failed] (*shouldn't happen*)];
  res = Switch[args,
   {} | {All | "Entities"}, 
       EntityValue[Entity[etype], "Entities"],
   {"Classes"}, 
       EntityValue[Entity[etype], "EntityClasses"],
   {"Properties" | "PropertyCanonicalNames" | "SampleEntities" | "SampleEntityClasses"| "EntityCanonicalNames"},
       EntityValue[etype, args[[1]]],
   {$entityStandardNamePattern[etype], ___},
       EntityValue[Entity[etype, args[[1]]], Sequence @@ Rest[args]],
   {{($entityStandardNamePattern[etype] | _Entity) ..}, __},
       EntityValue[If[MatchQ[#, _Entity], #, Entity[etype, #]] & /@ args[[1]], Sequence @@ Rest[args]],
   {_Entity, ___},
       EntityValue @@ args,
   _,
       $Failed
   ];
   If[!FreeQ[res, $Failed|EntityValue], res = $Failed];
   res
  ]
  
(*** End code for wrappers ***)

entityCount[class_String] := With[{r = EntityValue[class, "EntityCount"]}, Set[entityCount[class], r]/;IntegerQ[r]]
entityCount[___] := Throw[$Failed, $tag]

$EntityBatchThreshold = 1000;

batchEntitiesQ[class_String] := TrueQ[entityCount[class] > $EntityBatchThreshold]
batchEntitiesQ[__] := False

divideEntityCountIntoBatches[n_Integer] /; n > $EntityBatchThreshold := Block[
	{steps = Range[1, n, $EntityBatchThreshold], pairs},
	pairs = {#, # + $EntityBatchThreshold - 1} & /@ steps;
	ReplacePart[pairs, -1 -> {pairs[[-1, 1]], n}]]
	
divideEntityCountIntoBatches[n_Integer] /; n > 0 := {{1, n}}
divideEntityCountIntoBatches[___] := Throw[$Failed, $tag]

downloadEntityBatch[class_String, {start_Integer, stop_Integer}] := 
 EntityValue[class, EntityProperty[{"Entities", {start, stop}}]]
downloadEntityBatch[___] := Throw[$Failed, $tag]

$textStyle = 
  Sequence[FontFamily -> "Verdana", FontSize -> 11, 
   FontColor -> RGBColor[0.2, 0.4, 0.6]];

$frameStyle = 
  Sequence[FrameMargins -> {{24, 24}, {8, 8}}, 
   FrameStyle -> RGBColor[0.2, 0.4, 0.6], 
   Background -> RGBColor[0.96, 0.98, 1.],
   RoundingRadius -> 3];
   
$buttonStyle = Sequence[
	Appearance -> None, 
	ContentPadding -> False, 
	BaselinePosition -> Bottom
]

interruptableEntityDownloadManager[class_String, batches_List] := 
 Block[{elements = Length[batches], list = {}}, 
  DynamicModule[{stopQ = False, progress = 0, cell, stopping = False, 
    display = 
     Tooltip[Style["\[FilledSquare]", 20, Red], "Stop download"]},
   cell = 
    PrintTemporary[
     Framed[Grid[{{Style[
     Row[{"Downloading ", Dynamic[progress], " of ", elements, 
       " resources..."}], $textStyle]}, {Row[{ProgressIndicator[
       Dynamic[progress], {0, elements}], 
      Button[Dynamic[display], 
       Set[display, Style["Stopping...", $textStyle]];
       Set[stopQ, True], $buttonStyle]}, "  "]}}, 
  Alignment -> Left], $frameStyle]];
   list = Reap[Catch[
      Do[
       If[TrueQ[stopQ],
        Throw["Stopped", $tag],
        progress++; 
        Sow[downloadEntityBatch[class, 
          batches[[progress]]]]], {elements}];
      list,
      $tag]]];
  First[Join @@@ Last[list]]
  ]
  
GetAllEntities[Entity[class_String,___]]:=GetAllEntities[class]
GetAllEntities[class_String] := Catch[
  If[batchEntitiesQ[class],
   With[{batches = divideEntityCountIntoBatches[entityCount[class]]},
    interruptableEntityDownloadManager[class, batches]],
   EntityValue[class, "Entities"]
   ], $tag]

(*keep various flags & symbols from triggering Dynamic updating*)
Internal`SetValueNoTrack[{$dontFormatEntity}, True];

With[{s=$readProtectedSymbols},SetAttributes[s,{ReadProtected}]];
Protect@@$protectedSymbols;
Protect@@$readProtectedSymbols;
   
   
End[];

