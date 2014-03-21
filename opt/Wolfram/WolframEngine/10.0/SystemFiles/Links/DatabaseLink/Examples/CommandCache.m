(* Author:          Christopher Williamson *)
(* Copyright:       Copyright 2004, Wolfram Research, Inc. *)

BeginPackage[ "DatabaseLink`Examples`CommandCache`", {"DatabaseLink`", "GUIKit`"} ];

ClearCommandCache::usage="";
CommandCache::usage="";
CommandBrowser::usage="";
GetCommandAttributes::usage="";
StoreCommand::usage="";

(* Data Path *)

Begin["`Private`"];

$commandCacheConnection = Null;

$databaseLocation = ToFileName[{$UserBaseDirectory, "DatabaseResources", "Examples"}, "CommandCache"];

initializeConnection[] :=
  $commandCacheConnection = 
    OpenSQLConnection[JDBC["hsqldb", $databaseLocation], Username->"sa"];

initializeTable[] := 
  SQLExecute[$commandCacheConnection, 
             "CREATE TABLE COMMANDS (ID IDENTITY, EXPR VARCHAR, FULLFORM VARCHAR, IMAGE LONGVARBINARY, USED DATETIME)"];
             
CommandBrowser[] :=
  GUIRun[Widget["class:com.wolfram.databaselink.examples.CommandBrowser", {
    PropertyValue["pasteMenuItem", Name -> "pasteMenuItem"],
    BindEvent[{"pasteMenuItem", "action"}, 
      Script[
        Block[{selectedItems, notebook},
          selectedItems = PropertyValue[{"CommandBrowser", "selectedItems"}];
          notebook = SelectedNotebook[];
          NotebookWrite[notebook, Cell[BoxData[First[ToExpression[#@getExpr[]]]], "Input"]] & /@ selectedItems;
        ]
      ]
    ]
  },
  InitialArguments->{$databaseLocation},
    Name -> "CommandBrowser"]];

(* Box Data *)
StoreCommand[command:(_RowBox|_GridBox|_SuperscriptBox|_SubscriptBox|
  _SupersubscriptBox|_OverscriptBox|_UnderscriptBox|_UnderoverscriptBox|
  _FractionBox|_SqrtBox|_RadicalBox|_StyleBox|_FrameBox|_AdjustmentBox|
  _ButtonBox|_FormBox|_InterpretationBox|_TagBox|_ErrorBox|_CounterBox|
  _ValueBox|_OptionValueBox|_SliderBox|_PopupBox)] := 

  Module[{fullform = Null, image = Null, date = Date[]},
    
    If[$commandCacheConnection === Null, initializeConnection[]];
    If[$commandCacheConnection === $Failed, Return[]];
    
    If[SQLTableNames[$commandCacheConnection, "COMMANDS"] === {}, initializeTable[]];
    
    fullform = StringDrop[StringDrop[ToString[FullForm[MakeExpression[command, StandardForm]]], 13], -1];
    image = SQLBinary[ToCharacterCode[ExportString[Cell[BoxData[command], PageWidth->400], "GIF", ConversionOptions -> {"Transparency" -> GrayLevel[1]}]]];
    SQLInsert[$commandCacheConnection, 
              "COMMANDS",
              {"ID", "EXPR", "FULLFORM", "IMAGE", "USED"}, 
              {Null, SQLExpr[command], fullform, image, SQLDateTime[date]}];
              
    command
  ];

(*
StoreCommand[command_] := 
  (
   StoreCommand[MakeBoxes[command]];
   command
  )
*)

GetCommandAttributes[] := {"ID", "EXPR", "FULLFORM", "IMAGE", "USED", "*"};

CommandCache[] := CommandCache["", {"*"}];

CommandCache[filter_String] := CommandCache[filter, {"*"}];

CommandCache[data_List] := CommandCache["", data];

CommandCache[filter_String, data_List] := 
  Module[{columns, filterList, condition},
    If[!MemberQ[GetCommandAttributes[], #], Return[]] & /@ data;
    
    If[$commandCacheConnection === Null, initializeConnection[]];
    If[$commandCacheConnection === $Failed, Return[]];
    
    If[SQLTableNames[$commandCacheConnection, "COMMANDS"] === {}, Return[]];

    columns = StringJoin[mingleObject[data, ", "]];
    
    filterList = ReadList[StringToStream[filter], Word];
    condition = StringJoin[mingleObject[("FULLFORM LIKE '%" <> # <> "%'" & /@ filterList), " AND "]];
    If[condition =!= "", 
      condition = " WHERE " <> condition;
    ];
    SQLExecute[$commandCacheConnection, "SELECT " <> columns <> " FROM COMMANDS" <> condition]
  ];

CommandCache[id_Integer] := 
  (
    If[$commandCacheConnection === Null, initializeConnection[]];
    If[$commandCacheConnection === $Failed, Return[]];
    
    If[SQLTableNames[$commandCacheConnection, "COMMANDS"] === {}, Return[]];
    
    SQLSelect[$commandCacheConnection, "COMMANDS", SQLColumn["ID"] == id]
  );

mingleObject[lst_List, object_] := 
  If[lst === {}, 
    {}, 
    Drop[Flatten[Thread[{lst, object}, List, 1], 1, List], -1]
  ]

ClearCommandCache[] := 
  (
    If[$commandCacheConnection === Null, initializeConnection[]];
    If[$commandCacheConnection === $Failed, Return[]];
    
    If[SQLTableNames[$commandCacheConnection, "COMMANDS"] === {}, Return[]];
    
    SQLDropTable[$commandCacheConnection, "COMMANDS"]
  );
  
End[]

EndPackage[];