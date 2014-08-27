System`Private`NewContextPath[{"OAuthClient`","System`"}];

OAuthClient`ob;
OAuthClient`deob;
OAuthClient`getclientinfo;

OAuthClient`getdata;
OAuthClient`prettygrid;
OAuthClient`formatvalue;
OAuthClient`createquantity;
OAuthClient`filterparameters;
OAuthClient`camelcase;
OAuthClient`fromunicode;
OAuthClient`formatpath;
OAuthClient`FlatGrid;
OAuthClient`GridList;
OAuthClient`addtitles;
OAuthClient`assoc;
OAuthClient`createMap;
OAuthClient`updateOAuthPaclet;
OAuthClient`createAnimatedMap;
OAuthClient`eventtimeline;

Begin["OAuthUtilitiesDump`"] (* Begin Private Context *) 

Begin["`Private`"]

ServiceConnect::pacupd="The OAuth paclet was updated";
ServiceConnect::npacup="The OAuth paclet is up to date";

(Unprotect[#]; Clear[#])& /@ {ob,deob,getdata,prettygrid,filterparameters,getclientinfo,updateOAuthPaclet}

assoc=If[$VersionNumber>=10,Association,Identity];

formatvalue[_[label_,fun_]]:=(Rule[label,value_]:>Rule[label,fun[value]])

createquantity[l_List]:=createquantity/@l
createquantity[_[label_,unit_]]:=(Rule[label,value_]:>Rule[label,Quantity[value, unit]])
 
formatpath[fields_,values___]:=With[{n=Length[{values}]},
	Sequence@@Join[MapThread[(#1[#2])&,{Take[fields,n],{values}}],
		Map[#1[]&,Drop[fields,n]]
	]
]

getdata[data_,keys_List,rest___]:=Fold[getdata[##,rest]&,data,keys]
getdata[{},key_,___]:={}
getdata[data_,key_,___]:=(key/.data)/;!FreeQ[data,Rule[key,_]]
getdata[_,key_]:=(Message[ServiceExecute::ndata,key];Throw[$Failed])
getdata[_,key_,def_,___]:=def

prettygrid[args___]:=Grid[args, Background -> {None, {Lighter[Yellow, .9], {White, 
    Lighter[Blend[{Blue, Green}], .8]}}}, Dividers -> {{Darker[
    Gray, .6], {Lighter[Gray, .5]}, 
   Darker[Gray, .6]}, {Darker[Gray, .6], Darker[Gray, .6], {False}, 
   Darker[Gray, .6]}}, Alignment -> {{Left, Right, {Left}}}]


FlatGrid[params_,data_,formatrules_:{},rest___]:=flatgridfun[params,data/.formatrules,rest]
flatgridfun[params_List, data_, gridopts___] := 
 OAuthClient`prettygrid[{camelcase[#[[1]]],#[[2]]}&/@DeleteCases[Reap[flatgridfun0[#, data] & /@ params][[2, 1]],{a_,a_},{1}], gridopts]
 
flatgridfun0[{label_, params_List}, 
  data_] := (flatgridfun0[#, label /. data] & /@ params) /; !ListQ[label]
flatgridfun0[params_List, data_] := flatgridfun0[#, data] & /@ params
flatgridfun0[param_, data_] := Sow[{param, param /. data}]

addtitles[grid_Grid,title_]:=ReplacePart[grid,1->Prepend[grid[[1]],title]]

GridList[_,{},___]:={}

GridList[params_,data_,rest___]:=gridlistfun[params,data,rest]
gridlistfun[params_List, data_, gridopts___] := With[{tmp=Reap[flatgridfun0[#, data] & /@ params][[2, 1]]},
 OAuthClient`prettygrid[Join[{camelcase[tmp[[All, 1]]]}, Transpose[tmp[[All, 2]]]], gridopts]]
 
filterparameters[given:{(_Rule|_RuleDelayed)...},accepted_]:=Module[{camel=camelcase[accepted]},
	Cases[given,HoldPattern[Rule|RuleDelayed][Alternatives@@Join[accepted, camel],_],Infinity]/.Thread[camel->accepted]
]
filterparameters[___]:=Throw[$Failed]

camelcase[l_List, rest___]:=camelcase[#,rest]&/@l
camelcase[str_String, separators_:{"_"}]:=StringReplace[
 StringReplace[
  StringReplace[str, 
   Thread[separators -> " "]], {WordBoundary ~~ word_ :> 
    ToUpperCase[word]}], {"Id"~~WordBoundary->"ID",WhitespaceCharacter -> "","Url"~~WordBoundary->"URL","Urls"~~WordBoundary->"URLs"}]

fromunicode[str_]:=StringReplace[str, "\\u" ~~ x : (WordCharacter ..)/;StringLength[x]<5 :> (debugPrint["unicode"->str->x];FromCharacterCode@FromDigits[x, 16])]

(****************** createMap **********************)
llpattern={_?NumberQ,_?NumberQ}

createMap[latlongs:{{llpattern..}..}]:=Module[{range1, range2, bg,center,lines},
	range1=Through@{Min,Max}@latlongs[[All,All,1]];
	range2=Through@{Min,Max}@latlongs[[All,All,2]];
	center=Point[{Mean[range1],Mean[range2]}];
	bg=GeoGraphics[{Blue, center}, 
	 GeoBackground -> {GeoStyle["StreetMap"]}, 
	 GeoRange -> {range1, range2}, GeoRangePadding -> .0025];
	lines=Flatten[MapIndexed[{ColorData[1][#2[[1]]],Line[Reverse /@ #1]}&,latlongs]];
	bg/.center->{Thickness[Large], lines}
]

createAnimatedMap[latlongs:{llpattern..}, times_]:=Module[{range1, range2, bg,center,line,t,linesf},
	range1=Through@{Min,Max}@latlongs[[All,1]];
	range2=Through@{Min,Max}@latlongs[[All,2]];
	center=Point[{Mean[range1],Mean[range2]}];
	bg=GeoGraphics[{Blue, center}, 
	 GeoBackground -> {GeoStyle["StreetMap"]}, 
	 GeoRange -> {range1, range2}, GeoRangePadding -> .0025];
	line=Reverse /@ latlongs;
	linesf[tt_]:=Line[line[[;;First[Flatten@Position[times,_?(#>tt&),1,1]]]]];
	Animate[bg/.center->{Thickness[Large], linesf[t]},{{t,times[[1]],"time"},times[[1]],times[[-1]]}]
]

createMap[latlongs:{llpattern..}]:=createMap[{latlongs}]
createMap[{}]:={}
createMap[___]:=$Failed

(***************** timelines ********************)
eventtimeline[data_, times_]:=DateListPlot[MapThread[Tooltip[{#,1},#2]&,{times,data}],Filling->Axis,FrameTicks -> {None, {Automatic, Automatic}}]


(****************** ob/deob **********************)


getclientinfo[servicename_]:=Block[{OAuthUtilitiesDump`Private`deobflag = True, info},
	info=OAuthClient`OAuthServicesData[servicename,"ClientInfo"];
	If[!MatchQ[info,{_?IntegerQ..}],Throw[$Failed]];
	deob[info]
]

deobflag=False;
$count=1;
rand={19, 63, 112, 111, 75, 117, 1, 111, 51, 99, 8, 34, 67, 1, 73, 3, 35, 
87, 2, 51, 14, 82, 27, 92, 15, 16, 8, 101, 95, 61};

ob[l_]/;deobflag:=Block[{tf,tf2,res},
	tf = newfile[$TemporaryDirectory];
	Put[l, tf];
	tf2 = newfile[$TemporaryDirectory];
	Encode[tf, tf2, FromCharacterCode[rand,"UTF-8"]];
	DeleteFile[tf];
	res=Import[tf2,"String"];
	DeleteFile[tf2];
	ToCharacterCode[res,"UTF-8"]
	
]

deob[chars_]/;deobflag:=Block[{tf,res,string},
	tf = newfile[$TemporaryDirectory];
	string=FromCharacterCode[chars,"UTF-8"];
	Export[tf, string,"String"];
	res=Get[tf, FromCharacterCode[rand,"UTF-8"]];
	DeleteFile[tf];
	res
	
]

newfile[dir_]:=With[{file=FileNameJoin[{dir, "m-" <> ToString[RandomInteger[10000]] <> ".txt"}]},
	If[FileExistsQ[file],
		If[$count>100,Throw[$Failed]];$count++;newfile[dir],
		file
	]
]

ob[___]:=$Failed
deob[___]:=$Failed

(******************** paclet update ********************)

updateOAuthPaclet[]:=(
	Needs["PacletManager`"];
    Quiet[result = PacletUpdate["OAuth"]];
    If[Head[result] === Paclet,
        (* Update occurred. *)
        Message[ServiceConnect::pacupd];
        (* Load the code from the new version. Perhaps other "reset" steps are required? *)
        Get["OAuth`"],
    (* else *)
        (* No update occurred. *)
        Message[ServiceConnect::npacup]
    ])

End[]

End[] (* End Private Context *)

SetAttributes[{ob,deob,getdata,prettygrid,filterparameters,getclientinfo,updateOAuthPaclet},{ReadProtected, Protected}];

System`Private`RestoreContextPath[];
