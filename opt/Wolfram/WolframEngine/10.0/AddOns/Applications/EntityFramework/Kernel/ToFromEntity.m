(* Mathematica package *)

System`ToEntity;
System`FromEntity;

(Unprotect[#];Clear[#];)& /@ {
	ToEntity,
	FromEntity
}

Begin["EntityFramework`EntityTransformations`"];

$tag = "EFETCatchThrowFlag";

(*================ Utility functions =======================*)

CanonicalForm[g_] := Module[{h = System`CanonicalGraph[g]},
    AdjacencyList[h, #] & /@ VertexList[h]
]

initGraphData[] := Module[{},
	Set[$GDInit,True];
	GraphData[]; (* to load the definition of $GraphCanonicalForms *)
]


assoc := assoc = If[ValueQ[DataPaclets`GraphDataDump`$GraphCanonicalForms32],
	Association[
   Thread[If[$SystemWordLength === 64, DataPaclets`GraphDataDump`$GraphCanonicalForms,
DataPaclets`GraphDataDump`$GraphCanonicalForms32] -> 
     DataPaclets`GraphDataDump`$GraphAll]],
 Association[
   Thread[DataPaclets`GraphDataDump`$GraphCanonicalForms -> 
     DataPaclets`GraphDataDump`$GraphAll]]
]

$ColorHeadPattern = _RGBColor | _CMYKColor | _XYZColor | _LABColor | _LUVColor | _GrayLevel;

ToEntityColorData[RGBColor[r_, g_, b_]] :=  Entity["Color", {"RGB", {r, g, b}}]
ToEntityColorData[RGBColor[r_, g_, b_, \[Alpha]_]] := Entity["Color", {"RGB", {r, g, b}}]
ToEntityColorData[CMYKColor[c_, m_, y_, k_]] :=  Entity["Color", {"CMYK", {c, m, y, k}}]
ToEntityColorData[XYZColor[X_, Y_, Z_]] :=  Entity["Color", {"XYZ", {X, Y, Z}}]
ToEntityColorData[LABColor[L_, a_, b_]] :=  Entity["Color", {"CIE1976Lab", 100*{L, a, b}}]
ToEntityColorData[LUVColor[L_, u_, v_]] :=  Entity["Color", {"Luv", 100*{L, u, v}}]
ToEntityColorData[GrayLevel[frac_]]:=Entity["Color",{"GrayLevel",{frac}}]
ToEntityColorData[Hue[h_]] := Entity["Color", {"HSB", {h, 1, 1}}]
ToEntityColorData[Hue[h_, s_, b_, \[Alpha]_]] := Entity["Color", {"HSB", {h, s, b}}]

(**LCH color is currently (as of Apr 1,2014) not supported in Alpha.Will be a TODO item for Meng Lu.**)
ToEntityColorData[LCHColor[L_, c_, h_]] := $Failed
ToEntityColorData[___] := $Failed

EntityColorToWL[{"RGB", c: {r_,g_,b_} /; r>1 || g>1 || b>1}] := RGBColor@@(c/255)
EntityColorToWL[{"RGB", c: {_?(0<=#<=1&), _?(0<=#<=1&), _?(0<=#<=1&)}}] := RGBColor@@c
EntityColorToWL[{"CMYK", c: {c_?(0<=#<=1&), m_?(0<=#<=1&), y_?(0<=#<=1&), k_?(0<=#<=1&)}}] := CMYKColor@@c
EntityColorToWL[{"XYZ", c: {_?(0<=#<=1&), _?(0<=#<=1&), _?(0<=#<=1&)}}] := XYZColor@@c
EntityColorToWL[{"CIE1976Lab", c: {L_?(0<=#<=100&), a_?(-127<=#<=128&), b_?(-127<=#<=128&)}}] := LABColor@@(c/100)
EntityColorToWL[{"Luv", c: {L_?(0<=#<=100&), u_?(-100<=#<=100&), v_?(-100<=#<=100&)}}] := LUVColor@@(c/100)
EntityColorToWL[{"GrayLevel", {frac_?(0<=#<=1&)}}] := GrayLevel[frac]
EntityColorToWL[{"HSB", c: {_?(0<=#<=1&), _?(0<=#<=1&), _?(0<=#<=1&)}}] := Hue@@c

EntityColorToWL[{"Mathematica",c_String}] := Symbol[c]
EntityColorToWL[{scheme:("HTML"|"Legacy"|"Crayola"),name_}] := ColorData[scheme][name]

EntityColorToWL[{"Hex", name_String /; StringMatchQ[name, RegularExpression["#[a-fA-F0-9]+"]]}] := With[
    {hex = StringReplace[name, "#" -> ""]},
    RGBColor @@ (IntegerDigits[FromDigits[hex, 16], 256, 3]/255)
]

(*

Alternatively, it might be interesting to convert a hex number 
(in the sub set of 216 "Web safe color" c.f. http://websafecolors.info/) 
to ColorData["WebSafe"][index]:

hexToWebSafeColorIndex[
    hex_String /; StringMatchQ[hex, RegularExpression["#[a-fA-F0-9]+"]]
]:= Module[
    {r, g, b, webSafeColorIndex},
    {r, g, b} = IntegerDigits[FromDigits[hex, 16], 256, 3];
    {r, g, b} = 5 - {r, g, b}/51;
    webSafeColorIndex = r * 6^2 + g * 6 + b + 1;
    If[IntegerQ[r]&&IntegerQ[g]&&IntegerQ[b], webSafeColorIndex, $Failed]
]
 
EntityColorToWL[{"Hex", name_}] := With[
    {index = hexToWebSafeColorIndex[name]},
    If[IntegerQ[index], ColorData["WebSafe"][index], $Failed]
];

*)


EntityColorToWL[input:{_String,_}] := With[
    {r=EntityValue[Entity["Color",input],"Value"]/.HoldForm->Identity},
    If[MatchQ[r,RGBColor[__]|CMYKColor[__]], r, $Failed]
]

EntityColorToWL[__] := $Failed



(*================ Main functions =======================*)

ToEntity[args___] := With[{res=Catch[iToEntity[args],$tag]},
	res/;res=!=$Failed]

iToEntity[g_System`Graph] := Module[{},
	If[Not[TrueQ[$GDInit]],initGraphData[]];
	With[{res = assoc[Hash[CanonicalForm[g]]]}, 
		If[MatchQ[res, _Missing|$Failed], $Failed, Entity["Graph", res]]]
]

iToEntity[char_String]/;SameQ[StringLength[char],1] := Module[{},
	If[MatchQ[#,{_Integer}],Entity["Character",First[#]],Message[ToEntity::noent,char];$Failed]&[ToCharacterCode[char]]
]

iToEntity[e:$ColorHeadPattern] := ToEntityColorData[e]

iToEntity[e_Entity] := e

iToEntity[e:Except[_Entity]] := Module[{},
	Message[ToEntity::noent,e];$Failed
]

iToEntity[args___] := Module[{},
	ArgumentCountQ[FromEntity,Length[{args}],1,1];
	$Failed
]

FromEntity[args___] := With[{res=Catch[iFromEntity[args],$tag]},
	res/;res=!=$Failed]

iFromEntity[e:Entity["Graph", gname_]] := Module[{},
	If[Not[TrueQ[$GDInit]],initGraphData[]];
	If[DataPaclets`GraphDataDump`MyGraphQ[gname],
		GraphData[gname, "Graph"],
		With[{r=EntityValue[e,"Graph"]},
			If[GraphQ[r],
				r,
				Message[FromEntity::norep,e];$Failed
			]]
	]
]

iFromEntity[e:Entity["Character",char_Integer]] := Module[{},Quiet[
	Check[FromCharacterCode[char],Message[FromEntity::norep,e];$Failed,{FromCharacterCode::notunicode}],
	{FromCharacterCode::notunicode}]
]

iFromEntity[e:Entity["Color",colordata_]] := Module[{r=EntityColorToWL[colordata]},
	If[UnsameQ[r,$Failed],
		r,
		Message[FromEntity::norep,e];$Failed
	]
]
	
iFromEntity[e_Entity] := Module[{},
	Message[FromEntity::norep,e];$Failed
	]
	
iFromEntity[e:Except[_Entity]] := Module[{},
	Message[FromEntity::notent,e];$Failed
]

iFromEntity[args___] := Module[{},
	ArgumentCountQ[FromEntity,Length[{args}],1,1];
	$Failed
]

SetAttributes[{
	ToEntity,
	FromEntity
},{ReadProtected,Protected}]


End[];