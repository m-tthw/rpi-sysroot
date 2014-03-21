(* ::Package:: *)

BeginPackage["CloudObject`"]

HTMLEscape::usage = "EscapeHTML removes dangerous character inside a string: &<>\'\"";

Begin["`Private`"]

HTMLEscape[s_String] := StringReplace[s,{
	"&" -> "&amp;",
	"<" -> "&lt;",
	">" -> "&gt;",
	"\"" -> "&quot;",
	"'" -> "&#39;"
}]

End[]

EndPackage[]
