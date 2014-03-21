(* ::Package:: *)

(* ::Section::Closed:: *)
(*Copyright*)


(*************************************************************************

						Mathematica source file

		Copyright 1986 through 2010 by Wolfram Research Inc.

This material contains trade secrets and may be registered with the
U.S. Copyright Office as an unpublished work, pursuant to Title 17,
U.S. Code, Section 408.  Unauthorized copying, adaptation, distribution
or display is prohibited.

$Id: Export.m,v 1.3 2010/07/13 21:33:37 igora Exp $

*************************************************************************)


(* ::Section:: *)
(*Converter Registration*)


Begin["System`Convert`CCodeDump`"]


ImportExport`RegisterExport["C",
	ExportCCode,
	"FunctionChannels"->{"FileNames"},
	"BinaryFormat" -> True,
	"Sources" -> ImportExport`DefaultSources["C"]
]


End[]
