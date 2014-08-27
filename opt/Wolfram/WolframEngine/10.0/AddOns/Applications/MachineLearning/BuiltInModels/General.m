(* ::Package:: *)

Package["MachineLearning`BuiltInModels`Language`"]

PackageScope["GetResourcePath"]

GetResourcePath::usage = "GetResourcePath[path] returns the subdirectory 'path' in BuiltInModels";

(*
GetResourcePath[] := Replace[CalculateParse`Content`Calculate`$CalculateIncludesDirectory, Except[_String] :>
		FileNameJoin[{FileNameDrop[FindFile["CalculateUtilities`InformationRetrievalUtilities`"], -2], "CalculateIncludes"}]];
GetResourcePath[path_String] := ToFileName[{GetResourcePath[], path}];
GetResourcePath[path_String, file_String] := ToFileName[{GetResourcePath[], path}, file];*)
