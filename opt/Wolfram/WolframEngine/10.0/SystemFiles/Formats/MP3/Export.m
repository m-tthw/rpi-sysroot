(* ::Package:: *)

ImportExport`RegisterExport[
 "MP3",
 System`Convert`AudioDump`ExportAudio["MP3", ##]&,
 "Options" -> {"AudioChannels", "SampleRate"},
 "DefaultElement" -> "Sound",
 "FunctionChannels" -> {"FileNames"},
 "BinaryFormat" -> True
]
