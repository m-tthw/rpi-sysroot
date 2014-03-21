(* $Id: StreamsDemo.m,v 1.1.2.1 2013/10/10 19:08:48 bakshee Exp $ *)

(* "DriverVersion" -> 0.001 *)

BeginPackage["DeviceAPI`Drivers`Demos`StreamsDemo`Dump`"];

Begin["`Private`"];

(*----------- stream methods from tutorial/StreamMethods ----------*)

DefineInputStreamMethod["ByteList", {
  "ConstructorFunction" ->
   Function[{name, caller, opts}, 
    If[MatchQ[opts, {___, "Bytes" -> {_Integer ...}, ___}],
     {True, {0, "Bytes" /. opts}},
     {False, $Failed}
     ]],
  
  "ReadFunction" ->
   Function[{state, n},
    Module[{pos = state[[1]], bytes = state[[2]], bytesRead},
     If[pos >= Length[bytes],
      {{}, state},
      bytesRead = 
       Part[bytes, pos + 1 ;; Min[Length[bytes], pos + n]];
      {bytesRead, {pos + Length[bytesRead], bytes}}
      ]
     ]],
  
  "EndOfFileQFunction" -> ({#[[1]] >= Length[#[[2]]], #} &)
  }];

DefineOutputStreamMethod["Passthrough",
   {
      "ConstructorFunction" -> 
   Function[{streamname, isAppend, caller, opts},
    With[{state = Unique["PassthroughOutputStream"]},
     state["stream"] = OpenWrite[streamname, BinaryFormat -> True];
     state["pos"] = 0;
     {True, state}
     ]],
  
  "CloseFunction" -> 
   Function[state, Close[state["stream"]]; ClearAll[state]],
  
  "StreamPositionFunction" -> Function[state, {state["pos"], state}],
  
  "WriteFunction" ->
   Function[{state, bytes},
    Module[{result = BinaryWrite[state["stream"], bytes], nBytes},
     nBytes = If[result === state["stream"], Length[bytes], 0];
     Print["Wrote ", nBytes, " bytes"];
     state["pos"] += nBytes;
     {nBytes, state}
     ]
    ]
      }
  ];
  
bytes = ToCharacterCode[
	"Title: The Quick Brown Fox\nAbstract: A fox jumps over dogs.\nThe quick brown fox jumped over the lazy dogs.\n"];
		   
(*-----------------------------------------------------------------*)  

DeviceAPI`DeviceClassRegister["StreamsDemo", Null ,
	"OpenReadFunction" -> (
		OpenRead["Quick Brown Fox", Method -> {"ByteList", "Bytes" -> #2}]&),
	"OpenWriteFunction" -> (OpenWrite["test.bin", Method -> "Passthrough", BinaryFormat -> True]&)
];

End[];
EndPackage[];
