(* $Id: StreamsDemo.m,v 1.1.2.3 2013/12/18 23:08:58 bakshee Exp $ *)

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
  
$bytes = ToCharacterCode[
	"Title: The Quick Brown Fox\nAbstract: A fox jumps over dogs.\nThe quick brown fox jumped over the lazy dogs.\n"];
	
openRead[_] := openRead[Null,$bytes]

openRead[_,args__] := MapIndexed[
	OpenRead["inputStream"<>ToString[ #2[[1]] ], Method -> {"ByteList", "Bytes" -> toBytes[#]}]&,
	{args}
]

toBytes[s_String] := ToCharacterCode[s]
toBytes[l_] := l
		   
(*-----------------------------------------------------------------*)  

DeviceAPI`DeviceClassRegister["StreamsDemo",
	"OpenReadFunction" -> openRead,
	"OpenWriteFunction" -> (OpenWrite["test.bin", Method -> "Passthrough", BinaryFormat -> True]&),
	"StatusLabelFunction" -> ("Connected ("<>Sequence@@Riffle[ToString/@{##},", "]<>")"&),
	"DriverVersion" -> 0.001
];

End[];
EndPackage[];
