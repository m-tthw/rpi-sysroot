(* Mathematica package *)
BeginPackage["CloudObject`"]

System`CreateUUID::usage = "CreateUUID[] creates a UUID.";

ParseUUID;
UUIDQ;

Begin["`Private`"]

Unprotect[CreateUUID, ParseUUID, UUIDQ];

BlockRandom[
    SeedRandom[];
    uuidSeed = RandomInteger[2^128];
]

CreateUUID[base_String: ""] := Module[{x, y},
    BlockRandom[
        SeedRandom[uuidSeed];
        uuidSeed = RandomInteger[2^128];
        x = uuidSeed;
        x = StringTake[IntegerString[x, 16, 30], -30];
        y = RandomChoice[{"8", "9", "a", "b"}];
    ];
    base <> StringTake[x, 8] <> "-" <> StringTake[x, {9, 12}] <> "-4" <> 
        StringTake[x, {13, 15}] <> "-" <> y <> StringTake[x, {16, 18}] <> "-" <> 
        StringTake[x, {19, 30}]
]

SetAttributes[CreateUUID,{ReadProtected}];
Protect[CreateUUID];

createUUIDWithEncodedBase[base_String: ""] := Module[{b, x, y},
    (* reserve 6 hexadigits for the base *)
    b = StringTake[IntegerString[Hash[base, "MD5"], 16, 16], -6];
    BlockRandom[
        SeedRandom[uuidSeed + $SessionID];
        uuidSeed = RandomInteger[2^128];
        (* randomly generate a new seed for the next invocation *)
        x = RandomInteger[2^128];
        x = StringTake[IntegerString[x, 16, 32], -32];
        y = RandomChoice[{"8", "9", "a", "b"}];
    ];
    StringTake[x, 8] <> "-" <> StringTake[x, {9, 12}] <> "-4" <> 
        StringTake[b, 3] <> "-" <> y <> StringTake[b, {4, 6}] <> "-" <> 
        StringTake[x, {13, 24}]
]

(* maybe use extra digits for base hash *)

repeated[pattern_, count_Integer] := StringExpression[Sequence @@ Table[pattern, {count}]]

parseExtension[name_String] := Module[{split},
    split = StringSplit[name, ".", 2];
    If[Length[split] === 2,
        split,
    (* else *)
        {First @ split, False}
    ]
]

ParseUUID[uuid_] := Module[{name, ext, parts, number},
    {name, ext} = parseExtension[uuid];
    parts = StringSplit[name, "-"];
    If[Length[parts] >= 5 && And @@ MapThread[
	        StringMatchQ[#1, repeated[HexadecimalCharacter, #2]] &,
	        {number = Take[parts, 5], {8, 4, 4, 4, 12}}
        ],
    (* if: valid UUID *)
        {StringJoin@Riffle[number, "-"], ext},
    (* else *)
        {False, False}
    ]
]

SetAttributes[ParseUUID,{ReadProtected}];
Protect[ParseUUID];

UUIDQ[uuid_] := ParseUUID[uuid] =!= {False, False}

SetAttributes[UUIDQ,{ReadProtected}];
Protect[UUIDQ];

End[]

EndPackage[]
