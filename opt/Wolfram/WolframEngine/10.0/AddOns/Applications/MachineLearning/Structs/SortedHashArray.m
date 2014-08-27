BeginPackage["MachineLearning`Structs`SortedHashArray`"];

SortedHashArray::usage = "SortedHashArray[{key1 -> value1, key2 -> value2, ... }] outputs a lookup function for the associative array {key1 -> value1, key2 -> value2, ... }.
The lookup is fast and the structure is small in memory, but there is a small chance of falsely unfound key or falsely found key.
The underlying structure is a sorted array of hashed keys which are retrieved through binary search.
SortedHashArray[{key1, key2, ...} -> {value1, value2, ...}] can also be used."

LookupFunction::usage = "Lookup function used by SortedHashArray. LookupFunction[...][key] outputs the value associated to key."

Begin["`Private`"]

Needs["Developer`"]

Clear[SortedHashArray];

SortedHashArray[keys:{__String}, values_List] := Module[
	{hashkeys, order, hashtype},
	If[Length[keys] != Length[values], Return[$Failed]];
	hashtype = Switch[$SystemWordLength,
		32, "Murmur3-32",
		64, "Murmur3-64",
		_, Return[$Failed]
	];
	hashkeys = Data`StringHash[keys, hashtype];
	order = Ordering[hashkeys];
	hashkeys = hashkeys[[order]];
	LookupFunction["SortedHashArray", hashkeys, ToPackedArray[values[[order]]], hashtype]
];

SortedHashArray[keys_List, values_List] := SortedHashArray[ToString[#, InputForm] & /@ keys, values];
SortedHashArray[rule:{__Rule}] := SortedHashArray[rule[[All, 1]], rule[[All, 2]]];
SortedHashArray[Rule[keys_List, values_List]] := SortedHashArray[keys, values];

Format[LookupFunction["SortedHashArray", hashkeys_, values_, hashtype_], StandardForm] := "LookupFunction"[
	"SortedHashArray",
	"KeyNumber"-> Length[hashkeys],
	"CollisionNumber"-> Length[hashkeys] - Length[DeleteDuplicates[hashkeys]],
	"HashType" -> hashtype
];
LookupFunction["SortedHashArray", hashkeys_, values_, hashtype_][keys:{__String}, default_] := Module[
	{positions, hashes, retvalues},
	hashes = Data`StringHash[keys, hashtype];
	positions = If[Length[keys] < 100,
		Quiet[binarySearchSortedHashArrayList[hashes, hashkeys], {CompiledFunction::cfta, CompiledFunction::cfsa}]
		,
		If[$SystemWordLength == 32 && hashtype === "Murmur3-64",
			binarySearchSortedHashArraySingle[to32bit[hashes], to32bit[hashkeys]]
			,
			binarySearchSortedHashArraySingle[hashes, hashkeys]
		]
	];
	retvalues = values[[positions]];
	If[MemberQ[retvalues, List], retvalues = Replace[retvalues, List -> default, {1}]];
	retvalues
];
LookupFunction["SortedHashArray", hashkeys_, values_, hashtype_][keys_List, default_] := 
	LookupFunction["SortedHashArray", hashkeys, values, hashtype][ToString[#, InputForm] & /@ keys, default];

LookupFunction["SortedHashArray", hashkeys_, values_, hashtype_][key_, default_] := 
	First @ LookupFunction["SortedHashArray", hashkeys, values, hashtype][{key}, default];

LookupFunction["SortedHashArray", hashkeys_, values_, hashtype_][keys_] := 
	LookupFunction["SortedHashArray", hashkeys, values, hashtype][keys, Missing[]];

to32bit = Mod[#, 2^32 - 1, -2^31 + 1] &;

(*outputs the position of the keys inside a sorted array*)
binarySearchSortedHashArrayList = Compile[
	{{key, _Integer, 1}, {array, _Integer, 1}}, Module[
	{imin = 1, imax = Length[array], mid},
	While[imin < imax,
		mid = Floor[(imax + imin)/2];
		If[array[[mid]] < #, imin = mid + 1, imax = mid];
	];
	If[array[[imin]] == #, imin, 0]
	] & /@ key
(*	, CompilationTarget -> "C"*)
];

binarySearchSortedHashArraySingle = Compile[
	{{key, _Integer}, {array, _Integer, 1}}, Module[
	{imin = 1, imax = Length[array], mid},
	While[imin < imax,
		mid = Floor[(imax + imin)/2];
		If[array[[mid]] < key, imin = mid + 1, imax = mid];
	];
	If[array[[imin]] == key, imin, 0]
	]
	,
	RuntimeAttributes -> {Listable},
	Parallelization -> True
(*	, CompilationTarget -> "C"*)
(*	,RuntimeOptions -> "Speed"*)
];

(* Version Handling collisions. No keys are all retrieved. Still chance for a falsely retrieved key.
	~ 2 times slower than verion 1. With 64-bit Hash, we prefer version 1.

SortedHashArray2[rule_] := Module[
	{tmp, single, multiple, singlekeys, singlevalues},
	tmp = GatherBy[{Hash[#[[1]]], #[[1]], #[[2]]} & /@ rule, #[[1]] &];
	single = SortBy[Join @@ Select[tmp, Length[#]==1 &], #[[1]] &];
	single = single[[All, {1, 3}]];
	multiple = Join @@ Select[tmp, Length[#]>1 &];
	multiple = Rule @@@ multiple[[All, {2, 3}]];
	singlekeys = ToPackedArray @ single[[All, 1]];
	singlevalues = ToPackedArray @ single[[All, 2]];
	Print[Length[multiple], " colliding keys"];
	SortedHashArrayLookup2[singlekeys, singlevalues, Dispatch[Append[multiple, _ -> Missing[]]]]
];

SortedHashArrayLookup2[singlekeys_, singlevalues_, dispatchtable_][keys_List] := Module[
	{positions},
	positions = BinarySearch[Hash /@ keys, singlekeys];
	MapCounted[ If[#1 != -1, singlevalues[[#1]], Replace[keys[[#2]], dispatchtable]] &, positions]
];

Format[SortedHashArrayLookup2[singlekeys_, singlevalues_, dispatchtable_], StandardForm] := "SortedHashArray"["Nkeys"-> Length[singlekeys],
"Ncollisions"-> LengthDispatch[dispatchtable]
];
*)


End[]
EndPackage[]