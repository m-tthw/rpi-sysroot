BeginPackage["QuantityArray`"]

QuantityArray;
UnitArray;

Unprotect /@ {QuantityArray, UnitArray};
ClearAll  /@ {QuantityArray, UnitArray};

(* Messages *)
QuantityArray::array = "`1` is not a Quantity-free rectangular array.";
QuantityArray::uarray = "Invalid units specification `1`.";
QuantityArray::units = "Inconsistent argument dimensions `1` and `2`.";
UnitConvert::units = "Inconsistent unit arrays `1` and `2`.";

Begin["`Private`"]

(* Register QuantityArray as a type of StructuredArray *)
StructuredArray`RegisterArrayStructure[QuantityArray];


(*** Flatten utilities ***)

(* QuantityArray uses internally a levels notation as in Flatten[array, levels].
   This allows any Flatten or Transpose operation without actually having to change
   the array of numbers inside. *)

(* Validate a levels spec *)
levelsQ[{}] := True;
levelsQ[levels: {{__Integer}..}] := PermutationListQ[Flatten[levels, 1]];
levelsQ[_] := False;

(* Right action on a dimensions list. Return the dimensions of Flatten[A, levels]
   for an array A of dimensions dims. *)
dimsFlatten[dims_, levels_] := Times@@@ Extract[dims, List /@ levels];

(* Right product of levels. Return levels such that Flatten[Flatten[A, list1], list2]
   is identitcal to Flatten[A, levels] for any array A of appropriate depth. This is
   actually an external product in general. *)
levelsFlatten[levels1_List, levels2_List] := Join@@@ Extract[levels1, List /@ levels2];

(* The identity of that product, on both sides *)
levelsIdentity[n_Integer] := List /@ Range[n];
levelsIdentityQ[levels_List] := SameQ[levels, levelsIdentity[Length[levels]]];


(*** Array utilities ***)

(* Avoid ArrayDepth confusion, say ArrayDepth[x[1]] should be 0, and not 1 *)
arrayDepth[array: _List | _SparseArray | _StructuredArray] := ArrayDepth[array];
arrayDepth[x_] := 0;
arrayDimensions[array: _List | _SparseArray | _StructuredArray] := Dimensions[array];
arrayDimensions[x_] := {};

(* A key concept with QuantityArray is that of 'deep threading'. Given an array A of
   dimensions {d1, ..., dn} and an array U of dimensions {dm, ..., dn} with 1 <= m <= n.
   then we want to construct an array Q of dimensions {d1, ..., dn} such that the element
   Q[[i1, ..., in]] equals f[A[[i1, ..., in]], U[[im+1, ..., in]]] for some threading
   function f, in our case always Quantity. The following code assumes that f automatically
   threads in its first argument, like Quantity does. *)

(* Array cyclic transposition. Result has dimensions RotateRight[Dimensions[A], n] *)
transposeRight[A_, n_] := Transpose[A, RotateLeft[Range[arrayDepth[A]], n]];

(* Deep threading of a listable f *)
mapThread[f_, {A_, B_}, n_Integer?Positive] := MapThread[f, {A, B}, n];
mapThread[f_, {A_, B_}, 0] := f[A, B];
mapThread[f_, {A_, B_}, n_Integer?Negative] :=
	transposeRight[MapThread[f, {transposeRight[A, -n], transposeRight[B, -n]}, -n], n];


(*** QuantityArray as constructor ***)

(* Checks with no messages *)
dimensionsQ[dims_] := VectorQ[dims, Internal`PositiveMachineIntegerQ];
unitQ[unit_] := KnownUnitQ[unit];
unitArrayQ[units_] := ArrayQ[units, _, unitQ];
arrayQ[numbers_] := ArrayQ[numbers] && (Developer`PackedArrayQ[numbers] || FreeQ[numbers, Quantity]);

(* Checks with messages *)
Acheck[numbers_] :=
	(arrayQ[numbers] || (!ListQ[numbers] && !QuantityQ[numbers])) ||
	(Message[QuantityArray::array, numbers]; False);

Ucheck[units_List] := unitArrayQ[units] || (Message[QuantityArray::uarray, units]; False);
Ucheck[unit_] := unitQ[unit] || (Message[QuantityArray::uarray, unit]; False);

QAcheck[numbers_, units_] := With[{
	ndims = arrayDimensions[numbers],
	udims = arrayDimensions[units]
},
	(Length[udims] <= Length[ndims] && Take[ndims, -Length[udims]] === udims)||
	(Message[QuantityArray::units, ndims, udims]; False)
];

(* Internal constructor *)
quantityArray[scalar_, unit_, {}] := Quantity[scalar, unit];
quantityArray[numbers_, units_, levels_] := StructuredArray[
	QuantityArray,
	dimsFlatten[arrayDimensions[numbers], levels],
	StructuredArray`StructuredData[
		QuantityArray,
		Developer`ToPackedArray[numbers],
		units,
		levels
	]
];
quantityArray[numbers_, units_] :=
	quantityArray[numbers, units, levelsIdentity[arrayDepth[numbers]]];

(* Public constructor. Do not change the units structure provided. Remove UnitArray.
   No direct access to the levels argument. *)
QuantityArray[numbers_, UnitArray[units_]] := QuantityArray[numbers, units];
QuantityArray[numbers_, units_] := quantityArray[
	numbers,
	units
] /; Acheck[numbers] && Ucheck[units] && QAcheck[numbers, units];


(*** QuantityArray as converter ***)

(* QuantityArray[A], where A is an array of Quantity objects, returns a
   QuantityArray representation of the same object. It does not try to change
   the units to get homogeneity. This should be done externally, with UnitConvert. *)

(* Recursively extract a subarray of the units array *)
unitSubarray[units_List] := If[SameQ@@ units, unitSubarray[units[[1]]], units];
unitSubarray[unit_] := unit;

(* Converter *)
QuantityArray[nqa_?ArrayQ] := With[{
	numbers = QuantityMagnitude[nqa],
	units = QuantityUnit[nqa]
},
	QuantityArray[numbers, unitSubarray[units]]
];
QuantityArray[HoldPattern[Quantity[numbers_, unit_, ThreadDepth -> 0]]] :=
	QuantityArray[numbers, unit];
QuantityArray[q_Quantity] := q;
QuantityArray[qa: HoldPattern[StructuredArray[QuantityArray, __]]] := qa;

(* On any other input, stay unevaluated without complaining *)


(*** Basic manipulation of QuantityArray objects ***)

(* Accessors *)
getNumbers[HoldPattern[StructuredArray[QuantityArray, dims_, data_]]] := getNumbers[data];
getUnits[  HoldPattern[StructuredArray[QuantityArray, dims_, data_]]] := getUnits[data];
getLevels[ HoldPattern[StructuredArray[QuantityArray, dims_, data_]]] := getLevels[data];

getNumbers[HoldPattern[StructuredArray`StructuredData[QuantityArray, numbers_, units_, levels_]]] := numbers;
getUnits[  HoldPattern[StructuredArray`StructuredData[QuantityArray, numbers_, units_, levels_]]] := units;
getLevels[ HoldPattern[StructuredArray`StructuredData[QuantityArray, numbers_, units_, levels_]]] := levels;

(* Validation *)
QuantityArray /: StructuredArray`ValidateStructuredData[QuantityArray, dims_, qdata_] :=
	QAstructuredDataQ[qdata, dims];

QAstructuredDataQ[HoldPattern[StructuredArray`StructuredData[QuantityArray, numbers_, units_, levels_]], dims_] :=
	Quiet[Acheck[numbers] && Ucheck[units] && QAcheck[numbers, units]] &&
	dims === dimsFlatten[arrayDimensions[numbers], levels];

QuantityArrayQ[HoldPattern[StructuredArray[QuantityArray, dims_List, qdata_]]] :=
	dimensionsQ[dims] && QAstructuredDataQ[qdata, dims];
QuantityArrayQ[_] := False;

(* Normalization. Let Quantity do all the threading through levels without units.
   TODO: Generalize to destructure only some levels. It may be nontrivial to flatten then. *)
QuantityArray /: StructuredArray`Destructure[QuantityArray, qa_, _] := With[{
	numbers = getNumbers[qa],
	units = getUnits[qa],
	levels = getLevels[qa]
},
	If[unitQ[units],
		(* This is a particular case of the general case, but this way is faster *)
		Quantity[Flatten[numbers, levels], units],
		Flatten[mapThread[Quantity, {numbers, units}, -arrayDepth[units]], levels]
	]
];

(* Tests in ArrayQ, MatrixQ, etc. Need to check every single element, so destructure *)
QuantityArray /: StructuredArray`StructuredArrayTestElements[QuantityArray, qa_StructuredArray] := True;
QuantityArray /: StructuredArray`StructuredArrayTestElements[QuantityArray, qa_StructuredArray, test_] :=
	ArrayQ[Normal[qa], _, test];


(** Parts ***)

(* Part extraction *)
QuantityArray /: StructuredArray`StructuredPart[QuantityArray, qa_StructuredArray, indices___] :=
	getPart[getNumbers[qa], getUnits[qa], getLevels[qa], {indices}];

getPart[numbers_, units_, levels_?levelsIdentityQ, inds_List] := getPartSimple[numbers, units, inds];

getPartSimple[numbers_, units_, inds_List] := quantityArray[
	Part[numbers, Sequence@@ inds],
	Part[units, Sequence@@ Drop[inds, Min[Length[inds], arrayDepth[numbers] - arrayDepth[units]]]]
];

(* TODO: Check that this works with Span and generic levels argument *)
getPart[numbers_, units_, levels_, inds_List] := Module[{
	len = Length[inds],
	depth = arrayDepth[numbers],
	indsrules = indsPartition[inds, levels, arrayDimensions[numbers]],
	part, ranges
},
	ranges = Cases[indsrules, _[_, All | _Span]];
	part = getPartSimple[numbers, units, Last /@ Sort[indsrules]];
	(* TODO: This final transposition is ugly. Is there a better way to keep track of it? *)
	If[ranges =!= {},
		part = Transpose[part, Ordering[First /@ ranges]];
	];
	part
];

(* Numbers in mixed bases *)
indsPartition[inds_, levels_, dims_] :=
	Flatten@ MapThread[indPartition[dims], {PadRight[inds, Length[levels], All], levels}, 1];
indPartition[dims_][All, levels_List] := Thread[levels -> All];
indPartition[dims_][i_Integer, levels_List] := Thread[levels -> integerDigits[i, dims[[levels]]]];
integerDigits[i_, dims_] := Block[{n},
	Reap[
		Fold[
			Function[Sow[n = Quotient[#1, #2]]; #1 - n #2],
			i,
			Reverse@ FoldList[Times, 1, Reverse@ Rest[dims]]]
	][[2, 1]]
];


(*** Numerics ***)

(* Numerization. TODO: There is a bug somewhere in StructuredN *)
QuantityArray /: StructuredArray`StructuredN[QuantityArray, f_, qa_StructuredArray] :=
	quantityArray[f[getNumbers[qa]], getUnits[qa], getLevels[qa]];

(* Precision and Accuracy. TODO: This is not working *)
QuantityArray /: StructuredArray`StructuredPA[QuantityArray, Internal`EffectivePrecision, qa_StructuredArray] :=
	getNumbers[qa];


(*** Typesetting ***)

(* TODO: This should be done using upvalues, to avoid loading it in StructuredArray *)
(* TODO: Do not use the SymmetrizedArray icon. We need a new one *)
(* TODO: Note that MatrixPlot here unpacks *)

(* Summary box *)
Unprotect[StructuredArray];
StructuredArray /: MakeBoxes[sa: StructuredArray[QuantityArray, dims_, qdata_], fmt_] /;
	BoxForm`UseIcons && System`Private`ValidQ[Unevaluated@ sa] := Module[
	{numbers, units, levels, typeItem, dimsItem, unitsItem, levelsItem, icon, alwaysGrid, sometimesGrid},
	numbers = getNumbers[qdata];
	units = getUnits[qdata];
	levels = getLevels[qdata];
	typeItem = BoxForm`SummaryItem[{"Type: ", QuantityArray}];
	dimsItem = BoxForm`SummaryItem[{"Dimensions: ", dims}];
	unitsItem = BoxForm`SummaryItem[{"Unit block: ", units}];
	levelsItem = BoxForm`SummaryItem[{"Flattening: ", levels}];
	icon = Switch[dims,
		{n_} /; n <= 100,
			Quiet@ ElisionsDump`matrixPlot[],
		{n_, m_} /; n m <= 100,
			Quiet@ ElisionsDump`matrixPlot[sa],
		_,
			BoxForm`GenericIcon[SymmetrizedArray]
	];
	icon = Replace[icon, Except[_Graphics] :> BoxForm`GenericIcon[SymmetrizedArray]];
	alwaysGrid = {typeItem, dimsItem, unitsItem};
	sometimesGrid = {levelsItem};
	BoxForm`ArrangeSummaryBox[StructuredArray, sa, icon, alwaysGrid, sometimesGrid, fmt, "Interpretable" -> False]
];
Protect[StructuredArray];


(*** Quantity functionality ***)

(* These are private functions that need to be called from the respective public
   Quantity functions *)

(* Extractors *)
QAnumbers[qa_StructuredArray] := Flatten[getNumbers[qa], getLevels[qa]];
QAnumbers[qa_StructuredArray, units_] := QAnumbers[QAUnitConvert[qa, units]];
QAnumbers[$Failed] := $Failed;

replicateUnits[units_, dims_] := ConstantArray[units, Drop[dims, -arrayDepth[units]]];

QAunits[qa_StructuredArray] := With[{
	numbers = getNumbers[qa],
	units = getUnits[qa],
	levels = getLevels[qa]
},
	Flatten[replicateUnits[units, arrayDimensions[numbers]], levels]
];

(* Unit conversion *)
unitConvertFactor[oldunit_, newunit_] := QuantityMagnitude[Quantity[oldunit], newunit];

convertNumbersArray[numbers_, factors_, levels_?levelsIdentityQ] :=
	mapThread[Times, {numbers, factors}, -arrayDepth[factors]];

(* TODO: Temperatures cannot be handled using multiplicative factors only *)
simpleConvertableQ[UnitArray[ua_List]] := FreeQ[ua, _?QuantityUnits`Private`isTemperatureQ];

(* TODO: Should we take into account somehow the levels here?
   Note that ua could be a single unit. *)
QAUnitConvert[qa_StructuredArray, UnitArray[ua_]] := QAUnitConvert[qa, ua];
QAUnitConvert[qa_StructuredArray, ua_] := Module[{
	numbers = getNumbers[qa],
	units = getUnits[qa],
	levels = getLevels[qa],
	udepth, factors, newnumbers
},
	udepth = arrayDepth[units];
	factors = Quiet@ MapThread[unitConvertFactor, {units, replicateUnits[ua, arrayDimensions[units]]}, udepth];
	If[ArrayQ[factors, udepth, NumericQ],
		newnumbers = convertNumbersArray[numbers, factors, levels];
		quantityArray[newnumbers, ua, levels],
		Message[UnitConvert::units, units, ua];
		(* Return $Failed, as UnitConvert does *)
		$Failed
	]
];


(*** StructuredArray algorithms ***)

(* Times *)
QuantityArray /: StructuredArray`StructuredArrayAlgorithm[Times, QuantityArray, StructuredArray`ScalarQ] := QATimesScalar;

QATimesScalar[Times, qa_StructuredArray, scalar_?QuantityQ] := quantityArray[
	QuantityMagnitude[scalar] getNumbers[qa],
	QuantityUnit[scalar] getUnits[qa],
	getLevels[qa]
];

QATimesScalar[Times, qa_StructuredArray, scalar_] := quantityArray[
	scalar getNumbers[qa],
	getUnits[qa],
	getLevels[qa]
];

(* Equal *)
QuantityArray /: StructuredArray`StructuredArrayAlgorithm[Equal, QuantityArray, QuantityArray] := QAEqual;

QAEqual[comp_, qa1_StructuredArray, qa2_StructuredArray] := Module[{bool},
	If[getLevels[qa1] === getLevels[qa2],
		bool = comp[getNumbers[qa1], getNumbers[qa2]] && SameQ[getUnits[qa1], getUnits[qa2]],
		bool = comp[QAnumbers[qa1], QAnumbers[qa2]] && SameQ[QAunits[qa1], QAunits[qa2]]
	];
	If[TrueQ[bool || !bool],
		bool,
		Indeterminate
	]
];

(* Flatten *)
QuantityArray /: StructuredArray`StructuredArrayAlgorithm[Flatten, QuantityArray] := QAFlatten;

flattenFLevels[flattenF_] := flattenF[[1, 2]];

parseFLevels[Infinity, depth_] := {Range[depth]};
parseFLevels[n_Integer, depth_] := If[n<=depth,
	Join[{Range[n + 1]}, List /@ Range[n + 2, depth]],
	{Range[depth]}
];
parseFLevels[list: {__Integer}, depth_] := Join[{list}, List /@ Complement[Range[depth], list]];
parseFLevels[levels_, depth_] := Join[levels, List /@ Complement[Range[depth], Flatten[levels]]];

QAFlatten[flattenF_, qa_StructuredArray] := quantityArray[
	getNumbers[qa],
	getUnits[qa],
	levelsFlatten[getLevels[qa], parseFLevels[flattenFLevels[flattenF], arrayDepth[qa]]]
];

(* TODO: Partition *)

(* Transpose *)
QuantityArray /: StructuredArray`StructuredArrayAlgorithm[Transpose, QuantityArray] := QATranspose;

getTransposePerm[transposeF_] := transposeF[[1, 2]];

QATranspose[transposeF_, qa_StructuredArray] := quantityArray[
	getNumbers[qa],
	getUnits[qa],
	Permute[getLevels[qa], getTransposePerm[transposeF]]
];

(* TODO: Outer *)

End[]

EndPackage[]
