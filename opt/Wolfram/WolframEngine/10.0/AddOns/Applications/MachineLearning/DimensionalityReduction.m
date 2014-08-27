Package["MachineLearning`"]

PackageImport["Developer`"]

PackageScope["DimensionReduce"]


DimensionReduce::usage = "DimensionReduce[data] maps the elements of the matrix data into a lower dimensional space.
Outputs {newdata, DimensionReducer}, where newdata is the data in the new space, and DimensionReducer is the dimensionality reduction function."


Options[DimensionReduce] = {
	Method -> "PrincipalComponents",
	"FinalDimension" -> Automatic,
	Tolerance -> Automatic
};

DimensionReduce[data_, opts:OptionsPattern[]] := Module[
	{datamod, mean, u, v, w, singularvalues, m, n, k, tol},
	{m, n} = Dimensions[data];
	datamod = ToPackedArray[data, Real];
	mean = FastMeanColumn[datamod];
	datamod = # - mean & /@ datamod;
	k = OptionValue["FinalDimension"];
	tol = OptionValue[Tolerance];
	If[k === Automatic,
		(*should use *)
		singularvalues = SingularValueList[datamod, Tolerance -> tol];
		{u, v, w} = SingularValueDecomposition[datamod, Length[singularvalues]];
		,
		{u, v, w} = SingularValueDecomposition[datamod, k, Tolerance -> tol];
		singularvalues = Diagonal[v];
	];
	{u.v, Preprocessor["ReduceDimension", OptionValue[Method], w, mean, singularvalues]}
];

Preprocessor["ReduceDimension", "PrincipalComponents", w_, mean_, sv_][x:{__List}] := 
	(# - mean & /@ x).w;
Preprocessor["ReduceDimension", "PrincipalComponents", w_, mean_, sv_][x_] := 
	(x - mean).w;
Preprocessor["ReduceDimension", "PrincipalComponents", w_, mean_, sv_][x:{__List}, "Inverse"] :=
	# + mean & /@ (x.Transpose[w]);
Preprocessor["ReduceDimension", "PrincipalComponents", w_, mean_, sv_][x_, "Inverse"] :=
	x.Transpose[w] + mean;
Preprocessor["ReduceDimension", "PrincipalComponents", w_, mean_, sv_]["SingularValues"] := sv;
(*
Format[Preprocessor[type_, ___], StandardForm] := type;*)

(*Format[Preprocess["ReduceDimension", "PrincipalComponents", w_, mean_, sv_], StandardForm] := Module[
	{dim},
	dim = Dimensions[w];
	"DimensionReducer"["InitialDimension"-> First@dim, "FinalDimension"-> Last@dim, Method -> "PrincipalComponents"]
];*)
