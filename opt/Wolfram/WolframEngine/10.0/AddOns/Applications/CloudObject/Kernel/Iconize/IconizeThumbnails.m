BeginPackage["CloudObject`Iconize`IconizeThumbnails`"];
Needs["CloudObject`Iconize`IconizeImages`"];
Needs["CloudObject`Iconize`IconizeElidedForms`"];

Iconize::usage = "Iconize returns an icon for a given expression.  If the expression involves a template, such as a Manipulate, it will return an icon for the inner expression inside the template."
ManipulateCheck::usage = "Manipulates with ElidedForms";
iExpressionIcon::usage = "test";
makeThumbnail::usage = "test";
thumbnailResize::usage = "test";

Begin["`Private`"];

ElidedFormHeads = {
"VarianceTest", 
"FisherRatioTest", 
"LeveneTest", 
"BrownForsytheTest", 
"ConoverTest", 
"SiegelTukeyTest", 
"EventSeries", 
"LinearSolveFunction",
"ParametricFunction", 
"AutocorrelationTest", 
"CorrelationTest", 
"CoxModel", 
"EmpiricalDistribution", 
"HistogramDistribution", 
"KernelMixtureDistribution", 
"LocationEquivalenceTest", 
"LogRankTest", 
"SmoothKernelDistribution", 
"SurvivalDistribution", 
"UnitRootTest", 
"VarianceEquivalenceTest", 
"TemporalData", 
"TimeSeries", 
"WeightedData"
};

specialHeads = {Manipulate, APIFunction, Image, Graphics};

ImageEmbed[{img_,{{x1_,y1_},{x2_,y2_}}},img2_]:=ImageCompose[img,ImageResize[img2,{x2-x1,y2-y1}-4],{x1,y1}+2,{0,0}];
ImageEmbed[{img_,pos_,restpos__},img2_, restimg___]:=ImageEmbed[{ImageEmbed[{img,pos}, img2],restpos},restimg];
ImageEmbed[{img_,___}] := img;
ImageEmbed[{img_},___] := img;

SetAttributes[makeThumbnail,HoldAllComplete];
makeThumbnail[expr_] := ImageTake[Rasterize[Pane[Short[expr,10],150],ImageResolution->150],150]

thumbnailResize[x_Graphics | x_Image]:=Module[{dims,pad},
dims = ImageDimensions[x];
pad = Abs[(dims[[1]]-dims[[2]])/2];
If[dims[[1]]>dims[[2]],
ImageResize[ImagePad[x,{{0,0},{pad,pad}},White],{150,150}],
ImageResize[ImagePad[x,{{pad,pad},{0,0}},White],{150,150}]
]
]

(*SetAttributes[exprToIcon,HoldAllComplete];*)
Options[exprToIcon] = {Size -> Medium};

exprToIcon[Hold[x_]] := exprToIcon[x]

(*exprToIcon[x_ /; (!MemberQ[ElidedFormHeads,ToString @ Head[x]] && !MemberQ[specialHeads, Head[x]])] := thumbnailResize @ makeThumbnail[x]*)

exprToIcon[{x__Image | x__Graphics}] := ImageCollage[{x}]

exprToIcon[expr_Manipulate] := manipulateCheck[expr]

exprToIcon[x_ /; MemberQ[ElidedFormHeads,ToString @ Head[x]]] := With[{head = FindHead[x]},
	GenericIcon[head,OptionValue[exprToIcon,Size]/.{Small->7.8,Medium->12.2,Large->18.4}]
	]
	
exprToIcon[APIFunction[_, expr_,___]] := If[MemberQ[ElidedFormHeads, ToString@Head[expr]], 
	ImageEmbed[icon[APIFunction], ImageResize[GenericIcon[Head[expr],OptionValue[exprToIcon,Size]/.{Small->7.8,Medium->12.2,Large->18.4}],{150,150}]],
	ImageEmbed[icon[APIFunction], ImageResize[exprToIcon[expr],{64,64}]]
]

exprToIcon[i_Image | i_Graphics] := thumbnailResize[i]

exprToIcon[x_ /; (!MemberQ[ElidedFormHeads,ToString @ Head[x]] && !MemberQ[specialHeads, Head[x]] && !MatchQ[x,{y__Image | y__Graphics}])] := thumbnailResize @ makeThumbnail[x]

SetAttributes[manipulateCheck, HoldAllComplete];
manipulateCheck[expr_] := Module[{inactiveForm, innerHead},
inactiveForm = Inactivate[expr];
innerHead = If[SameQ[Head[inactiveForm[[1,0]]],Inactive],inactiveForm[[1,0,1]],inactiveForm[[1,0]]];
If[MemberQ[ElidedFormHeads,ToString @ innerHead], 
	ImageEmbed[icon[Manipulate], ImageResize[GenericIcon[innerHead,OptionValue[exprToIcon,Size]/.{Small->7.8,Medium->12.2,Large->18.4}],{64,64}]], 
	thumbnailResize[Rasterize[expr]]
	]
]

SetAttributes[FindHead, HoldAllComplete];
FindHead[expr_] := Module[{ihead},
ihead = Head[Inactivate[expr]];
If[SameQ[ihead[[0]],Inactive],ihead[[1]],Head[expr]]
]

SetAttributes[Iconize,HoldAllComplete];
Options[Iconize]={Size->Medium};
Iconize[expr_,OptionsPattern[]]:= Module[{},
Switch[OptionValue[Size],
Small, ImageResize[exprToIcon[expr],{64,64}],
Medium, ImageResize[exprToIcon[expr],{100,100}],
Large, ImageResize[exprToIcon[expr],{150,150}]
]
]

(*Iconize[expr_,OptionsPattern[]]:= Module[{head},
head = FindHead[expr];
If[MemberQ[ElidedFormHeads, ToString@head], GenericIcon[head,OptionValue[Size]/.{Small->7.8,Medium->12.2,Large->18.4}],

Switch[OptionValue[Size],
Small, ImageResize[exprToIcon[expr],{64,64}],
Medium, ImageResize[exprToIcon[expr],{100,100}],
Large, ImageResize[exprToIcon[expr],{150,150}]
]
]
]*)

(*************** Experimental ***********************)
SetAttributes[IconizeOverlay,HoldAllComplete];
IconizeOverlay[expr_,category_]:=ImageCompose[Iconize[Evaluate@expr],SetAlphaChannel[ImageResize[category,{100,100}],.2]]

SetAttributes[IconizeTreeForm, HoldAllComplete];
IconizeTreeForm[expr_,category_] := ImageCompose[ImageResize[Rasterize@TreeForm[expr,AspectRatio->1,VertexLabeling->Automatic],{100,100}],SetAlphaChannel[ImageResize[category,{100,100}],.2]]

End[];
EndPackage[];