Package["Iconize`"]

PackageExport["Iconize"]

(*This needs to be above the error message*)
$deployments = {"Default", "API", "WebForm", "EmbedCode", 
	"Report", "Mobile", "Publish", "EditPage", "EmbedContent", 
	"WebComputation", "ExportDoc", "ScheduledProg"};

Iconize::deployment = "Deployment type `1` is not one of "~~StringJoin[Riffle[Most[$deployments], ", "]] ~~ " or " ~~ Last[$deployments] ~~ ".";

(*******************************Global Variables*********************************************)
$sys = Association[Thread[Names["System`*"] -> True]];

$cat = {"cloud", "core", "data", "documents",
	"engineering", "external", "finance", "geography",
	"geometry", "graphs", "images", "math", "sound",
	"strings", "time", "UI", "viz"};
	
$graphicsHeads = {Graphics, Graphics3D, GeoGraphics, Image, Legended, Graph};

$dynamicObjects = {Manipulate, MenuView, Slider, Button, 
   ChoiceButtons, PopupMenu, ActionMenu, TabView, FlipView, 
   SlideView, OpenerView, Opener, Checkbox, Toggler, Setter, Panel, 
   Slider2D, VerticalSlider, ProgressIndicator, Animator, 
   Manipulator, Control};
	
$dir = DirectoryName[$InputFileName];
$allHeads = Import[FileNameJoin[{$dir, "Resources","allHeads.m"}]];
$formatFormats = Import[FileNameJoin[{$dir, "Resources","formatFormats.m"}]];
   
$deployColors = {"#e8e8e8", "#ffdd9b", "#ffe4c9", "#dfb5ff", "#e0f1fc", 
"#bdc8f9", "#eef9bd", "#ffd5b3", "#dbfff3", "#d2e7ff", "#dfffca", "#fbe2ff"}; 
    
$deployColorInfo = Association[Rule@@@Transpose[{$deployments,$deployColors}]];

(*These are all images that are only used occasionally *)
$smallAPI := $smallAPI = Import[FileNameJoin[{PacletManager`PacletResource["Iconize","DeploymentIcons"], "API_small.png"}]]
$questionMark := $questionMark = Import[FileNameJoin[{$dir, "Resources","questionmark.png"}]]
$notebookTemplate := $notebookTemplate = Import[FileNameJoin[{$dir, "Resources","notebook_template.png"}]]

(*********************Image constants*************************)
(*The fraction of the icon devoted to the expression (i.e. the remainder without the pane)*)
$paneFraction = N[68.4/85];

(*The x position to place the first icon, i.e. the deployment icon*)
$startFraction = N[59.76/85];

(*The vertical position of the category icons*)
(*This is also the horizontal position in the case of square icons*)
$heightFraction = N[76.4/85];

(*The spacing between category icons*)
(*Also the size of the upper icon pane*)
$diffFraction = N[16.6/85];

(*The relative width of the category icons compared to the whole icon*)
$catIconFraction = N[20/128];

(*LHS padding*)
$textSpacing = .12;

(*Upper padding*)
$upperTextSpacing = .12;

(*Width of category icons*)
$iconFraction = N[12/85];

(*Aspect ratio of Helvetica*)
$aspectRatio = .52;

(*Height of document icons*)
$documentHeight = N[50/85];

(*Background color of upper pane for mobile icons*)
$mobilePaneColor = RGBColor[247,247,247];

(********************* Functions that do not depend on the input expressiopn**************************************)

(*A function that lazily loads the icon and corresponding background color for a given deployment*)
deployInfo[x_] := deployInfo[x] = {Import[FileNameJoin[{PacletManager`PacletResource["Iconize","DeploymentIcons"], StringJoin[x]<>".png"}]],$deployColorInfo[x]}

(*A function that lazily loads the category icons*)
icons[x_] := icons[x] = Import[FileNameJoin[{PacletManager`PacletResource["Iconize","CategoryIcons"], StringJoin[x]<>".png"}]]

(*Function to fade the bottom of the icons when there is too much text*)
fade[row_, pixel_, color_, arraySize_, numRow_] := 
 List @@ Blend[{RGBColor @@ pixel, color}, 
   If[row < (arraySize - numRow), 0, 
    N[(row - (arraySize - numRow))/numRow]]]

fadeArray[image_, color_, fraction_] := Module[{array, size, numRow},
  array = ImageData[image];
  size = Length@array;
  numRow = Round[fraction*size];
  Image@Partition[Reap[
      For[i = 1, i <= Length[array], i++,
       For[j = 1, j <= Length[array], j++,
        Sow[fade[i, array[[i, j]], color, size, numRow]]
        ]
       ]
      ][[2, 1]],
    Length[array]
    ]
  ]

(*Grows the font size as a function of the image size*)
fontsize[n_] := Round[N[6/85]*n + 4]

(*Finds which top-level HelpDocs categories a given head belongs to*)
headCategory[x_String]:=Module[{categories},
categories = Reap[
	For[i=1,i<=Length[$cat],i++,
		If[MemberQ[$allHeads[[i,1]],x],Sow[$allHeads[[i,2]]]]
	]
];

If[Length[categories[[2]]]>0,categories[[2,1]],{}]

]

(*Determine the positions of the category icons on the top pane of the icon*)
(*If the deployment type is default, then the icons are shifted over to RHS and no deployment icon is shown*)
catIconPositions[x_List, sz_, deploy_] := Module[{start, diff, height},
	start = If[deploy=="Default", $heightFraction*sz[[1]], $startFraction*sz[[1]]];
	diff = $diffFraction*sz[[1]];
	height = $paneFraction*sz[[2]] + .5*(1-$paneFraction)*sz[[2]];
	MapIndexed[Inactive[Sequence][ImageResize[icons[#], $iconFraction*sz[[2]]],{start - diff*(#2[[1]]-1), height}]&,x]
]

hexToRGB[x_String]:=Module[{std, length},
	std = x;
	length = StringLength[StringDrop[x,1]];
	If[length == 3,std= "#"~~StringJoin[Flatten[Characters[StringDrop[x,1]]/.a_String:>{a,a}]]];
	RGBColor@@(IntegerDigits[std~StringDrop~1~FromDigits~16,256,3]/255.)
]

(*CloudExport File Style*)
exportedFileStyle[format_]:=Style[format, 
   			FontSize -> 24, 
   			FontColor -> RGBColor[.5, .5, .5],
   			FontFamily -> "Helvetica", 
   			Bold
   			]
   			
(*Make thumbnail simply pads and resizes thumbnails*)
MakeThumbnail[img_Image, {tw_,th_}, bg_] := Module[{w,h},
	{w,h} = ImageDimensions[img];
	If[w > tw || h > th,
		ImageResize[img, {{tw},{th}}],
		img] // ImageCrop[#, {tw, th}, Padding->hexToRGB[bg]]&
];

(********************* Functions that involve the input expressiopn**************************************)
(*Find the categories that the heads of an expression belong to.*)

(*
Note that in findCategory, symbols are extracted in two ways, by 
looking at subexpressions of the form head_[body___] as well as simply
taking all wordcharacter strings in a stringified expression.  
The first method will miss symbols in expressions such as 
Through[{first, second, third}[arg]], and the second will miss 
shorthand characters such as !MemberQ and so on. On it's own, the 
first method gets most heads in expressions, and the stringified
method is just to get some of these corner cases. 
*)

SetAttributes[findCategory,HoldAllComplete];
findCategory[expr_]:=Module[{$commonheads, heads, headsAndOther},
$commonheads={"List","Rule","Hold"};
heads = Cases[Hold[expr],
	h_Symbol[___]:>With[{hs = Quiet[SymbolName[Unevaluated[h]], {SymbolName::sym}]}, 
	hs /; $sys[hs] && Context[Unevaluated[h]] === "System`"&&!MemberQ[$commonheads, hs]], 
	Infinity];
headsAndOther = DeleteDuplicates @ Select[StringCases[ToString[InputForm[Hold[expr]]], RegularExpression["\\w+"]], $sys[#] &];
DeleteDuplicates[Flatten[headCategory/@Union[heads,headsAndOther]]]]

(*Format expression*)
SetAttributes[styledExpression, HoldFirst];
styledExpression[expr_, fs_] := Style[HoldForm[expr], 
		LineIndentMaxFraction -> 0.05, 
		LineSpacing -> {1, 0}, 
		FontSize -> fs, 
		Italic,
		FontFamily -> "Helvetica", 
		FontColor -> Gray
		]		
		
(*Decrease font size if expression is larger than pane*)	
SetAttributes[findFontSize,HoldFirst];	
findFontSize[expr_, sz_] := Module[{paneSize, maxChar, fsTemp},
	paneSize = (1 - $textSpacing)*sz[[1]]*(1 - $textSpacing)*sz[[2]];
	fsTemp = fontsize[sz[[1]]];
	maxChar = Round[paneSize/((fsTemp^2)*$aspectRatio)];
	If[StringLength[ToString[Unevaluated[expr],StandardForm]]>maxChar, .9*fsTemp, fsTemp]
]		

(*Creates thumbnail icons for expressions.  This is the final catch-all category*)
SetAttributes[ExpressionThumbnail, HoldFirst];
ExpressionThumbnail[expr_, sz_, bg_] := Module[{img,fs}, 
	fs = findFontSize[Unevaluated[expr], sz];
    img = Rasterize[Pane[styledExpression[Unevaluated[expr], fs], sz[[1]], ImageSizeAction -> "Clip"], "Image", Background->hexToRGB[bg]];
    If[Last@ImageDimensions[img] > Last[sz], ImageTake[img, Last[sz]], img]
  ];
 
(*Create thumbnail for graphics objects*)  
SetAttributes[GraphicsThumbnail, HoldFirst];
GraphicsThumbnail[expr_, sz_, bg_] := Module[{img},
  img = Rasterize[Pane[Show[Unevaluated[expr], Ticks->None], First@sz, ImageSizeAction -> "ResizeToFit"], "Image", Background -> hexToRGB[bg]];
  If[Last@ImageDimensions[img] > Last[sz], ImageTake[img, Last[sz]], img]
  ]  

(*Commonly used patterns in toicon*)
SetAttributes[MakeThumbnailRaster, HoldFirst];
MakeThumbnailRaster[expr_, sz_, bg_] := MakeThumbnail[Rasterize[Unevaluated[expr],"Image",Background->hexToRGB[bg]], sz, bg]

SetAttributes[FormThumbnail, HoldFirst];
FormThumbnail[expr_, sz_, bg_] := MakeThumbnail[
	Rasterize[Pane[styledExpression[Unevaluated[expr], fontsize[sz[[1]]]], 2*sz[[1]]], 
		"Image", 
		Background->hexToRGB[bg], 
		ImageSizeAction->"Clip"], 
	sz, bg]

(*Used below to extract heads from lists of objects inside function conditions*)	
SetAttributes[safeHead, HoldAllComplete];
safeHead[expr_] := Part[HoldComplete[expr], 1, 0]

(*********************************************************************************************)
SetAttributes[toicon, HoldAll];

toicon[i_Image, sz_, bg_] := {"Image", MakeThumbnail[i, sz, bg]};

toicon[{x__Image}, sz_, bg_] := {"ImageList", MakeThumbnail[ImageCollage@{x}, sz, bg]}

toicon[g_Graphics | g_Graphics3D | g_Legended | g_Graph | g_GeoGraphics, sz_, bg_] := {"Graphics", MakeThumbnailRaster[Show[Unevaluated[g], Ticks->None], sz, bg]}

toicon[{x__Graphics | x__Graphics3D | x__Legended | x__Graph | x__GeoGraphics}, sz_, bg_] := {"GraphicsList", MakeThumbnail[ImageCollage[Show[#,Ticks->None, Background->White]&/@{x}],sz,bg]}

toicon[expr_NotebookObject | expr_Notebook, sz_, bg_] := Module[{pre, isz, pad, temp, img},
 temp = expr;
 If[Head[expr] == NotebookObject, SetOptions[temp, WindowSize -> 500, Visible -> False]];
 pre = ImageCrop@Quiet@Check[Rasterize[temp], 
  Rasterize[Notebook[NotebookRead /@ Cells[temp][[1 ;; 100]]]], 
  Rasterize::bigraster];
 isz = ImageDimensions[pre];
 
 pad = (sz[[1]] - isz[[1]])/2;
 img = If[isz[[1]] >= sz[[1]], 
  ImageResize[ImageTake[pre, isz[[1]]], sz[[1]]], 
  ImagePad[ImageTake[pre, sz[[1]]], {{pad, pad}, {0, 0}}, White]];
  {"Notebook", MakeThumbnail[img, sz, bg]}
 ]

toicon[type_ExportForm, sz_, bg_]:= Module[{deployPad, documentIcon, format,imageSize, stringLength, text},
	format = Lookup[$formatFormats, type[[2]], "?"];
	stringLength = StringLength[format];
	imageSize = Which[1 <= stringLength <= 4, {Automatic, 94}, 4 <= stringLength <= 8, 250, stringLength > 8, 275];
	text = Rasterize[exportedFileStyle[format], Background -> None, ImageResolution -> 500, ImageSize -> imageSize];
   	documentIcon = ImageResize[ImageCompose[$notebookTemplate, text, {169.5, 100}], $documentHeight*sz[[1]]];
   	deployPad = ImageCompose[ImageResize[Graphics[{hexToRGB[bg], Rectangle[{0, 0}, sz]}, PlotRangePadding->None], sz], documentIcon];	
   {"ExportedFile", deployPad}
  ]
 
toicon[expr_, sz_, bg_] := {"DynamicObject", MakeThumbnailRaster[expr, sz, bg]} /; MemberQ[$dynamicObjects, Head[Unevaluated[expr]]]

toicon[{x__}, sz_, bg_] := ({"DynamicObjectList", MakeThumbnail[ImageCollage[Rasterize/@{x}, Background -> hexToRGB[bg]], sz, bg]}) /; SubsetQ[$dynamicObjects, List@ReleaseHold[safeHead/@ Hold[x]]]

toicon[{x__}, sz_, bg_] := {"MixedGraphicsList", MakeThumbnail[ImageCollage[{x}, Background -> White], sz, bg]} /; SubsetQ[$graphicsHeads, List@ReleaseHold[safeHead/@ Hold[x]]]

toicon[expr_, sz_, bg_] := {"GraphicsGrid", toicon[expr[[1,1,1]], sz, bg][[2]]} /; MatchQ[Unevaluated[expr], Grid[{{_Graphics | _Graphics3D | _Image | _Legended | _GeoGraphics}..}]]
 
toicon[expr_FormFunction, sz_, bg_] := {"FormFunction", With[{expr1=expr[[1]]}, FormThumbnail[expr1, sz, bg]]}

toicon[expr_FormObject, sz_, bg_] := {"FormObject", FormThumbnail[Unevaluated[expr], sz, bg]}

toicon[expr_, sz_, bg_] := {"Expression", MakeThumbnail[ExpressionThumbnail[Unevaluated[expr], {(1 - $textSpacing)*sz[[1]], (1 - $upperTextSpacing)*sz[[2]]}, bg], sz, bg]}

(*********************************************************************************************)

SetAttributes[Iconize, HoldFirst];

Options[Iconize] = {ImageSize->Small, Platform->"FileBrowser"};
Iconize[expr_, dpl_: "Default", OptionsPattern[]] := Module[{$fail, $iconType, itheicon, paneColor, imageSize, deploy, sz, width, height, dIcon, dColor, theicon, categories, cat4, iconpositions, temp},
	(*$fail sets whether or not the icon generation failed below.  
	This is used to determine whether or not to return the question mark icon*)
	$fail = False;
	(*$iconType will be used to determine whether or not to fade the bottom of the image*)
	$iconType = "Expression";
	imageSize = OptionValue[ImageSize];
	sz = Switch[imageSize,
		Small, {85,85},
		Medium, {128, 128},
		Large, {256, 256},
		_, imageSize
	];
	
	width = sz[[1]];
	height = sz[[2]];

	(*Alert the user if an invalid deployment is specified*)
	deploy = If[MemberQ[$deployments, dpl], dpl, Message[Iconize::deployment,dpl];"Default"];

	(*Deployment icon and color*)
	dIcon = If[deploy == "API" && sz == {85,85}, $smallAPI, ImageResize[deployInfo[deploy][[1]], $iconFraction*sz[[2]]]];
	dColor = deployInfo[deploy][[2]];

	(*Primary icon for expression*)
	(*The image pad is for the thin border separating the upper and lower panes*)
	itheicon = Quiet @ Check[
		With[{icon = toicon[Unevaluated[expr], {width, $paneFraction*height}, dColor]},
			$iconType = icon[[1]];
			ImagePad[icon[[2]],{{0,0},{0,1}}, Padding->hexToRGB["#b2b2b2"]]
		],
			$fail = True;ImageResize[$questionMark, sz]
];

	(*If for whatever reason, the iconization process fails, return the gray question mark*)
	theicon = If[UnsameQ[Head[itheicon],Image], $fail = True;ImageResize[$questionMark, sz], itheicon];	

	(*Add in "Core Language" as a category only if nothing else matches*)
	categories = With[{icat = Complement[findCategory[Unevaluated[expr]],{"core"}]},
		If[Length[icat]==0,{"core"}, icat]
	];
	
	(*NotebookObjects have the head LinkObject which is matched to the category ExternalInterfaces.
	We only want a single notebook icon for notebooks, so we simply override the initial category scoring*)
	If[$iconType=="Notebook", Set[categories, {"documents"}]];

	(*If a failure happens, display the gray question mark*)
	If[$fail,
		theicon,
	(*Restrict to only 4 categories*)
	cat4 = If[Length[categories]>4, Take[categories,4],categories];
	iconpositions = catIconPositions[cat4, sz, dpl];
	
	(*If the deployment is not default, then place the deployment icon in the upper RHS*)
	If[dpl != "Default",
	AppendTo[iconpositions, Inactive[Sequence][dIcon,{$heightFraction*width, $paneFraction*height + .5*(1-$paneFraction)*height}]]];
	
	(*The background color for the upper pane can be different for different platforms.
	For the mobile browser, we want the pane to match the background of the surrounding box, 
	so that it looks transparent.*)
	paneColor = Switch[OptionValue[Platform],
		"mobile", $mobilePaneColor,
		"Android", $mobilePaneColor,
		"IOS", $mobilePaneColor,
		"FileBrowser", hexToRGB["#fafafa"],
		_, $mobilePaneColor
	];
	
	temp = Fold[Inactive[ImageCompose], ImagePad[theicon,{{0,0},{0, $diffFraction*height}}, paneColor], iconpositions];

	Switch[$iconType, 
		"Expression", fadeArray[Activate@temp, hexToRGB[dColor], .3], 
		_, Activate@temp
		]
	]
]
