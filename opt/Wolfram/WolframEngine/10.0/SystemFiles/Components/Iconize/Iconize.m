(* toicon is the core function that generates the central pane for each icon.
Each downvalue corresponds to a different category of expressions.  Currently, there are:
Graphics (includes Graphics3D, Legended, and Graph)
Image
GraphicsList (includes Graphics3D, Legended, Graph, and GeoGraphics)
ImageList
Notebook
DynamicObject 
DynamicObjectList
GraphicsGrid (a Grid of the form Grid[{_Graphics}..].  Supports the same types as GraphicsList and Image)
FormFunction
FormObject
MixedList- a list with mixed graphics elements
Expression- this is the the catch all final category
*)

Package["Iconize`"]
PackageImport["GeneralUtilities`"]

PackageExport["Iconize"]
   
$deployments = {"Default", "API", "WebForm", "EmbedCode", 
	"Report", "Mobile", "Publish", "EditPage", "EmbedContent", 
	"WebComputation", "ExportDoc", "ScheduledProg"};

Iconize::deployment = "Deployment type `1` is not one of "~~StringJoin[Riffle[Most[$deployments], ", "]] ~~ " or " ~~ Last[$deployments] ~~ ".";

(*******************************Global Variables*********************************************)
$cat = {"cloud", "core", "data", "documents",
	"engineering", "external", "finance", "geography",
	"geometry", "graphs", "images", "math", "sound",
	"strings", "time", "UI", "viz"};
	
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

(*********************************************************************************************)

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

(*Find the categories that the heads of an expression belong to*)
SetAttributes[findCategory,HoldAllComplete];
findCategory[expr_]:= Module[{$commonheads, heads},
	$commonheads = {"List","Rule","Hold"};
	heads = Complement[ToString /@ Cases[Hold[expr],h_[body___]:>h,Infinity], $commonheads];
	DeleteDuplicates[Flatten[headCategory/@heads]]
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

(*Format expression*)
styledExpression[expr_, fs_] := Style[expr, 
		LineIndentMaxFraction -> 0.05, 
		LineSpacing -> {1, 0}, 
		FontSize -> fs, 
		Italic,
		FontFamily -> "Helvetica", 
		FontColor -> Gray
		]
		
(*CloudExport File Style*)
exportedFileStyle[format_]:=Style[format, 
   			FontSize -> 24, 
   			FontColor -> RGBColor[.5, .5, .5],
   			FontFamily -> "Helvetica", 
   			Bold
   			]		
		
(*Decrease font size if expression is larger than pane*)		
findFontSize[expr_, sz_] := Module[{paneSize, maxChar, fsTemp},
	paneSize = (1 - $textSpacing)*sz[[1]]*(1 - $textSpacing)*sz[[2]];
	fsTemp = fontsize[sz[[1]]];
	maxChar = Round[paneSize/((fsTemp^2)*$aspectRatio)];
	If[StringLength[ToString[Unevaluated[expr],StandardForm]]>maxChar, .9*fsTemp, fsTemp]
]		

(*Creates thumbnail icons for expressions.  This is the final catch-all category*)
SetAttributes[ExpressionThumbnail, HoldFirst];
ExpressionThumbnail[expr_, sz_, bg_] := Module[{img,fs}, 
	fs = findFontSize[expr, sz];
    img = Rasterize[Pane[styledExpression[expr, fs], sz[[1]], ImageSizeAction -> "Clip"], "Image", Background->hexToRGB[bg]];
    If[Last@ImageDimensions[img] > Last[sz], ImageTake[img, Last[sz]], img]
  ]

(*Create thumbnail for graphics objects*)  
GraphicsThumbnail[expr_, sz_, bg_] := Module[{img},
  img = Rasterize[Pane[Show[expr, Ticks->None], First@sz, ImageSizeAction -> "ResizeToFit"], "Image", Background -> hexToRGB[bg]];
  If[Last@ImageDimensions[img] > Last[sz], ImageTake[img, Last[sz]], img]
  ]  

(*Make thumbnail simply pads and resizes thumbnails*)
MakeThumbnail[img_Image, {tw_,th_}, bg_] := Module[{w,h},
	{w,h} = ImageDimensions[img];
	If[w > tw || h > th,
		ImageResize[img, {{tw},{th}}],
		img] // ImageCrop[#, {tw, th}, Padding->hexToRGB[bg]]&
];

(*A commonly used pattern*)
MakeThumbnailRaster[expr_, sz_, bg_] := MakeThumbnail[Rasterize[expr,"Image",Background->hexToRGB[bg]], sz, bg]

(*********************************************************************************************)

$graphicsHeads = {Graphics, Graphics3D, GeoGraphics, Image, Legended, Graph};

$dynamicObjects = {Manipulate, MenuView, Slider, Button, 
   ChoiceButtons, PopupMenu, ActionMenu, TabView, FlipView, 
   SlideView, OpenerView, Opener, Checkbox, Toggler, Setter, Panel, 
   Slider2D, VerticalSlider, ProgressIndicator, Animator, 
   Manipulator, Control};

SetAttributes[toicon, HoldAll];

toicon[i_Image, sz_, bg_] := {"Image", MakeThumbnail[i, sz, bg]};

toicon[{x__Image}, sz_, bg_] := {"ImageList", MakeThumbnail[ImageCollage@Evaluate[{x}], sz, bg]}

toicon[g_Graphics | g_Graphics3D | g_Legended | g_Graph | g_GeoGraphics, sz_, bg_] := {"Graphics", MakeThumbnailRaster[Show[g, Ticks->None], sz, bg]}

toicon[{x__Graphics | x__Graphics3D | x__Legended | x__Graph | x__GeoGraphics}, sz_, bg_] := {"GraphicsList", MakeThumbnail[ImageCollage[Show[#,Ticks->None, Background->White]&/@Evaluate[{x}]],sz,bg]}

toicon[expr_NotebookObject | expr_Notebook, sz_, bg_] := Module[{pre, isz, pad, temp, img},
 temp = expr;
 If[Head[expr] == NotebookObject, SetOptions[temp, WindowSize -> 500, Visible -> False]];
 pre = ImageCrop@Rasterize[temp];
 isz = ImageDimensions[pre];
 
 pad = (sz[[1]] - isz[[1]])/2;
 img = If[isz[[1]] >= sz[[1]], 
  ImageResize[ImageTake[pre, isz[[1]]], sz[[1]]], 
  ImagePad[ImageTake[pre, sz[[1]]], {{pad, pad}, {0, 0}}, White]];
  {"Notebook", MakeThumbnail[img, sz, bg]}
 ]

toicon[type_Global`CloudExportFormat, sz_, bg_]:= Module[{deployPad, documentIcon, format,imageSize, stringLength, text},
	format = Lookup[$formatFormats, type[[1]], "?"];
	stringLength = StringLength[format];
	imageSize = Which[1 <= stringLength <= 4, {Automatic, 94}, 4 <= stringLength <= 8, 250, stringLength > 8, 275];
	text = Rasterize[exportedFileStyle[format], Background -> None, ImageResolution -> 500, ImageSize -> imageSize];
   	documentIcon = ImageResize[ImageCompose[$notebookTemplate, text, {169.5, 100}], $documentHeight*sz[[1]]];
   	deployPad = ImageCompose[ImageResize[Graphics[{hexToRGB[bg], Rectangle[{0, 0}, sz]}, PlotRangePadding->None], sz], documentIcon];	
   {"ExportedFile", deployPad}
  ]
  
toicon[expr_, sz_, bg_] := {"DynamicObject", MakeThumbnailRaster[expr, sz, bg]} /; MemberQ[$dynamicObjects, Head[expr]]

toicon[{x__}, sz_, bg_] := ({"DynamicObjectList", MakeThumbnail[ImageCollage[Rasterize/@Evaluate[{x}], Background -> hexToRGB[bg]], sz, bg]}) /; SubsetQ[$dynamicObjects, Head /@ {x}]

toicon[{x__}, sz_, bg_] := {"MixedGraphicsList", MakeThumbnail[ImageCollage[Evaluate[{x}], Background -> White], sz, bg]} /; SubsetQ[$graphicsHeads, Head /@ {x}]

toicon[expr_, sz_, bg_] := {"GraphicsGrid", toicon[expr[[1,1,1]], sz, bg][[2]]} /; MatchQ[expr, Grid[{{_Graphics | _Graphics3D | _Image | _Legended | _GeoGraphics}..}]]
 
toicon[expr_FormFunction, sz_, bg_] := {"FormFunction", MakeThumbnail[Rasterize[Pane[styledExpression[Grid[{List @@ expr}], fontsize[sz[[1]]]], 2*sz[[1]]], "Image", Background->hexToRGB[bg], ImageSizeAction->"Clip"], sz, bg]}

toicon[expr_FormObject, sz_, bg_] := {"FormObject", MakeThumbnail[Rasterize[Pane[styledExpression[expr, fontsize[sz[[1]]]], 2*sz[[1]]], "Image", Background->hexToRGB[bg], ImageSizeAction->"Clip"], sz, bg]} 
 
toicon[expr_, sz_, bg_] := {"Expression", MakeThumbnail[ExpressionThumbnail[Unevaluated[expr], {(1 - $textSpacing)*sz[[1]], (1 - $upperTextSpacing)*sz[[2]]}, bg], sz, bg]};

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
		With[{icon = toicon[expr, {width, $paneFraction*height}, dColor]},
			$iconType = icon[[1]];
			ImagePad[icon[[2]],{{0,0},{0,1}}, Padding->hexToRGB["#b2b2b2"]]
		],
			$fail = True;ImageResize[$questionMark, sz]
];

	theicon = If[UnsameQ[Head[itheicon],Image], $fail = True;ImageResize[$questionMark, sz], itheicon];	

	(*Add in "Core Language" as a category only if nothing else matches*)
	categories = With[{icat = Complement[findCategory[expr],{"core"}]},
		If[Length[icat]==0,{"core"}, icat]
	];
	
	If[$iconType=="Notebook", Set[categories, {"documents"}]];
	
	If[$fail,
		theicon,
	(*Restrict to only 4 categories*)
	cat4 = If[Length[categories]>4, Take[categories,4],categories];
	iconpositions = catIconPositions[cat4, sz, dpl];
	(*If the deployment is not default, then place the deployment icon in the upper RHS*)
	If[dpl != "Default",
	AppendTo[iconpositions, Inactive[Sequence][dIcon,{$heightFraction*width, $paneFraction*height + .5*(1-$paneFraction)*height}]]];
	
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
