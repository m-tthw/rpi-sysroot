(* Mathematica package *)

Needs["Security`"]

Block[{$Path},
	Quiet[CloudObject[]]  (*ensure auto-loader doesn't fire during initialization*)
];

With[{dir=DirectoryName[$InputFileName]},
	Get[FileNameJoin[{dir, "UUID.m"}]];
	Get[FileNameJoin[{dir, "URL.m"}]];
	Get[FileNameJoin[{dir, "Dialogs.m"}]];
	Get[FileNameJoin[{dir, "Authentication.m"}]];
	Get[FileNameJoin[{dir, "CloudObjects.m"}]];
	Get[FileNameJoin[{dir, "APIFunction.m"}]];
    Get[FileNameJoin[{dir, "Delayed.m"}]];
	Get[FileNameJoin[{dir, "FormFunction.m"}]];
    Get[FileNameJoin[{dir, "ExternalFunction.m"}]];
	Get[FileNameJoin[{dir, "Server.m"}]];
    Get[FileNameJoin[{dir, "ServerAPI.m"}]];
	Get[FileNameJoin[{dir, "Client.m"}]];
    Get[FileNameJoin[{dir, "EmbedCode.m"}]];
    Get[FileNameJoin[{dir, "Iconize", "IconizeElidedForms.m"}]];
    Get[FileNameJoin[{dir, "Iconize", "IconizeImages.m"}]];
    Get[FileNameJoin[{dir, "Iconize", "IconizeThumbnails.m"}]];
]

$ContextPath = DeleteCases[$ContextPath, 
	"CloudObject`Iconize`IconizeElidedForms`" | 
	"CloudObject`Iconize`IconizeImages`" |
	"CloudObject`Iconize`IconizeThumbnails`"
];
