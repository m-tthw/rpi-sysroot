(* Mathematica package *)

Needs["Security`"]

With[{dir=DirectoryName[$InputFileName]},
	Get[FileNameJoin[{dir,"UUID.m"}]];
	Get[FileNameJoin[{dir,"URL.m"}]];
	Get[FileNameJoin[{dir,"Dialogs.m"}]];
	Get[FileNameJoin[{dir,"Authentication.m"}]];
	Get[FileNameJoin[{dir,"CloudObjects.m"}]];
	Get[FileNameJoin[{dir,"APIFunction.m"}]];
	Get[FileNameJoin[{dir,"FormFunction.m"}]];
	Get[FileNameJoin[{dir,"Server.m"}]];
    Get[FileNameJoin[{dir,"ServerAPI.m"}]];
	Get[FileNameJoin[{dir,"Client.m"}]];
]