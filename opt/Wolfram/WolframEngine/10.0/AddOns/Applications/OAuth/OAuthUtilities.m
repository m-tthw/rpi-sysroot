System`Private`NewContextPath[{"OAuthClient`","OAuthClientDump`","System`"}];

OAuthClient`ob;
OAuthClient`deob;

Begin["OAuthUtilitiesDump`"] (* Begin Private Context *) 

Begin["`Private`"]

deobflag=False;
$count=1;
(*rand={128, 146, 124, 203, 193, 132, 149, 192, 152, 141, 133, 107, 125,
145, 178, 194, 169, 205, 201, 140, 133, 97, 150, 169, 136, 206, 152, 204, 123, 133}; *)
rand={19, 63, 112, 111, 75, 117, 1, 111, 51, 99, 8, 34, 67, 1, 73, 3, 35, 
87, 2, 51, 14, 82, 27, 92, 15, 16, 8, 101, 95, 61};

ob[l:{_String...}]/;deobflag:=Block[{tf,tf2,res},
	tf = newfile[$TemporaryDirectory];
	Put[l, tf];
	tf2 = newfile[$TemporaryDirectory];
	Encode[tf, tf2, FromCharacterCode[rand,"UTF-8"]];
	DeleteFile[tf];
	res=Import[tf2,"String"];
	DeleteFile[tf2];
	ToCharacterCode[res,"UTF-8"]
	
]

deob[chars_]/;deobflag:=Block[{tf,res,string},
	tf = newfile[$TemporaryDirectory];
	string=FromCharacterCode[chars,"UTF-8"];
	debugPrint["string"->string];
	Export[tf, string,"String"];
	debugPrint["string"->string];
	res=Get[tf, FromCharacterCode[rand,"UTF-8"]];
	DeleteFile[tf];
	res
	
]

newfile[dir_]:=With[{file=FileNameJoin[{dir, "m-" <> ToString[RandomInteger[10000]] <> ".txt"}]},
	If[FileExistsQ[file],
		If[$count>100,Throw[$Failed]];$count++;newfile[dir],
		file
	]
]

ob[___]:=$Failed
deob[___]:=$Failed

End[]

End[] (* End Private Context *)

SetAttributes[{deob},{ReadProtected, Protected}];

System`Private`RestoreContextPath[];

(*
{OAuthClient`predefinedservicelist,OAuthClient`OAuthServicesData,OAuthClient`oauthcookeddata,OAuthClient`oauthsendmessage}
*)