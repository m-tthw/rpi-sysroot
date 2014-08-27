(* Mathematica package *)
BeginPackage["CloudObject`"]

System`EmbedCode::usage = "EmbedCode generates code for an external environment.";

System`EmbeddingObject::usage = "EmbeddingObject describes various elements necessary for an embedding returned by EmbedCode .";

EmbedCode::noembed = "The given object `1` at `3` cannot be embedded in `2`."

Begin["`Private`"]

Unprotect[EmbedCode]

iEmbedCode[ExternalFunction["Python", params_, body_], "Python", uri_] :=
    Module[{parameters, paramNames, paramList, paramDict},
        parameters = parseAPIParameters[params];
        paramNames = parameters /. (name_ -> _) :> name;
        paramList = StringJoin[Riffle[paramNames, ", "]];
        paramDict = StringJoin[Riffle[# <> "=" <> # & /@ paramNames, ", "]];
        EmbeddingObject[Association[{
            "Files" -> {
                "wolfram.py" -> "https://cvs.wolfram.com/viewvc/Mathematica/Paclets/CloudObject/FCI/Python/wolfram.py?view=co"
            },
            "Code" -> "import wolfram
            
def f(" <> paramList <> "):
    return wolfram.call(\"" <> uri <> "\", " <> paramDict <> ")"
        }]]
    ]

iEmbedCode[expr_, dest_, uri_] := (Message[EmbedCode::noembed, expr, dest, uri]; $Failed)

iEmbedCode[f : ExternalFunction[lang_, __], Automatic, uri_] := iEmbedCode[f, lang, uri] 
iEmbedCode[expr_, Automatic, uri_] := iEmbedCode[expr, "HTML", uri]

EmbedCode[expr_] := EmbedCode[expr, Automatic]

EmbedCode[expr_ ? (!MatchQ[#, _CloudObject] &), dest_] := EmbedCode[CloudDeploy[expr, Permissions -> "Public"], dest]

(* TODO: need to think about an authentication/token system for FCI. *)

EmbedCode[obj : CloudObject[uri_], dest_] := Module[{expr},
    expr = Get[obj];
    iEmbedCode[expr, dest, uri]
]

Protect[EmbedCode];

hasItem[assoc_, key_] := !MatchQ[assoc[key], _Missing]

embeddingRow[{caption_, content_}] := {Style[caption <> ":", FontFamily -> "Helvetica"], content}

Unprotect[EmbeddingObject]

If[$CloudEvaluation =!= True,
    Format[EmbeddingObject[data_Association], StandardForm] :=
        Module[{items = {}},
            If[hasItem[data, "Files"],
                AppendTo[items, {"Files", Column[Join[
                    data["Files"] /. (name_ -> url_) :> Hyperlink[name, url],
                    {Button["Save to directory", Module[{directory},
                        directory = SystemDialogInput["Directory"];
                        files /. (name_ -> url_) :> URLSave[url, directory <> name];
                    ]]} /. files -> data["Files"]
                ]]}];
            ];
            If[hasItem[data, "Code"],
                AppendTo[items, {"Code", Column[{data["Code"],
                    Button["Copy to clipboard", CopyToClipboard[code], ImageSize -> Automatic] /. code -> data["Code"]
                }]}];
            ];
            Interpretation[Framed[Grid[embeddingRow /@ items, Alignment -> {Left, Top}], RoundingRadius -> 3],
                EmbeddingObject[data]
            ]
        ]
]

Protect[EmbeddingObject];

End[]

EndPackage[]
