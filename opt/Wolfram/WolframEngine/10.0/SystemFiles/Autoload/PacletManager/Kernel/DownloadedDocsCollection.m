(* :Title: DownloadedDocsCollection.m *)

(* :Author:
        Todd Gayley
        tgayley@wolfram.com
*)

(* :Package Version: 2.2 *)

(* :Mathematica Version: 8.1 *)

(* :Copyright: Mathematica source code (c) 1999-2014, Wolfram Research, Inc. All rights reserved. *)

(* :Discussion: This file is a component of the PacletManager Mathematica source code. *)


Begin["`Package`"]


DownloadedDocsCollection

initializeDownloadedDocsCollection

DDCfindForDocResource


End[]  (* `Package` *)


(* Current context will be PacletManager`. *)

Begin["`LayoutDocsCollection`Private`"]

$downloadedDocsCollection
$downloadedDocsDirectory


(* Currently unused and unimplemented. *)

(*****************************  Initialization  ******************************)

initializeDownloadedDocsCollection[downloadedDocsDir_String] :=
    (
        (* Defer doing any work until actual use. *)
        $downloadedDocsDirectory = downloadedDocsDir;  (* Typically $UserBaseDirectory/Repository/SystemDocumentation. *)
        $downloadedDocsCollection = Null
    )
    

(*********************************  Getters  *********************************)

(* You must always call the initialize functions once in a session before calling these getters. *)


(* This incurs a small overhead (few hundredths of a second), so don't call it as part of PM startup
   process. Instead, defer until a doc paclet lookup is first needed.
*)
getDownloadedDocsCollection[] := 
    (
        If[$downloadedDocsCollection === Null,
            $downloadedDocsCollection = DownloadedDocsCollection[]
        ];
        $downloadedDocsCollection
    )


DDCfindForDocResource[linkBase:(_String | All), context:(_String | All),
                        expandedResourceName:(_String | All), language_String] :=
    Module[{},
        {}
    ]
    

End[]

