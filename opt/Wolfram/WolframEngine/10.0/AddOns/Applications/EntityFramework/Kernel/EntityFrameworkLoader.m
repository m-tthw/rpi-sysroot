(* This EntityFrameworkLoader file only exists to separate the updating of the EntityFramework paclet from its use. 
   This allows an update to the paclet, triggered below, to take effect in the same session. In other words,
   we load the EntityFrameworkLoader.m file from the currently-installed version of the paclet, it calls PacletUpdate
   (via getPacletWithProgress[] below), and then the Get["EntityFramework`"] will load the implementation code
   from the new paclet.
*)

(* This Quiet is only temporary. It silences warnings about the "UpdateSites" option, which is
   unknown in older v10 kernel builds.
*)
Quiet[
   PacletManager`Package`getPacletWithProgress["EntityFramework", "EntityFramework", "IsDataPaclet" -> True,
                          "AllowUpdate" -> TrueQ[PacletManager`$AllowDataUpdates], "UpdateSites" -> False]
]

Get["EntityFramework`"]
