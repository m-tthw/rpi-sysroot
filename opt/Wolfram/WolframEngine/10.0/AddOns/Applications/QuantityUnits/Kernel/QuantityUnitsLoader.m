(* Mathematica package *)

PacletManager`Package`getPacletWithProgress["QuantityUnits", "QuantityUnits", 
	"IsDataPaclet" -> True, "AllowUpdate" -> TrueQ[PacletManager`$AllowDataUpdates], "UpdateSites" -> False];

If[Internal`$DisableQuantityUnits=!=True,AbortProtect[Catch[Get[ "QuantityUnits`"],"NOGET"]]]