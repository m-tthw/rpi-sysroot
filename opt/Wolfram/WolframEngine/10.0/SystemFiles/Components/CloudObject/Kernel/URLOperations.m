BeginPackage["CloudObject`"]

Begin["`Private`"]

(* URLFetch *)

Unprotect[CloudObject];

CloudObject /: URLFetch[CloudObject[url_, ___], args___] :=
    If[TrueQ[$CloudEvaluation] || !TrueQ[$CloudConnected], URLFetch, authenticatedURLFetch][url, args]

Protect[CloudObject];

End[]

EndPackage[]
