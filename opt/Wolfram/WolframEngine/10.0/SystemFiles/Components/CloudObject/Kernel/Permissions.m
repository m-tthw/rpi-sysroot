BeginPackage["CloudObject`"]

System`$Permissions;
System`Permissions;

General::invperm = "Invalid permissions specification `1`.";
General::selfperm = "The currently authenticated user `1` cannot be assigned specific permissions. Owners always have full permissions on their objects."

Begin["`Private`"]

$Permissions = "Private";

permissionSpecs = "Read" | "Write" | "Execute" | "Edit" | "Save" | "EditRestricted" | "CellEdit" | "CellCreate" | "CellDelete" | "Evaluate" | "IncrementalEvaluate" | "Interact";

normalizePermissionsSpec["r", type_, _] = {"Read"};
normalizePermissionsSpec["w", type_, _] = {"Write"};
normalizePermissionsSpec["x", type_, _] = {"Execute"};

normalizePermissionsSpec["Edit", type_, _] = {"CellEdit", "CellCreate", "CellDelete"};

normalizePermissionsSpec[list_List, type_, head_] :=
    Map[normalizePermissionsSpec[#, type, head]&, list]
normalizePermissionsSpec[spec_String?(StringMatchQ[#, Characters["rwx"]..]&), type_, head_] :=
    Map[normalizePermissionsSpec[#, type, head]&, Characters[spec]]

normalizePermissionsSpec[Automatic, "application/vnd.wolfram.expression", _] = {"Read"};
normalizePermissionsSpec[Automatic, "application/vnd.wolfram.expression.api", _] = {"Execute"};
normalizePermissionsSpec[Automatic, "application/vnd.wolfram.expression.computation", _] = {"Execute"};
normalizePermissionsSpec[Automatic, "application/vnd.wolfram.expression.fci", _] = {"Execute"};
normalizePermissionsSpec[Automatic, "application/vnd.wolfram.expression.form", _] = {"Execute"};
normalizePermissionsSpec[Automatic, "application/mathematica", _] = {"Read", "Interact"};
normalizePermissionsSpec[Automatic, "application/vnd.wolfram.notebook", _] = {"Read", "Interact"};
normalizePermissionsSpec[Automatic, "application/vnd.wolfram.notebook.element", _] = {"Read", "Interact"};
normalizePermissionsSpec[Automatic, _, _] = {"Read"};

normalizePermissionsSpec[spec:permissionSpecs, type_, _] = {spec};

normalizePermissionsSpec[spec_, type_, head_] := (Message[head::invperm, spec]; {})

normalizeUserSpec[user_String] :=
    If[StringFreeQ[user, ":"] && FreeQ[{"All", "Authenticated", "Owner"}, user],
        "user:" <> user,
        user
    ]

isOwner[user_] := user === $WolframID || user === ("user-" <> $WolframUUID)

normalizePermissions["Public", type_, head_] :=
    {"All" -> Flatten[normalizePermissionsSpec[Automatic, type, head]], "Owner" -> {"Read", "Write", "Execute"}}
normalizePermissions["Private", type_, head_] :=
    {"Owner" -> {"Read", "Write", "Execute"}}
normalizePermissions[list_List, type_, head_] :=
    Join @@ Map[normalizePermissions[#, type, head]&, list]
normalizePermissions[user_String -> spec_, type_, head_] :=
    {normalizeUserSpec[user] -> Flatten[normalizePermissionsSpec[spec, type, head]]}
normalizePermissions[user_String -> _, _, head_] := (Message[head::selfperm, user]; {}) /; isOwner[user]
normalizePermissions[All -> spec_, type_, head_] := normalizePermissions["All" -> spec, type, head]
normalizePermissions[spec_String, type_, head_] := normalizePermissions[{"All" -> spec}, type, head]
normalizePermissions[Automatic, type_, head_] := normalizePermissions[$Permissions, type, head]
normalizePermissions[spec_, type_, head_] := (Message[head::invperm, spec]; {})
normalizePermissions[users_List -> spec_, type_, head_] := Join @@ Map[normalizePermissions[# -> spec, type, head]&, users]

groupIdentifier[group_PermissionsGroup] :=
    Module[{cloud, uuid},
        {cloud, uuid} = getCloudAndUUID[CloudObject @@ group];
        (* TODO: What should happen when the group is in a different cloud? *)
        If[uuid === None, Return[$Failed]];
        "wolfram:" <> uuid
    ]
normalizePermissions[group_PermissionsGroup -> spec_, type_, head_] :=
    Module[{id},
        id = groupIdentifier[group];
        If[id === $Failed, (Message[head::invperm, group -> spec]; Return[{}])];
        normalizePermissions[id -> spec, type, head]
    ]

escapeAndNormalizePermissions = Composition[toJSON, normalizePermissions]

fromServerPermissions["r"] := "Read"
fromServerPermissions["w"] := "Write"
fromServerPermissions["x"] := "Execute"
fromServerPermissions[p:("Read" | "Write" | "Execute" | "Edit" | "Save" |
	"EditRestricted" | "CellEdit" | "CellCreate" | "CellDelete" | "Evaluate" |
	"IncrementalEvaluate" | "Interact")] := p

fromServerPermissions[permjson_] :=
    ImportString[permjson, "JSON"] /. {
        serverPermissions_List :>
            Map[convertFromServerPermissions, serverPermissions],
        other_ :> ($lastServerPermissionsJSON = permjson; $Failed)
    }

fromServerUserClass[class_] :=
    If[StringMatchQ[class, "wolfram:" ~~ __],
        PermissionsGroup[class], (* TODO: denormalize to the group's name, take into account the cloud base *)
        StringReplace[class, StartOfString ~~ "user:" -> ""]
    ]

convertFromServerPermissions[class_ -> perms_String] :=
    fromServerUserClass[class] -> Cases[Map[fromServerPermissions, Characters[perms]], _String]
convertFromServerPermissions[class_ -> perms_List] :=
    fromServerUserClass[class] -> Cases[Map[fromServerPermissions, perms], _String]

End[]

EndPackage[]
