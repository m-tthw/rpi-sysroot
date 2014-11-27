BeginPackage["CloudObject`"]

System`$CloudRootDirectory;
System`CloudDirectory;
System`SetCloudDirectory;

Begin["`Private`"]

CloudDirectory[] := $CloudDirectory;

SetCloudDirectory[dir_] := ($CloudDirectory = CloudObject[dir]);

$CloudRootDirectory := CloudObject[CloudObject`JoinURL[{$CloudBase, $CloudObjectsRoot, "~"}]]

$CloudDirectory := $CloudRootDirectory;

End[]

EndPackage[]
