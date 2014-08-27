(* Mathematica Package *)

BeginPackage["GarminFootPod`", {"ANTConnector`"}]
(* Exported symbols added here with SymbolName::usage *)  

Begin["`Private`"] (* Begin Private Context *) 

openFunction[args___]:= (ANTConnector`LoadANTLibrary[]; ANTConnector`ANTInit[1, 57600])

readFunction[args___]:= (ANTConnector`ANTAssign[]; ANTConnector`ANTDeviceConnect["SDM"]; ANTConnector`ANTDeviceData["SDM"])

closeFunction[{_,task_},___]:= (RemoveAsynchronousTask[task])

DeviceAPI`DeviceClassRegister["GarminFootPod",
    "OpenFunction" :> (openFunction[#]&),
    "ReadFunction" -> readFunction,
     "CloseFunction" :> closeFunction
]

End[] (* End Private Context *)

EndPackage[]