(* Master Loader of Parallel Computing Toolkit *)

(* :Package Version: 3.0 ($Id: noparinit.m,v 1.2 2012/11/27 14:29:03 maeder Exp $) *)

(* :Summary:
   Get["Parallel`Kernel`noparinit`"]
*)

(* :Discussion:
   this initializes player kernels; it is called from sysload.m.
   This is a shortened version of subinit.m
*)

(* create all the symbols *)

Symbol/@Parallel`Private`mainNames;
Symbol/@Parallel`Private`nonLoadNames;
(* Symbol/@Parallel`Private`devNames; *)


BeginPackage["Parallel`"] (* empty context *)

(* load remaining stuff,  but should not show up on context path *)
Get["Parallel`NullMaster`"]


EndPackage[]
$ContextPath=DeleteCases[$ContextPath, "Parallel`", 1, 1]; (* no longer on context path *)

(* e o f *)
