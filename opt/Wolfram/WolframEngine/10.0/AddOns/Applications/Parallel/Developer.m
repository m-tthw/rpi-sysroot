(* Dummy Loader of Parallel Computing Toolkit *)

(* :Package Version: 3.0 ($Id: Developer.m,v 1.2 2008/10/08 20:40:04 maeder Exp $) *)

(* :Summary:
   Needs["Parallel`Developer`"]
*)

(* :Discussion:
   this is only ever read if Needs is called before PT has been autoloaded.
*)

$KernelID (* cause autoloading of Parallel Tools *)

(* make it go onto $ContextPath *)
BeginPackage["Parallel`Developer`"]
EndPackage[]

(* e o f *)
