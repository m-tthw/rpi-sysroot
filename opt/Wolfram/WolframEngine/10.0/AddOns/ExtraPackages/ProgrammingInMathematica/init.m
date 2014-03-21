(* :Title: init -- Master package *)


(* :Context: ProgrammingInMathematica`init` *)

(* :Author: Roman E. Maeder *)

(* :Summary:
   the autoloading package for the ProgrammingInMathematica directory
 *)

(* :Copyright: © 1996 by Roman E. Maeder *)

(* :Package Version: 2.0 *)

(* :Mathematica Version: 3.0 *)

(* :History:
   2.0 for Programming in Mathematica, 3rd ed.
*)

(* :Keywords: template, skeleton, package *)

(* :Sources:
   Roman E. Maeder. Programming in Mathematica, 3rd ed. Addison-Wesley, 1996.
*)

(* :Warnings:
   this file was generated automatically
*)

(* :Discussion:
   See Section 2.5 of "Programming in Mathematica"
*)

(* Created by MakeMaster *)
BeginPackage["ProgrammingInMathematica`init`"]
EndPackage[]

DeclarePackage["ProgrammingInMathematica`AffineMaps`", 
  {"AffineMap", "AverageContraction", "map", "rotation", "scale", 
   "translation", "$CirclePoints"}]
DeclarePackage["ProgrammingInMathematica`AlgExp`", {"AlgExpQ"}]
DeclarePackage["ProgrammingInMathematica`Atoms`", {"Explode", "Intern"}]
DeclarePackage["ProgrammingInMathematica`ChaosGame`", 
  {"ChaosGame", "Coloring"}]
DeclarePackage["ProgrammingInMathematica`Collatz`", 
  {"Collatz", "FindMaxima", "StoppingTime"}]
DeclarePackage["ProgrammingInMathematica`ComplexMap`", 
  {"CartesianMap", "Lines", "PolarMap", "$Lines"}]
DeclarePackage["ProgrammingInMathematica`ContinuedFraction`", 
  {"CF", "CFValue"}]
DeclarePackage["ProgrammingInMathematica`FoldRight`", 
  {"FoldLeft", "FoldLeftList", "FoldRight", "FoldRightList"}]
DeclarePackage["ProgrammingInMathematica`IFS`", 
  {"ifs", "IFS", "Probabilities"}]
DeclarePackage["ProgrammingInMathematica`MakeFunctions`", 
  {"LinearFunction", "MakeRule", "MakeRuleConditional", "StepFunction"}]
DeclarePackage["ProgrammingInMathematica`MakeMaster`", 
  {"MakeMaster", "PackageContext"}]
DeclarePackage["ProgrammingInMathematica`Newton`", 
  {"NewtonFixedPoint", "NewtonZero"}]
DeclarePackage["ProgrammingInMathematica`NotebookLog`", {"NotebookLog"}]
DeclarePackage["ProgrammingInMathematica`Options`", 
  {"SetAllOptions", "SymbolsWithOptions"}]
DeclarePackage["ProgrammingInMathematica`RandomWalk`", {"RandomWalk"}]
DeclarePackage["ProgrammingInMathematica`RungeKutta`", {"RKSolve"}]
DeclarePackage["ProgrammingInMathematica`SessionLog`", 
  {"CloseLog", "OpenLog"}]
DeclarePackage["ProgrammingInMathematica`SphericalCurve`", {"SphericalCurve"}]
DeclarePackage["ProgrammingInMathematica`Struve`", {"StruveH"}]
DeclarePackage["ProgrammingInMathematica`SwinnertonDyer`", 
  {"SwinnertonDyerP"}]
DeclarePackage["ProgrammingInMathematica`Tensors`", {"li", "Tensor", "ui"}]
DeclarePackage["ProgrammingInMathematica`TrigDefine`", {"TrigDefine"}]
DeclarePackage["ProgrammingInMathematica`TrigSimplification`", 
  {"TrigArgument", "TrigLinear"}]
DeclarePackage["ProgrammingInMathematica`Until`", {"Until"}]
DeclarePackage["ProgrammingInMathematica`VectorCalculus`", 
  {"Div", "Grad", "JacobianMatrix", "Laplacian"}]
Null
