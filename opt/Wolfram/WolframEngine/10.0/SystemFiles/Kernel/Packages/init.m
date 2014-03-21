
(** User Mathematica initialization file **)

(** Decide how to display graphics on this machine **)

Begin["System`Private`"]
(* Hide any symbols which are created *)

Which[
        $Linked || $ParentLink =!= Null,               <<"FrontEndGraphics.m",
        True,                                          <<"NoGraphics.m"
     ]

End[]

