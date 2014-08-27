(* ::Package:: *)

BeginPackage["CloudObject`"]

Begin["`Private`"]

loginDialog[username_String] := With[{btnlblStyle = {"DialogStyle", "ControlStyle", White, FontSize -> (Inherited*0.95)}, boxid = "username"},
  Block[{$loginCredentials}, 
   Module[{uname = username, pwd = "", leftCol, rightCol, columns}, 
   	Clear[$loginCredentials];
    If[Developer`UseFrontEnd[CurrentValue["UserInteractionEnabled"]],
     leftCol = Column[{
        Column[{
          TextCell[Row[{
             "Wolfram ID ",
             Style["(your email address)",FontSize -> (Inherited*0.85)]}], "DialogStyle","ControlStyle"],
             InputField[Dynamic[uname], String, ContinuousAction -> True,  System`BoxID -> boxid]
          }],
        Column[{
          TextCell["Password", "DialogStyle", "ControlStyle"],
          
          InputField[Dynamic[pwd], String, ContinuousAction -> True, 
           FieldMasked -> True]
          }
         ]
        },
       Alignment -> Left,
       Spacings -> .5];
     rightCol = Column[{CloudDialogImage["NewAccountIcon"],
        TextCell["Don't have a Wolfram Cloud account yet?", 
         "DialogStyle", "ControlStyle", LineSpacing -> {1, 0}]
        },
       Alignment -> Center,
       ItemSize -> Scaled[.6],
       Spacings -> 1];
     
     
     columns = Grid[{
        {"", Pane[
          leftCol,
          Full,
          Alignment -> Left
          ], Pane[
          rightCol,
          Full,
          Alignment -> Center
          ]},
        {"", DynamicWrapper[Grid[{{
             Button[
              
              Pane[Style["Sign In", btnlblStyle], 
               ImageMargins -> {{10, 10}, {0, 0}}],
              DialogReturn[$loginCredentials],
              ImageSize -> Dynamic[CurrentValue["DefaultButtonSize"]],
              Appearance -> {
              	"Default" -> CloudDialogImage["SigninButton","Default"], 
              	"Hover" -> CloudDialogImage["SigninButton","Hover"], 
                "Pressed" -> CloudDialogImage["SigninButton","Pressed"]}],
             
             Style["Forgot your password?", "DialogStyle", 
              "ControlStyle", Gray]
             
             }}],
          $loginCredentials = {uname, pwd}], Button[
          
          Pane[Style["Join Now", btnlblStyle], 
           ImageMargins -> {{10, 10}, {0, 0}}],
          Null,
          ImageSize -> Dynamic[CurrentValue["DefaultButtonSize"]],
          Appearance -> {
          	"Default"->CloudDialogImage["JoinNowButton","Default"], 
          	"Hover" ->CloudDialogImage["JoinNowButton","Hover"],
          	"Pressed" -> CloudDialogImage["JoinNowButton","Pressed"]
          	}]}
       } ,
       Dividers -> {{None, {3 -> 
            Directive[RGBColor[0.74, 0.74, 0.74]]}}, None},
       Spacings -> {0, 1},
       ItemSize -> {{Automatic, 
          1 -> FEPrivate`If[
            FEPrivate`SameQ[FEPrivate`$OperatingSystem, "MacOSX"], 4, 
            3]}, Automatic},
       Alignment -> {{Left, {-1 -> Center}}, Top}];
     
     DialogInput[
      ExpressionCell[
       Framed[
        Column[{Panel["", Appearance -> {"Default" -> CloudDialogImage["TopBanner"]}, ImageSize -> {Full, Automatic},
           FrameMargins -> {{10, 10}, {0, 0}}, 
           Alignment -> {Left, Center}],
          
          Panel[
           Pane[
            columns,
            Full,
            ImageMargins -> {{0, 0}, {0, 15}}
            ],
           Appearance -> {"Default" -> CloudDialogImage["BackgroundImage"]}
           ]
          },
         Spacings -> -0.1
         ],
        ImageSize -> {Full, Full},
        FrameMargins -> 0,
        ImageMargins -> 0,
        FrameStyle -> None
        ],
       CellMargins -> {{-1, -5}, {0, -2}},
       CellFrameMargins -> 0
       ],
      WindowTitle -> "Enter Login Credentials",
      WindowSize -> {520, FitAll}, 
      Evaluator -> CurrentValue["Evaluator"],
      Modal -> True,
      NotebookEventActions -> {
        "EscapeKeyDown" :> DialogReturn[$loginCredentials = $Canceled],
        "ReturnKeyDown" :> DialogReturn[]
        },
      NotebookDynamicExpression :> (
        FrontEndExecute[
         FrontEnd`MoveCursorToInputField[EvaluationNotebook[], boxid]
         ])
      ];
     $loginCredentials,
     (*Else,no interactive FE*)Return[$Canceled];]]
     ]
  ]
  
(*----------------------------------------------------------------------------------------------------------------------------------*) 
(*----------------------------------------------------------------------------------------------------------------------------------*) 
(*----------------------------------------------------------------------------------------------------------------------------------*)   
(*----------------------------------------------------------------------------------------------------------------------------------*)   
(*----------------------------------------------------------------------------------------------------------------------------------*)  
(*---------------------------------------------------------------Images Below Here--------------------------------------------------*) 
(*----------------------------------------------------------------------------------------------------------------------------------*) 
(*----------------------------------------------------------------------------------------------------------------------------------*) 
(*----------------------------------------------------------------------------------------------------------------------------------*) 
(*----------------------------------------------------------------------------------------------------------------------------------*) 
  
imgr[relPth_List, flNm_String] := FrontEnd`FileName[relPth, flNm]

imgr[flNm_String] := imgr[{"Dialogs", "CloudLogin"}, flNm]


imgimportr[relPth_List, flNm_String] := 
  Dynamic[RawBoxes@
    FEPrivate`ImportImage[FrontEnd`ToFileName[relPth, flNm]]]
    
imgimportr[flNm_String] := 
 imgimportr[{"Bitmaps", "Dialogs", "CloudLogin"}, flNm]

CloudDialogImage["NewAccountIcon"] = imgimportr["NewAccountIcon.png"](*Image[\!\(\*
GraphicsBox[
TagBox[RasterBox[CompressedData["
1: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
Z7q5nYSe/vW+r8/ST1q0WHA6MZF57QC5pqFNIXnukMT7m5grUe1zJio8cZAp
r2MjwVcoxxLShL/5iJgo9s+0Zuos4fHzCMe5vv0MU9U0fN7IMcdTrZ1868S5
pNZS1rKiHmqtYjPbSpSqA+MUf8loUwRJDMmK53g1rq2QlayZ1BmozvzzWUOo
um+t9kmM87MAsd8iz0URK9UevdbgrKXRMyRxru1kqtuaKp27KP0zVj7X9JJ5
VPNRMdlfxa9Asa8h9+3k2a2mMe8Ghan1icEvy6ilyFu65q/m8TMI/5Vn2vId
VpJn33z2aD/XVjwKbHXG3tkTzzTt5/aO7zg08zcEwjexdlTfEdgxq7N3ufay
Sp9g3mz+Qt66nST/mnEOb5P8yrzrrvbKzeRmIq4pXUmesYoz7DpdM8505Tco
nRt+l9AQo8L3+Nm0mep/ByLX/gZuF8mDkK/BowfJfQ8Pe598biHH+bP9GxLw
2FTN2VTkLE31qfeJfQdN+JD5m5r637E88byvwXcluunbkMbP1ky4bU0MXdcZ
/Enczez8m7+DkXHMWZ59m76DUd/C1OkOudaT8WPf+Zh4Nd839q1EI98F1cPX
2Jlv/TnMfNTXu9aglfbkVO/s6gm8mnkw7bersyEzX4/P9b+alvhg
"], {{0, 43}, {56, 0}}, {0, 255},
ColorFunction->RGBColor],
BoxForm`ImageTag["Byte", ColorSpace -> "RGB", Interleaving -> True],
Selectable->False],
BaseStyle->"ImageGraphics",
ImageSizeRaw->{56, 43},
PlotRange->{{0, 56}, {0, 43}}]\), Magnification -> 1]*);

CloudDialogImage["BackgroundImage"] = imgr["Background.9.png"](*\!\(\*
GraphicsBox[
TagBox[RasterBox[CompressedData["
1:eJzt11mKZEUAhtEq9cE11JtL0SW0K+gGfRVaQXpfbqnmeZ47rFKjcAENTXyc
H07eyHy6XD4u5A8ffnv36zcbGxu/f//y8e79nz9+/Pj+08/fvXz56dMfv3z4
9uXw139efxxj8IX8+zi//n1gtv62trZeL3q2xPRspenZSvvf/xez5adnK03P
VpqerTQ9W2l6ttL0bKXp2UrTs5WmZytNz1aanq00PVtperbS9Gyl6dlK07OV
pmcrTc9Wmp6tND1baXq20vRspenZStOzlaZnK03PVpqerTQ9W2l6ttL0bKXp
2UrTs5WmZytNz1aanq00PVtperbS9Gyl6dlK07OVpmcrTc9Wmp6tND1baXq2
0vRspenZStOzlaZnK03PVpqerTQ9W2l6ttL0bKXp2UrTs5WmZytNz1aanq00
PVtperbS9Gyl6dlK07OVpmcrTc9Wmp6tND1babPnz58/D1idninRMyV6pkTP
lOiZEj1TMnt+fn4esDo9U6JnSvRMiZ4p0TMls+enp6cBq9MzJXqmRM+U6JkS
PVMye358fBywOj1T8trzq699H/AleD9TomdKZs8PDw8DVqdnSvRMiZ4pmT3f
398PWJ2eKdEzJXqmRM+UzJ7v7u4GrE7PlOiZEj1TMnu+vb0dsDo9U6JnSvRM
yez55uZmwOr0TImeKdEzJbPn6+vrAavTMyV6pkTPlMyer66uBqxOz5TomRI9
UzJ7vry8HLA6PVOiZ0r0TMns+eLiYsDq9EyJninRMyWz5/Pz8wGr0zMleqZk
9nx2djZgdXqmRM+UzJ5PT08HrE7PlMyeT05OBqxOz5TomZLZ8/HLGVb31vPx
8YDV6ZkSPVMyez46OhqwOj1TomdKZs+Hh4cDVqdnSvRMyez54OBgwOr0TIme
KdEzJbPn/f39AavTMyV6pmT2vPdyhtW99by3N2B1eqZEz5TomZLZ8+7u7oDV
6ZkSPVOiZ0pmzzsvZ1jdW887OwNWp2dK9EyJninRMyWbm5v/9Ly9vT1gdfP9
/HqF1b22/DeS42nU
"], {{0, 180}, {180, 0}}, {0, 255},
ColorFunction->RGBColor],
BoxForm`ImageTag["Byte", ColorSpace -> "RGB", Interleaving -> True],
Selectable->False],
BaseStyle->"ImageGraphics",
ImageSizeRaw->{180, 180},
PlotRange->{{0, 180}, {0, 180}}]\)*);

CloudDialogImage["TopBanner"] = imgr["Banner.9.png"](*Dynamic@RawBoxes@
  FEPrivate`FrontEndResource["LocalizedBitmaps", "CloudLoginBanner.9"]*)(*\!\(\*
GraphicsBox[
TagBox[RasterBox[CompressedData["
1:eJztnXmUVNW1h30v74/3d+Y5MYlCTzVXz3PTgAgKAs5xwNmgII44IUpwQMEo
4oQD4CyDcdY4RYwzkjhAk5WXPDRqkudKRCPJennJefvbt/bldnVV0920Vjec
u9aPqjr3nL33PVX3u/uce26z+7QZ+x3777vtttup/yn/7HfkmW2zZh05e/J/
yIf22acdM+1z8iYrmvJvu+1GoXPOy8vLa5dWgMLBsdXU1OS8vLyGv5qbm0se
w64qY3Kp4/Dy8vIa7vI89fLy8hocGU8bGxudl5eXVynU0NCwQ/uHijxPvby8
vAZHnqdeXl5egyPPU6/hpKE87hvKsXl9NjKe8lvw8vLy8hq4PE+Hh7j2lToG
f7xeXr3L89TLa/jr077++Otb32Q8ra+vd15eXl5eA5fnqZeXl9fgyHhaV1fn
vLy8vIar4FmpY/A8HboaCr8PLy+vvsvz1MvLy6unBpLPGE9ra2udl5eXl9fA
5XnqNZT0wx/+0I0aNarXOpMmTXInnniivpY63h2W5DS9arj5GaDPmpqa0n8X
gyDjKcfj5dVX2TkwGLrxxhvdM8884z766CN+i+74448vWG/ixIlu3bp1Lrpd
eOGFJe+Lfol+U9W5mjpTvaupb+guythXa6otrh3x1c1PLz76pbqBH1+pv58d
lOfprivYVEjFGAX3yB3zy8kp+2Mnqurqanf99de7p556KmTkMccco+X5PuDt
T3/6U3fyySe7l156Ses+/fTTPeoOWRkzQsY0upqGJlfd2CxqiahZy2saGoM6
IX/qt3Eon4EFfdX13Rf7823XFVBtAUW5aVJuNgbHoH4Df9ne/NrxlPp72gEZ
T/lNDjWNHj3azZgxQzV16tSSx7MzKZPJuKOPPto9+eSTIcdeeeUVd9RRR7nx
48e7bDbbrf4+++yjdebOndvDVjqdVlv33Xef1oF7M2fOLGgnX6lUypWVlbnd
d989jOPwww/X+KxOR0eHe/fdd93GjRvd9773Pfetb33Lfe1rX3Nz5sxR+9go
dX+iXo+Va0dNrasWXlTXNbhq4Ui2oVn5km1udxlRNqqmVpeBO7BI6lbXN3SX
MKha+KP2sJu7NvXqS2xme/hpC2KQ/eqnLmc7lPnL85lTls/RevUNuXgjx4cP
jpHXlo5ux5i1Y6R9eDw13Y9nGGko8nTatGnuoYce0vMnqhdeeMEtWrRIORut
b3VLGTNxEQPsL3X/9UUwaOTIkS4Wi7ktW7Yoxx577DG3xx57uKqqqm48QwsW
LNA68DfKDd4nEgk3YsQId/rpp2udU0891f3gBz8oaKdQHPD029/+dshTclEY
bXUuuOACLb/pppuUu8bUr3/96+onmUyWvD97UzbkW13ADeEHHAn4MsplW0e7
bNsYlxFl20a7TGvnNu7kmNNNxr6QqxGeFvJlHA19Bf70lc9SnolyFb7llDEm
9srbCDtNoc8O9ZHBV/vY3DHmfBOPMV3tFzieYaahxlNYuX79emXTgw8+qOcx
uueee8Jy2GX1OV+Np9Fz8DM9X4QpCxcu1BhOOumk7eZk/RXHx5h4MG3SV+Xl
5cqxO++8M2QZXISP0WOgjzk228g7o7aMiUuXLnUffvihcm7PPfd08Xh8u31h
+SV+ozw1DrP/5Zdf1nL6mJjhJ6zmetAXZpdcsCGXKwZ5Yo6jcKV9L3fErLPd
ouWrQ510/qXBPuXqqG755LQZZ6i2MS6X11lOF2FpIV/YjvqaJr4p78E3eKhq
czPOneemHnHstlwymifnrg3sn3HevICfytBOtTnlqBPdyXMuc9PF70kXLHDT
5T2vU4+envObO078Yl+ZPXyZajzldzsYsnNgoDIuXXrppXpeRgUDLrvsMtfS
0qLnkOVG3//+9zVvIdcarOPoj4wHxMD5TpyDZRtb9AdMtWMeDGHLctQjjjgi
ZBlzk/l+xo0b1+0eEN9Bvi3Y+c4777i77rpL80c+9yfeioqK0P6hhx4a9iHs
/P3vfx/mvcZobFNnMPvkU1F1kJ8yLs4KKzKNrS5NPtg21p0051L3wm8/cF0f
OvfUW++4h17ZpK8b//Iv98v3t7qFy1ZKrhrkq5mWTpcSPfxKlyotnGM+AJvZ
2nrxUxPxVe8y9U2ac6abA1+zFywJfT355tvuQfFlnx96uUuZnm4dE/rKEKOI
9hulziKJJd0o9oTR2E5jvz7IXylnP7bStBVGpsVnWni5cPkaLcdGV57wD9Mz
Uo/j5Jiwm+VaINcFPaZSf3/bUT7vBpunOyLODXJS+PGVr3zFffOb33Tf/e53
lVPkUTCVcR4MgAW02dl5Cj/ojwceeCDMGwdT2P/Od76jeSXbmjVrejCKaxtz
Aps3b9Y6NhaI1tlrr710H/Oo9EHUxiGHHKLXSeZeeV8ojmI85Tu17aCDDirY
B8ztnnbaafq+vb1dfbFGoFA9YqAu9QrFQXzRtry/4YYbCtqz4+rNXsC4WmUE
7Mk0dwhrxrgfX7vc/XqLc6ueXe8mTjvRVbSMdpXNoqZRbtIRx7lHhJnsn7/k
VuHcaFVKePfIq/B0o0thRzim43HJ55Sj1VFutwQ8FK5duWK12/Thv9z1qx5z
rZMOchXiB+HvsJlni81NbpP4IiYYmMmJOPFJHIuWr3RJ/DUFPkPJ51RTu+6n
Xkr8pTrGufSo8S7ZMd4tWrFGyyvax7vKtr1VVaLDTjlHWfr6H7a6Zza84zon
HxJcO8SWHlOtHdOO5WeftYyn/P5LLc4VuMH5aucY5ztiXMeYEm5VVlaGuQks
4z7yFVdcoe+j9qZMmaLjZHI7NG/ePC1jvsDyMKtnZWjFihVan9dovWIilh/9
6EcaA2siLbYdtUt75jroj1/84hdqn2PIr0fZypUr+2XbRP7HHCRt2eAmZbaf
Pn3iiSc077z44otDth1wwAHd7JxzzjlazvUPBlIGZ9577z29P8W9Lu4psVF2
3HHHdWsPg6M8xe8dd9zhim2bNm3S/byyvfrqq8pbW3Nl/MX2hAkTdB1AdKPe
wQcfrPuJhd8dcbHxmzn22GPDeKM+29ratB3+itnrpmy1y9TUuUxdo/IvJYw7
/JSz3G8kzNseXesqmjpdReMoV17f5irrWl1FbbOrqGl0laIfL17q4g1tmpem
hDUxYS48fVQUb2wXvrUG+SL5KPMySN6nyB+bgnbnSF6Kr3lLlgW+Gjpchfip
rBdfDe3qm/I1a18Ttn3i9pNxeEpYiBJte7lY61htv1B4mWjqcCn12RKKz5TD
U+rFpE28fZxLjtrHxTsmuIXCcsrLmvdy5c1jRaODOHKC52/8cat7duM7AYsl
H8ZuhuuPXB8y2WzJudQfDSWewszFixcrP2699VZdm5PPLc5145WVRedPrbyz
szOcb42KdTm8kldYfeY8o/vyVYhh+XFF50+tfEftMg63fP21117T99ddd123
4yxmGyb0pc9tHpX7aLZxP9D283/gshEL1zvbli9f3s3G6tWr3aOPPqr3s/iO
7B6SXRspR9SzDWYV4ym+9t9/f80BbSO3POywwzQWrjVLliwJmYcfWwNgG8zn
GsTG2gPGNa2treH8QVdXl8Z+4IEHumuvvTZkMfuxdc011+hzA48//nhok+sC
9Th+5nlvvvnmcB9tevRxjnFwLy05ZVKYeOdjz7l1mz9wzRMPzDGu3cVQXbOL
CUerqutdRbpGX+P1LS4pvIrn8tfH13WpYlKW1PF3S4SnNTlfLTpOT0puiZ/H
121SP+X18FP8CEtjYrdKWK1MlX3EsuFPW90dEluic6JLjp7oEpJfkkv+7mPn
rhJexuGp5I9pmJoTnylnP/Won4Clnfu6WMc+7srbVmt5ufC0omWMq2qBp3L9
aOxQtpc3drrp58/XOpdct1yvATbu1+tQtrrkXOovTxG/q1KLvBMOwg7jAqwk
x4M9kydP7tGG886YQ15k5ca3W265RT8zT8C6RrN9+eWX63lPHsQ5Y/6YG2T8
S33GuZTBZeoVixs72KMutqi7o3Yp5/pCvkfd+++/X9+To3PM0WMkt+I+HvMj
jLftGOFOX/odP9Hx9rJly8K4yDvJWbHNfMtzzz0XMseubYwfqDNr1iyNb++9
9w5t8Qw0czXEbnM2zz//fJjTMReODdpFeUpMjEXwaxu5P5+xw9wKog9s4/7/
N77xDbfvvvvqNYH1/2zERozmn/zdNsr53TFnZHFxXaCP+b6o/+Uvf1nHB3bc
nDccD76Yl+K7tc2+m/B7ZD5IGJdsCLgYE6a8/VfnblnzmCvX3LRDuNbuEuyv
bXTJGmFottbF0tUuJq/xuiZpJ/uFj/D0qdc2qSqlneausBOGCnfSwtN0bYOU
tbqk1J85Z776mr3gKuHWqLBNQliaELv4jOVyVGIhJurHRu3r4sJTXiuEj5Qt
XrFK5yIS5Nhi3wTTY1LOfuqVt+4t7cYrk6uEp4tvW+M2Uy65aaWwNC51Y8J0
8mvlufguE98vdr2jx8V1A0Zrji08TWeyJedSf2T5aanjQPCQ3+l+++2n/CiU
dzGuJW+NssDmCGCCsYh1PcbYr371q7peEV199dUh3ziXYALjPcoY01KHc4g2
nEfkH+wjLyoWN3awRz1sYXNH7XIMnOec08ZTeGD339lv1xH8YxdxfpNrUk5u
1pd+pz33kGzMDzPMB2N91lbAGxRlEcyiPWuD2ehrYrZ1qG+++abyj3v9fDfk
hxwDebtt5513XkGeYos2/B5sww8xkOdiE57aOi7mdu07/tKXvqR9TV0YyzpV
7NCGMn5ftuGDmLFl1wpsRpkNN7kGssFVvpPoui34bZvNRYWCcxHGnSKMe/8T
52acf4nmhbAIJqXqmwMuVtcKg2tcMlPtkvI+CU9lv/H0mde63M/Xb8qxTcb8
1s54WteoY/CY1L/2tpXqq6wxGFvDsAQcFJsak7RNKA87dP95ly/W+jPnXu4S
oye5mDCxvH28li0RWxorrGuM8FQ+U74k5wv+xoXD8LRSeLrktlVazjhfWSlx
wUu9Rkg8HAc8XSYsp16l+eC44Gl2aPLUxmXFeGo5VSll6xg5F774xS/qHD/r
y/kt33333SFTGWtZG84HYy/nJGWci8YgzgXOP7tfdOSRR4brB6gHSxh3Usb8
IOcR5x3iPWXsO+GEE4rGjR3LObFlc747ahcbxG7HQlt8weroMdo6TDtGmBq9
h7W9fscebaOshF12L4icFxYyJo9yj7E7bZmjYW0wrOL6ZuPphx9+WOPiOMwP
dniOxDZyYcqjdhnj27Uuuo6K+Unsc0zsJx7LT2Gh9bHNLeCb75/+sesQ1wBi
tc3yfeKyHJSxBnbwRR/QljLjKXbhJvuIgXmJqL2wb7kuSn5FzpmsJxdsU57+
6W9OXi8OGMe4HTZJbgpHqa9teFWeNuo8aTzHvLW/7FKR1+kcqthNif2U8FTb
ix3NO6X+dbevVF/lTdzn6tR5TuoTD7api2+zTUzUnzV3gbI0NgqeTtCy625f
tc1GY2soYmD8fu1t9wa+uN8kHGWszz0o2lEeHqvwO9mQa59jMay91upJDh1j
vzBfY8xkSs6l/mgo8dTYxG+ZvAYukMuZ4Cu/Z1jB+2I8pSzKIM4Xzk3K4XMx
nl5yySVqw/JLzhXKLO8sxqbt8XSgdhHt7FiIP3odsXLYYfyBATDVeEpsfel3
7EXnMOEcfGWsTG5n/MC23SdiH/42bNig86V2r9C2Rx55RG3m91X0WagXX3xR
Y8/nqfVJsXKEb+aCjHPGTOMtxwTjqcv3w/wmImfN5x91bbyPTWwX8kOd6DHh
B84X5CmCixGeHj3zTPfnvzs394prZPw7RnM2uEKemTLOmbRdszKIcTJMeuFX
XSruz1c1d0ba1oX1yTkZ298gPMXXpKNOdFXiK9nSGfAMVkl+anVtbvaCKxZr
/aNPPUeZSH5Z0TZey66/Y5XWSeCTuVnE/EUub75B9lOvohWeTpAcVdoLT8Ny
8c/9NGunXJb3xMW+2+97xL37l63K0/hOwFM714eCLPew8RnjK/IMxnGW1zE3
aWwynpJjWA5krIlyjHOZOVTjKedJlHuUwQOrz/t8ThaKFzu98XSgdvOPxday
93aMsIR+izK4L31unLMxL4xctWpVONZnv9nmeVDb5s+fr6/8X2T5+auxJ+oH
G/DfNuzTHxxPNA+1PilWbv2Yz7l8X+eee67O06Kzzz5bj4V5eNvsN0PbKE/z
v69ifvJ5avZCSY6aZPwuuWCsrkXv47/34Sfu/qeezTFmjI7llWvki3CupiHI
aeW9znU2BnkcPH3pjS5VGfORwjZtC39ol6sfz91ngt0f/q8cz43Lcuweozki
9ROwlFeYlls/9cCTa937W7bqvSNb21TeMs5tERt33f9oGC/8RbBV7y9J26V3
rtR65S176T2pmLAUti69c9W28lwMCY27U1jcqesHmFv97fsfuKdeXCf9Izlv
XXPQB/RbKv2ZMAcWDoadocLTQsdjeQa84jfMfJXNf/IbZn8xnjLvR1n+b9/m
OYvxlLLeODlQng7E7vZ4Sk6G8jnC80u0YY42f18xGSvhj23knzbWj+b4jB1s
vSp13nrrLf1u7JmlN954I9xXyH90DM/cps2tDjZPub9vG/fImA9AJeGp8CFW
26RrlR4Wln7yD+f2l7xR1xe1GlOD+1LKUHjHa0OwHknHxcLTV9/sEm3S9UeV
reOCtjDS2jB+F55yr6esocO9Kuzd/IcPXPvkQ5RdyrNcfohPPlM+9agTNKab
hYvluflW+MdYHBsbf/u2MDGoG4+Iz3CW/T9/aZ3Wr6K8bZyUjxV7q9Qu96mU
s7k1WOSqxE5dq3P6+fNcZW1zcF2prgvmkD8jng6WhgpPTdxvYn6U8Xz+Pu5T
cO8a2bxcMZ7a/R67j49YK2PzBcONp1wf8nlqx8hz89E2rCOiHDb2lafGlOjc
po31ozkueSrsZH2RbfzdKZtzIC67R8TGvad8P9OnTw/38zfNPy2e0g+2Mb4h
bq4ZrI/6zHgKD5gPFT7EhamVwtS2faa4P3+01f36d2/rOva4MmZMLucbpawb
PXGqW//mRnfCrDO0zHi6Xni6/q1NIZ9Y66n5IvesYC+vuXvn5MLHzjzD/f3/
nHvs6bXKrniO35qr8iqfOycfrLFs+q/NrnXCZFcp7apy94rIc6++8Rb3N+Hd
/EWLxYb4k9wz0Di1ecbc+erj6htu0TVQ5LscD3nrrXetcn+TfWUSL+P/eMfe
gaQtbF28dJm2fVSuMWU1za6qptElyM25BqUz0n8DyxsHK9/sr4ynNu9USrG+
Jbpm9N5779X7DQjGWjlreOAp5zbjS+OpzZ9Rxt8ksvrc6+feNbZtLZGNwfPn
ACizeKLjcurYmDdfheoNhl27P2e5NsfB+lO7f0KeFb1HRz/ZPX+uG9xvhFV9
7X9bT/D6668rG3iu3+4xRecliYn+tY01bpbD2jMXZoN5A/7v9mj7n/3sZ7qP
OWSbm43mrKzDtz6Jll900UXd+opji3LO7i0h6p155plhW575oP6YMWO6rSdl
bSzP09LWeMrfIYj2W9QPaxby/RCvbfyGu/WrnGMJObcTkmvFs3WuMlsv3Ghy
x8w41W35eKt7/08fuCU3LVOukrd17neQW7L0Vi3f8vEnbvYF83RNE3wrF1b9
6q0u95v/ftuddeElbvZFOV14sZs998da92zR6H2nBHMLtS1uRE2LW3zjzd18
jZ5yiLLswGNO1M/s+43w9LgZp2lsMF/XwtbxbEGT2Gh2z7+8Tustv3uVtJvu
qoSNnVMOdcvkM+WPP/2s1uOZhJjOT3Qoi5fftdL9459O4r04iHXepaoV96x2
r2/o0n2rH3hY25ZXN7gq6aN4psYl0tmg35LJknOpPxpKPOW6z/OwsIbfdv5a
KRjBPRLWxETP33ye8vtnHQv38o1FsJS1qLQfbjyFm3Yc8NLutdgxWs5toj/I
75kPjbJweyIOrlP2rBPrV/GPv2g9880aJcQY2vhjTGZdqTGVmE455ZRufx+Q
/JZ7jOS1rJkyxrIxf0AZ9Xmuwzb+PgA5J+U8ZwCP7W9jsXEPDT8WB6y2GNhs
HvXKK690+RtrFMwWr6zlx4f5IaaoH+YS8EOcUT5zHBZDyNOkcEFyrbgwIia8
qBCmjqxudFMOP9o98cyz7p//ct308V+3uiee/rk74PCjXFWOa1X1ra5MWPXG
ho096ufr7DkXBm2Ei+XiZ2R1k5vai68Vd93rWsdPciMlLmKL8RwB62Aln64S
xsE5bKy48x6t37P9PbpfeVjTGLC4vkXY2qJtCsX4x//5QOM5a85F2hdlmXpX
mal1MVgq156E5PXDjaVRntoYtVTiXIaRzM1x34lzjXWm3I9HvLdyG4PafWH4
yloA+5tGMAQusB6TdVfsow6fozzFBnUZB1LHuGAx8Z4y9tn8YKHYC9UbDLt2
fIxVv/CFL2j8MNLiJnfEBv3C80L0E3mjrb+09VX9+R74DrimrV27Vv3iP7+O
HRvjdvoxegy2PoP+/vznP69rYeENfyMKcY+L6wMx0gfYh10/+clP1BbsIheE
b3A9Wk7+TTnzGOSZvGeNLfuYY7jqqquUt7YO1taI3n777Xp9xjecp29ow5iF
69lZZ52lfrBBOTaZpx87dqyK9/l+WHeFH2InXmJEsJq4u/U71yrmSpJpFxOm
VqZrXJloRLrOjcg2aG42+4KLVCfMPE24JvySfWXpWlchjOE5KbhWIRoh3Nkj
2+j2EH7tKe1GSA7KOLlM2pTBw0yd5nixal6D9mVSVtSX2IGjI8VXucRUyXME
ongmUJW851kt9o8QO9Q/XnLrbe0bg3Lap6S++ItX16t/YiHePTMNbs/qJs2V
R8o1YaTErJ+JR+IamaoJ2qbkepOS645cf+KJpPZbqbnUXw0VniLOAVufaYyM
ijL7myP2e+W9PXcEC6LMOv/88/XcsbawmrX18JQ1g8YlciTac+4RQzQeW7Nt
fxe0WNz59QbDrtW1dZQcQ/Rawit2rK+41vBKXVsj2d/vAJv0p7G7mA1bJ4xv
y2HzY7Zni4gnuv6dNnzHfF/4ow/4brj2cd2gHXUppx7lsJmYOH78wWHqUMY+
4zP7outGscV+bFAXP8RjNq2/KCdWK7PfGeK6RczEhh/6B/8Wu13vkD2/0O07
hQvwgflW4QXMgFswZKRoRCrHVtiS48vIZFb3K98kZ+NZKeWwlO0p+0akaoP6
mYCF2NE2okq4lOOi8jC1zZdyMdeWVz6X5fZXGs9U6YD/8h7OYaNMbdQE7XPS
WFOBD+WhXC80D08HcZQRazJ3jNTXeOvCmNlfIfWqcv0ynFka5amNUUstu5fP
uWB/NwrxnjJjoNXn3OW3zblp43dEPgE3yUvsb6jac1T2rJHZsvsh+I3atljs
vI/uKxRztN5g2DWZHc5TjtWO03xYX8EXOJbfF/3tf3v2KMqmQjGZT7u+5e/H
DvyDoXZvHRbRxv52KfWI2a6JyOrY9087s8Fn7NoxR/fhC75h034bdk2zv01m
z1fhw55dpZ09D2u2qEdfG7ctPouB+NmXH7s9h9Wj/+VcU2mumnJVwqpK4Ud5
MuPKEhnlSij5XJ6Q/clAyjSpx/sKUVkiXaBNWttUJFKuUl6r5DUWaUu7Yr4q
crZpUxVPBDGGSmo5Niu0fTpsOzIRxI/tbT5NaT1G4inkF+5rvFpPYhQ/6g//
8XjJOTRQDTWe5p/b/C7R9piT3471kIzF8udguSdOHcsZS32Mg91X/emnYsKO
PYfZmz/qbO86w3673hmDonFaHcsF+yKLrVC73q63Ud+2VtbaFPNRzE9v+4r2
iTE1xywYUpVjEdwxKZuUb0nlmakqZFtQx3hbmbNBuTLR2NSNib34irYxnuXF
2o2r8Vz7eKqHz6oebZLb4k1EY93mVxke9TsEzqeBaijzdEd4YGM05ohtDpb3
jNfIS+z/+vDy+szVg1WJbtwMywqxbXttCrbr6asq3ouvYrHGi/jN52FRv4kC
ce4cHDUZT22MujPI5mEZf0WfWbW/UW1jwlLH6eXVTbHYzulrmCo2gD7aGXmK
7BlG5rdsDtbmFj1Lvby8Pg0ZT20+aKCKzjcNJUXnt4ZqjF5eu7p2lnNzsHjq
5eXltavL89TLy8trcGQ8tbUvu4I47lLHsCvL97/XzqpdkadeXl6DJ3993CbP
U6+hKH+Oeg1HeZ56eXltT/761jcZT/vzvJ+Xl5eXV095nnp59S57zt/La3sy
nvLq5eXl5bVD2u3/ARCMOhs=
"], {{0, 53}, {340, 0}}, {0, 255},
ColorFunction->RGBColor],
BoxForm`ImageTag["Byte", ColorSpace -> "RGB", Interleaving -> True],
Selectable->False],
BaseStyle->"ImageGraphics",
ImageSizeRaw->{340, 53},
PlotRange->{{0, 340}, {0, 53}}]\)*);

CloudDialogImage["SigninButton","Default"] = imgr["SigninButton-Default.9.png"](*\!\(\*
GraphicsBox[
TagBox[RasterBox[CompressedData["
1:eJy1171PE3Ecx/ETqjEWNcYE+9wedfB6VwZJnNSosGhigNYHJEFsDUYXTcDE
8Ae4GBYXdfMvcDMUWvqASmkr1FagyqB/giOGRPPxcwZi652L6febvJtrv8Pr
2l+XU5MPYnc6FEWZ2s+XWOLR+cnJxHTcwTcXph9OJDt5EdzJ/LANA47yPzWN
k91nG2xLoG/sJTv1l+084HAgEdaQGRjC58Gxtle/PIqZvtNQuw6Z6N0me+q2
R8Wn0ElsuPS21/AY2IyexdeLI5jrHzTRepO9+fpoGOvdmmgNtQ9fhsdN9Kf5
P9mxt6vdJ7B2TJPNFUFjaAw7579rb1Vo17kXzc0zsLHL3NV4X6Lx3NeHb1rs
Eu0P3ItGe83GLro0rLojsnkNfIxZ7SXaK9yLRrtuY791a6h4IrL5DNRs7EWP
hpI3IpvfQDVutQu0l726bP4oVuPjFjvv1VD06bIFaF+x2jnaS9yL9g8769Pw
zq/LFoxipdX+fZ2h/YZ70Wi/t/neab+GxYAuWyiKylWrPU+7wL1otMs29lwg
gnzQEK2g9qJ87ZbFTtHOcS9ZnnbJxp6lneVeshztZTs7SDtkiJbroX3dYn+f
DepY4F6yLO2i1d5O0c5wL9mCvV195j2OtGqIlgn3onIjaZo/muyJSwePYF7Y
TvdE8eRMv2m+Uv5MZ4eyB+ech/HYFcJz/gYv2txTTxj3fCq69u4z7QGldczH
xBGWYjVWb3MlNsPCNs+CuwPBWp5DfwG437Of
"], {{0, 31}, {31, 0}}, {0, 255},
ColorFunction->RGBColor],
BoxForm`ImageTag["Byte", ColorSpace -> "RGB", Interleaving -> True],
Selectable->False],
BaseStyle->"ImageGraphics",
ImageSizeRaw->{31, 31},
PlotRange->{{0, 31}, {0, 31}}]\)*);
                    
CloudDialogImage["SigninButton","Hover"] = imgr["SigninButton-Hover.9.png"](*\!\(\*
GraphicsBox[
TagBox[RasterBox[CompressedData["
1:eJy110lLW2EUxvHrUClarBRcitGS4UakEjMIQiW2UFxIwYIErEgCsW6soIL4
GSoIrlyZT+DaZapxiNYMV41t4pAqIq5cKoLl+LwSaTRnJfe88A83eRY/Mixy
m0JjfcPlmqZNvMRDX3DKPz4enP5SiSfd05PhUAUuGgupF004hKM9p6JTg76j
DLoS6BJFkPeJXVNd+YLCeivFPgfodCBserlAiOY6/dRc+1qhI0X2xDeLlU7f
vacTh8/89A466+yhi/4gRXv7FWoU2dmopZX+2jyinbR10fngiEL/qd9Jwb7J
Wd2Uxy6a3Ut5fP6F7//Bvtq3tdOx3SOb7qP811J7D/YhdtFgHzH2DuwcdtFg
HzK2ATuLXTTYB4ydsrnot90tm+6lLGMnYGewiwb7D2Nv212063DL5vTS/mCp
vQXbwC4a7Axjb8JOYxcN9h5jx2GnsIsGe5exN2AnsYtWat9fr8NOYBcN9g7z
vldh/8IuGuw0Y8ccLtrS3bK18PYK7E3sosFOMfYy7Dh20WAnGfsn7A3sosFO
MHYU9jp20Xj7Wtlr2EXj7Zuoo51WsYsGe7vUTi40OSmGXTTYxtD9/9TbIjvc
W1dPy9K200Oz/k/KXNT+n4pyrYw+1L6hHw1WUp9BpLnF1OYtOo1abPSqqkrZ
H7XHR90mBtASSiPD5OJoBr1l7gUfDgn26D70DrNzC5w=
"], {{0, 31}, {31, 0}}, {0, 255},
ColorFunction->RGBColor],
BoxForm`ImageTag["Byte", ColorSpace -> "RGB", Interleaving -> True],
Selectable->False],
BaseStyle->"ImageGraphics",
ImageSizeRaw->{31, 31},
PlotRange->{{0, 31}, {0, 31}}]\)*);
                    
CloudDialogImage["SigninButton","Pressed"] = imgr["SigninButton-Pressed.9.png"](*\!\(\*
GraphicsBox[
TagBox[RasterBox[CompressedData["
1:eJy1l8tPE1EUhytgIDxCKW2n0+kLWvqYsnYv0YU7gxtWJpBgXKkBEsPSlRsV
FEGCYEFaUIECAmmBtoR3odACxoX/hEsMieb4u1i0MMNGLjf5SOk5+X33npk0
uVVND+vv5Wk0mtYi/KlvfHy9paWx/U4B/qlrb2tuyscHexb2JYdFWJr/IWeV
gAfgKzi8BL6DALh2xl1SeCWP6kq19ERwUI9Uw4VuyUVdoNPspKdiFTVoDaQv
uMqk93PcrbfKdBSyemnE5qVRm+/CsJwQGEbmgMVNvdjLK+yhTW9h0v0c9zf2
/aRdpmm7nxtTYByZo9hDAP4ezKBDdDLpL/aeZN1HYew14vAfE+XASdasQ6YJ
u4+CVg/1wf0cs88+/xP34We4Y45ainOE5c3j7DM2mT7CPYC5d5iqFe4ZuBPo
5c0i3LNwj+G5B+B+qeKeg3vJXsudGNxzVpnGLV4aNON9E5TuiNVHy7Za7iRs
eO4WmcKSl4bEGupScUfhXkEfb5Yw76jFR5OSh96LLnotKN+1BdTXrH7uLGPe
LHva7KGgyUXdRqU7Jvlow+LnzirmzbJnRA+FBBf1GJTuOOqb6OXNGtzxrHsE
7jcq7oTZR0nJz511SSaWPWvy0CjcvXqlewn1LfTyZgNulj0H94dz3Muil1Jm
mTtJeFl2RHDTJ6OT+iodue7jzyuo76CXN1tws+xo1v32tPv43KsmL+2KMne2
RR+x7Hm4xwxO6ldxr6GeRi9vUln3Atzj57jXBS9lTDJ3dkz4zUL2otFNE3on
DeiU7g3U99DLm1242blicIfhfqfi3kR9H728ScPNzhWHexLugJrbCLcgcyct
wI3suAHuSrgrFO4fW0YPHaCPNxmceRPZcUMN3NU0WGE/6z7aRv0LennD3Mms
e0rdnR7BLA6M6OdMxgC3wUMJPdy6agr+mfnPHHfz7aJySqEvw5E0SMG7qnfT
fKWLwnA/KjUy54Tm38rHFY5uFpZRZ7mFhrG30AUI/sVOQ1o79Wtt9KJcorvF
OirG/Qe+G5rTi10TG0AE7IF9ziTBM+BUuQueLLpETt1DfwNGSYWo
"], {{0, 31}, {31, 0}}, {0, 255},
ColorFunction->RGBColor],
BoxForm`ImageTag["Byte", ColorSpace -> "RGB", Interleaving -> True],
Selectable->False],
BaseStyle->"ImageGraphics",
ImageSizeRaw->{31, 31},
PlotRange->{{0, 31}, {0, 31}}]\)*);

CloudDialogImage["JoinNowButton","Default"] = imgr["JoinNowButton-Default.9.png"](*\!\(\*
GraphicsBox[
TagBox[RasterBox[CompressedData["
1:eJztl79qwlAUh2MtWPEBHEspODl3T9FZsCDFTdGaQRNN1BhFQRBB6Oh/hfoC
nfsKnUodOvQlOlqEltP7k0hjzVRya4Ye+EKS5eP87rkJ9yytxG+OBEHQTtgl
ntJFVU0ZV8fs4dIoZ9NednNqgpcOFLESfoOlAgyZ8cJYceCNcce4+OEO+Hw+
ikaj1O12aTabOc5wOKRMJkPBYBBSyeLWRFGkdrtNzWbTcVqtFvV6PZrP59Tp
dCBdWtyvkiRRvV7nCnpbLBaQfmJOTPda13Xu7kajscnfXP+te1WpVMgwDK7A
PZ1OXeUul8tUq9W4gtwnk8meW9M0wprzBL2Px2NXuVVVpWq1yhXkbuculUqE
eeMJeh+NRnvuYrFImDeeoHd8W93kVhSFMG88Qe4ucm/uZVkmzDpPkLtd34d0
FwoFwj7jCXIfDAb/btOdz+cJe5wnWPN+v+8m9zvc2OM8Qe427vUB3U/JZPJP
3OZ/7MPizobDYcKs4xvDC/hzuRyc98J3eT0eD4VCIYrFYoQMnCaRSFAkEiG/
3w93RNgtHBOvGQ+MZ8bSYR4Zt4xzm7PgtogjO+fQL4e9j4w=
"], {{0, 31}, {31, 0}}, {0, 255},
ColorFunction->RGBColor],
BoxForm`ImageTag["Byte", ColorSpace -> "RGB", Interleaving -> True],
Selectable->False],
BaseStyle->"ImageGraphics",
ImageSizeRaw->{31, 31},
PlotRange->{{0, 31}, {0, 31}}]\)*);
                
CloudDialogImage["JoinNowButton","Hover"] = imgr["JoinNowButton-Hover.9.png"](*\!\(\*
GraphicsBox[
TagBox[RasterBox[CompressedData["
1:eJzNl7tKK1EUhidHISf4BKlErNKlsj9yIIF0SsAmxQwoFsEDai6T+/0eSKpA
7ldShFP7ClaihYUvYakIynL/EjFmxkZmYRZ8w55pvllr/hlmbyn/9o5+SZJ0
9lsc9uTAn9NTWd1fFye76vmhsiYWm3Nw0YAiUdJ3WKgNwYngVvDAwL2gJ9hZ
cm+YzWZyuVxUr9dpOp0azmAwIK/XS1arFdLjBfeZw+GgWq1G5XLZcCqVCjUa
jbd7qFarkN4suO98Ph+VSiVW0NtsNoP0BTmZu59yuRwVi0V2JpMJzZ//u/sh
nU5ToVBg5St3KpWifD7PCvzj8VjjTiaThLlzAv9oNNK4E4kEZbNZVuAfDoca
dzwep0wmwwr8eu5YLEbIGyfw4xuz7I5Go4S8cQJ/v9/XuCORCCFvnMCv5w6H
w4S8cQJ/r9fTdSNvnMDd7XZXxf22DoVChKxzgrl3Oh1N36qqErLOCXrXcweD
QULWOUHv7XZ7pdyBQICQN04w91arpXH7/X5C3jhB7yvmfsT/GrLOCebebDaX
3U/o+4fcV7IsE7LOCeY+f7+fF9yHdrudkHVON3rH/kD4/ksftWYymchms5Hb
7SbMQFEUQ/F4POR0OsliscD9V/pc2CYeCC4E14Ibg7kUVATbOnvB9yJGPu1D
XwEQI/Os
"], {{0, 31}, {31, 0}}, {0, 255},
ColorFunction->RGBColor],
BoxForm`ImageTag["Byte", ColorSpace -> "RGB", Interleaving -> True],
Selectable->False],
BaseStyle->"ImageGraphics",
ImageSizeRaw->{31, 31},
PlotRange->{{0, 31}, {0, 31}}]\)*);
                
CloudDialogImage["JoinNowButton","Pressed"] = imgr["JoinNowButton-Pressed.9.png"](*\!\(\*
GraphicsBox[
TagBox[RasterBox[CompressedData["
1:eJzNl8mKE0EYxzvGgGQeQowkl0AuAU+5aCSQfd/Xzr4vM4LMO/gOPsGc5xU8
iXPw4Et4HBlQPuvXJJhp4mWsOCn4hU5XUf/+1up+0V3lhk8Mw7h4pn5y5vvX
5+fmZf6p+vPm8t2g61QXz7dwU8MQNYyHsDfOFEvFV8XtEfiu+Kh4ZdM+czqd
4vF4JBwOSzKZ1EYikZB4PC6RSEQCgYC43W5Ex3vaFz6fT4rFopRKJSmXy/8M
+0ChUJBcLifpdNp6jlAohOjNnvY37tfrdWk0Glqp1WrWs+TzeUmlUhKLxRD9
RZ5ste9Y0263tdNsNi19fJrJZCQajco2/jvtW9Z0u13tdDodS79SqUg2m93Z
/d+1ifsh7VarJb1eTzumaQp7V6tVK+bk/Clpkxf9fl87+J29yTfqjXo6JW1y
YjAYaAe/sze9A2163SHt4XCoHWwn5vQZavzUtJkfjUbawe/EnBqnv9NX7drM
j8dj7WA7MafO6OucKXva1jXzk8lEO9iO38l1ehs93W73Y2ozP51OtYPfiTm5
TG/jPLFrMz+bzbSD7cScXP6bNr5ZLBbaQZ+9yWX6Cz3dro1vlsuldubzueV3
8okap7fZtfHNarXSDrazN/lEjR/Q/oFv1uu1drCdnCOfqHF6m0377hG1P5OD
rN1sNlqxa1PjSu/nnvbA7/dbsTl2vLfv51fGn+F0OBzi9Xqtc4a+ix8eCrbt
4PygrnhnCAaD4nK50H5r3B98JlYU14ovihvNfFJ8ULw88C24G3JE7n2H/gY9
6vbm
"], {{0, 31}, {31, 0}}, {0, 255},
ColorFunction->RGBColor],
BoxForm`ImageTag["Byte", ColorSpace -> "RGB", Interleaving -> True],
Selectable->False],
BaseStyle->"ImageGraphics",
ImageSizeRaw->{31, 31},
PlotRange->{{0, 31}, {0, 31}}]\)*);
  
End[]

EndPackage[]
