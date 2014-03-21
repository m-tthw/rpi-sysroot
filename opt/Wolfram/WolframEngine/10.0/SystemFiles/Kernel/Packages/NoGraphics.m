Begin["System`"]

$DisplayFunction
$SoundDisplayFunction

End[]

Begin["System`Private`"]

$DisplayFunction :=
(
  Message[Graphics::terminal]; 
  $DisplayFunction = $SoundDisplayFunction = Identity; 
  #
)&
  
$SoundDisplayFunction :=
(
  Message[Graphics::terminal]; 
  $DisplayFunction = $SoundDisplayFunction = Identity; 
  #
)&
            

End[]

Null
