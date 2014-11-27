(* Structured types: manually maintained. Keep this in alphabetic order. *)
<|
	"English" -> <|
		"AIFF" -> <|
			"AutocompleteScore" -> 100
		|>,
		"Boolean" -> <|
			"VerboseName" -> "Boolean", 
			"VerboseNamePlural" -> "Booleans", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"true", "F"},
			"AutocompleteScore" -> 150
		|>, 
		"CachedFile" -> <|
			"VerboseName" -> "cached file",
			"VerboseNamePlural" -> "cached files",
			"VerboseNameArticle" -> "a",
			"Examples" -> {},
			"AutocompleteScore" -> 270
		|>,
		"Color" -> <|
			"VerboseName" -> "color", 
			"VerboseNamePlural" -> "colors", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"Azure", "#ffffff", "RGB(1,0,0)"},
			"AutocompleteScore" -> 230
		|>,
		"ComplexNumber" -> <|
			"VerboseName" -> "complex number", 
			"VerboseNamePlural" -> "complex numbers", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"3I", "-.5i+4", "-i"},
			"AutocompleteScore" -> 250
		|>,
		"Databin" -> <|
			"VerboseName" -> "databin", 
			"VerboseNamePlural" -> "databins", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"nJi3pg", "mRTfO6"},
			"AutocompleteScore" -> 100
		|>,
		"Date" -> <|
			"VerboseName" -> "date", 
			"VerboseNamePlural" -> "dates", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"2010/12/15", "1999/09/09", "10/11/12"},
			"AutocompleteScore" -> 220
		|>,
		"DateTime" -> <|
			"VerboseName" -> "date with time", 
			"VerboseNamePlural" -> "dates with time", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"2010/12/15 13:30:00", "1999/09/09 09:09:09", "10/11/12 21:00:00"},
			"AutocompleteScore" -> 210
		|>,
		"EmailAddress" -> <|
			"VerboseName" -> "e-mail address", 
			"VerboseNamePlural" -> "e-mail addresses", 
			"VerboseNameArticle" -> "an", 
			"Examples" -> {"john.doe@example.com"},
			"AutocompleteScore" -> 50
		|>, 
		"Expression" -> <|
			"VerboseName" -> "expression", 
			"VerboseNamePlural" -> "expressions", 
			"VerboseNameArticle" -> "an", 
			"Examples" -> {"3+2", "Sin[Pi/2]"},
			"AutocompleteScore" -> 400 (* must be above MathExpression and MathFormula, for Held* and Inactive* types *)
		|>, 
		"FileName" -> <|
			"VerboseName" -> "file name", 
			"VerboseNamePlural" -> "file names", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {},
			"AutocompleteScore" -> 160
		|>,
		"GeoCoordinates" -> <|
			"VerboseName" -> "geographic coordinate pair", 
			"VerboseNamePlural" -> "geographic coordinate pairs", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"15N, 33E", "78.3 -34"},
			"AutocompleteScore" -> 200
		|>,
		"HexInteger" -> <|
			"VerboseName" -> "hexadecimal number", 
			"VerboseNamePlural" -> "hexadecimal numbers", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"ff42ca", "00ff"},
			"AutocompleteScore" -> 100
		|>, 
		"Image" -> <|
			"VerboseName" -> "image", 
			"VerboseNamePlural" -> "images", 
			"VerboseNameArticle" -> "an", 
			"Examples" -> {},
			"AutocompleteScore" -> 590 (* must be above MathExpression, MathFormula, Expression, SemanticExpression for the Inactive* cases *)
		|>, 
		"Integer" -> <|
			"VerboseName" -> "integer number", 
			"VerboseNamePlural" -> "integer numbers", 
			"VerboseNameArticle" -> "an", 
			"Examples" -> {"3", "-12e2", "10"},
			"AutocompleteScore" -> 600 (* must be above MathExpression, MathFormula, Expression, SemanticExpression for the Inactive* cases *)
		|>, 
		"Integer128" -> <|
			"VerboseName" -> "128-bit integer", 
			"VerboseNamePlural" -> "128-bit integers", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"3", "-12e2", "10"},
			"AutocompleteScore" -> 100
		|>, 
		"Integer16" -> <|
			"VerboseName" -> "16-bit integer", 
			"VerboseNamePlural" -> "16-bit integers", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"3", "-12e2", "10"},
			"AutocompleteScore" -> 100
		|>, 
		"Integer24" -> <|
			"VerboseName" -> "24-bit integer", 
			"VerboseNamePlural" -> "24-bit integers", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"3", "-12e2", "10"},
			"AutocompleteScore" -> 100
		|>, 
		"Integer32" -> <|
			"VerboseName" -> "32-bit integer", 
			"VerboseNamePlural" -> "32-bit integers", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"3", "-12e2", "10"},
			"AutocompleteScore" -> 100
		|>, 
		"Integer64" -> <|
			"VerboseName" -> "64-bit integer", 
			"VerboseNamePlural" -> "64-bit integers", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"3", "-12e2", "10"},
			"AutocompleteScore" -> 100
		|>, 
		"Integer8" -> <|
			"VerboseName" -> "8-bit integer", 
			"VerboseNamePlural" -> "8-bit integers", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"3", "-12e2", "10"},
			"AutocompleteScore" -> 100
		|>, 
		"IPAddress" -> <|
			"VerboseName" -> "IP address", 
			"VerboseNamePlural" -> "IP addresses", 
			"VerboseNameArticle" -> "an", 
			"Examples" -> {},
			"AutocompleteScore" -> 160
		|>, 
		"IPv4" -> <|
			"VerboseName" -> "IPv4 address", 
			"VerboseNamePlural" -> "IPv4 addresses", 
			"VerboseNameArticle" -> "an", 
			"Examples" -> {}
		|>, 
		"IPv6" -> <|
			"VerboseName" -> "IPv6 address", 
			"VerboseNamePlural" -> "IPv6 addresses", 
			"VerboseNameArticle" -> "an", 
			"Examples" -> {}
		|>, 
		"LaTeX" -> <|
			"VerboseName" -> "LaTeX file", 
			"VerboseNamePlural" -> "LaTeX files", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {},
			"AutocompleteScore" -> 100
		|>,
		"MathML" -> <|
			"VerboseName" -> "MathML file", 
			"VerboseNamePlural" -> "MathML files", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {},
			"AutocompleteScore" -> 1260 (* to cope with the penalty that $ImportFormats get, since "MathML" is an Import format *)
		|>,
		"Number" -> <|
			"VerboseName" -> "number", 
			"VerboseNamePlural" -> "numbers", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"2", "10.324", "3.2e5"},
			"AutocompleteScore" -> 110
		|>, 
		"PhoneNumber" -> <|
			"VerboseName" -> "phone number", 
			"VerboseNamePlural" -> "phone numbers", 
			"VerboseNameArticle" -> "a",
			"Examples" -> {"(202) 456-1111", "+44 020 1234 5678"},
			"AutocompleteScore" -> 320
		|>, 
		"Quantity" -> <|
			"VerboseName" -> "quantity", 
			"VerboseNamePlural" -> "quantities", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"4.5e2 Meters", "10 A", "1.5 Hours"},
			"AutocompleteScore" -> 240
		|>, 
		"Sound" -> <|
			"VerboseName" -> "sound", 
			"VerboseNamePlural" -> "sounds", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {},
			"AutocompleteScore" -> 420
		|>,
		"String" -> <|
			"VerboseName" -> "string", 
			"VerboseNamePlural" -> "strings", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"foo", "b19nj2gf"},
			"AutocompleteScore" -> 1430 (* to cope with the penalty that $ImportFormats get, since "String" is also an Import format *)
		|>,
		"TeX" -> <|
			"VerboseName" -> "TeX file", 
			"VerboseNamePlural" -> "TeX files", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {},
			"AutocompleteScore" -> 110
		|>,
		"TextArea" -> <|
			"VerboseName" -> "string", 
			"VerboseNamePlural" -> "strings", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"1. one /n2. two /n3. three", "Tiger, tiger, burning bright	 
In the forests of the night,	 
What immortal hand or eye	 
Could frame thy fearful symmetry?"},
			"AutocompleteScore" -> 130
		|>,
		"TextLine" -> <|
			"VerboseName" -> "single line of text", 
			"VerboseNamePlural" -> "single lines of text", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"To be, or not to be--that is the question", "Lorem ipsum dolor sit amet"},
			"AutocompleteScore" -> 120
		|>,
		"Time" -> <|
			"VerboseName" -> "time", 
			"VerboseNamePlural" -> "times", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"3:30", "21", "10:11:43"},
			"AutocompleteScore" -> 190
		|>, 
		"UnsignedInteger128" -> <|
			"VerboseName" -> "128-bit unsigned integer", 
			"VerboseNamePlural" -> "128-bit unsigned integers", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"3", "12e2", "10"},
			"AutocompleteScore" -> 100
		|>,
		"UnsignedInteger16" -> <|
			"VerboseName" -> "16-bit unsigned integer", 
			"VerboseNamePlural" -> "16-bit unsigned integers", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"3", "12e2", "10"},
			"AutocompleteScore" -> 100
		|>,
		"UnsignedInteger24" -> <|
			"VerboseName" -> "24-bit unsigned integer", 
			"VerboseNamePlural" -> "24-bit unsigned integers", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"3", "12e2", "10"},
			"AutocompleteScore" -> 100
		|>,
		"UnsignedInteger32" -> <|
			"VerboseName" -> "32-bit unsigned integer", 
			"VerboseNamePlural" -> "32-bit unsigned integers", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"3", "12e2", "10"},
			"AutocompleteScore" -> 100
		|>,
		"UnsignedInteger64" -> <|
			"VerboseName" -> "64-bit unsigned integer", 
			"VerboseNamePlural" -> "64-bit unsigned integers", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"3", "12e2", "10"},
			"AutocompleteScore" -> 100
		|>,
		"UnsignedInteger8" -> <|
			"VerboseName" -> "8-bit unsigned integer", 
			"VerboseNamePlural" -> "8-bit unsigned integers", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"3", "12e2", "10"},
			"AutocompleteScore" -> 100
		|>,
		"UploadedFile" -> <|
			"VerboseName" -> "uploaded file", 
			"VerboseNamePlural" -> "uploaded files", 
			"VerboseNameArticle" -> "an", 
			"Examples" -> {},
			"AutocompleteScore" -> 130
		|>,
		"URL" -> <|
			"VerboseName" -> "URL address", 
			"VerboseNamePlural" -> "URL addresses", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"http://www.wolframalpha.com", "https://www.coursera.org/"},
			"AutocompleteScore" -> 140
		|>, 
		"URLQueryString" -> <|
			"VerboseName" -> "URL query string", 
			"VerboseNamePlural" -> "URL query strings", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"&location=apple+tree&person=newton", "&query=are%20you%20ok?"},
			"AutocompleteScore" -> 120
		|>, 
		"URLString" -> <|
			"VerboseName" -> "URL-encoded string", 
			"VerboseNamePlural" -> "URL-encoded strings", 
			"VerboseNameArticle" -> "a", 
			"Examples" -> {"Espa%C3%B1a", "Gau%C3%9Fstra%C3%9Fe+190"},
			"AutocompleteScore" -> 100
		|>,
		"XLS" -> <|
			"AutocompleteScore" -> 100
		|>,
		"XLSX" -> <|
			"AutocompleteScore" -> 90
		|>,
		"XML" -> <|
			"AutocompleteScore" -> 80
		|>
	|>
|>

