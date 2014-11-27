Paclet[
	Name -> "Templating",
	Version -> "0.9.32",
	MathematicaVersion -> "10+",
	Description -> "Templating",
	Loading -> Automatic,
	Creator -> "Riccardo Di Virgilio <riccardod@wolfram.com>, Taliesin Beynon <taliesinb@wolfram.com>",
	Extensions -> {
		{
			"Kernel", 
			Context -> {"TemplatingLoader`", "Templating`"}, 
			Symbols -> {
				"System`CombinerFunction", 
				"System`InsertionFunction", 
				"System`StringTemplate", 
				"System`DefaultValue",
				"System`FileTemplate",
				"System`FileTemplateApply", 
				"System`TemplateApply", 
				"System`TemplateObject",
				"System`TemplateIf", 
				"System`TemplateSequence", 
				"System`TemplateSlot", 
				"System`TemplateExpression", 
				"System`TemplateWith", 
				"System`XMLTemplate",
				"System`Pluralize",
				"System`$HTMLExportRules",
				"System`$TemplatePath",
				"Templating`ExportHTML" (* In startup they are using Templating`ExportHTML *)
			}
		},
		{"Resource", Root -> "Resources", Resources -> {"Pluralize", "StaticLoader", "TemplateLoader"}}
	}
]
