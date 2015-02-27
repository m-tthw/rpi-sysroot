Paclet[
	Name -> "CloudObject",
	Version -> "0.0.2.174",
	MathematicaVersion -> "10.0+",
	Loading -> Automatic,
    Creator -> "Jan Poeschko <jpoeschko@wolfram.com>, Joel Klein <jfklein@wolfram.com>",
	Extensions -> {
		{"Kernel",
			Root->"Kernel",
			Context->{
                "CloudObjectLoader`",
                "CloudObject`"
            },
			Symbols-> {
				"System`$CloudBase",
				"System`$CloudConnected",
				"System`$CloudCreditsAvailable",
				"System`$CloudRootDirectory",
				"System`$CloudSymbolBase",
                "System`$Permissions",
                "System`$PermissionsGroupBase",
				"System`$RegisteredUserName",
                "System`AddUsers",
				"System`APIFunctionGroup",
				"System`AutoRemove",
				"System`CloudBase",
				"System`CloudAccountData",
				"System`CloudConnect",
				"System`CloudDeploy",
				"System`CloudDirectory",
				"System`CloudDisconnect",
				"System`CloudEvaluate",
				"System`CloudExport",
				"System`CloudFunction",
				"System`CloudGet",
				"System`CloudImport",
				"System`CloudObject",
				"System`CloudObjects",
				"System`CloudObjectInformation",
				"System`CloudObjectInformationData",
				"System`CloudPut",
				"System`CloudSave",
				"System`CloudSymbol",
                "System`CreatePermissionsGroup",
				"System`CreateUUID",
				"System`Delayed",
				"System`DocumentDescription",
				"System`DocumentGenerator",
                "System`DocumentGeneratorInformation",
                "System`DocumentGeneratorInformationData",
                "System`DocumentGenerators",
                (*"System`EvaluateScheduledTask",*)
				"System`EvaluationData",
				"System`ExternalBundle",
				"System`ExportForm",
				"System`GeneratedDocumentBinding",
				"System`GeneratedDocumentHistoryLength",
				"System`IconRules",
				"System`IncludeDocumentGeneratorTasks",
				"System`LocalizeDefinitions",
				"System`MailOptions",
                (*"System`NextScheduledTaskTime",*)
				"System`NotificationOptions",
				"System`OutputFormat",
				"System`Permissions",
                "System`PermissionsGroup",
                "System`PermissionsGroups",
                (* monkey patching system symbols like this is incredibly unstable. Remove- and
                   RunScheduledTask were causing major startup over-autoloading problems, 
                   and removing them seemed to fix 283906 *)
                (* "System`RemoveScheduledTask", *)
                "System`RemoveUsers",
				"System`ResponseForm",
                (* "System`RunScheduledTask", *)
				"System`ScheduledTask",
                "System`ScheduledTasks",
                (*"System`ScheduledTaskActiveQ",*)
                "System`ScheduledTaskInformation",
                "System`ScheduledTaskInformationData",
                "System`SetCloudDirectory",
                "System`SetUsers"
                (*"System`StartScheduledTask",*)
                (*"System`StopScheduledTask"*)
			}
		},
		{"Documentation", Language -> "English"},
		{"LibraryLink", Root->"LibraryResources"}
	}
]
