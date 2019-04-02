RCB_DefaultParameterList <- list(analysis_type               = "P2", 
                                 alpha                       = 0.1,
                                 ResponseVariableColumnName  = "numValue",
                                 FieldIDColumnName           = "fieldId",
                                 TreatmentFactorColumnName   = "factor1",
                                 ReplicateIDColumnName       = "repId",
                                 FactorTypeColumnName        = "experimentalUnitId", # 
                                 SufficientDataThreshold     = 20,         # Minimum number of data required to run models
                                 ResponseVariableShouldBeGT0 = TRUE)       # Should the response be restricted to just postive values

library(jsonlite)
write_json(RCB_DefaultParameterList,
           "/repos/RCB4Cloud/Reference/Data/RCB_DefaultParameterList.json",
           digits=12)
