# RCB_DefaultParameterList.json
{"ResponseVariableColumnName":["numValue"],
 "SetIDColumnName":["TEST_SET"],
 "FieldIDColumnName":["fieldId"],
 "TreatmentFactorColumnName":["factor1"],
 "ReplicateIDColumnName":["repId"],
 "OutlierPValueThreshold":[0.05],
 "IQR1_multiplier":[3],
 "IQR2_multiplier":[2],
 "summary":[false],
 "log":[false],
 "NumberOfIterations":[2],
 "multi_rep_option":["by field"],
 "NoVarianceInY_Threshold":[0.0001],
 "SufficientDataThreshold":[20],
 "ResponseVariableShouldBeGT0":[true]}

# RCB P1 model from SmartModelMaker.R script
asreml_fixed_formula <- formula(paste0(response_variable_name, " ~ 1 + ",
                                       treatment_factor1_name))
asreml_random_formula <- formula(paste0("~ ",blocking_factor1_name))  # TPS blocking_factor1_name = replicate name or rep
DSR_frml <- formula(paste0(response_variable_name," ~ 1 + ",
                           treatment_factor1_name," + ",
                           blocking_factor1_name))

# Input parameters to a model formula making funciton
Experiment type, IE. RCB, ...
Module, IE. DSR, SmartQAQC, RCSM, ...

multi-loc = T/F
multi-rep = T/F

By_Local = T/F

response_variable_name

treatment_factor1_name
treatment_factor2_name
blocking_factor1_name
blocking_factor2_name

