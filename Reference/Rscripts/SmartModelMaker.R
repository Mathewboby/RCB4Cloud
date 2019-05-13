# CRD one-way         BLUE (by location)
frml     <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                           treatment_factor1_name))
asreml_fixed_formula <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                                       treatment_factor1_name))
DSR_frml <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                           treatment_factor1_name))
# CRD one-way         BLUE (across locations)
frml     <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                           treatment_factor1_name))
DSR_frml <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                           treatment_factor1_name))
# CRD two-way crossed BLUE (by location)
frml     <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                           treatment_factor1_name," + ",
                           treatment_factor2_name," + ",
                           treatment_factor1_name,":",treatment_factor2_name))
DSR_frml <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                           treatment_factor1_name," + ",
                           treatment_factor2_name," + ",
                           treatment_factor1_name,":",treatment_factor2_name))
# CRD two-way crossed BLUE (across locations)
frml     <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                           treatment_factor1_name," + ",
                           treatment_factor2_name," + ",
                           treatment_factor1_name,":",treatment_factor2_name))
DSR_frml <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                           treatment_factor1_name," + ",
                           treatment_factor2_name," + ",
                           treatment_factor1_name,":",treatment_factor2_name))
# CRD two-way nested  BLUE (by location)
frml     <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                           treatment_factor1_name," + ",
                           treatment_factor1_name,":",treatment_factor2_name))
# frml <- formula(paste0(repsonse_variable_name,"~ 1 + ",
#                        treatment_factor1_name,"/",treatment_factor2_name)) # alternate R-formula
DSR_frml <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                           treatment_factor1_name," + ",
                           treatment_factor1_name,":",treatment_factor2_name))
# CRD two-way nested  BLUE (across location)
frml     <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                           treatment_factor1_name," + ",
                           treatment_factor1_name,":",treatment_factor2_name))
DSR_frml <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                           treatment_factor1_name," + ",
                           treatment_factor1_name,":",treatment_factor2_name))


# RCB single   Location  multiple rep  BLUE (P1) (by location)
frml     <- formula(paste0(response_variable_name," ~ 1 + ",
                           treatment_factor1_name," + ",
                           blocking_factor1_name))
asreml_fixed_formula <- formula(paste0(response_variable_name," ~ 1 + ",
                                       treatment_factor1_name))
asreml_random_formula <- formula(paste0("~ ",blocking_factor1_name))  # TPS blocking_factor1_name = replicate name or rep
DSR_frml <- formula(paste0(response_variable_name," ~ 1 + ",
                           treatment_factor1_name," + ",
                           blocking_factor1_name))
# RCB multiple locations single   rep  BLUE (P2) (across locations)
frml     <- formula(paste0(response_variable_name," ~ 1 + ",
                           treatment_factor1_name," + ",
                           blocking_factor1_name))
asreml_fixed_formula <- formula(paste0(response_variable_name," ~ 1 + ",
                                       treatment_factor1_name))
asreml_random_formula <- formula(paste0("~ ",blocking_factor2_name))  # TPS blocking_factor1_name = loc name or something like field id
DSR_frml <- formula(paste0(response_variable_name," ~ 1 + ",
                           treatment_factor1_name," + ",
                           blocking_factor1_name))
# RCB multiple locations multiple reps BLUE (P4) (across locations)
frml     <- formula(paste0(response_variable_name," ~ 1 + ",
                           treatment_factor1_name," + ",
                           blocking_factor1_name," + ",
                           blocking_factor2_name))
asreml_fixed_formula <- formula(paste0(response_variable_name," ~ 1 + ",
                                       treatment_factor1_name))
asreml_random_formula <- formula(paste0("~ ",blocking_factor1_name," + ",
                                        blocking_factor1_name,":",blocking_factor2_name," + ",
                                        blocking_factor1_name,":",treatment_factor1_name))  # TPS blocking_factor1_name = loc name and blocking_factor2_name = replicate
DSR_frml <- formula(paste0(response_variable_name," ~ 1 + ",
                           treatment_factor1_name," + ",
                           blocking_factor1_name))
# RCB multiple locations single   rep  BLUP (P3) (across locations)
frml     <- formula(paste0(response_variable_name," ~ 1 + ",
                           treatment_factor1_name," + ",
                           blocking_factor1_name))
asreml_fixed_formula <- formula(paste0(response_variable_name," ~ 1"))
asreml_random_formula <- formula(paste0("~ ",blocking_factor2_name," + ",
                                             treatment_factor1_name))  # TPS blocking_factor1_name = loc name or something like field id
DSR_frml <- formula(paste0(response_variable_name," ~ 1 + ",
                           treatment_factor1_name," + ",
                           blocking_factor1_name))
# RCB multiple locations multiple reps BLUP (P5) (across locations)
frml     <- formula(paste0(response_variable_name," ~ 1 + ",
                           treatment_factor1_name," + ",
                           blocking_factor1_name," + ",
                           blocking_factor2_name))
asreml_fixed_formula <- formula(paste0(response_variable_name," ~ 1"))
asreml_random_formula <- formula(paste0("~ ",blocking_factor1_name," + ",
                                        treatment_factor1_name," + ",
                                        blocking_factor1_name,":",blocking_factor2_name," + ",
                                        blocking_factor1_name,":",treatment_factor1_name))  # TPS blocking_factor1_name = loc name and blocking_factor2_name = replicate
DSR_frml <- formula(paste0(response_variable_name," ~ 1 + ",
                           treatment_factor1_name," + ",
                           blocking_factor1_name))

# RCB2 single   location  multiple reps BLUE
# RCB2 multiple locations single   rep  BLUE
# RCB2 multiple locations multiple reps BLUE

# Split-plot single   location  multiple reps BLUE
# Split-plot multiple locations single   rep  BLUE
# Split-plot multiple locations multiple reps BLUE

# GUBD2 single   location  multiple reps BLUE
# GUBD2 multiple locations single   rep  BLUE
# GUBD2 multiple locations multiple reps BLUE

