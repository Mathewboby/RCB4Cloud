# The models listed below were collected from the work of Christopher Leeds, Steven Johnson,
# Radha Mohanty and Subash Kasyap. Chris Leeds programmed a list of models for DSR.
# Steve Johnson extended to Subash's list of RCB models to include other models.  Radha Mohanty
# added formulae for RCB2, GUBD2 and Split-plot designs as currently (14 May 2019) accepted
# at Bayer (This are standard models with certain terms excluded or set to 0).

# The 20 models are listed and then they are relisted with rough out R and asreml code.

# CRD one-way         BLUE (by location)      [line 38] {author: CL}
# CRD one-way         BLUE (across locations) [line 45] {author: CL}

# CRD two-way crossed BLUE (by location)      [line 50] {author: MT}
# CRD two-way crossed BLUE (across locations) [line 59] {author: MT}

# CRD two-way nested  BLUE (by location)      [line 68] {author: CL}
# CRD two-way nested  BLUE (across location)  [line 77] {authors: CL, MT}

# RCB single   Location  multiple rep  BLUE (P1) (by location)      [line  86] {authors: SK, SJ}
# RCB multiple locations single   rep  BLUE (P2) (across locations) [line  96] {authors: SK, SJ}
# RCB multiple locations multiple reps BLUE (P4) (across locations) [line 106] {authors: SK, SJ}

# RCB multiple locations single   rep  BLUP (P3) (across locations) [line 119] {authors: SK, SJ}
# RCB multiple locations multiple reps BLUP (P5) (across locations) [line 129] {authors: SK, SJ}
######### There are differing opions on the formulae for the models given below. ##########
# RCB2 single   location  multiple reps BLUE (by location)          [line 143] {author: SJ & RM}
# RCB2 multiple locations single   rep  BLUE (across locations)     [line 157] {author: SJ, alternate: RM & GL}
# RCB2 multiple locations multiple reps BLUE (across locations)     [line 174] {author: SJ, alternate: RM & GL}

# Split-plot single   location  multiple reps BLUE (by location)
# Split-plot multiple locations single   rep  BLUE (across locations)
# Split-plot multiple locations multiple reps BLUE (across locations)

# GUBD2 single   location  multiple reps BLUE (by location)
# GUBD2 multiple locations single   rep  BLUE (across locations)
# GUBD2 multiple locations multiple reps BLUE (across locations)


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


# RCB single Location  multiple rep BLUE (P1) (by location)
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
asreml_random_formula <- formula(paste0("~",blocking_factor1_name," + ",
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
asreml_random_formula <- formula(paste0("~",blocking_factor2_name," + ",
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
asreml_random_formula <- formula(paste0("~",blocking_factor1_name," + ",
                                        treatment_factor1_name," + ",
                                        blocking_factor1_name,":",blocking_factor2_name," + ",
                                        blocking_factor1_name,":",treatment_factor1_name))  # TPS blocking_factor1_name = loc name and blocking_factor2_name = replicate
DSR_frml <- formula(paste0(response_variable_name," ~ 1 + ",
                           treatment_factor1_name," + ",
                           blocking_factor1_name))

# RCB2 single location  multiple reps BLUE (by location)
frml <- formula(paste0(response_variable_name, " ~ 1 + ",
                       treatment_factor1_name, " + ",
                       treatment_factor1_name, ":", treatment_factor2_name, " + ",
                       blocking_factor1_name))
asreml_fixed_formula <- formula(paste0(response_variable_name, " ~ 1 + ",
                                       treatment_factor1_name, " + ",
                                       treatment_factor1_name, ":", treatment_factor2_name))
asreml_random_formula <- formula(paste0("~",
                                        blocking_factor1_name))
DSR_frml <- formula(paste0(response_variable_name, " ~ 1 + ",
                       treatment_factor1_name, " + ",
                       treatment_factor1_name, ":", treatment_factor2_name, " + ",
                       blocking_factor1_name))  # Note: treatment_factor1_name/treatment_factor2_name is expanded internally to treatment_factor1_name + treatment_factor1_name:treatment_factor2_name
# RCB2 multiple locations single   rep  BLUE (across location)
frml <- formula(paste0(response_variable_name, " ~ 1 + ",
                       treatment_factor1_name, " + ",
                       treatment_factor1_name, ":", treatment_factor2_name, " + ",
                       blocking_factor1_name))
asreml_fixed_formula <- formula(paste0(response_variable_name, " ~ 1 + ",
                                       treatment_factor1_name, " + ",
                                       treatment_factor1_name, ":", treatment_factor2_name))
asreml_random_formula <- formula(paste0("~",
                                        blocking_factor1_name))
DSR_frml <- formula(paste0(response_variable_name, " ~ 1 + ",
                           treatment_factor1_name, " + ",
                           treatment_factor1_name, ":", treatment_factor2_name, " + ",
                           blocking_factor1_name))
# RCB2 multiple locations multiple reps BLUE (across location)
frml <- formula(paste0(response_variable_name, " ~ 1 + ",
                       treatment_factor1_name, " + ",
                       treatment_factor1_name, ":", treatment_factor2_name, " + ",
                       blocking_factor1_name, " + ",
                       blocking_factor1_name, ":",blocking_factor2_name, " + ",
                       blocking_factor1_name, ":",treatment_factor1_name, " + ",
                       blocking_factor1_name, ":",treatment_factor1_name, ":", treatment_factor2_name))
asreml_fixed_formula <- formula(paste0(response_variable_name, " ~ 1 + ",
                                       treatment_factor1_name, " + ",
                                       treatment_factor1_name, ":", treatment_factor2_name))
asreml_random_formula <- formula(paste0("~",
                                        blocking_factor1_name, " + ",
                                        blocking_factor1_name, ":",blocking_factor2_name, " + ",
                                        blocking_factor1_name, ":",treatment_factor1_name, " + ",
                                        blocking_factor1_name, ":",treatment_factor1_name, ":", treatment_factor2_name))
DSR_frml <- formula(paste0(response_variable_name, " ~ 1 + ",
                           treatment_factor1_name, " + ",
                           treatment_factor1_name, ":", treatment_factor2_name, " + ",
                           blocking_factor1_name, " + ",
                           blocking_factor1_name, ":", blocking_factor2_name, " + ",
                           blocking_factor1_name, ":", treatment_factor1_name, " + ",
                           blocking_factor1_name, ":", treatment_factor1_name, ":", treatment_factor2_name))

# Split-plot single   location  multiple reps BLUE
frml <- formula(paste0(response_variable_name, " ~ 1 + ",
                       blocking_factor1_name, " + ",
                       treatment_factor1_name, " + ",
                       blocking_factor1_name, ":",treatment_factor1_name, " + ",
                       treatment_factor2_name, " + ",
                       treatment_factor1_name, ":",treatment_factor2_name))
asreml_fixed_formula <- formula(pase0(response_variable_name, " ~ 1 + ",
                                      treatment_factor1_name, " + ",
                                      treatment_factor2_name, " + ",
                                      treatment_factor1_name, ":",treatment_factor2_name))
asreml_random_formula <- formula(paste0("~",
                                        blocking_factor1_name, " + ",
                                        blocking_factor1_name, ":",treatment_factor1_name))
DSR_frml <- formula(paste0(response_variable_name, " ~ 1 + ",
                       blocking_factor1_name, " + ",
                       treatment_factor1_name, " + ",
                       blocking_factor1_name, ":",treatment_factor1_name, " + ",
                       treatment_factor2_name, " + ",
                       treatment_factor1_name, ":",treatment_factor2_name))
# Split-plot multiple locations single   rep  BLUE
# Split-plot multiple locations multiple reps BLUE

# GUBD2 single   location  multiple reps BLUE
# GUBD2 multiple locations single   rep  BLUE
# GUBD2 multiple locations multiple reps BLUE

