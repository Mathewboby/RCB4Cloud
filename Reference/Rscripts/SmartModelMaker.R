# The models listed below were collected from the work of Christopher Leeds, Steven Johnson,
# Radha Mohanty and Subash Kashyap. Chris Leeds programmed a list of models for DSR.
# Steve Johnson extended to Subash's list of RCB models to include other models.  Radha Mohanty
# added formulae for RCB2, GUBD2 and Split-plot designs as currently (14 May 2019) accepted
# at Bayer (These are standard models with certain terms excluded or set to 0).  This file contains full standard versions of the models.

# The 20 models are listed and then they are relisted with rough out R and asreml code.

# CRD one-way         BLUE (by location)      [line 40] {author: CL}
# CRD one-way         BLUE (across locations) [line 49] {author: CL}

# CRD two-way crossed BLUE (by location)      [line 58] {author: MT}
# CRD two-way crossed BLUE (across locations) [line 71] {author: MT}

# CRD two-way nested  BLUE (by location)      [line 84] {author: CL}
# CRD two-way nested  BLUE (across location)  [line 97] {authors: CL, MT}

# RCB single   Location  multiple rep  BLUE (P1) (by location)      [line 108] {authors: SK, SJ}
# RCB multiple locations single   rep  BLUE (P2) (across locations) [line 119] {authors: SK, SJ}
# RCB multiple locations multiple reps BLUE (P4) (across locations) [line 130] {authors: SK, SJ}

# RCB multiple locations single   rep  BLUP (P3) (across locations) [line 143] {authors: SK, SJ}
# RCB multiple locations multiple reps BLUP (P5) (across locations) [line 154] {authors: SK, SJ}

######### There are differing opions on the formulae for the models given below. ##########

# RCB2 single   location  multiple reps BLUE (by location)          [line 167] {author: SJ & RM}
# RCB2 multiple locations single   rep  BLUE (across locations)     [line 181] {author: SJ, alternate: RM & GL}
# RCB2 multiple locations multiple reps BLUE (across locations)     [line 195] {author: SJ, alternate: RM & GL}

# Split-plot single   location  multiple reps BLUE (by location)      [line 215] {author: MT}
# Split-plot multiple locations single   rep  BLUE (across locations) [line 233] {author: MT}
# Split-plot multiple locations multiple reps BLUE (across locations) [line 251] {author: MT}

# GUBD2 single   location  multiple reps BLUE (by location)           [line 279] {authors: RM, SJ, MT}
# GUBD2 multiple locations single   rep  BLUE (across locations)      [line 295] {authors: RM, SJ, MT}
# GUBD2 multiple locations multiple reps BLUE (across locations)      [line 311] {authors: RM, SJ, MT}

########################################
# CRD one-way         BLUE (by location)
########################################

asreml_fixed_formula <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                                       treatment_factor1_name))
DSR_formula <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                           treatment_factor1_name))

#############################################
# CRD one-way         BLUE (across locations) {same as by-location because there are no blocking factors}
#############################################

asreml_fixed_formula <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                                       treatment_factor1_name))
DSR_formula <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                           treatment_factor1_name))

########################################
# CRD two-way crossed BLUE (by location)
########################################

asreml_fixed_formula <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                           treatment_factor1_name," + ",
                           treatment_factor2_name," + ",
                           treatment_factor1_name,":",treatment_factor2_name))
DSR_formula <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                           treatment_factor1_name," + ",
                           treatment_factor2_name," + ",
                           treatment_factor1_name,":",treatment_factor2_name))

#############################################
# CRD two-way crossed BLUE (across locations) {same as by-location because there are no blocking factors}
#############################################

asreml_fixed_formula <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                           treatment_factor1_name," + ",
                           treatment_factor2_name," + ",
                           treatment_factor1_name,":",treatment_factor2_name))
DSR_formula <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                           treatment_factor1_name," + ",
                           treatment_factor2_name," + ",
                           treatment_factor1_name,":",treatment_factor2_name))

########################################
# CRD two-way nested  BLUE (by location)
########################################

asreml_fixed_formula <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                           treatment_factor1_name," + ",
                           treatment_factor1_name,":",treatment_factor2_name))
# asreml_fixed_formula <- formula(paste0(repsonse_variable_name,"~ 1 + ",
#                        treatment_factor1_name,"/",treatment_factor2_name)) # alternate R-formula
DSR_formula <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                           treatment_factor1_name," + ",
                           treatment_factor1_name,":",treatment_factor2_name))

############################################
# CRD two-way nested  BLUE (across location) {same as by-location because there are no blocking factors}
############################################

asreml_fixed_formula <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                           treatment_factor1_name," + ",
                           treatment_factor1_name,":",treatment_factor2_name))
DSR_formula <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                           treatment_factor1_name," + ",
                           treatment_factor1_name,":",treatment_factor2_name))

###########################################################
# RCB single Location  multiple rep BLUE (P1) (by location) {blocking_factor1_name = something like repId}
###########################################################

asreml_fixed_formula <- formula(paste0(response_variable_name," ~ 1 + ",
                                       treatment_factor1_name))
asreml_random_formula <- formula(paste0("~ ",blocking_factor1_name))  # TPS blocking_factor1_name = replicate name or rep
DSR_formula <- formula(paste0(response_variable_name," ~ 1 + ",
                           treatment_factor1_name," + ",
                           blocking_factor1_name))

###################################################################
# RCB multiple locations single   rep  BLUE (P2) (across locations) {blocking_factor1_name = something like locId}
###################################################################

asreml_fixed_formula <- formula(paste0(response_variable_name," ~ 1 + ",
                                       treatment_factor1_name))
asreml_random_formula <- formula(paste0("~ ",blocking_factor2_name))  # TPS blocking_factor1_name = loc name or something like field id
DSR_formula <- formula(paste0(response_variable_name," ~ 1 + ",
                           treatment_factor1_name," + ",
                           blocking_factor1_name))

###################################################################
# RCB multiple locations multiple reps BLUE (P4) (across locations) {blocking_factor1_name = something like locId, blocking_factor2_name = something like repId}
###################################################################

asreml_fixed_formula <- formula(paste0(response_variable_name," ~ 1 + ",
                                       treatment_factor1_name))
asreml_random_formula <- formula(paste0("~",blocking_factor1_name," + ",
                                        blocking_factor1_name,":",blocking_factor2_name," + ",
                                        blocking_factor1_name,":",treatment_factor1_name))  # TPS blocking_factor1_name = loc name and blocking_factor2_name = replicate
DSR_formula <- formula(paste0(response_variable_name," ~ 1 + ",
                           treatment_factor1_name," + ",
                           blocking_factor1_name))

###################################################################
# RCB multiple locations single   rep  BLUP (P3) (across locations)
###################################################################

asreml_fixed_formula <- formula(paste0(response_variable_name," ~ 1"))
asreml_random_formula <- formula(paste0("~",blocking_factor2_name," + ",
                                            treatment_factor1_name))  # TPS blocking_factor1_name = loc name or something like field id
DSR_formula <- formula(paste0(response_variable_name," ~ 1 + ",
                           treatment_factor1_name," + ",
                           blocking_factor1_name))

###################################################################
# RCB multiple locations multiple reps BLUP (P5) (across locations)
###################################################################

asreml_fixed_formula <- formula(paste0(response_variable_name," ~ 1"))
asreml_random_formula <- formula(paste0("~",blocking_factor1_name," + ",
                                        treatment_factor1_name," + ",
                                        blocking_factor1_name,":",blocking_factor2_name," + ",
                                        blocking_factor1_name,":",treatment_factor1_name))  # TPS blocking_factor1_name = loc name and blocking_factor2_name = replicate
DSR_formula <- formula(paste0(response_variable_name," ~ 1 + ",
                           treatment_factor1_name," + ",
                           blocking_factor1_name))

########################################################
# RCB2 single location  multiple reps BLUE (by location) {blocking_factor1_name = something like repId}
########################################################

asreml_fixed_formula <- formula(paste0(response_variable_name, " ~ 1 + ",
                                       treatment_factor1_name, " + ",
                                       treatment_factor1_name, ":", treatment_factor2_name))
asreml_random_formula <- formula(paste0("~",
                                        blocking_factor1_name))
DSR_formula <- formula(paste0(response_variable_name, " ~ 1 + ",
                       treatment_factor1_name, " + ",
                       treatment_factor1_name, ":", treatment_factor2_name, " + ",
                       blocking_factor1_name))  # Note: treatment_factor1_name/treatment_factor2_name is expanded internally to treatment_factor1_name + treatment_factor1_name:treatment_factor2_name

##############################################################
# RCB2 multiple locations single   rep  BLUE (across location) {blocking_factor1_name = something like locId}
##############################################################

asreml_fixed_formula <- formula(paste0(response_variable_name, " ~ 1 + ",
                                       treatment_factor1_name, " + ",
                                       treatment_factor1_name, ":", treatment_factor2_name))
asreml_random_formula <- formula(paste0("~",
                                        blocking_factor1_name))
DSR_formula <- formula(paste0(response_variable_name, " ~ 1 + ",
                           treatment_factor1_name, " + ",
                           treatment_factor1_name, ":", treatment_factor2_name, " + ",
                           blocking_factor1_name))

##############################################################
# RCB2 multiple locations multiple reps BLUE (across location)
##############################################################

asreml_fixed_formula <- formula(paste0(response_variable_name, " ~ 1 + ",
                                       treatment_factor1_name, " + ",
                                       treatment_factor1_name, ":", treatment_factor2_name))
asreml_random_formula <- formula(paste0("~",
                                        blocking_factor1_name, " + ",
                                        blocking_factor1_name, ":",blocking_factor2_name, " + ",
                                        blocking_factor1_name, ":",treatment_factor1_name, " + ",
                                        blocking_factor1_name, ":",treatment_factor1_name, ":", treatment_factor2_name))
DSR_formula <- formula(paste0(response_variable_name, " ~ 1 + ",
                           treatment_factor1_name, " + ",
                           treatment_factor1_name, ":", treatment_factor2_name, " + ",
                           blocking_factor1_name, " + ",
                           blocking_factor1_name, ":", blocking_factor2_name, " + ",
                           blocking_factor1_name, ":", treatment_factor1_name, " + ",
                           blocking_factor1_name, ":", treatment_factor1_name, ":", treatment_factor2_name))

##################################################
# Split-plot single   location  multiple reps BLUE (Blocking_factor1_name = something like repId)
##################################################

asreml_fixed_formula <- formula(pase0(response_variable_name, " ~ 1 + ",
                                      treatment_factor1_name, " + ",
                                      treatment_factor2_name, " + ",
                                      treatment_factor1_name, ":",treatment_factor2_name))
asreml_random_formula <- formula(paste0("~",
                                        blocking_factor1_name, " + ",
                                        blocking_factor1_name, ":",treatment_factor1_name))
DSR_formula <- formula(paste0(response_variable_name, " ~ 1 + ",
                       blocking_factor1_name, " + ",
                       treatment_factor1_name, " + ",
                       blocking_factor1_name, ":",treatment_factor1_name, " + ",
                       treatment_factor2_name, " + ",
                       treatment_factor1_name, ":",treatment_factor2_name))

##################################################
# Split-plot multiple locations single   rep  BLUE (Blocking_factor1_name = something like locId)
##################################################

asreml_fixed_formula <- formula(pase0(response_variable_name, " ~ 1 + ",
                                      treatment_factor1_name, " + ",
                                      treatment_factor2_name, " + ",
                                      treatment_factor1_name, ":",treatment_factor2_name))
asreml_random_formula <- formula(paste0("~",
                                        blocking_factor1_name, " + ",
                                        blocking_factor1_name, ":",treatment_factor1_name))
DSR_formula <- formula(paste0(response_variable_name, " ~ 1 + ",
                           blocking_factor1_name, " + ",
                           treatment_factor1_name, " + ",
                           blocking_factor1_name, ":",treatment_factor1_name, " + ",
                           treatment_factor2_name, " + ",
                           treatment_factor1_name, ":",treatment_factor2_name))

##################################################
# Split-plot multiple locations multiple reps BLUE
##################################################

asreml_fixed_formula <- formula(pase0(response_variable_name, " ~ 1 + ",
                                      treatment_factor1_name, " + ",
                                      treatment_factor2_name, " + ",
                                      treatment_factor1_name, ":", treatment_factor2_name))
asreml_random_formula <- formula(paste0("~",
                                        blocking_factor1_name, " + ",
                                        blocking_factor1_name, ":", treatment_factor1_name, " + ",
                                        blocking_factor1_name, ":", blocking_factor2_name, " + ",
                                        blocking_factor1_name, ":", blocking_factor2_name, ":", treatment_factor1_name, " + ",
                                        blocking_factor1_name, ":", treatment_factor2_name, " + ",
                                        blocking_factor1_name, ":", blocking_factor2_name, ":", treatment_factor2_name, " + ",
                                        blocking_factor1_name, ":", treatment_factor1_name, ":", treatment_factor2_name))
DSR_formula <- formula(paste0(response_variable_name, " ~ 1 + ",
                           blocking_factor1_name, " + ",
                           blocking_factor1_name, ":", blocking_factor2_name, " + ",
                           treatment_factor1_name, " + ",
                           blocking_factor1_name, ":", treatment_factor1_name, " + ",
                           blocking_factor1_name, ":", blocking_factor2_name, ":", treatment_factor1_name, " + ",
                           treatment_factor2_name, " + ",
                           treatment_factor1_name, ":", treatment_factor2_name, " + ",
                           blocking_factor1_name, ":", treatment_factor2_name, " + ",
                           blocking_factor1_name, ":", blocking_factor2_name, ":", treatment_factor2_name, " + ",
                           blocking_factor1_name, ":", treatment_factor1_name, ":", treatment_factor2_name))

#############################################
# GUBD2 single   location  multiple reps BLUE {Blocking_factor1_name = something like repId}
#############################################

asreml_fixed_formula <- formula(pase0(response_variable_name, " ~ 1 + ",
                                      treatment_factor1_name, " + ",
                                      treatment_factor1_name, ":",treatment_factor2_name))
asreml_random_formula <- formula(paste0("~",
                                        blocking_factor1_name, " + ",
                                        blocking_factor1_name, ":",treatment_factor1_name))
DSR_formula <- formula(paste0(response_variable_name, " ~ 1 + ",
                           blocking_factor1_name, " + ",
                           treatment_factor1_name, " + ",
                           blocking_factor1_name, ":",treatment_factor1_name, " + ",
                           treatment_factor1_name, ":",treatment_factor2_name))

#############################################
# GUBD2 multiple locations single   rep  BLUE {Blocking_factor1_name = something like locId}
#############################################

asreml_fixed_formula <- formula(pase0(response_variable_name, " ~ 1 + ",
                                      treatment_factor1_name, " + ",
                                      treatment_factor1_name, ":",treatment_factor2_name))
asreml_random_formula <- formula(paste0("~",
                                        blocking_factor1_name, " + ",
                                        blocking_factor1_name, ":",treatment_factor1_name))
DSR_formula <- formula(paste0(response_variable_name, " ~ 1 + ",
                           blocking_factor1_name, " + ",
                           treatment_factor1_name, " + ",
                           blocking_factor1_name, ":",treatment_factor1_name, " + ",
                           treatment_factor1_name, ":",treatment_factor2_name))

#############################################
# GUBD2 multiple locations multiple reps BLUE {blocking_factor1_name is something like locId, blocking_factor2_name is somethings like repId}
#############################################

asreml_fixed_formula <- formula(pase0(response_variable_name, " ~ 1 + ",
                                      treatment_factor1_name, " + ",
                                      treatment_factor1_name, ":", treatment_factor2_name))
asreml_random_formula <- formula(paste0("~",
                                        blocking_factor1_name, " + ",
                                        blocking_factor1_name, ":", blocking_factor2_name, " + ",
                                        blocking_factor1_name, ":", treatment_factor1_name, " + ",
                                        blocking_factor1_name, ":", blocking_factor2_name, ":", treatment_factor1_name, " + ",
                                        blocking_factor1_name, ":", treatment_factor1_name, ":", treatment_factor2_name))
DSR_formula <- formula(paste0(response_variable_name, " ~ 1 + ",
                           blocking_factor1_name, " + ",
                           blocking_factor1_name, ":", blocking_factor2_name, " + ",
                           treatment_factor1_name, " + ",
                           blocking_factor1_name, ":", treatment_factor1_name, " + ",
                           blocking_factor1_name, ":", blocking_factor2_name, ":", treatment_factor1_name, " + ",
                           treatment_factor1_name, ":", treatment_factor2_name, " + ",
                           blocking_factor1_name, ":", treatment_factor1_name, ":", treatment_factor2_name))

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

