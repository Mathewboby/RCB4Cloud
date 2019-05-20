# The models listed below were collected from the work of Christopher Leeds, Steven Johnson,
# Radha Mohanty and Subash Kashyap. Chris Leeds programmed a list of models for DSR.
# Steve Johnson extended to Subash's list of RCB models to include other models.  Radha Mohanty
# added formulae for RCB2, GUBD2 and Split-plot designs as currently (14 May 2019) accepted
# at Bayer (These are standard models with certain terms excluded or set to 0). This file contains the Modified versions of the model as avocated by Grace Liu.

# The 20 models are listed and then they are relisted with rough out R and asreml code.

# CRD one-way         BLUE (by location)      [line 97] {author: CL}
# CRD one-way         BLUE (across locations) [line 110] {author: CL}

# CRD two-way crossed BLUE (by location)      [line 123] {author: MT}
# CRD two-way crossed BLUE (across locations) [line 140] {author: MT}

# CRD two-way nested  BLUE (by location)      [line 158] {author: CL}
# CRD two-way nested  BLUE (across location)  [line 175] {authors: CL, MT}

# RCB single   Location  multiple rep  BLUE (P1) (by location)      [line 192] {authors: SK, SJ}
# RCB multiple locations single   rep  BLUE (P2) (across locations) [line 208] {authors: SK, SJ}
# RCB multiple locations multiple reps BLUE (P4) (across locations) [line 224] {authors: SK, SJ}

# RCB multiple locations single   rep  BLUP (P3) (across locations) [line 242] {authors: SK, SJ}
# RCB multiple locations multiple reps BLUP (P5) (across locations) [line 258] {authors: SK, SJ}

######### There are differing opions on the formulae for the models given below. ##########

# RCB2 single   location  multiple reps BLUE (by location)          [line 276] {author: SJ & RM}
# RCB2 multiple locations single   rep  BLUE (across locations)     [line 295] {author: SJ, alternate: RM & GL}
# RCB2 multiple locations multiple reps BLUE (across locations)     [line 314] {author: SJ, alternate: RM & GL}

# Split-plot single   location  multiple reps BLUE (by location)      [line 339] {author: MT}
# Split-plot multiple locations single   rep  BLUE (across locations) [line 369] {author: MT}
# Split-plot multiple locations multiple reps BLUE (across locations) [line 385] {author: MT}

# GUBD2 single   location  multiple reps BLUE (by location)           [line 419] {authors: RM, SJ, MT}
# GUBD2 multiple locations single   rep  BLUE (across locations)      [line 440] {authors: RM, SJ, MT}
# GUBD2 multiple locations multiple reps BLUE (across locations)      [line 461] {authors: RM, SJ, MT}
SmartModelMaker <- function(named_list_of_parameters=NULL){
  
  # Note single-loc-multi-rep and multi-loc-single-rep models below have the same formuale.  The difference is in the name of the blocking variable.
  # Multi-loc-multi-rep models have different formulae because there is a extra blocking factor in the design.
  possible_models <-  c("CRD1_BLUE",        # Oneway ANOVA with 1 treatment factor (single factor CRD, completely randomized design)
                        "CRD2c_BLUE",       # Twoway ANOVA with 2 crossed treatment factors (traditional 2 factor CRD)
                        "CRD2n_BLUE",       # Twoway ANOVA with 2 nested treatment factors (non traditional 2 factor CRD)
                        "RCB_slmr_BLUE",    # RCB with 1 treatment factor and 1 blocking factor like rep within a location: slmr = single-loc-multi-rep. Treatment means are BLUE.
                        "RCB_mlsr_BLUE",    # RCB with 1 treatment factor and 1 blocking factor like locs with only 1 rep each: mlsr = multi-loc-single-rep. Treatment means are BLUE.
                        "RCB_mlmr_BLUE",    # RCB with 1 treatment factor and 2 blocking factors like loc and rep: mlmr = multi-loc-multi-rep. Treatment means are BLUE.
                        "RCB_mlsr_BLUP",    # RCB with 1 treatment factor and 1 blocking factor like locs with only 1 rep each: mlsr = multi-loc-single-rep. Treatment means are BLUP.
                        "RCB_mlml_BLUP",    # RCB with 1 treatment factor and 2 blocking factors like loc and rep: mlmr = multi-loc-multi-rep. Treatment means are BLUP.
                        "RCB2_slmr_BLUE",   # RCB with 2 nested treatment factors and 1 blocking factor like rep within a location: slmr = single-loc-multi-rep. Treatment means are BLUE.
                        "RCB2_mlsr_BLUE",   # RCB with 2 nested treatment factors and 1 blocking factor like locs with only 1 rep each: mlsr = multi-loc-single-rep. Treatment means are BLUE.
                        "RCB2_mlmr_BLUE",   # RCB with 2 nested treatment factors and 2 blocking factors and 2 blocking factors like loc and rep: mlmr = multi-loc-multi-rep. Treatment means are BLUE.
                        "SPD_slmr_BLUE",    # Split-plot design with 2 cross treatment factors and 2 blocking factors or experiment units:  slmr = single-loc-multi-rep. Treatment means are BLUE.
                        "SPD_mlsr_BLUE",    # Split-plot design with 2 cross treatment factors and 2 blocking factors or experiment units:  mlsr = multi-loc-single-rep. Treatment means are BLUE.
                        "SPD_mlmr_BLUE",    # Split-plot design with 2 cross treatment factors and 3 blocking factors or experiment units:  mlmr = multi-loc-multi-rep. Treatment means are BLUE.
                        "GUBD2_slmr_BLUE",  # Group-UnBalanced-Block-Design with 2 treatment factors, which is essentially a split-plot design with 2 nested treatment factors and 2 blocking factors or experiment units:  slmr = single-loc-multi-rep. Treatment means are BLUE.
                        "GUBD2_mlsr_BLUE",  # Group-UnBalanced-Block-Design with 2 treatment factors, which is essentially a split-plot design with 2 nested treatment factors and 2 blocking factors or experiment units:  mlsr = multi-loc-single-rep. Treatment means are BLUE.
                        "GUBD2_mlmr_BLUE")  # Group-UnBalanced-Block-Design with 2 treatment factors, which is essentially a split-plot design with 2 nested treatment factors and 3 blocking factors or experiment units:  mlmr = multi-loc-multi-rep. Treatment means are BLUE.
  if(is.null(named_list_of_parameters)==TRUE){
    named_list_of_parameters                        <- list(ResponseVariableColumnName  = "NUM_VALUE",           # Name of the column holding the numeric repsonse data in the data frame used in the model
                                                                                    FieldIDColumnName           = "FIELD_NAME",            # Name of the column holding hte field or location IDs in the input data frame
                                                                                    TreatmentFactorColumnName   = "GERMPLASM_ID",            # Name of the column holding the treatment level names in the input data frame
                                                                                    ReplicateIDColumnName       = "BR_REP_ID",              # Name of the column holding the replicated IDs in the input data frame
                                                                                    FactorTypeColumnName        = "PLOT_ID", # Name of the column holding the type of treatment ("CONTROL" or "TREATMENT").  It is often replace with some sort of Plot ID.
                                                                                    analysis_type               = "P2",                 # Can be any of "P1", "P2", "P3", "P4" and "P5".  "P2" is the most commonly used.
                                                                                    alpha                       = 0.1,                  # Alpha level for calculating confidence limits and doing pairwise comparisons
                                                                                    SufficientDataThreshold     = 1,                    # Minimum number of data required to run models
                                                                                    ResponseVariableShouldBeGT0 = TRUE)                 # Should the response be restricted to just postive values
    
    named_list_of_parameters$repsonse_variable_name <- named_list_of_parameters$ResponseVariableColumnName
    named_list_of_parameters$blocking_factor1_name  <- named_list_of_parameters$FieldIDColumnName
    named_list_of_parameters$blocking_factor2_name  <- named_list_of_parameters$ReplicateIDColumnName
    named_list_of_parameters$treatment_factor1_name <- named_list_of_parameters$TreatmentFactorColumnName
    
    named_list_of_parameters$treatment_factor2_name         <- NULL
    named_list_of_parameters$model_type                     <- "RCB_slmr_BLUE"
    named_list_of_parameters$module_name                    <- "ANOVA"
    named_list_of_parameters$estimation_method              <- "BLUE" # or "BLUP"
    named_list_of_parameters$has_multiple_locations         <- FALSE
    named_list_of_parameters$has_multiple_reps_per_location <- TRUE
    named_list_of_parameters$analysis_is_local              <- TRUE
  }
  
  response_variable_name <- named_list_of_parameters$repsonse_variable_name
  blocking_factor1_name  <- named_list_of_parameters$blocking_factor1_name
  blocking_factor1_name  <- named_list_of_parameters$blocking_factor2_name
  treatment_factor1_name <- named_list_of_parameters$treatment_factor1_name
  treatment_factor2_name <- named_list_of_parameters$treatment_factor2_name
  
  if (named_list_of_parameters$analysis_is_local==TRUE){
    # Override any settings for "has_multiple_locations" and "has_multiple_reps_per_location".
    named_list_of_parameters$has_multiple_locations         <- FALSE
    named_list_of_parameters$has_multiple_reps_per_location <- TRUE
  }
  
  ########################################
  # CRD one-way         BLUE (by location)
  ########################################
  if (named_list_of_parameters$model_type        == "CRD1_BLUE" & 
      named_list_of_parameters$estimation_method == "BLUE"){
    
    asreml_fixed_formula <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                                           treatment_factor1_name))   # Needs ",fixed only" added to formula somehow
    asreml_random_formula <- NULL
    DSR_formula <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                                  treatment_factor1_name))
  }
  
  #############################################
  # CRD one-way         BLUE (across locations) {same as by-location because there are no blocking factors}
  #############################################
  if (named_list_of_parameters$model_type        == "CRD1_BLUE" & 
      named_list_of_parameters$estimation_method == "BLUE"){
    
    asreml_fixed_formula <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                                           treatment_factor1_name))   # Needs ",fixed only" added to formula somehow
    asreml_random_formula <- NULL
    DSR_formula <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                                  treatment_factor1_name))
  }
  
  ########################################
  # CRD two-way crossed BLUE (by location)
  ########################################
  if (named_list_of_parameters$model_type        == "CRD2c_BLUE" & 
      named_list_of_parameters$estimation_method == "BLUE"){
    
    asreml_fixed_formula <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                                           treatment_factor1_name," + ",
                                           treatment_factor2_name," + ",
                                           treatment_factor1_name,":",treatment_factor2_name))   # Needs ",fixed only" added to formula somehow
    asreml_random_formula <- NULL
    DSR_formula <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                                  treatment_factor1_name," + ",
                                  treatment_factor2_name," + ",
                                  treatment_factor1_name,":",treatment_factor2_name))
  }
  
  #############################################
  # CRD two-way crossed BLUE (across locations) {same as by-location because there are no blocking factors}
  #############################################
  if (named_list_of_parameters$model_type        == "CRD2c_BLUE" & 
      named_list_of_parameters$estimation_method == "BLUE"){
    
    asreml_fixed_formula <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                                           treatment_factor1_name," + ",
                                           treatment_factor2_name," + ",
                                           treatment_factor1_name,":",treatment_factor2_name))   # Needs ",fixed only" added to formula somehow
    asreml_random_formula <- NULL
    DSR_formula <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                                  treatment_factor1_name," + ",
                                  treatment_factor2_name," + ",
                                  treatment_factor1_name,":",treatment_factor2_name))
  }
  
  
  ########################################
  # CRD two-way nested  BLUE (by location)
  ########################################
  if (named_list_of_parameters$model_type        == "CRD2n_BLUE" & 
      named_list_of_parameters$estimation_method == "BLUE"){
    
    asreml_fixed_formula <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                                           treatment_factor1_name," + ",
                                           treatment_factor1_name,":",treatment_factor2_name))   # Needs ",fixed only" added to formula somehow
    asreml_random_formula <- NULL
    # asreml_fixed_formula <- formula(paste0(repsonse_variable_name,"~ 1 + ",
    #                        treatment_factor1_name,"/",treatment_factor2_name))                 # alternate R-formula
    DSR_formula <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                                  treatment_factor1_name," + ",
                                  treatment_factor1_name,":",treatment_factor2_name))
  }
  
  ############################################
  # CRD two-way nested  BLUE (across location) {same as by-location because there are no blocking factors}
  ############################################
  if (named_list_of_parameters$model_type        == "CRD2n_BLUE" & 
      named_list_of_parameters$estimation_method == "BLUE"){
    
    asreml_fixed_formula <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                                           treatment_factor1_name," + ",
                                           treatment_factor1_name,":",treatment_factor2_name))   # Needs ",fixed only" added to formula somehow
    asreml_random_formula <- NULL
    # asreml_fixed_formula <- formula(paste0(repsonse_variable_name,"~ 1 + ",
    #                        treatment_factor1_name,"/",treatment_factor2_name))                 # alternate R-formula
    DSR_formula <- formula(paste0(repsonse_variable_name,"~ 1 + ",
                                  treatment_factor1_name," + ",
                                  treatment_factor1_name,":",treatment_factor2_name))
  }
  
  ###########################################################
  # RCB single Location  multiple rep BLUE (P1) (by location) {blocking_factor1_name = something like repId}
  ###########################################################
  if (named_list_of_parameters$model_type                     == "RCB_slmr_BLUE" & 
      named_list_of_parameters$estimation_method              == "BLUE" &
      named_list_of_parameters$has_multiple_locations         == FALSE  &
      named_list_of_parameters$has_multiple_reps_per_location == TRUE){
    
    asreml_fixed_formula <- formula(paste0(response_variable_name," ~ 1 + ",
                                           treatment_factor1_name))
    asreml_random_formula <- formula(paste0("~ ",blocking_factor1_name))  # TPS blocking_factor1_name = replicate name or rep
    DSR_formula <- formula(paste0(response_variable_name," ~ 1 + ",
                                  treatment_factor1_name," + ",
                                  blocking_factor1_name))
  }
  
  ###################################################################
  # RCB multiple locations single   rep  BLUE (P2) (across locations) {blocking_factor1_name = something like locId}
  ###################################################################
  if (named_list_of_parameters$model_type                     == "RCB_mlsr_BLUE" & 
      named_list_of_parameters$estimation_method              == "BLUE" &
      named_list_of_parameters$has_multiple_locations         == TRUE   &
      named_list_of_parameters$has_multiple_reps_per_location == FALSE){
    
    asreml_fixed_formula <- formula(paste0(response_variable_name," ~ 1 + ",
                                           treatment_factor1_name))
    asreml_random_formula <- formula(paste0("~ ",blocking_factor2_name))  # TPS blocking_factor1_name = loc name or something like field id
    DSR_formula <- formula(paste0(response_variable_name," ~ 1 + ",
                                  treatment_factor1_name," + ",
                                  blocking_factor1_name)) 
  }
  
  ###################################################################
  # RCB multiple locations multiple reps BLUE (P4) (across locations) {blocking_factor1_name = something like locId, blocking_factor2_name = something like repId}
  ###################################################################
  if (named_list_of_parameters$model_type                     == "RCB_mlmr_BLUE" & 
      named_list_of_parameters$estimation_method              == "BLUE" &
      named_list_of_parameters$has_multiple_locations         == TRUE   &
      named_list_of_parameters$has_multiple_reps_per_location == TRUE){
    
    asreml_fixed_formula <- formula(paste0(response_variable_name," ~ 1 + ",
                                           treatment_factor1_name))
    asreml_random_formula <- formula(paste0("~",blocking_factor1_name," + ",
                                            blocking_factor1_name,":",blocking_factor2_name," + ",
                                            blocking_factor1_name,":",treatment_factor1_name))  # TPS blocking_factor1_name = loc name and blocking_factor2_name = replicate
    DSR_formula <- formula(paste0(response_variable_name," ~ 1 + ",
                                  treatment_factor1_name," + ",
                                  blocking_factor1_name))
  }
  
  ###################################################################
  # RCB multiple locations single   rep  BLUP (P3) (across locations)
  ###################################################################
  if (named_list_of_parameters$model_type                     == "RCB_mlsr_BLUP" & 
      named_list_of_parameters$estimation_method              == "BLUP" &
      named_list_of_parameters$has_multiple_locations         == TRUE   &
      named_list_of_parameters$has_multiple_reps_per_location == FALSE){
    
    asreml_fixed_formula <- formula(paste0(response_variable_name," ~ 1"))
    asreml_random_formula <- formula(paste0("~",blocking_factor2_name," + ",
                                            treatment_factor1_name))  # TPS blocking_factor1_name = loc name or something like field id
    DSR_formula <- formula(paste0(response_variable_name," ~ 1 + ",
                                  treatment_factor1_name," + ",
                                  blocking_factor1_name)) 
  }
  
  ###################################################################
  # RCB multiple locations multiple reps BLUP (P5) (across locations)
  ###################################################################
  if (named_list_of_parameters$model_type                     == "RCB_mlmr_BLUP" & 
      named_list_of_parameters$estimation_method              == "BLUP" &
      named_list_of_parameters$has_multiple_locations         == TRUE   &
      named_list_of_parameters$has_multiple_reps_per_location == TRUE){
    
    asreml_fixed_formula <- formula(paste0(response_variable_name," ~ 1"))
    asreml_random_formula <- formula(paste0("~",blocking_factor1_name," + ",
                                            treatment_factor1_name," + ",
                                            blocking_factor1_name,":",blocking_factor2_name," + ",
                                            blocking_factor1_name,":",treatment_factor1_name))  # TPS blocking_factor1_name = loc name and blocking_factor2_name = replicate
    DSR_formula <- formula(paste0(response_variable_name," ~ 1 + ",
                                  treatment_factor1_name," + ",
                                  blocking_factor1_name)) 
  }
  
  ########################################################
  # RCB2 single location  multiple reps BLUE (by location) {blocking_factor1_name = something like repId}
  ########################################################
  if (named_list_of_parameters$model_type                     == "RCB2_slmr_BLUE" & 
      named_list_of_parameters$estimation_method              == "BLUE" &
      named_list_of_parameters$has_multiple_locations         == FALSE  &
      named_list_of_parameters$has_multiple_reps_per_location == TRUE){
    
    asreml_fixed_formula <- formula(paste0(response_variable_name, " ~ 1 + ",
                                           treatment_factor1_name, " + ",
                                           treatment_factor1_name, ":", treatment_factor2_name))
    asreml_random_formula <- formula(paste0("~",
                                            blocking_factor1_name))
    DSR_formula <- formula(paste0(response_variable_name, " ~ 1 + ",
                                  treatment_factor1_name, " + ",
                                  treatment_factor1_name, ":", treatment_factor2_name, " + ",
                                  blocking_factor1_name))  # Note: treatment_factor1_name/treatment_factor2_name is expanded internally to treatment_factor1_name + treatment_factor1_name:treatment_factor2_name 
  }
  
  ##############################################################
  # RCB2 multiple locations single   rep  BLUE (across location) {blocking_factor1_name = something like locId}
  ##############################################################
  if (named_list_of_parameters$model_type                     == "RCB2_mlsr_BLUE" & 
      named_list_of_parameters$estimation_method              == "BLUE" &
      named_list_of_parameters$has_multiple_locations         == TRUE  &
      named_list_of_parameters$has_multiple_reps_per_location == FALSE){
    
    asreml_fixed_formula <- formula(paste0(response_variable_name, " ~ 1 + ",
                                           treatment_factor1_name, " + ",
                                           treatment_factor1_name, ":", treatment_factor2_name))
    asreml_random_formula <- formula(paste0("~",
                                            blocking_factor1_name))
    DSR_formula <- formula(paste0(response_variable_name, " ~ 1 + ",
                                  treatment_factor1_name, " + ",
                                  treatment_factor1_name, ":", treatment_factor2_name, " + ",
                                  blocking_factor1_name)) 
  }
  
  ##############################################################
  # RCB2 multiple locations multiple reps BLUE (across location) {blocking_factor1_name is something like Location_Id, blocking_factor2_name is somethings like Rep_Id}
  ##############################################################
  if (named_list_of_parameters$model_type                     == "RCB2_mlmr_BLUE" & 
      named_list_of_parameters$estimation_method              == "BLUE" &
      named_list_of_parameters$has_multiple_locations         == TRUE  &
      named_list_of_parameters$has_multiple_reps_per_location == TRUE){
    
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
  }
  
  ##################################################
  # Split-plot single   location  multiple reps BLUE (Blocking_factor1_name = something like repId)
  ##################################################
  if (named_list_of_parameters$model_type                     == "SPD_slmr_BLUE" & 
      named_list_of_parameters$estimation_method              == "BLUE" &
      named_list_of_parameters$has_multiple_locations         == FALSE  &
      named_list_of_parameters$has_multiple_reps_per_location == TRUE){
    
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
  }
  
  ##################################################
  # Split-plot multiple locations single   rep  BLUE (Blocking_factor1_name = something like locId)
  ##################################################
  if (named_list_of_parameters$model_type                     == "SPD_mlsr_BLUE" & 
      named_list_of_parameters$estimation_method              == "BLUE" &
      named_list_of_parameters$has_multiple_locations         == TRUE  &
      named_list_of_parameters$has_multiple_reps_per_location == FALSE){
    
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
  }
  
  ##################################################
  # Split-plot multiple locations multiple reps BLUE {blocking_factor1_name is something like locId, blocking_factor2_name is something like repId}
  ##################################################
  if (named_list_of_parameters$model_type                     == "SPD_mlmr_BLUE" & 
      named_list_of_parameters$estimation_method              == "BLUE" &
      named_list_of_parameters$has_multiple_locations         == TRUE  &
      named_list_of_parameters$has_multiple_reps_per_location == TRUE){
    
    asreml_fixed_formula <- formula(pase0(response_variable_name, " ~ 1 + ",
                                          treatment_factor1_name, " + ",
                                          treatment_factor2_name, " + ",
                                          treatment_factor1_name, ":", treatment_factor2_name))
    asreml_random_formula <- formula(paste0("~",
                                            blocking_factor1_name, " + ",
                                            blocking_factor1_name, ":", treatment_factor1_name, " + ",
                                            blocking_factor1_name, ":", blocking_factor2_name, " + ",
                                            blocking_factor1_name, ":", blocking_factor2_name, ":", treatment_factor1_name, " + ",blocking_factor1_name, ":", treatment_factor2_name, " + ",blocking_factor1_name, ":", blocking_factor2_name, ":", treatment_factor2_name, " + ",
                                            blocking_factor1_name, ":", treatment_factor1_name, ":", treatment_factor2_name))
    DSR_formula <- formula(paste0(response_variable_name, " ~ 1 + ",
                                  blocking_factor1_name, " + ",
                                  blocking_factor1_name, ":", blocking_factor2_name, " + ",
                                  treatment_factor1_name, " + ",
                                  blocking_factor1_name, ":", treatment_factor1_name, " + ",
                                  blocking_factor1_name, ":", blocking_factor2_name, ":", treatment_factor1_name, " + ",
                                  treatment_factor2_name, " + ",
                                  treatment_factor1_name, ":", treatment_factor2_name, " + ",blocking_factor1_name, ":",
                                  blocking_factor1_name, ":", blocking_factor2_name, ":", treatment_factor2_name, " + ",
                                  blocking_factor1_name, ":", treatment_factor1_name, ":", treatment_factor2_name))
    
  }
  
  #############################################
  # GUBD2 single   location  multiple reps BLUE {Blocking_factor1_name = something like repId}
  #############################################
  if (named_list_of_parameters$model_type                     == "GUBD2_slmr_BLUE" & 
      named_list_of_parameters$estimation_method              == "BLUE" &
      named_list_of_parameters$has_multiple_locations         == FALSE  &
      named_list_of_parameters$has_multiple_reps_per_location == TRUE){
    
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
  }
  
  #############################################
  # GUBD2 multiple locations single   rep  BLUE {Blocking_factor1_name = something like locId}
  #############################################
  if (named_list_of_parameters$model_type                     == "GUBD2_mlsr_BLUE" & 
      named_list_of_parameters$estimation_method              == "BLUE" &
      named_list_of_parameters$has_multiple_locations         == TRUE  &
      named_list_of_parameters$has_multiple_reps_per_location == FALSE){
    
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
  }
  
  #############################################
  # GUBD2 multiple locations multiple reps BLUE {blocking_factor1_name is something like locId, blocking_factor2_name is somethings like repId}
  #############################################
  if (named_list_of_parameters$model_type                     == "GUBD2_mlmr_BLUE" & 
      named_list_of_parameters$estimation_method              == "BLUE" &
      named_list_of_parameters$has_multiple_locations         == TRUE  &
      named_list_of_parameters$has_multiple_reps_per_location == TRUE){
    
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
  }
  
  named_list_of_parameters$asreml_fixed_formula  <- asreml_fixed_formula
  named_list_of_parameters$asreml_random_formula <- asreml_random_formula
  named_list_of_parameters$DSR_formula           <- DSR_formula
  
  return(named_list_of_parameters)
  
}
