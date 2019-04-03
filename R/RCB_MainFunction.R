#####################################################################################################################
# This is the main function for doing RCBD analysis.  It will fit 5 versions of the RCB model.  These are selected by
# the input argument analysis_type, which can one of five strings: "P1", "P2", "P3", "P4" and "P5.  RCB types
# P1 and P2 only diffference in which data are used to indicate the blocking factor. In p1 the blocking factor is
# some sort of replicate ID and in P2 it is some sort of field or location ID.  Models P1, P2 and P4 use BLUE
# estimation which is most people are familiar with from textbooks.  Models P3 and P5 use BLUP estimation which
# give mean estimates closer to the over average of the data than BLUE estimates and are preferred in genetics experiments.
#
# This funciton is written so it can be used in one of two ways.  Either two inputs are given: the data frame
# containing the data and a named list of the remaining parameters; or with ten input arguments: the data frame
# and the nine individual input arguments.  This is done to accomodate different function calling methods in other code.
#
# DataIn                      dataframe: of data to be analyzed. Column names should match those specified
#                                        as the values in the column name variables below.
# ListOfRCBParameters         named list: (default is NULL) giving values for the next nine parameters. The list names should
#                                         match the following argument names.
# analysis_type               string:  specifying the type of analysis to be conducted.
# alpha                       numeric: between 0 and 1 specifying the confidence level
# ResponseVariableColumnName  sting:   giving the column name of the response variable in DataIn
# FieldIDColumnName           sting:   giving the column name of the field ID variable in DataIn
# TreatmentFactorColumnName   sting:   giving the column name of the treatment variable in DataIn
# ReplicateIDColumnName       sting:   giving the column name of the rep ID variable in DataIn
# FactorTypeColumnName        sting:   giving the column name of the factor type variable in DataIn: indicates wheter or not a treatment level is a CONTROL or not.
# SufficientDataThreshold     numeric: giving the lower bound on the number of data values to be analyzed
# ResponseVariableShouldBeGT0 logical: If TRUE, then response variable values <= 0 are treated as
#                                      missing or deactivated data.
# The output is a named list with several components from the fitted model,
# if analysis_type is "P1", "P2" or "P4", the returned list includes LS table, delta table, ANOVA table for fixed effects,
# the variance component table and residual table;
# if analysis_type is "P3" or "P5", returned list includes LS table, variance component table, residual table and BLUP table for factor1

#' This is the main function for doing RCBD analysis.  It will fit 5 versions of the RCB model.  These are selected by
#' the input argument analysis_type, which can one of five strings: "P1", "P2", "P3", "P4" and "P5.  RCB types
#' P1 and P2 only diffference in which data are used to indicate the blocking factor. In p1 the blocking factor is
#' some sort of replicate ID and in P2 it is some sort of field or location ID.  Models P1, P2 and P4 use BLUE
#' estimation which is most people are familiar with from textbooks.  Models P3 and P5 use BLUP estimation which
#' give mean estimates closer to the over average of the data than BLUE etimates and are preferred in genetics experiments.
#'
#' This funciton is written so it can be used in one of two ways.  Either two inputs are given: the data frame
#' containing the data and a named list of the remaining parameters; or with ten input arguments: the data frame
#' and the nine individual input arguments.  This is done to accomodate different function calling methods in other code.
#'
#' @param DataIn                      a dataframe with observation data. Column names should match those specified below in the column name arguments.
#' @param ListOfRCBParameters         a named list giving values for the following nine arguments.
#' @param analysis_type               a string specifying the type of analysis to be conducted.
#' @param alpha                       a number between 0 and 1 specifying the confidence level and comparison significance
#' @param ResponseVariableColumnName  a sting giving the column name of the response variable in DataIn
#' @param FieldIDColumnName           a sting giving the column name of the field ID variable in DataIn
#' @param TreatmentFactorColumnName   a sting giving the column name of the treatment variable in DataIn
#' @param ReplicateIDColumnName       a sting giving the column name of the rep ID variable in DataIn
#' @param FactorTypeColumnName        a sting giving the column name of the factor type variable in DataIn: indicates wheter or not a treatment level is a CONTROL or not.
#' @param SufficientDataThreshold     a number giving the lower bound on the number of data values to be analyzed
#' @param ResponseVariableShouldBeGT0 a logical. If TRUE, then response variable values <= 0 are treated as missing or deactivated data.
#' @return A list with several components for the fit model, if analysis_type is "P1", "P2" or "P4", returned list includes delta table, LS table, ANOVA table for fixed effects, variance component table and residual table; if analysis_type is "P3" or "P5", returned list includes LS table, variance component table, residual table and BLUP table for factor1
#'
#' @importFrom asreml asreml
#' @export

# Old function name was 'R.ASReml_RCB_Return'
RCB_ModelFittingFunction <- function(DataIn,
                                ListOfRCBParameters         = NULL,
                                analysis_type               = "P2",
                                alpha                       = 0.1,
                                ResponseVariableColumnName  = "numValue",
                                FieldIDColumnName           = "fieldId",
                                TreatmentFactorColumnName   = "factor1",
                                ReplicateIDColumnName       = "repId",
                                FactorTypeColumnName        = "experimentalUnitId",
                                SufficientDataThreshold     = 20,
                                ResponseVariableShouldBeGT0 = TRUE){

  #
  if(exists("RowID",where=DataIn)==FALSE){
    DataIn$RowID <- 1:nrow(DataIn)  # Add a row index for later merging and reordering.
  }

  # Create an intial list of RCB parameters set to their corresponding defaults.
  # Call this named list, ListOfIndividualParameters.
  ListOfIndividualParameters <- list(analysis_type               = analysis_type,
                                     alpha                       = alpha,
                                     ResponseVariableColumnName  = ResponseVariableColumnName,
                                     FieldIDColumnName           = FieldIDColumnName,
                                     TreatmentFactorColumnName   = TreatmentFactorColumnName,
                                     ReplicateIDColumnName       = ReplicateIDColumnName,
                                     FactorTypeColumnName        = FactorTypeColumnName,
                                     SufficientDataThreshold     = SufficientDataThreshold,
                                     ResponseVariableShouldBeGT0 = ResponseVariableShouldBeGT0)

  # Either read in the parameters from a file or use the individual inputs from above.

  if(is.null(ListOfRCBParameters)==TRUE){ # A user supplied list of parameters was not given. Therefore use the default individual inputs from above which may have been over written by the user.
    ListOfRCBParameters <- ListOfIndividualParameters
  }

  # Check for missing parameters and add them, if needed, with default or individual input values.
  nloip <- names(ListOfIndividualParameters)
  CheckNames <- names(ListOfIndividualParameters) %in% names(ListOfRCBParameters) # This is TRUE for each name in names(ListOfIndividualParameters) which is also in names(ListOfSmartQAQCParameters)
  if(any(CheckNames==FALSE)){
    wcn <- which(CheckNames==FALSE) # Get the indicies of the missing parameters
    if(length(wcn)>0){
      for(jj in wcn){
        ListOfRCBParameters[[ nloip[jj] ]] <- ListOfIndividualParameters[[ nloip[jj] ]]
      }
    }
  }
  # Check for missing parameter values.  If there are some,
  # replace them with the individual input default values.
  Check4Missing <- sapply(ListOfRCBParameters,function(zx){is.null(zx) | is.na(zx) | is.nan(zx)})
  if(any(Check4Missing)==TRUE){
    wcm  <- which(Check4Missing==TRUE) # Get the indicies of the missing parameters values
    if(length(wcm)>0){
      for(kk in wcm){ # Loop over the indices of the missing parameters
        ListOfRCBParameters[[kk]] <- ListOfIndividualParameters[[kk]] # Replace the missing value with the individual default value.
      }
    }
  }

  # Set values of the individual parameters in the code below.
  # This is where the inputs are actually made available to the DSR code.
  analysis_type               <- ListOfRCBParameters$analysis_type
  alpha                       <- ListOfRCBParameters$alpha
  ResponseVariableColumnName  <- ListOfRCBParameters$ResponseVariableColumnName
  FieldIDColumnName           <- ListOfRCBParameters$FieldIDColumnName
  TreatmentFactorColumnName   <- ListOfRCBParameters$TreatmentFactorColumnName
  ReplicateIDColumnName       <- ListOfRCBParameters$ReplicateIDColumnName
  FactorTypeColumnName        <- ListOfRCBParameters$FactorTypeColumnName
  SufficientDataThreshold     <- ListOfRCBParameters$SufficientDataThreshold
  ResponseVariableShouldBeGT0 <- ListOfRCBParameters$ResponseVariableShouldBeGT0

  # Check for a misspelled field id column name.
  if(exists("FieldId",where=DataIn)){ # should be "fieldId"
    DataIn$fieldId <- DataIn$FieldId
    DataIn$FieldId <- NULL
  }
  #
  # Check to see if data cleaning algorithms were run on the input data prior to this routine's call.
  #
  if(exists("IS_DEACTIVATED",where=DataIn)==FALSE){ # Data quality checking algorithms like DSR and smartQAQC create this column.  If it is missing, they were not run.
    DataIn$IS_DEACTIVATED     <- FALSE  # Create this missing column and set it equal to FALSE indicating all the data are acceptable
    DataIn$DeactivationReason <- "None" # There is no rason to exclude data.
    if(ResponseVariableShouldBeGT0==TRUE){ # In the case IS_DEACTIVTED does not exist, so check the response for negative or zero values if required.
      wLE0 <- which(DataIn[,ResponseVariableColumnName]<=0)
      if(length(wLE0)>0){
        DataIn$IS_DEACTIVATED[wLE0] <- TRUE # Change the deactiviation status of these rows
        DataIn$DeactivationReason   <- "RCB_Data<=0"
      }
    }
  }
  # The following named list is used to make all of the variables available to several functions as well as
  # to the Global environment (see lines 99-101 below)
  data_fields <- list(
      CROP_OBSRVTN_DETAIL_ID = ResponseVariableColumnName,
      FIELD_ID               = FieldIDColumnName,
      FACTOR_1               = TreatmentFactorColumnName,
      REP_ID                 = ReplicateIDColumnName,
      Factortype_name        = FactorTypeColumnName)

  if(exists("IS_DEACTIVATED",where=DataIn)){
    data <- DataIn[DataIn$IS_DEACTIVATED==FALSE,]
  }else{
    data <- DataIn
  }
  if(nrow(data)<SufficientDataThreshold){ # This is currently a fixed limit determined by commitee.  It should be based on the number of treatment levels and blocks
    txt1 <- "Error: there are too few data (n<SufficientDataThreshold) to run an RCB model. Data frame has n = "
    txt2 <- paste0(txt1,nrow(data))
    txt3 <- paste0(txt2," rows and SufficientDataThreshold = ")
    txt4 <- paste0(txt3,SufficientDataThreshold)
    message(txt4)
    return(DataIn)
  }

  # Initialize output variable
  Out_return <- NULL

  # Sets the ASReml license path if it is not set already
  setLicense()

  # Check inputs
  checkAnalysisType(analysis_type)
  checkData(data, data_fields)

  # Add the data fields to the global environment
  lapply(seq_along(data_fields),
         function(x) assign(names(data_fields)[[x]], value = data_fields[[x]], envir = .GlobalEnv))

  # Manipulate data
  data <- reformatData(data, data_fields)

  # Set fixed and random effects from analysis type
  setFixedRandomEffects(analysis_type)

  # for timing
  time.scale <- ifelse(nrow(data) < 4000, "secs", "mins")
  start      <- Sys.time()
  message(paste("Running ASReml using analysis type:", analysis_type), appendLF = TRUE )


  # Run R-ASReml, capture output
  RCB_asr  <-  asreml::asreml(
                      fixed     = fixed_formula,
                      random    = random_formula,
                      tolerance = 1E-10,
                      data      = data,
                      predict   = list(classify    = paste(FACTOR_1),
                                        present    = list(),
                                        ignore     = character(0),
                                        except     = character(0),
                                        only       = character(0),
                                        associate  = list(),
                                        margin     = FALSE,
                                        average    = list(),
                                        as.average = list(),
                                        vcov       = FALSE,
                                        sed        = list(dotstring = T),
                                        parallel   = FALSE,
                                        inrandom   = TRUE,
                                        exrandom   = logical(0),
                                        aliased    = FALSE,
                                        estimable  = FALSE,
                                        xform      = list()),
                      workspace  = 1000 * 1e6 / 8, # approximately 125 million bytes
                      pworkspace = 1000 * 1e6 / 8, # approximately 125 million bytes
                      trace      = FALSE)


  message("finished in ", round(difftime(Sys.time(), start, units = time.scale[1]), digits = 2), " ", time.scale)
  message("Creating output files...", appendLF = TRUE)

  # Out_return$console$modeling <- RCB_asr

  if (analysis_type %in% c('P3','P5') ) {
  # LS Means
  Out_return$LSM_TABLE <- lsmAnalysis_r(RCB_asr, data)

  ## ANOVA table
  Out_return$var_analysis <- ANOVA_output_r(RCB_asr)

  ## add residual tables
  Out_return$resid <- resid_table(RCB_asr,data)
  colnames(Out_return$resid) <- c('residuals',FIELD_ID,REP_ID)

  ## add blup table for random effects
  Out_return$BLUP <- as.matrix(blup_table(RCB_asr,analysis_type))
  colnames(Out_return$BLUP) <- c('BLUP for random effects')

  }

  if (analysis_type %in% c('P1','P2','P4') ) {

  # to use wald.asreml() only once to save computational time
  LSM_ALL <- lsmAnalysis(RCB_asr, data, alpha=alpha)

  # basic LSM table
  Out_return$LSM_TABLE <- LSM_ALL[[1]]
  # Degrees of freedom
  degrees_freedom = LSM_ALL[[2]]  ## a vector including all the fixed effect


  del=deltaAnalysis(RCB_asr,alpha=alpha, degrees_freedom[2])
  # Deltas and p-values
  Out_return$Deltas <- del[[1]]
  # Mean Separation Grouping
  mean_sep_group <- del[[2]]

  ## combine LSM_TABLE and MSG
  Out_return$LSM_TABLE <- cbind(Out_return$LSM_TABLE,mean_sep_group)

  ## sort by descending order
  Out_return$LSM_TABLE <- Out_return$LSM_TABLE[order(Out_return$LSM_TABLE$Yield,decreasing = TRUE),]


  ## ANOVA table
  Out_return$aov <- LSM_ALL[[3]]
  Out_return$varcomp <- LSM_ALL[[4]]

  ## add residual tables
  Out_return$resid <- resid_table(RCB_asr,data)
  colnames(Out_return$resid) <- c('residuals',FIELD_ID,REP_ID)

 }

  return(Out_return)
}
