#####################################################################################################################
# This is the main function for doing RCBD analysis.  It will fit 5 versions of the RCB model.  These are selected by
# the input argument analysisType, which can one of five strings: "P1", "P2", "P3", "P4" and "P5.  RCB types
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
# analysisType               string:  specifying the type of analysis to be conducted.
# alpha                       numeric: between 0 and 1 specifying the confidence level
# value  sting:   giving the column name of the response variable in DataIn
# subSiteId           sting:   giving the column name of the field ID variable in DataIn
# factorLevelId   sting:   giving the column name of the treatment variable in DataIn
# repId       sting:   giving the column name of the rep ID variable in DataIn
# TODO: handle control type indicating wheter or not a treatment level is a CONTROL or not.
# sufficientDataThreshold     numeric: giving the lower bound on the number of data values to be analyzed
# positiveValueCheck logical: If TRUE, then response variable values <= 0 are treated as
#                                      missing or deactivated data.
# The output is a named list with several components from the fitted model,
# if analysisType is "P1", "P2" or "P4", the returned list includes LS table, delta table, ANOVA table for fixed effects,
# the variance component table and residual table;
# if analysisType is "P3" or "P5", returned list includes LS table, variance component table, residual table and BLUP table for factor1

#' This is the main function for doing RCBD analysis.  It will fit 5 versions of the RCB model.  These are selected by
#' the input argument analysisType, which can one of five strings: "P1", "P2", "P3", "P4" and "P5.  RCB types
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
#' @param params.input         a named list giving values for the following nine arguments.
#' @param alpha                       a number between 0 and 1 specifying the confidence level and comparison significance
#' @param value  a sting giving the column name of the response variable in DataIn
#' @param subSiteId           a sting giving the column name of the field ID variable in DataIn
#' @param factorLevelId   a sting giving the column name of the treatment variable in DataIn
#' @param repId       a sting giving the column name of the rep ID variable in DataIn
#' @param locationId
#' @param questionCode
#' @param isDsrDeactivated
#' @param isQaqcDeactivated
#' @param isAnswerDeactivated
#' @param isSetEntryDeactivated
#' @param entryId
#' @param sufficientDataThreshold     a number giving the lower bound on the number of data values to be analyzed
#' @param positiveValueCheck a logical. If TRUE, then response variable values <= 0 are treated as missing or deactivated data.
#' @return A list with several components for the fit model, if analysisType is "P1", "P2" or "P4", returned list includes delta table, LS table, ANOVA table for fixed effects, variance component table and residual table; if analysisType is "P3" or "P5", returned list includes LS table, variance component table, residual table and BLUP table for factor1
#'
# @importFrom asreml asreml - may not need this
#' @export

# Old function name was 'R.ASReml_RCB_Return'
RCB_ModelFittingFunction <- function(DataIn, params.input, analysisType){

  # Either read in the parameters from a file or use the default inputs from above.
  params.list <- params.input
  mapply(assign, names(params.list), params.list, MoreArgs = list(envir = .GlobalEnv))
  print(paste0('input ', nrow(DataIn), ' rows'))

  DataIn <- DataIn[(DataIn[,params.list$isQaqcDeactivated]       == FALSE &
                      DataIn[,params.list$isDsrDeactivated]      == FALSE &
                      DataIn[,params.list$isAnswerDeactivated]   == FALSE &
                      DataIn[,params.list$isSetEntryDeactivated] == FALSE),]

  print(paste0('analyzing ', nrow(DataIn), ' rows'))
  # create unique repId
  DataIn$holdRepId <- DataIn[params.list$repId]
  temp <- paste(DataIn[,params.list$repId], DataIn[,params.list$locationId], sep = "_")
  DataIn[,params.list$repId] <- temp

  DataIn$holdRepId <- DataIn[,params.list$repId]
  temp <- paste(DataIn[,params.list$repId],
                DataIn[,params.list$locationId], sep = "_")
  DataIn[,params.list$repId] <- temp

  obs.name <-  unique(DataIn[,params.list$questionCode])

  print(paste0('current question code ', obs.name))

  if(length(obs.name) > 1){
    err <- 'multiple question codes detected'
    Out_return <- list(lsmTable = NA, deltas = NA, aov = NA, varcomp = NA, resid = NA, errorMessage = err)
    return(Out_return)
  }
  if(positiveValueCheck==TRUE & sum(DataIn[,value]<=0) > 1){
    err <- "RCB_Data<=0"
    Out_return <- list(lsmTable = NA, deltas = NA, aov = NA, varcomp = NA, resid = NA, errorMessage = err)
    return(Out_return)
  }

  data <- DataIn
  # This is currently a fixed limit determined by commitee.  TODO: It should be based on the number of treatment levels and blocks
  if(nrow(data) < sufficientDataThreshold){
    txt1 <- "Error: there are too few data (n<sufficientDataThreshold) to run an RCB model. Data frame has n = "
    txt2 <- paste0(txt1,nrow(data))
    txt3 <- paste0(txt2," rows and sufficientDataThreshold = ")
    txt4 <- paste0(txt3,sufficientDataThreshold)
    message(txt4)
    return(DataIn)
  }

  # Initialize output variable
  Out_return <- NULL

  # Sets the ASReml license path if it is not set already
  setLicense()

  # The following named list is used to make all of the variables available to several functions
  data_fields <- list(
    CROP_OBSRVTN_DETAIL_ID = value,
    FIELD_ID               = subSiteId,
    FACTOR_1               = factorLevelId,
    REP_ID                 = repId)

  # Check inputs
  checkAnalysisType(analysisType)
  checkData(data, data_fields)

  # Add the data fields to the global environment
  lapply(seq_along(data_fields),
         function(x) assign(names(data_fields)[[x]], value = data_fields[[x]], envir = .GlobalEnv))

  # Manipulate data
  data <- reformatData(data, data_fields)
  # Set fixed and random effects from analysis type
  setFixedRandomEffects(analysisType)
  # for timing
  time.scale <- ifelse(nrow(data) < 4000, "secs", "mins")
  start      <- Sys.time()
  message(paste("Running ASReml using analysis type:", analysisType), appendLF = TRUE )

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
                      trace      = FALSE,
                      na.method.Y = "omit",
                      na.method.X = "omit")

  message("finished in ", round(difftime(Sys.time(), start, units = time.scale[1]), digits = 2), " ", time.scale)
  message("Creating output files...", appendLF = TRUE)

  # Out_return$console$modeling <- RCB_asr

  if (analysisType %in% c('P3','P5') ) {
    # LS Means
    Out_return$lsmTable <- lsmAnalysis_r(RCB_asr, data)
    ## ANOVA table
    Out_return$varianceAnalysis <- ANOVA_output_r(RCB_asr)
    ## add residual tables
    # Out_return$residuals <- resid_table(RCB_asr,data)
    # colnames(Out_return$residuals) <- c('residuals',FIELD_ID,REP_ID)
    ## add blup table for random effects
    # Out_return$BLUP <- as.matrix(blup_table(RCB_asr,analysisType))
    # colnames(Out_return$BLUP) <- c('BLUP for random effects')
  }

  if (analysisType %in% c('P1','P2','P4') ) {
    # to use wald.asreml() only once to save computational time
    LSM_ALL <- lsmAnalysis(RCB_asr, data, alpha=alpha)
    # basic LSM table
    Out_return$lsmTable <- LSM_ALL[[1]]
    # Degrees of freedom
    degrees_freedom = LSM_ALL[[2]]  ## a vector including all the fixed effect

    if(is.na(degrees_freedom[2])){
      err <- 'failed delta analysis:  undefined degrees of freedom'
      Out_return <- list(lsmTable = NA, deltas = NA, aov = NA, varcomp = NA, resid = NA, errorMessage = err)
      return(Out_return)
      }
    del <- deltaAnalysis(RCB_asr,alpha=alpha, degrees_freedom[2])
    # deltas and p-values
    Out_return$deltas <- del[[1]]
    # Mean Separation Grouping
    meanSeparationGroup <- del[[2]]
    ## combine lsmTable and MSG
    Out_return$lsmTable <- cbind(Out_return$lsmTable,meanSeparationGroup)
    ## sort by descending order
    Out_return$lsmTable <- Out_return$lsmTable[order(Out_return$lsmTable$value,decreasing = TRUE),]
    ## ANOVA table
    Out_return$anova <- LSM_ALL[[3]]
    Out_return$varianceComposition <- LSM_ALL[[4]]

  }
  ##  convert output to strings
  out.list <- list()
  for(i in 1:length(Out_return)){
    curr.df <- as.data.frame(Out_return[[i]])
    curr.df[] <- lapply(curr.df, as.character)
    out.list[[i]] <- curr.df
  }
  names(out.list) <- names(Out_return)
  return(out.list)
}
