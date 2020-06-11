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
#' @param params.input         a named list giving values for the following 12 arguments.
# @param alpha                       a number between 0 and 1 specifying the confidence level and comparison significance
# @param value  a sting giving the column name of the response variable in DataIn
# @param subSiteId           a sting giving the column name of the field ID variable in DataIn
# @param factorLevelId   a sting giving the column name of the treatment variable in DataIn
# @param repId       a sting giving the column name of the rep ID variable in DataIn
# @param locationId
# @param questionCode
# @param isDsrDeactivated
# @param isQaqcDeactivated
# @param isAnswerDeactivated
# @param isSetEntryDeactivated
# @param entryId
# @param sufficientDataThreshold: a number giving the lower bound on the number of data values to be analyzed
#' @return A list with several components for the fit model, if analysisType is "P1", "P2" or "P4", returned list includes delta table, LS table, ANOVA table for fixed effects, variance component table and residual table; if analysisType is "P3" or "P5", returned list includes LS table, variance component table, residual table and BLUP table for factor1
#'
# @importFrom asreml asreml - may not need this
#' @export

# Old function name was called 'R.ASReml_RCB_Return'
RCB_ModelFittingFunction <- function(DataIn, params.input, analysisType){

  # Either read in the parameters from a file or use the default inputs from above.
  params.list <- params.input
  mapply(assign, names(params.list), params.list, MoreArgs = list(envir = .GlobalEnv))
  print(paste0('input ', nrow(DataIn), ' rows'))

  obs.name <-  unique(DataIn[,params.list$questionCode])
  print(paste0('current question code is ', obs.name))

tmp <- as.logical(as.character(DataIn[,params.list$isQaqcDeactivated]))

DataIn[,params.list$isQaqcDeactivated]     <- NULL
DataIn[,params.list$isQaqcDeactivated]     <- tmp
DataIn[,params.list$isDsrDeactivated]      <- as.logical(as.character(DataIn[,params.list$isDsrDeactivated]))
DataIn[,params.list$isAnswerDeactivated]   <- as.logical(as.character(DataIn[,params.list$isAnswerDeactivated]))
DataIn[,params.list$isPlaceHolder]         <- as.logical(as.character(DataIn[,params.list$isPlaceHolder]))
DataIn[,params.list$isSetEntryDeactivated] <- as.logical(as.character(DataIn[,params.list$isSetEntryDeactivated]))

isRowOK <- (DataIn[,params.list$isQaqcDeactivated]     == FALSE &
            DataIn[,params.list$isDsrDeactivated]      == FALSE &
            DataIn[,params.list$isAnswerDeactivated]   == FALSE &
            DataIn[,params.list$isPlaceHolder]         == FALSE &
            DataIn[,params.list$isSetEntryDeactivated] == FALSE)

  DataIn <- DataIn[isRowOK, ]

  print(paste0('analyzing ', nrow(DataIn), ' rows'))

  # This is currently a fixed limit determined by commitee.  TODO: It should be based on the number of treatment levels and blocks
  if(nrow(DataIn) < sufficientDataThreshold){
    errorMessage        <- "Insufficient data"
    return(errorMessage)
  }

  if(length(unique(DataIn[, params.list$factorLevelId])) < 2){
    errorMessage        <- "Single factor level"
    return(errorMessage)
  }

  # create unique repId
  DataIn$holdRepId           <- DataIn[params.list$repId]
  temp                       <- paste(DataIn[,params.list$repId],
                                      DataIn[,params.list$subSiteId], sep = "_")
  DataIn[,params.list$repId] <- temp
  DataIn$errorMessage        <- ""

  Treatment_Factor_Names  <- params.input$factorLevelId
  Treatment_Factor_Number <- 1

  # In previous versions there were alignment issues with treatment level names, due to sorting of results by treatment factor level names.
  # However, the outcome of these sorts are not always predictable.  Converting the treatment factors to character vectors
  # and then into R-factor vectors did not fix the problem.  So far it has been found that relabeling
  # the treatment levels with names that sort in a predictable manner eliminates the problem.  Therefore,
  # the treatment level names are temporarily replaced with these more sortable names and then the original names
  # are put back in the output at the end of the function.
  treatment_abbreviations  <- "T"  # Initialize factor identifying prefixes for the treatment level names
  nchar_abbreviation       <-  1

  treatment_factor_as_a_factor         <- as.factor(as.character(DataIn[, Treatment_Factor_Names[1]]))
  old_names                            <- levels(treatment_factor_as_a_factor)
  levels(treatment_factor_as_a_factor) <- sortable_string_integers(nlevels(treatment_factor_as_a_factor),
                                                                   prefix = treatment_abbreviations[1])
  DataIn[, Treatment_Factor_Names[1] ] <- NULL  # Remove the orginal vector
  DataIn[, Treatment_Factor_Names[1] ] <- treatment_factor_as_a_factor   # Insert the replacement vector

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
  checkData(DataIn, data_fields)

  # Add the data fields to the global environment
  lapply(seq_along(data_fields),
         function(x) assign(names(data_fields)[[x]], value = data_fields[[x]], envir = .GlobalEnv))

  # Manipulate data
  data <- reformatData(DataIn, data_fields)
  # Set fixed and random effects from analysis type
  setFixedRandomEffects(analysisType)
  # for timing
  time.scale <- ifelse(nrow(DataIn) < 4000, "secs", "mins")
  start      <- Sys.time()
  message(paste("Running ASReml using analysis type:", analysisType), appendLF = TRUE )
  # Run R-ASReml, capture output
  RCB_asr  <-  asreml::asreml(
                      fixed     = fixed_formula,
                      random    = random_formula,
                      tolerance = 1E-10,
                      data      = DataIn,
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
                      workspace  = 125 * 1e6, # approximately 125 million bytes
                      pworkspace = 125 * 1e6, # approximately 125 million bytes
                      trace      = FALSE,
                      na.method.Y = "omit",
                      na.method.X = "omit")

  message("finished in ", round(difftime(Sys.time(), start, units = time.scale[1]), digits = 2), " ", time.scale)
  message("Creating output files...", appendLF = TRUE)

  if (analysisType %in% c('P3','P5') ) {
    # LS Means
    Out_return$blupTable <- lsmAnalysis_r(RCB_asr, DataIn)
    ## ANOVA table
    Out_return$varianceAnalysis <- ANOVA_output_r(RCB_asr)
  }

  if (analysisType %in% c('P1','P2','P4') ) {

    # to use wald.asreml() only once to save computational time
    LSM_ALL <- lsmAnalysis(RCB_asr, DataIn, alpha=alpha)

    # basic LSM table
    Out_return$blueTable                       <- LSM_ALL[[1]]

    # Restore original treatment factor level names
    Out_return$blueTable$factorLevelId <- as.factor(as.character(Out_return$blueTable$factorLevelId))
    # levels(Out_return$blueTable$factorLevelId) <- old_names
    factorLevelIdAsNumeric             <- as.numeric(Out_return$blueTable$factorLevelId)
    factorLevelIdAsCharacter           <- old_names[factorLevelIdAsNumeric]
    Out_return$blueTable$factorLevelId <- NULL
    Out_return$blueTable$factorLevelId <- factorLevelIdAsCharacter
    Out_return$blueTable               <- Out_return$blueTable[, c(7,1:6)]

    # Degrees of freedom
    degrees_freedom = LSM_ALL[[2]]  ## a vector including all the fixed effect
    if(is.na(degrees_freedom[2])){
      err <- 'failed delta analysis:  undefined degrees of freedom'
      Out_return <- list(blueTable = NA, deltas = NA, aov = NA, varcomp = NA, resid = NA, errorMessage = err)
      return(Out_return)
    }

    del <- deltaAnalysis(RCB_asr, alpha=alpha, degrees_freedom[2])

    # deltas and p-values
    Out_return$deltas <- del[[1]]

    # Restore original treatment factor level names
    Out_return$deltas$head               <- as.factor(as.character(Out_return$deltas$head))
    Out_return$deltas$comparison         <- as.factor(as.character(Out_return$deltas$comparison))
    levels(Out_return$deltas$head)       <- old_names
    levels(Out_return$deltas$comparison) <- old_names
    # Round off results to 10 decimal places
    Out_return$deltas$differences              <- round(Out_return$deltas$differences, 10)
    Out_return$deltas$probabilityDifferences   <- round(Out_return$deltas$probabilityDifferences, 10)
    Out_return$deltas$standardErrorDifferences <- round(Out_return$deltas$standardErrorDifferences, 10)
    Out_return$deltas$t                        <- round(Out_return$deltas$t, 10)
    Out_return$deltas$lowerConfidenceInterval  <- round(Out_return$deltas$lowerConfidenceInterval, 10)
    Out_return$deltas$upperConfidenceInterval  <- round(Out_return$deltas$upperConfidenceInterval, 10)

    # Add the Mean Separation Grouping to the BLUE table
    Out_return$blueTable$meanSeparationGroup <- del[[2]]

    ## sort by descending order
    Out_return$blueTable <- Out_return$blueTable[order(Out_return$blueTable$value, decreasing = TRUE),]
    # Round off results to 10 decimal places
    Out_return$blueTable$value                   <- round(Out_return$blueTable$value, 10)
    Out_return$blueTable$standardError           <- round(Out_return$blueTable$standardError, 10)
    Out_return$blueTable$lowerConfidenceInterval <- round(Out_return$blueTable$lowerConfidenceInterval, 10)
    Out_return$blueTable$upperConfidenceInterval <- round(Out_return$blueTable$upperConfidenceInterval, 10)

    ## ANOVA table
    Out_return$anova                      <- LSM_ALL[[3]]
    Out_return$varianceComposition        <- LSM_ALL[[4]]
    Out_return$leastSignificantDifference <- data.frame(mean=del$LSD)
  }
  ##  convert output to strings
  out.list <- list()
  for(i in 1:length(Out_return)){
    curr.df       <- as.data.frame(Out_return[[i]])
    curr.df[]     <- lapply(curr.df, as.character)
    out.list[[i]] <- curr.df
  }
  names(out.list) <- names(Out_return)
  return(out.list)
}
