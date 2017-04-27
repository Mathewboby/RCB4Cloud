
#' Main function for doing RCBD analysis.
#' 
#' @param data A dataframe with trial observation data. Column names should match those specified as the values in \code{data_fields}.
#' @param analysis_type A string specifying the type of analysis to be conducted.
#' @param alpha A number between 0 and 1 specifying the confidence level
#' @param data_fields A list specifying the environmental variables to be used in fitting the model and the associated column names in the data dataframe.
#' @return A list with a component for the fit model, the LS means analysis, and the delta means analysis
#' 
#' @importMethodsFrom asreml
#' @importMethodsFrom asremlPlus
#' @export

R.ASReml_RCB_Return <- function(data, 
                                analysis_type,
                                alpha = .05,
                                data_fields = 
                                  list(
                                    CROP_OBSRVTN_DETAIL_ID = "NUMVALUE",
                                    FIELD_ID = "FIELDID",
                                    FACTOR_1 = "FACTOR1",
                                    REP_ID = "REPID",
                                    Factortype.name = "EXPERIMENTALUNITID" )){
  
  # Initialize output variable
  Out.return <- NULL
  
  # Sets the ASReml license path if it is not set already
  setLicense()
  
  # Check inputs
  checkAnalysisType(analysis_type)
  checkData(data, data_fields)
  
  # Add the data fields to the global environment
  lapply(seq_along(data_fields), function(x) assign(names(data_fields)[[x]], value = data_fields[[x]], envir = .GlobalEnv ) )
  
  # Manipulate data
  data <- reformatData(data, data_fields)
  
  # Set fixed and random effects from analysis type
  setFixedRandomEffects(analysis_type)
  
  # for timing
  time.scale <- ifelse(nrow(data) < 4000, "secs", "mins")
  start = Sys.time()
  message(paste("Running ASReml using analysis type:", analysis_type), appendLF = TRUE )
  
  # Run R-ASReml, capture output
  RCB.asr  <-  asreml(fixed = fixed.formula,
                      random = random.formula,
                      tolerance = 1E-10,
                      data = data,
                      predict = list(classify = paste(FACTOR_1),
                                     present = list(),
                                     ignore = character(0),
                                     except = character(0),
                                     only = character(0),
                                     associate = list(),
                                     margin = FALSE,
                                     average = list(),
                                     as.average = list(),
                                     vcov = FALSE,
                                     sed = list(dotstring = T),
                                     parallel = FALSE,
                                     inrandom = TRUE,
                                     exrandom = logical(0),
                                     aliased = FALSE,
                                     estimable = FALSE,
                                     xform = list()), 
                      workspace = 1000*1e6/8, 
                      pworkspace = 1000*1e6/8)
  
  message("finished in ", round(difftime(Sys.time(), start, units = time.scale[1]), digits = 2), " ", time.scale)
  message("Creating output files...", appendLF = FALSE)
  
  Out.return$console$modeling <- RCB.asr
  
  # LS Means
  Out.return$LSM_TABLE <- lsmAnalysis(RCB.asr, data, alpha)
  
  # Deltas and p-values
  Out.return$Deltas <- deltaAnalysis(RCB.asr)
  
  return(Out.return)
}

