
#' Main function for doing RCBD analysis.
#'
#' @param data A dataframe with trial observation data. Column names should match those specified as the values in \code{data_fields}.
#' @param analysis_type A string specifying the type of analysis to be conducted.
#' @param alpha A number between 0 and 1 specifying the confidence level
#' @param data_fields A list specifying the environmental variables to be used in fitting the model and the associated column names in the data dataframe.
#' @return A list with several components for the fit model, if analysis_type is "P1", "P2" or "P4", returned list includes delta table, LS table, ANOVA table for fixed effects, variance component table and residual table; if analysis_type is "P3" or "P5", returned list includes LS table, variance component table, residual table and BLUP table for factor1 
#'
#' @importFrom asreml asreml
#' @export

R.ASReml_RCB_Return <- function(data,
                                analysis_type,
                                alpha = .1,
                                data_fields =
                                  list(
                                    CROP_OBSRVTN_DETAIL_ID = "numValue",
                                    FIELD_ID = "fieldId",
                                    FACTOR_1 = "factor1",
                                    REP_ID = "repId",
                                    Factortype_name = "experimentalUnitId" )){

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
  start <- Sys.time()
  message(paste("Running ASReml using analysis type:", analysis_type), appendLF = TRUE )

 
  # Run R-ASReml, capture output
  RCB_asr  <-  asreml::asreml(fixed = fixed_formula,
                      random = random_formula,
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
                      workspace = 1000 * 1e6 / 8,
                      pworkspace = 1000 * 1e6 / 8, trace=F)

  
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
