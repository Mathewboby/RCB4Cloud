
#' Main function for doing RCBD analysis.
#'
#' @param data A dataframe with trial observation data. Column names should match those specified as the values in \code{data_fields}.
#' @param analysis_type A string specifying the type of analysis to be conducted.
#' @param alpha A number between 0 and 1 specifying the confidence level
#' @param data_fields A list specifying the environmental variables to be used in fitting the model and the associated column names in the data dataframe.
#' @return A list with a component for the fit model, the LS means analysis, and the delta means analysis
#'
#' @importFrom asreml asreml
#' @export

R.ASReml_RCB_Return <- function(data,
                                analysis_type,
                                alpha = .05,
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
  RCB_asr  <-  asreml(fixed = fixed_formula,
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
                      pworkspace = 1000 * 1e6 / 8)

  message("finished in ", round(difftime(Sys.time(), start, units = time.scale[1]), digits = 2), " ", time.scale)
  message("Creating output files...", appendLF = FALSE)

  # Out_return$console$modeling <- RCB_asr

  # LS Means
  Out_return$LSM_TABLE <- lsmAnalysis_r(RCB_asr, data)
  
  ## ANOVA table
  Out_return$var_analysis <- ANOVA_output_r(RCB_asr)
  
  
  if (analysis_type %in% c('P1','P2','P4') ) {
  Out_return$LSM_TABLE <- lsmAnalysis(RCB_asr, data, alpha=alpha)    
  
  # Degrees of freedom
  degrees_freedom = asreml::wald.asreml( RCB_asr,denDF ="default",data=data)[[1]][,2]  ## a vector including all the fixed effect

  
  del=deltaAnalysis(RCB_asr,alpha=alpha, degrees_freedom[2])
  # Deltas and p-values
  Out_return$Deltas <- del[[1]]
  # Mean Separation Grouping
  mean_sep_group <- del[[2]]$M
  
  ## combine LSM_TABLE and MSG
  Out_return$LSM_TABLE <- cbind(Out_return$LSM_TABLE,mean_sep_group)
  ## sort by descending order
  Out_return$LSM_TABLE <- Out_return$LSM_TABLE[order(Out_return$LSM_TABLE$Yield,decreasing = TRUE),]
  
  ## ANOVA table
  Out_return$var_analysis <- ANOVA_output(RCB_asr,degrees_freedom)
  }
  
  ## add residual tables
  Out_return$resid <- resid_table(RCB_asr,data)
  colnames(Out_return$resid) <- c('residuals',FIELD_ID,REP_ID)
  

  return(Out_return)
}
