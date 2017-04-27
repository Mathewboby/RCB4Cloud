#' Check that the analysis_type is valid
#' @param analysis_type A string specifying the type of analysis to be done
checkAnalysisType <- function(analysis_type){
  check.path <- c("P1", "P2", "P3", "P4", "P5")
  if (!(analysis_type %in% check.path) ) {
    analysis_type
    stop("Invalid experiment design value")
  }
}

#' Sets the location of the ASReml license file if it does not exist
setLicense <- function(){
  if (Sys.getenv("ASREML_LICENSE_FILE") == "") {
    Sys.setenv("ASREML_LICENSE_FILE" = "/usr/local/asreml3/bin/asreml.lic")
  }
}

#' Checks that data conditions are met
#' @param data A dataframe
#' @param data_fields A list of column names
#' @export
checkData <- function(data, data_fields){
  
  # Check for all required fields in data_fields list
  required_fields <- c ( "CROP_OBSRVTN_DETAIL_ID",
                         "FIELD_ID",
                         "FACTOR_1",
                         "REP_ID" )
  # TODO: require Factortype.name ?
  
  if(sum(!(required_fields %in% names( data_fields )))){
    missing_fields <- required_fields[!(required_fields %in% names(data_fields))]
    missing_fields <- paste(missing_fields, collapse = ", ")
    out_message <- paste("ERROR: Missing these required fields in the data_fields list: ", missing_fields)
    message(out_message)
    return(out_message)
  }
  
  # Check for all the data_fields in the data
  if(sum(!(data_fields %in% colnames(data)))){
    not.in <- data_fields[which( !(data_fields %in% colnames(data)))]
    not.in <- paste(not.in, collapse = ", ")
    out_message <- paste("ERROR: Variable(s)", not.in, "not in data header")
    message(out_message)
    return(out_message)
  }
}

#' Reformats data to play nice with ASReml
#' @inheritParams checkData
#' @export
reformatData <- function(data, data_fields){
  #keep only needed variables
  data <- data[, unlist(data_fields)]
  
  # In each column, replace each of the following with "-" for the specified reason
  # 1) "," because the code needs to parse entry and covariate value by "," later in the code
  # 2) ":" because this character is reserved for specifying an interaction terms in formulas
  # 3) " " because ASReml does not like spaces
  data <- apply(data, 2, gsub, pattern = ",", replacement = "-")
  data <- apply(data, 2, gsub, pattern = ":", replacement = "-")
  data <- apply(data, 2, gsub, pattern = " ", replacement = "-")
  
  data <- data.frame(data)
  
  # Convert data types
  data[, FACTOR_1] <- as.character(data[, FACTOR_1]) 
  
  
  # Adjust control names
  if("Factortype.name" %in% data_fields){
    control.indecies <- which(data[,Factortype.name] == "CONTROL")  
    if(length(unique(data[control.indecies, FACTOR_1])) > 1){
      message(paste(FACTOR_1,
                    " has more than one level specified as control, only the first will be returned.",
                    sep = ""))}
    # take the first(and hopefully only) unique control factor name
    # append "AAAA" and change name
    Control.level <- unique(data[control.indecies, FACTOR_1])[1]
    adjusted.control.name <- paste("AAAA", Control.level, sep = "")   
    data[data[, FACTOR_1] == Control.level, FACTOR_1] <- adjusted.control.name 
    assign("Control.level", Control.level, env = .GlobalEnv)
    assign("adjusted.control.name", adjusted.control.name, env = .GlobalEnv)
  }
  
  data[, CROP_OBSRVTN_DETAIL_ID] <- as.numeric(as.character(data[,CROP_OBSRVTN_DETAIL_ID]))
  data[, FIELD_ID] <- as.factor(data[, FIELD_ID])
  data[, FACTOR_1] <- as.factor(data[, FACTOR_1])
  data[, REP_ID] <- as.factor(data[, REP_ID])
  
  return(data)
}

#' Sets the fixed and random formulas based on the analysis type
#' @inheritParams checkAnalysisType
#' @export
setFixedRandomEffects <- function(analysis_type){
  ###Specify the fixed, sparse and random formulas:
  switch(analysis_type,
         P1 = {
           print (" Model1: Single LOC Multi REP BLUE")
           assign("fixed.formula",
                  formula(paste(CROP_OBSRVTN_DETAIL_ID , "~ 1 + ", FACTOR_1,  sep = "")), 
                  envir = .GlobalEnv)
           assign("random.formula",
                  formula(paste("~", REP_ID, sep = "")),
                  envir = .GlobalEnv)
         }, 
         P2 = {
           print (" Model2: Multi LOC Single REP BLUE")
           assign("fixed.formula", 
                  formula(paste(CROP_OBSRVTN_DETAIL_ID, "~ 1 + ", FACTOR_1, sep = "")), 
                  envir = .GlobalEnv)
           assign("random.formula",
                  formula(paste("~ ",FIELD_ID,sep="")),
                  envir = .GlobalEnv)
         }, 
         P3 = {
           print (" Model3: Multi LOC SINGLE REP BLUP")
           assign("fixed.formula", 
                  formula(paste(CROP_OBSRVTN_DETAIL_ID, "~ 1 ", sep = "")), 
                  envir = .GlobalEnv)
           assign("random.formula",
                  formula(paste("~ ", FIELD_ID," + ", FACTOR_1,sep="")),
                  envir = .GlobalEnv)
         }, 
         P4 = {
           print (" Model3: Multi LOC MULTI REP BLUE")
           assign("fixed.formula", 
                  formula(paste(CROP_OBSRVTN_DETAIL_ID, "~ 1 + ", FACTOR_1, sep = "")), 
                  envir = .GlobalEnv)
           assign("random.formula",
                  formula(paste("~",FIELD_ID,"+",FIELD_ID,":",REP_ID,"+", FIELD_ID,":",FACTOR_1,sep="")),
                  envir = .GlobalEnv)
         }, 
         P5 = {
           print (" Model3: Multi LOC MULTI REP BLLUP")
           assign("fixed.formula", 
                  formula(paste(CROP_OBSRVTN_DETAIL_ID, "~ 1 ", sep = "")), 
                  envir = .GlobalEnv)
           assign("random.formula",
                  formula(paste("~",FIELD_ID,"+",FIELD_ID,":",REP_ID,"+",FACTOR_1,"+","FIELD_ID",":","FACTOR_1", sep="")),
                  envir = .GlobalEnv)
         }
  )
}

#' Computes the LS means analysis
#' @param RCB.asr A fitted ASReml object
#' @param data A dataframe that contains the data used to create the ASReml object
#' @param alpha A number between 0 and 1 specifying the confidence level
#' @importFrom Matrix sparse.model.matrix rankMatrix
#' @return A dataframe containing the Entry, Yield, SE, and upper and lower confidence intervals.
#' @export
lsmAnalysis <- function(RCB.asr, data, alpha){
  # residual degrees of freedom
  dm <- Matrix::sparse.model.matrix(fixed.formula, data) #sparse design matrix
  #        n   -               # missing_obs              -              rank(X)
  DDoF <- nrow(dm) - sum(is.na(data[,CROP_OBSRVTN_DETAIL_ID])) - Matrix::rankMatrix(x = dm, method = "qr.R")[[1]]
  
  LSM <- data.frame(
    Entry = RCB.asr$predictions$pvals[[FACTOR_1]],
    Yield = RCB.asr$predictions$pvals$predicted.value,
    SE = RCB.asr$predictions$pvals$standard.error)
  
  # create the CI
  LSM$CI_L <- LSM$Yield - qt(1 - alpha/2, DDoF)*LSM$SE
  LSM$CI_U <- LSM$Yield + qt(1 - alpha/2, DDoF)*LSM$SE
  ##replace adjusted control name with actual control name
  if(exists("adjusted.control.name", envir = .GlobalEnv)){
    levels(LSM$Entry)[levels(LSM$Entry) == adjusted.control.name] <- Control.level  
  }
  return(LSM)
}

# TODO:
# Compute the correct degrees of freedom
#' Computes the deltas between the treatment means and associated p-values
#' @param RCB.asr A fitted ASReml object
#' @param total_df A number specifying the total degrees of freedom to be used in the p-value computations.
#' @return A dataframe with columns for head, check, difference between the mean, and associated p-value for all combinations of heads and checks.
#' @importFrom asremlPlus alldiffs predictiondiffs.asreml
#' @export
deltaAnalysis <- function(RCB.asr, total_df = 150.8){
  
  test_diffs <- alldiffs ( predictions = RCB.asr$predictions$pvals,
                           sed = RCB.asr$predictions$sed,
                           tdf = total_df )
  
  # There is an error/mistake/bug between these two steps. In the above data
  # the factor names can be numeric, but treated as characters and ordered in 
  # alphabetic order. When the predictiondiffs.asreml is called, it keeps
  # the data in thet same order, but renames the rows and columns in purely
  # numeric order, thus giving incorrect values in the tables. This can be
  # resolved by renaming the rows/columns using the order of the names in 
  # the alldiffs function.
  
  diffs_out <- predictiondiffs.asreml( classify = "FACTOR1",
                                       alldiffs.obj = test_diffs )
  
  # Fix the potential re-ordering issue
  correct_order_names <- as.character ( diffs_out$predictions$FACTOR1 ) 
  rownames( diffs_out$differences ) <- colnames ( diffs_out$differences ) <- correct_order_names
  rownames( diffs_out$p.differences ) <- colnames ( diffs_out$p.differences ) <- correct_order_names
  rownames( diffs_out$sed ) <- colnames ( diffs_out$sed ) <- correct_order_names 
  
  # Output the data as a data table
  out_data <- merge ( correct_order_names, correct_order_names )
  names ( out_data ) <- c("head", "comp")
  out_data$diff <- as.vector ( diffs_out$differences )
  out_data$p.diff <- as.vector ( diffs_out$p.differences )
  out_data$sed <- as.vector ( diffs_out$sed )
  
  return(out_data)
}