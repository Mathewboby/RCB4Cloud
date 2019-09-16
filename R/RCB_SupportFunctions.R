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

#' Checks that data conditions are met by making sure the data frame has the correctly named columns
#' @param data A dataframe
#' @param data_fields A list of column names
#' @export
checkData <- function(data, data_fields){
  # Check for all required fields in data_fields list
  required_fields <- c ( "CROP_OBSRVTN_DETAIL_ID",
                         "FIELD_ID",
                         "FACTOR_1",
                         "REP_ID" )
  # TODO: require Factortype_name ?

  if (sum(!(required_fields %in% names( data_fields )))){
    missing_fields <- required_fields[!(required_fields %in% names(data_fields))]
    missing_fields <- paste(missing_fields, collapse = ", ")
    out_message    <- paste("ERROR: Missing these required fields in the data_fields list: ", missing_fields)
    message(out_message)
    return(out_message)
  }
  # Check for all the data_fields in the data
  if (sum(!(data_fields %in% colnames(data)))){
    not.in      <- data_fields[which( !(data_fields %in% colnames(data)))]
    not.in      <- paste(not.in, collapse = ", ")
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
  dflds     <- unlist(data_fields)
  # Separate the numeric response from the factors so it does not get changed by the
  # gsub statements below.  After the changes to the factor level names, the numeric
  # the response is recombined with the factors.
  ResponseY <- data[, dflds[1]]               # Numeric response variable
  data      <- data[, dflds[2:length(dflds)]] # Data frame of just the factors
  # In each column, replace each of the following with "-" for the specified reason
  # 1) "," because the code needs to parse entry and covariate value by "," later in the code
  # 2) ":" because this character is reserved for specifying an interaction terms in formulas
  # 3) " " because ASReml does not like spaces
  # The next line was added and excluded.  Some of the factor level names are very long integers.
  # Somtimes R does not convert these to character well as it will round them off or truncate them.
  # The result in this case will reduce the number of factor levels. One way to prevent this is to
  # prepend a character to the begining of the factor level names.  The following commented out line did this.
  #data <- as.data.frame(lapply(data,function(z){paste0("z",as.character(z))})) # Prepend 'z' to factor level names to prevent rounding of purely integer names by R
  data <- apply(data, 2, gsub, pattern = ",", replacement = "_")
  data <- apply(data, 2, gsub, pattern = ":", replacement = "_")
  data <- apply(data, 2, gsub, pattern = " ", replacement = "")

  # Recombine the numeric response variable with the modified factors
  data            <- as.data.frame(data) # Make sure data is a data frame
  data[,dflds[1]] <- ResponseY           # Add the numeric response variable back into the data frame

  # Convert data types
  data[, FACTOR_1] <- as.character(data[, FACTOR_1])

  # Adjust control names
  if ("Factortype_name" %in% data_fields){
    control_indices <- which(as.character(data[, Factortype_name]) == "CONTROL")
    if (length(unique(data[control_indices, FACTOR_1])) > 1){
      message(paste(FACTOR_1,
                    " has more than one level specified as control, only the first will be returned.",
                    sep = ""))
    }
    # take the first(and hopefully only) unique control factor name
    # append "AAAA" and change name
    control_level                                     <- unique(data[control_indices, FACTOR_1])[1]
    adjusted_control_name                             <- paste("AAAA", control_level, sep = "")
    data[data[, FACTOR_1] == control_level, FACTOR_1] <- adjusted_control_name
    assign("control_level"        , control_level        , env = .GlobalEnv)
    assign("adjusted_control_name", adjusted_control_name, env = .GlobalEnv)
  }

  data[, CROP_OBSRVTN_DETAIL_ID] <- as.numeric(as.character(data[, CROP_OBSRVTN_DETAIL_ID]))
  data[, FIELD_ID] <- as.factor(data[, FIELD_ID])
  data[, FACTOR_1] <- as.factor(data[, FACTOR_1])
  data[, REP_ID]   <- as.factor(data[, REP_ID])

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
           assign("fixed_formula",
                  formula(paste(CROP_OBSRVTN_DETAIL_ID, "~ 1 + ", FACTOR_1,  sep = "")),
                  envir = .GlobalEnv)
           assign("random_formula",
                  formula(paste("~", REP_ID, sep = "")),
                  envir = .GlobalEnv)
         },
         P2 = {
           print (" Model2: Multi LOC Single REP BLUE")
           assign("fixed_formula",
                  formula(paste(CROP_OBSRVTN_DETAIL_ID, "~ 1 + ", FACTOR_1, sep = "")),
                  envir = .GlobalEnv)
           assign("random_formula",
                  formula(paste("~ ", FIELD_ID, sep = "")),
                  envir = .GlobalEnv)
         },
         P3 = {
           print (" Model3: Multi LOC SINGLE REP BLUP")
           assign("fixed_formula",
                  formula(paste(CROP_OBSRVTN_DETAIL_ID, "~ 1 ", sep = "")),
                  envir = .GlobalEnv)
           assign("random_formula",
                  formula(paste("~ ", FIELD_ID, " + ", FACTOR_1, sep = "")),
                  envir = .GlobalEnv)
         },
         P4 = {
           print (" Model4: Multi LOC MULTI REP BLUE")
           assign("fixed_formula",
                  formula(paste(CROP_OBSRVTN_DETAIL_ID, "~ 1 + ", FACTOR_1, sep = "")),
                  envir = .GlobalEnv)
           assign("random_formula",
                  formula(paste("~", FIELD_ID, "+", FIELD_ID, ":", REP_ID, "+", FIELD_ID, ":", FACTOR_1, sep = "")),
                  envir = .GlobalEnv)
         },
         P5 = {
           print (" Model5: Multi LOC MULTI REP BLUP")
           assign("fixed_formula",
                  formula(paste(CROP_OBSRVTN_DETAIL_ID, "~ 1 ", sep = "")),
                  envir = .GlobalEnv)
           assign("random_formula",
                  formula(paste("~", FIELD_ID, "+", FIELD_ID, ":", REP_ID, "+", FACTOR_1, "+", FIELD_ID, ":",
                                FACTOR_1, sep = "")),
                  envir = .GlobalEnv)
         }
  )
}



#' Computes the LS means analysis for model P1, P2 and P4
#' @param asreml.obj A fitted ASReml object
#' @param data A dataframe that contains the data used to create the ASReml object
#' @param alpha A number between 0 and 1 specifying the confidence level
#' @importFrom asreml wald.asreml
#' @return A dataframe containing the Entry, Yield, degree of freedom for t-test, SE, total number of observation for the entry after excluding missing data, upper and lower confidence intervals and mean separation grouping
#' @export


lsmAnalysis <- function(asreml.obj, data, alpha){
  # residual degrees of freedom
  # dm <- Matrix::sparse.model.matrix(fixed_formula, data) #sparse design matrix
  #        n   -               # missing_obs              -              rank(X)
  # DDoF <- nrow(dm) - Matrix::rankMatrix(x = dm, method = "qr.R")[[1]]



  ## WARNING: here if you don't specifiy data=data, it will return error, because the "denDF = xx" argument here
  ##          calls update() in the backgroud to calculate the df for denominator, if you don't specify the "data=data"
  ##          it will by default search in the global enviroment. With all being said, you need to specify the data name in the local enviroment.

  ALL=asreml::wald.asreml(asreml.obj,denDF ="default",ssType= "conditional",data=data)

  ## for degree of freedom
  DDoF_ <- ALL[[1]][,2]
  DDoF <- DDoF_[2]


  ## for variance component table
  VAR=summary(asreml.obj)$varcomp

  for (j in 1:length(rownames(VAR))) {
      rownames(VAR)[j] <- strsplit(rownames(VAR),'!')[[j]][1]
  }

  rownames(VAR) <- c("subSiteId", "residual")

  VAR=VAR[,c(2,5)]
  colnames(VAR) <- c("varianceEstimates","constraint")

  ## for fix effect ANOVA table
  AOV <- ALL[[1]]
  colnames(AOV) <- c("degreesFreedomNumerator","degreesFreedomDenominator","ssIncremental","ssConditional","margin","probability")
  rownames(AOV) <- c("factorLevelId", "intercept")

  ## for LSM table
  LSM <- data.frame(
    factorLevelId = asreml.obj$predictions$pvals[[FACTOR_1]],
    value = asreml.obj$predictions$pvals$predicted.value,
    standardError = asreml.obj$predictions$pvals$standard.error,
    degreesFreedom = DDoF
  )

  ## count N in each Entry level with missing removed.
  N<-rep(NA,length(LSM$factorLevelId))
  for (i in 1:length(LSM$factorLevelId)) {
    N[i]=nrow(data[data[,FACTOR_1]==LSM$factorLevelId[i],])-sum(is.na(data[data[,FACTOR_1]==LSM$factorLevelId[i],CROP_OBSRVTN_DETAIL_ID]))
  }
  LSM$n <- N

  # create the CI
  LSM$lowerConfidenceInterval <- LSM$value - qt(1 - alpha / 2, DDoF) * LSM$standardError
  LSM$upperConfidenceInterval <- LSM$value + qt(1 - alpha / 2, DDoF) * LSM$standardError
  ##replace adjusted control name with actual control name
  if (exists("adjusted_control_name", envir = .GlobalEnv)){
    levels(LSM$factorLevelId)[levels(LSM$factorLevelId) == adjusted_control_name] <- control_level
  }
  return(list(LSM, DDoF_, AOV, VAR))
}

#' Computes the LS means analysis for model P3 and P5
#' @param asreml.obj A fitted ASReml object
#' @param data A dataframe that contains the data used to create the ASReml object
#' @return A dataframe containing the Entry, value, SE and total number of observation for the entry after excluding missing data
#' @export

lsmAnalysis_r <- function(RCB_asr, data){

  LSM <- data.frame(
    factorLevelId = RCB_asr$predictions$pvals[[FACTOR_1]],
    value = RCB_asr$predictions$pvals$predicted.value,
    standardError = RCB_asr$predictions$pvals$standard.error
  )

  ## count N in each Entry level with missing removed.
  N<-rep(NA,length(LSM$value))
  for (i in 1:length(LSM$value)) {
    N[i]=nrow(data[data[,FACTOR_1]==LSM$value[i],])-sum(is.na(data[data[,FACTOR_1]==LSM$value[i],CROP_OBSRVTN_DETAIL_ID]))
  }
  LSM$n<-N


  ##replace adjusted control name with actual control name
  if (exists("adjusted_control_name", envir = .GlobalEnv)){
    levels(LSM$value)[levels(LSM$value) == adjusted_control_name] <- control_level
  }
  return(LSM)
}


# TODO:
# Compute the correct degrees of freedom
#' Computes the deltas between the treatment means and associated p-values
#' @param RCB_asr A fitted ASReml object
#' @param total_df A number specifying the total degrees of freedom to be used in the p-value computations.
#' @param alpha A number between 0 and 1 specifying the confidence level
#' @return A dataframe with columns for head, check, difference between the mean, associated p-value for all combinations of heads and checks, degree of freedom for t-test, test of statistic, lower and upper confidence intervals
#' @importFrom asremlPlus alldiffs predictiondiffs.asreml
#' @export
deltaAnalysis <- function(RCB_asr, alpha, total_df){

  test_diffs <- asremlPlus::alldiffs ( predictions = RCB_asr$predictions$pvals,
                           sed = RCB_asr$predictions$sed,
                           tdf = total_df )

  # There is an error/mistake/bug between these two steps. In the above data
  # the factor names can be numeric, but treated as characters and ordered in
  # alphabetic order. When the predictiondiffs.asreml is called, it keeps
  # the data in thet same order, but renames the rows and columns in purely
  # numeric order, thus giving incorrect values in the tables. This can be
  # resolved by renaming the rows/columns using the order of the names in
  # the alldiffs function.

  diffs_out <- asremlPlus::predictiondiffs.asreml( classify = FACTOR_1,
                                       alldiffs.obj = test_diffs ,alpha =alpha )

  # Fix the potential re-ordering issue
  correct_order_names <- as.character( diffs_out$predictions[,1] )
  rownames( diffs_out$differences ) <- colnames( diffs_out$differences ) <- correct_order_names
  rownames( diffs_out$p.differences ) <- colnames( diffs_out$p.differences ) <- correct_order_names
  rownames( diffs_out$sed ) <- colnames( diffs_out$sed ) <- correct_order_names

  # Output the data as a data table
  out_data <- merge( correct_order_names, correct_order_names )
  names( out_data ) <- c("head", "comparison")
  out_data$differences <- as.vector( diffs_out$differences )
  out_data$probabilityDifferences <- as.vector( diffs_out$p.differences )
  out_data$standardErrorDifferences <- as.vector( diffs_out$sed )
  out_data$degreesFreedom <- rep( total_df,nrow(out_data) );
  out_data$degreesFreedom[is.na(out_data[,'probabilityDifferences'])] <- NA
  out_data$t <- as.vector( diffs_out$differences/diffs_out$sed )
  out_data$lowerConfidenceInterval <- out_data$differences - qt(1 - alpha / 2, out_data$degreesFreedom) * out_data$standardErrorDifferences
  out_data$upperConfidenceInterval <- out_data$differences + qt(1 - alpha / 2, out_data$degreesFreedom) * out_data$standardErrorDifferences

  ## the diagonal shouldn't be NA, set it as p-value = 1
  diag(diffs_out$p.differences)=1
  # MSG
  # msg=agricolae::orderPvalue(treatment=RCB_asr$predictions$pvals[,1],means=RCB_asr$predictions$pvals[,2],alpha=alpha,pvalue=diffs_out$p.differences,console=F)
  # msg=MSG(treatment=as.character(RCB_asr$predictions$pvals[,1])[order(LSM,decreasing =T)],means=LSM[order(LSM,decreasing =T)],alpha=alpha,pvalue=diffs_out$p.differences,console=F)
  msg=MSG(diffs_out$p.differences,alpha)

  return(list('Delta_table'=out_data,'Mean Separation Grouping'=msg))
}



# computeDF <- function(RCB_asr){
#   degrees_freedom <- asreml::wald.asreml( RCB_asr,denDF ="default")
#   return( degrees_freedom)
# }
# computeDF <- function(RCB_asr, FACTOR_1){
#   degrees_freedom <- asreml::wald.asreml( RCB_asr, maxiter = 1 )
#   return( degrees_freedom[FACTOR_1, "Df"] )
# }



# ANOVA table

# ANOVA_output <- function(asreml.obj,degrees_freedom){
#   anova_<-list()
#
#   anova_[[1]]=asreml::wald.asreml(asreml.obj,)                        ## for fixed effect
#   anova_[[2]]=summary(asreml.obj)$varcomp                      ## for random effect
#   anova_[[1]]$denDF= c(degrees_freedom,'')
#
#   for (j in 1:length(row.names(anova_[[2]]))) {
#   row.names(anova_[[2]])[j] <- strsplit(row.names(anova_[[2]]),'!')[[j]][1]
#   }
#   row.names(anova_[[2]])[j] <- 'residual'
#
#   names(anova_) <- c('fixed effect','random effect')
#   return(anova_)
#
# }





#' Computes the variance component table for model P3 and P5
#' @param asreml.obj A fitted ASReml object
#' @return A dataframe with variance component estimates
#' @export
ANOVA_output_r <- function(asreml.obj){
  ## for random effect
  anova_=summary(asreml.obj)$varcomp
  for (j in 1:length(row.names(anova_))) {
    row.names(anova_)[j] <- strsplit(row.names(anova_),'!')[[j]][1]
  }
  row.names(anova_)[j] <- 'residual'
  anova_=anova_[,c(2,5)]
  colnames(anova_) <- c("varianceEstimates","constraint")
  return(anova_)
}

#' Computes the residuals
#' @param asreml.obj A fitted ASReml object
#' @param data A dataframe that contains the data used to create the ASReml object
#' @return A dataframe with residuals align with corresponding location ID and replication ID
#' @export
resid_table <- function(asreml.obj,data) {
  cbind(resid(asreml.obj),data[,FIELD_ID],data[,REP_ID])
}

#' Computes the blup for random effects
#' @param asreml.obj A fitted ASReml object
#' @param analysis_type analysis_type defined by the user in the input argument
#' @return A dataframe with blup for random effects
#' @export
blup_table <- function(asreml.obj,analysis_type) {
  if (analysis_type == 'P5') {
    bp=rownames(coefficients(asreml.obj)$random)
    bp1=bp[grep("factor1",bp)]  ## all terms with factor1
    bp2=bp1[-grep(":",bp1)] ## exclude those with interaction
    bp2_ind=match(bp2,bp)
    blup <- coefficients(asreml.obj)$random[bp2_ind,]
  } else {
    bp=rownames(coefficients(asreml.obj)$random)
    bp1=bp[grep("factor1",bp)]  ## all terms with factor1
    bp1_ind=match(bp1,bp)
    blup <- coefficients(asreml.obj)$random[bp1_ind,]
  }
  return(blup)
}

#' Computes the Mean separation grouping for the fixed effect
#' @param P A symmetric matrix with pairwise comparsion P-value, make sure the diagnoal P-vlaues filled with 1 before parse to calculation
#' @param alpha A number between 0 and 1 specifying the confidence level
#' @return A vector of letter assignments for each level of fixed effect
#' @importFrom igraph simplify graph.data.frame maximal.cliques
#' @export

MSG <- function(P,alpha) {
  a <- P
  b <- which(a>alpha,arr.ind = TRUE)
  top <- data.frame(N1=b[,1],N2=b[,2])
  g3 <- igraph::simplify(igraph::graph.data.frame(top[order(top[[1]]),],directed=FALSE))
  cliq <- igraph::maximal.cliques(g3)
  nn <- length(cliq )
  temp1 <- rep("",nrow(a))
  assignment <- c(letters, LETTERS,paste(letters,'.',sep=''),paste(LETTERS,'.',sep=''))
  cliq2<-list()
  for (j in 1:nn){
    cliq2[[j]] <- colnames(a)[cliq[[j]]]
  }
  for (ind in 1:nrow(a)){
    ## check which list number contains this col name
    ## this one is problematic, since "1" is in "10" so list number containing "10" but no "1" will still be returned.
    ## tt <- grep(colnames(a)[ind],cliq2)
    ## solve the issue above
    tt=which(sapply(1:length(cliq2), function(x) colnames(a)[ind] %in% cliq2[[x]])==TRUE)
    temp1[ind]=paste0(assignment[tt],collapse = "")
  }
  return(temp1)
}

checkParameters <- function(params.default, params.input){
  # Create an intial list of RCB parameters set to their corresponding defaults.
  # Call this named list, params.default.

  # Either read in the parameters from a file or use the individual inputs from above.

  if(is.null(params.input)==TRUE){ # A user supplied list of parameters was not given. Therefore use the default individual inputs from above which may have been over written by the user.
    params.input <- params.default
  }

  # Check for missing parameters and add them, if needed, with default or individual input values.
  nloip <- names(params.default)
  CheckNames <- names(params.default) %in% names(params.input) # This is TRUE for each name in names(params.default) which is also in names(ListOfSmartQAQCParameters)
  if(any(CheckNames==FALSE)){
    wcn <- which(CheckNames==FALSE) # Get the indicies of the missing parameters
    if(length(wcn)>0){
      for(jj in wcn){
        params.input[[ nloip[jj] ]] <- params.default[[ nloip[jj] ]]
      }
    }
  }
  # Check for missing parameter values.  If there are some,
  # replace them with the individual input default values.
  Check4Missing <- sapply(params.input,function(zx){is.null(zx) | is.na(zx) | is.nan(zx)})
  if(any(Check4Missing)==TRUE){
    wcm  <- which(Check4Missing==TRUE) # Get the indicies of the missing parameters values
    if(length(wcm)>0){
      for(kk in wcm){ # Loop over the indices of the missing parameters
        params.input[[kk]] <- params.default[[kk]] # Replace the missing value with the individual default value.
      }
    }
  }
  return(params.input)
}
