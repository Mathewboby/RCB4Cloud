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
  # TODO: require Factortype_name ?

  if (sum(!(required_fields %in% names( data_fields )))){
    missing_fields <- required_fields[!(required_fields %in% names(data_fields))]
    missing_fields <- paste(missing_fields, collapse = ", ")
    out_message <- paste("ERROR: Missing these required fields in the data_fields list: ", missing_fields)
    message(out_message)
    return(out_message)
  }

  # Check for all the data_fields in the data
  if (sum(!(data_fields %in% colnames(data)))){
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
  if ("Factortype_name" %in% data_fields){
    control_indices <- which(data[, Factortype_name] == "CONTROL")
    if (length(unique(data[control_indices, FACTOR_1])) > 1){
      message(paste(FACTOR_1,
                    " has more than one level specified as control, only the first will be returned.",
                    sep = ""))
      }
    # take the first(and hopefully only) unique control factor name
    # append "AAAA" and change name
    control_level <- unique(data[control_indices, FACTOR_1])[1]
    adjusted_control_name <- paste("AAAA", control_level, sep = "")
    data[data[, FACTOR_1] == control_level, FACTOR_1] <- adjusted_control_name
    assign("control_level", control_level, env = .GlobalEnv)
    assign("adjusted_control_name", adjusted_control_name, env = .GlobalEnv)
  }

  data[, CROP_OBSRVTN_DETAIL_ID] <- as.numeric(as.character(data[, CROP_OBSRVTN_DETAIL_ID]))
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
           print (" Model3: Multi LOC MULTI REP BLUE")
           assign("fixed_formula",
                  formula(paste(CROP_OBSRVTN_DETAIL_ID, "~ 1 + ", FACTOR_1, sep = "")),
                  envir = .GlobalEnv)
           assign("random_formula",
                  formula(paste("~", FIELD_ID, "+", FIELD_ID, ":", REP_ID, "+", FIELD_ID, ":", FACTOR_1, sep = "")),
                  envir = .GlobalEnv)
         },
         P5 = {
           print (" Model3: Multi LOC MULTI REP BLLUP")
           assign("fixed_formula",
                  formula(paste(CROP_OBSRVTN_DETAIL_ID, "~ 1 ", sep = "")),
                  envir = .GlobalEnv)
           assign("random_formula",
                  formula(paste("~", FIELD_ID, "+", FIELD_ID, ":", REP_ID, "+", FACTOR_1, "+", "FIELD_ID", ":",
                                "FACTOR_1", sep = "")),
                  envir = .GlobalEnv)
         }
  )
}

#' Computes the LS means analysis
#' @param RCB_asr A fitted ASReml object
#' @param data A dataframe that contains the data used to create the ASReml object
#' @param alpha A number between 0 and 1 specifying the confidence level
#' @importFrom Matrix sparse.model.matrix rankMatrix
#' @return A dataframe containing the Entry, Yield, SE, and upper and lower confidence intervals.
#' @export
lsmAnalysis <- function(RCB_asr, data, alpha){
  # residual degrees of freedom
  dm <- Matrix::sparse.model.matrix(fixed_formula, data) #sparse design matrix
  #        n   -               # missing_obs              -              rank(X)
  # DDoF <- nrow(dm) - Matrix::rankMatrix(x = dm, method = "qr.R")[[1]]
  DDoF <- asreml::wald.asreml(RCB_asr,denDF ="default",data=data)[[1]][2,2]
  
  LSM <- data.frame(
    Entry = RCB_asr$predictions$pvals[[FACTOR_1]],
    Yield = RCB_asr$predictions$pvals$predicted.value,
    SE = RCB_asr$predictions$pvals$standard.error,
    df = DDoF
  )
  

  ## count N in each Entry level with missing removed.
  N<-rep(NA,length(LSM$Entry))
  for (i in 1:length(LSM$Entry)) {
    N[i]=nrow(data[data[,FACTOR_1]==LSM$Entry[i],])-sum(is.na(data[data[,FACTOR_1]==LSM$Entry[i],CROP_OBSRVTN_DETAIL_ID]))
  }
  LSM$N<-N
  
  
  # create the CI
  LSM$CI_L <- LSM$Yield - qt(1 - alpha / 2, DDoF) * LSM$SE
  LSM$CI_U <- LSM$Yield + qt(1 - alpha / 2, DDoF) * LSM$SE
  ##replace adjusted control name with actual control name
  if (exists("adjusted_control_name", envir = .GlobalEnv)){
    levels(LSM$Entry)[levels(LSM$Entry) == adjusted_control_name] <- control_level
  }
  return(LSM)
}

## for random effect models
lsmAnalysis_r <- function(RCB_asr, data){

  LSM <- data.frame(
    Entry = RCB_asr$predictions$pvals[[FACTOR_1]],
    Yield = RCB_asr$predictions$pvals$predicted.value,
    SE = RCB_asr$predictions$pvals$standard.error
  )
  
  ## count N in each Entry level with missing removed.
  N<-rep(NA,length(LSM$Entry))
  for (i in 1:length(LSM$Entry)) {
    N[i]=nrow(data[data[,FACTOR_1]==LSM$Entry[i],])-sum(is.na(data[data[,FACTOR_1]==LSM$Entry[i],CROP_OBSRVTN_DETAIL_ID]))
  }
  LSM$N<-N
  
  
  ##replace adjusted control name with actual control name
  if (exists("adjusted_control_name", envir = .GlobalEnv)){
    levels(LSM$Entry)[levels(LSM$Entry) == adjusted_control_name] <- control_level
  }
  return(LSM)
}


# TODO:
# Compute the correct degrees of freedom
#' Computes the deltas between the treatment means and associated p-values
#' @param RCB_asr A fitted ASReml object
#' @param total_df A number specifying the total degrees of freedom to be used in the p-value computations.
#' @return A dataframe with columns for head, check, difference between the mean, and associated p-value for all combinations of heads and checks.
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
  names( out_data ) <- c("head", "comp")
  out_data$diff <- as.vector( diffs_out$differences )
  out_data$p.diff <- as.vector( diffs_out$p.differences )
  out_data$sed <- as.vector( diffs_out$sed )
  out_data$df <- rep( total_df,nrow(out_data) ); out_data$df[is.na(out_data[,'p.diff'])] <- NA
  out_data$t <- as.vector( diffs_out$differences/diffs_out$sed )
  out_data$CI_L <- out_data$diff - qt(1 - alpha / 2, out_data$df) * out_data$sed
  out_data$CI_U <- out_data$diff + qt(1 - alpha / 2, out_data$df) * out_data$sed
  
  ## the diagonal shouldn't be NA, set it as p-value = 1
  diag(diffs_out$p.differences)=1
  # MSG
  # msg=agricolae::orderPvalue(treatment=RCB_asr$predictions$pvals[,1],means=RCB_asr$predictions$pvals[,2],alpha=alpha,pvalue=diffs_out$p.differences,console=F)
  msg=MSG(treatment=RCB_asr$predictions$pvals[,1],means=RCB_asr$predictions$pvals[,2],alpha=alpha,pvalue=diffs_out$p.differences,console=F)
  
  
  return(list('Delta_table'=out_data,'Mean Separation Grouping'=msg))
}

#' Function for computing the residual degrees of freedom.
#' @param RCB_asr A fitted ASReml object
#' @export
#' @importFrom asreml wald.asreml
computeDF <- function(RCB_asr, FACTOR_1){
  degrees_freedom <- wald.asreml( RCB_asr, maxiter = 1 )
  return( degrees_freedom[FACTOR_1, "Df"] )
}


#' ANOVA table
# for model P1,P2,P4

ANOVA_output <- function(asreml.obj,degrees_freedom){
  anova_<-list()
  
  anova_[[1]]=asreml::wald.asreml(asreml.obj)                        ## for fixed effect
  anova_[[2]]=summary(asreml.obj)$varcomp                      ## for random effect
  anova_[[1]]$denDF= c(degrees_freedom,'')
  
  for (j in 1:length(row.names(anova_[[2]]))) {
  row.names(anova_[[2]])[j] <- strsplit(row.names(anova_[[2]]),'!')[[j]][1]
  }
  row.names(anova_[[2]])[j] <- 'residual'
  
  names(anova_) <- c('fixed effect','random effect')
  return(anova_)  
  
}


# for model P3,P5
ANOVA_output_r <- function(asreml.obj){
 
  anova_=summary(asreml.obj)$varcomp                      ## for random effect
  
  for (j in 1:length(row.names(anova_))) {
    row.names(anova_)[j] <- strsplit(row.names(anova_),'!')[[j]][1]
  }
  row.names(anova_)[j] <- 'residual'
  
  return(anova_)  
  
}



resid_table <- function(asreml.obj,data){
  cbind(resid(asreml.obj),data[,FIELD_ID],data[,REP_ID])
}

LastC=function (x) 
{
  
  y <- sub(" +$", "0", x)
  p1 <- nchar(y)
  cc <- substr(y, p1, p1)
  while (cc=='.') {
  cc <- substr(y, p1, p1)
  if (cc != '.') break
  p1=p1-1
  }
  
  cc <- substr(y, p1, nchar(y))
  return(cc)
}


MSG=function (treatment, means, alpha, pvalue, console) 
{
  n <- length(means)
  z <- data.frame(treatment, means)
  letras <- c(letters[1:26], LETTERS[1:26],paste(letters,'.',sep=''),paste(LETTERS,'.',sep=''),paste(letters,'..',sep=''),
              paste(LETTERS,'..',sep=''),paste(letters,'...',sep=''),
              paste(LETTERS,'...',sep=''),paste(letters,'....',sep=''),paste(LETTERS,'....',sep=''))
  w <- z[order(z[, 2], decreasing = TRUE), ]
  M <- rep("", n)
  k <- 1
  k1 <- 0
  j <- 1
  i <- 1
  cambio <- n
  cambio1 <- 0
  chequeo = 0
  M[1] <- letras[k]
  q <- as.numeric(rownames(w))
  while (j < n) {
    chequeo <- chequeo + 1
    if (chequeo > n) 
      break
    for (i in j:n) {
      s <- pvalue[q[i], q[j]] > alpha
      if (s) {
        if (LastC(M[i]) != letras[k]) M[i] <- paste(M[i], letras[k], sep = "")
      }
      else {
        k <- k + 1
        cambio <- i
        cambio1 <- 0
        ja <- j
        for (jj in cambio:n) M[jj] <- paste(M[jj], "", 
                                            sep = "")
        M[cambio] <- paste(M[cambio], letras[k], sep = "")
        for (v in ja:cambio) {
          if (pvalue[q[v], q[cambio]] <= alpha) {
            j <- j + 1
            cambio1 <- 1
          }
          else break
        }
        break
      }
    }
    if (cambio1 == 0) 
      j <- j + 1
  }
  w <- data.frame(w, stat = M)
  w <- w[match(treatment,w$treatment),]
  trt <- as.character(w$treatment)
  means <- as.numeric(w$means)
  output <- data.frame(trt, means, M)
  output1 <- cbind(trt, means)
  rownames(output1) <- M
  for (i in 1:n) {
    if (console) 
      cat(rownames(output1)[i], "\t", output1[i, 1], "\t", 
          means[i], "\n")
  }
  invisible(output)
}
