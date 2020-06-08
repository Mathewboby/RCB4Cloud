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
#'
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
#'
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

# Calculate lsm results
  ALL=asreml::wald.asreml(asreml.obj, denDF ="default", ssType= "conditional",data=data)

  ## for degree of freedom
  DDoF_ <- ALL[[1]][,2]
  DDoF <- DDoF_[2]

  ## for variance component table
  VAR=summary(asreml.obj)$varcomp

  for (j in 1:length(rownames(VAR))) {
      rownames(VAR)[j] <- strsplit(rownames(VAR),'!')[[j]][1]
  }

  rownames(VAR)[length(rownames(VAR))] <- "residual"

  VAR=VAR[,c(2,5)]
  colnames(VAR) <- c("varianceEstimates","constraint")

  ## for fix effect ANOVA table
  AOV <- ALL[[1]]
  colnames(AOV) <- c("degreesFreedomNumerator","degreesFreedomDenominator","ssIncremental","ssConditional","margin","probability")

  ## for LSM table
  LSM <- data.frame(
    factorLevelId  = asreml.obj$predictions$pvals[[FACTOR_1]],
    value          = asreml.obj$predictions$pvals$predicted.value,
    standardError  = asreml.obj$predictions$pvals$standard.error,
    degreesFreedom = DDoF
  )

  ## count N in each Entry level with missing removed.
  spdata <- split(data, data[,FACTOR_1])
  zcnt   <- sapply(spdata, nrow)
  zisna  <- sapply(spdata, function(zy){sum(is.na(zy))})
  zdf    <- data.frame(factorLevelId=names(spdata),count=(zcnt-zisna))
  LSM    <- merge(LSM, zdf, by="factorLevelId")

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
  spdata <- split(data, data[,FACTOR_1])
  zcnt   <- sapply(spdata, nrow)
  zisna  <- sapply(spdata, function(zy){sum(is.na(zy))})
  zdf    <- data.frame(factorLevelId=names(spdata),count=(zcnt-zisna))
  LSM    <- merge(LSM, zdf, by="factorLevelId")


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
#' @export
#'
deltaAnalysis <- function(RCB_asr, alpha, total_df){

  test_diffs <- alldiffs ( predictions = RCB_asr$predictions$pvals,   #asremlPlus::alldiffs ( predictions = RCB_asr$predictions$pvals,
                           sed = RCB_asr$predictions$sed,
                           tdf = total_df )

  # There is an error/mistake/bug between these two steps. In the above data
  # the factor names can be numeric, but treated as characters and ordered in
  # alphabetic order. When the predictiondiffs.asreml is called, it keeps
  # the data in thet same order, but renames the rows and columns in purely
  # numeric order, thus giving incorrect values in the tables. This can be
  # resolved by renaming the rows/columns using the order of the names in
  # the alldiffs function.

  diffs_out <- predictiondiffs.asreml( classify     = FACTOR_1,
                                       alldiffs.obj = test_diffs ,
                                       alpha        = alpha )

  # Fix the potential re-ordering issue
  correct_order_names                 <- as.character( diffs_out$predictions[,1] )
  rownames( diffs_out$differences )   <- colnames( diffs_out$differences )   <- correct_order_names
  rownames( diffs_out$p.differences ) <- colnames( diffs_out$p.differences ) <- correct_order_names
  rownames( diffs_out$sed )           <- colnames( diffs_out$sed )           <- correct_order_names

  # Output the data as a data table
  out_data                          <- merge( correct_order_names, correct_order_names )
  names( out_data )                 <- c("head", "comparison")
  out_data$differences              <- as.vector( diffs_out$differences )
  out_data$probabilityDifferences   <- as.vector( diffs_out$p.differences )
  out_data$standardErrorDifferences <- as.vector( diffs_out$sed )
  out_data$degreesFreedom           <- rep( total_df,nrow(out_data) );
  out_data$degreesFreedom[is.na(out_data[,'probabilityDifferences'])] <- NA
  out_data$t                       <- as.vector( diffs_out$differences/diffs_out$sed )
  out_data$lowerConfidenceInterval <- out_data$differences - qt(1 - alpha / 2, out_data$degreesFreedom) * out_data$standardErrorDifferences
  out_data$upperConfidenceInterval <- out_data$differences + qt(1 - alpha / 2, out_data$degreesFreedom) * out_data$standardErrorDifferences

  ## the diagonal shouldn't be NA, set it as p-value = 1
  diag(diffs_out$p.differences)    <- 1
  msg                              <- MSG(diffs_out$p.differences,alpha)

  return(list('Delta_table'              = out_data,
              'Mean Separation Grouping' = msg,
              'LSD'                      = diffs_out$LSD$meanLSD))
}

#' Computes the variance component table for model P3 and P5
#' @param asreml.obj A fitted ASReml object
#' @return A dataframe with variance component estimates
#' @export
#'
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

MSG <- function(symmetric_matrix_of_difference_p.values, alpha_level) {
  # Given a symmetric matrix of difference p-values and an alpha-level find all the groups of differences (called cliques)
  # which are ALL not significantly different from each other pairwise.  A clique is a group of node points in an
  # undirected graph which are all connected to each other.  In a statistical testing context, these are statistics,
  # such as means, which are ALL not different from each other when compared pairwise. There are NO statistics which are
  # equal to some but not all of the other statistics in the clique.  The p-values are interpreted as the importance,
  # strength or length of the graph edges connecting the pairs of statistics.  Thresholding the p-values at the alpha-level
  # creates a binary image whose true points are treated as the nodes in an undirected graph. The blobs or clusters of TRUE
  # points make up cliques and the maximal cliques are the cliques which cannot be made larger by adding additional points.
  # This algorithm does rest upon the assumption that thresholding the matrix p-values at the alpha-level does not create
  # a binary image at or near a state of percolation.

  # Since a statistic is equal to itself, the p-value of the comparison is 1.0.  However, in this funtion
  # graph-theoretic methods are used to cluster statistics which are not significantly different from each other.
  # The p-values arranged into a grid are the edge lengths between the statistics represented by the rows
  # and columns.  The diagonal of such a grid or matrix would logically be 1.0, but the methods used here
  # will remove all loop-edges or edges between a statistic and itself to produce a simple graph.
  # Setting the diagonal elements to zero will remove the need to remove loop edges.  Then the only edges
  # needing removal will be the multiple-edges, edges having the same end points or pairs of nodes connected
  # by more than one edge.

  diag(symmetric_matrix_of_difference_p.values) <- 1

  # threshold the p-values and multiply by 2 to create the adjacency matrix.
  # The row and column coordinates of the edges longer than alpha define the
  # adjacency graph, which is considered an undirected graph.

  pValue_adjacency_matrix  <- 2*(symmetric_matrix_of_difference_p.values > alpha_level)

  column_names_of_input_matrix <- colnames(symmetric_matrix_of_difference_p.values)
  nrows_pvalue_matrix          <- nrow(symmetric_matrix_of_difference_p.values)
  # The rows and columns of the adjacency matrix need to have names
  if(is.null(column_names_of_input_matrix) == TRUE){
    column_names_of_input_matrix <- sortable_string_integers(nrows_pvalue_matrix, prefix="T")
  }
  rownames(pValue_adjacency_matrix) <- colnames(pValue_adjacency_matrix) <- column_names_of_input_matrix

  pValue_adjacency_matrixG     <- igraph::graph_from_adjacency_matrix(pValue_adjacency_matrix, mode='undirected')  # Igraph form of adjacency matrix
  simplified_adjacency_matrixG <- igraph::simplify(pValue_adjacency_matrixG)  # Remove loops and multiple edges
  cliq_node_names0             <- igraph::maximal.cliques(simplified_adjacency_matrixG)  # Find all maximal cliques
  cliq_node_names              <- lapply(cliq_node_names0, function(zi){column_names_of_input_matrix[zi]})

  # The maximal cliques are clusters of large connected edges (non-significant differences) such that all
  # of the nodes represented in the clique are connected to all of the other nodes in the clique by just one edge.
  # From a statistical comparison point of view, a maximal clique is a collection of statistics which are all
  # considered to be the same as each other.  Every member of the clique is equal to every other member of
  # the clique.  Statistics which are the same as some but not all of the members of the clique are excluded.

  # Create the letters for marking the mean separation grouping.  These are the potential clique names.
  zOutput      <- rep("",nrows_pvalue_matrix)
  clique_names <- c(letters, LETTERS, paste(letters,'.',sep=''), paste(LETTERS,'.',sep=''))

  # Check which list number contains this column name
  column_name_in_clique_checker <- function(clique_names){column_names_of_input_matrix[ind] %in% clique_names}
  for_loop_range                <- 1:nrows_pvalue_matrix
  for (ind in for_loop_range){
    column_name_is_in_clique            <- sapply(cliq_node_names, column_name_in_clique_checker)  # Returns a logical vector
    indices_of_cliques_with_column_name <- which(column_name_is_in_clique == TRUE)  # Vector of indices of TRUE cases

    if(length(indices_of_cliques_with_column_name) > 0){
      zOutput[ind] <- paste0(clique_names[indices_of_cliques_with_column_name], collapse = "")  # Combine the clique names of the cliques containing the p-value matric column name
    }else{
      zOutput[ind] <- "_"
    }
  }
  return(zOutput)
}

sortable_string_integers <- function(zn0, prefix="", suffix=""){
  # This function zero-pads integers and converts them into strings.  The zero-padding
  # allows the string version of the integers to sort into their numerical order.  This
  # is useful to naming columns of data frames and matrices.  It is also useful for creating
  # factor level labels.
  # zn0 is usually an integer indicating the highest number needed. It can be a vector of integers or even a vector of numbers in string format.
  # prefix and suffix are empty strings by default but can be given string inputs to add to the string numbers.
  if(is.character(zn0) == TRUE){
    zn <- as.integer(zn0)
  }else{
    zn <- zn0
  }
  if (length(zn) == 1 & max(zn) < 10){
    return(paste0( prefix, 1:zn, suffix))
  }
  if (length(zn) > 1 & max(zn)<10){
    return(paste0(prefix, zn, suffix))
  }

  if (length(zn) > 1){
    vector_of_integers <- zn
  } else{
    vector_of_integers <- 1:zn
  }
  vector_of_integers_as_strings          <- as.character(vector_of_integers)
  number_of_characters_per_integer       <- nchar(vector_of_integers_as_strings)
  max_number_of_characters               <- max(number_of_characters_per_integer)
  number_of_zeros_needed_for_padding     <- max_number_of_characters - number_of_characters_per_integer
  zero_padding_groups_of_integer_strings <- split(vector_of_integers_as_strings, number_of_zeros_needed_for_padding)
  number_of_zeros_for_padding            <- as.numeric(names(zero_padding_groups_of_integer_strings))
  func01 <- function(zx,zy){
    if(zy > 0){  # If the number of 0s needed for padding > 0
      po <- paste(rep("0",zy), collapse="") # Create a string of "0"s for padding
      zo <- paste0(po,zx)  # Zero-pad the integer string by prepending the string of "0"s
    } else{  # else leave the integer string as is
      zo <- zx
    }
    return(zo)
  }
  list_of_zero_padding_integer_strings  <- mapply(func01, zero_padding_groups_of_integer_strings, as.list(number_of_zeros_for_padding))
  unsplit_integer_strings               <- unsplit(list_of_zero_padding_integer_strings, number_of_zeros_needed_for_padding)
  return(paste0(prefix, unsplit_integer_strings, suffix))
}

MSG_original <- function(P,alpha) {
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


##############################################################################################
# Add 7 needed asreml functions: asremlPlus::alldiffs and asremlPlus::predictiondiffs.asreml
# asremlPlus::fac.getinTerm, asremlPlus::getTermVars, asremlPlus::fac.formTerm,
# asremlPlus::separateFunction, and asremlPlus::rmFunction.
# This is done in order to avoid having to load the asremlPlus package and its dependencies.
# This will eliminate version incompatibilities.  This aaproach successfully helped smartQAQC.
# This makes aour code more self-contained.  The two functions are long but basic.  It is
# likely these two functions are very mature and not likely to change in the future.
# These two functions are from asremlPlus_2.0-12.tar.gz, which was last modified 2016-09-16 12:50	417K
# This package can be found at http://cran.wustl.edu//src/contrib/Archive/asremlPlus.  Note
# asremlPlus requires the asreml, DAE and ggplot2 to be loaded first.  the Version of dae compatible
# this version of asremlPlus is 2.7-20.  The following code was used to load all needed package into
# the environment in which they were used.
# library(lattice)
# library(asreml)
# library(ggplot2)
# daeURL <- "http://cran.wustl.edu//src/contrib/Archive/dae/dae_2.7-20.tar.gz"
# suppressMessages(install.packages(daeURL,repos=NULL,type="source"))
# suppressMessages(library(dae, quietly=TRUE))
# asremlPlusURL <- "http://cran.wustl.edu//src/contrib/Archive/asremlPlus/asremlPlus_2.0-12.tar.gz"
# suppressMessages(install.packages(asremlPlusURL,repos=NULL,type="source"))
# suppressMessages(library(asremlPlus, quietly=TRUE))
##############################################################################################

"alldiffs" <- function(predictions, differences = NULL, p.differences = NULL,
                       sed = NULL, LSD = NULL, backtransforms = NULL,
                       response = NULL, response.title = NULL,
                       term = NULL, classify = NULL,
                       tdf = NULL)
{ #Check arguments
  if (!("predicted.value" %in% colnames(predictions)) ||
      !("standard.error" %in% colnames(predictions)) || !("est.status" %in% colnames(predictions)))
    warning("Predictions argument does not include the expected column names (e.g. predicted.value)")
  npred <- nrow(predictions)
  if ((!is.null(differences) && !("matrix" %in% class(differences))) ||
      (!is.null(p.differences) && !("matrix" %in% class(p.differences))) ||
      (!is.null(sed) && !("matrix" %in% class(sed))))
    warning("At least one of differences, p.differences and sed is not of type matrix")
  if (!is.null(differences) && !is.null(p.differences) && !is.null(sed))
  { dimens <- c(nrow(differences), nrow(p.differences), nrow(sed),
                ncol(differences), ncol(p.differences), ncol(sed))
  if (any(npred != dimens))
    stop("At least one of differences, p.differences or sed is not conformable with predictions")
  }
  if (!is.null(backtransforms))
  { if (!("backtransformed.predictions" %in% colnames(backtransforms)))
    warning("Backtransforms argument does not include a column named backtransformed.predictions")
    if (npred != nrow(backtransforms))
      stop("Backtransforms do not contain the same number of rows as the predictions")
  }
  #ensure diag of sed is NA
  if (!is.null(sed))
    diag(sed) <- NA
  meanLSD <- NULL
  if (!is.null(LSD))
    attr(predictions, which = "meanLSD") <- LSD$meanLSD
  p <- list(predictions = predictions, differences = differences,
            p.differences = p.differences, sed = sed, LSD = LSD,
            backtransforms = backtransforms)
  attr(p, which = "response") <- response
  attr(p, which = "response.title") <- response.title
  attr(p, which = "term") <- term
  attr(p, which = "classify") <- classify
  attr(p, which = "tdf") <- tdf
  class(p) <- "alldiffs"
  return(p)
}

###########################################################################

"predictiondiffs.asreml" <- function(classify, alldiffs.obj,
                                     x.num = NULL, x.fac = NULL,
                                     levels.length = NA,
                                     pairwise = TRUE, alpha = 0.05,
                                     inestimable.rm = TRUE)
  #a function to get a table of asreml predictions and associated statistics
  #  for all pairwise differences
{ #Check alldiffs.obj
  if (!("alldiffs" %in% class(alldiffs.obj)))
    stop("Alldiffs.obj is not of class alldiffs")
  if (is.null(alldiffs.obj$predictions))
    stop("No predictions supplied in alldiffs.obj")
  if (is.null(alldiffs.obj$sed))
    stop(paste("No sed supplied in alldiffs.obj \n",
               "- can obtain using sed=TRUE in predict.asreml"))
  predictions <- alldiffs.obj$predictions
  #Retain only estimable predictions
  which.estim <- (predictions$est.status == "Estimable")
  if (inestimable.rm & sum(which.estim) != nrow(predictions))
  { predictions <- predictions[which.estim, ]
  if (nrow(predictions) == 0)
    warning("There are no estimable predictions")
  #Make sure all factors have only observed levels
  predictions[1:ncol(predictions)] <-
    lapply(1:ncol(predictions),
           function(k, data)
           { if (is.factor(data[[k]]))
             data[[k]] <- factor(data[[k]])
           return(data[[k]])
           }, predictions)
  alldiffs.obj$predictions <- predictions
  if (!is.null(alldiffs.obj$sed))
  { if (inestimable.rm)
    alldiffs.obj$sed <- alldiffs.obj$sed[which.estim, which.estim]
  diag(alldiffs.obj$sed) <- NA
  }
  #Reset the other components to NULL
  alldiffs.obj$difference <- NULL
  alldiffs.obj$p.difference <- NULL
  alldiffs.obj$LSD <- NULL
  attr(alldiffs.obj, which = "meanLSD") <- NULL
  }

  response <- as.character(attr(alldiffs.obj, which = "response"))
  #Form all pairwise differences, if not present and to be stored
  if (is.null(alldiffs.obj$differences) & pairwise)
  {  #determine factors for row and columns name
    #Make sure no functions in classify
    factors <- fac.getinTerm(classify, rmfunction = TRUE)
    classify <- fac.formTerm(factors)
    nfac <- length(factors)
    #Check all factors in classify are in predictions
    if (length(setdiff (factors, names(predictions))) != 0)
    { if (!is.null(response))
      stop("For ",response,
           ", there are factors in the classify argument that do not have columns in alldiffs.obj$predictions")
      else
        stop("There are factors in the classify argument that do not have columns in alldiffs.obj$predictions")
    }
    #Make sure only one of the numeric and factor that are parallel
    if ((!is.null(x.num) && x.num %in% factors) && (!is.null(x.fac) && x.fac %in% factors))
    { k <- match(x.num, names(predictions))
    predictions <- predictions[, -k]
    nfac <- nfac - 1
    }
    pred.diff <- outer(predictions$predicted.value, predictions$predicted.value, "-")
    #Generate row and column names as the combinations of the levels of factors
    if (nfac > 1)
    { pred.faclist <- vector("list", length=nfac)
    nclassify <- ncol(predictions) - 3
    pred.names <- names(predictions)
    kk <- 0
    for (k in 1:nclassify)
    { if (pred.names[k] %in% factors)
    { kk <- kk + 1
    pred.faclist[[kk]] <- predictions[[k]]
    if (is.numeric(pred.faclist[[kk]]))
      pred.faclist[[kk]] <- factor(pred.faclist[[kk]])
    names(pred.faclist)[kk] <- pred.names[k]
    }
    }
    pred.faclist <- lapply(pred.faclist, FUN=function(ff){if (is.character(levels(ff)) && !is.na(levels.length))
      ff <- factor(ff, labels=substr(levels(ff), start=1, stop=levels.length))
    invisible(ff)})
    pred.lev <- levels(fac.combine(pred.faclist, combine.levels=TRUE))
    }
    else
    { k <- match(factors[[1]], names(predictions))
    pred.fac <- predictions[[k]]
    if (is.numeric(pred.fac))
      pred.fac <- factor(pred.fac)
    pred.lev <- levels(pred.fac)
    if (is.character(pred.lev) && !is.na(levels.length))
      pred.lev <- substr(pred.lev, start=1, stop=levels.length)
    }
    if (nrow(alldiffs.obj$sed) != nrow(pred.diff) |
        ncol(alldiffs.obj$sed) != ncol(pred.diff))
      stop("The matrix of pairwise differences and sed are not conformable")
    if (ncol(alldiffs.obj$sed) != length(pred.lev) | nrow(alldiffs.obj$sed) != length(pred.lev))
      stop(paste("Dimensions of differences and sed not equal to \n",
                 "the number of observed levels combinations of the factors"))
    dimnames(pred.diff) <- list(pred.lev, pred.lev)
    dimnames(alldiffs.obj$sed) <- list(pred.lev, pred.lev)
  } else
  { if (!pairwise)
  { alldiffs.obj["differences"] <- list(NULL)
  alldiffs.obj["p.differences"] <- list(NULL)
  }
  }
  #Check if tdf available
  denom.df <- attr(alldiffs.obj, which = "tdf")
  if (is.null(denom.df))
    warning(paste("The degrees of freedom of the t-distribtion are not available in alldiffs.obj\n",
                  "- p-values and LSDs not calculated"))
  else
  { #calculate p-values, if not present
    if (is.null(alldiffs.obj$p.differences) & pairwise)
    { p.diff <- abs(pred.diff)/alldiffs.obj$sed
    p.diff <- 2*pt(p.diff, df = denom.df, lower.tail = FALSE)
    alldiffs.obj$differences <- pred.diff
    alldiffs.obj$p.differences <- p.diff
    }
    #calculate LSDs, if not present
    if (is.null(alldiffs.obj$LSD) & pairwise)
    { t.value = qt(1-alpha/2, denom.df)
    minLSD <- t.value * min(alldiffs.obj$sed, na.rm = TRUE)
    maxLSD <- t.value * max(alldiffs.obj$sed, na.rm = TRUE)
    meanLSD <- t.value * mean(alldiffs.obj$sed, na.rm = TRUE)
    alldiffs.obj$LSD <- data.frame(minLSD  = minLSD,
                                   meanLSD = meanLSD,
                                   maxLSD = maxLSD)
    attr(alldiffs.obj, which = "meanLSD") <- meanLSD
    }
  }
  return(alldiffs.obj)
}

##########################################################################################

"fac.getinTerm" <- function(term, rmfunction=FALSE)
  #function to return the set of factors/variables in a term separated by ':"
{ if (length(term) != 1)
  stop("Multiple terms supplied where only one allowed")
  vars <- unlist(strsplit(term, ":", fixed=TRUE))
  if (rmfunction)
    vars <- unlist(lapply(vars, rmFunction))
  return(vars)
}

##########################################################################################

"getTermVars" <- function(term)
  #This function gets the vars in each random term from an asreml termlist
  #It strips off stuff to the right of an !, provided it is not to the right of
  #a term starting with R!
{ if(substr(term, 1, 2) != "R!")  term <- rmTermDescription(term)
vars <- fac.getinTerm(term)
return(vars)
}

##########################################################################################

"fac.formTerm" <- function(factors)
  #function to form a term from a set of factors/variables in a structure
{ term <- paste(factors,collapse=":")
return(term)
}

##########################################################################################

"separateFunction" <- function(var)
  #A function to separate the name of a function and the argument to the function
{ #Remove description, if there is one, from term in an asreml termlist
  if (length(grep("(", var, fixed=TRUE))!=0)
  { var <- (strsplit(var, "(", fixed=TRUE) )[[1]]
  var[2] <- (strsplit(var[2], ")", fixed=TRUE) )[[1]][1]
  }
  return(var)
}

##########################################################################################

"rmFunction" <- function(var, asreml.obj)
  #A function that returns the variable without any function
{ var <- separateFunction(var)
if (length(var)==2)
{ var <- var[2]
#Check for further arguments and strip, if found
if (length(grep(",", var, fixed=TRUE))!=0)
{ var <- (strsplit(var, ",", fixed=TRUE) )[[1]]
var <- var[1]
}
}
return(var)
}

