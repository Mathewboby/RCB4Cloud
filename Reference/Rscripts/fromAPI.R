library(httr)
set_config(config(ssl_verifypeer = 0L))

library(jsonlite)
# client_id = "XXX"
# client_secret = "XXX"
# ping token function
# fromAPI <- function(get_output_ID){
get_ping_token <- function(client_id=NULL, client_secret=NULL, client_prod=FALSE)
{
  #Pron or non-prod
  amp_url <- "https://test.amp.monsanto.com/as/token.oauth2"
  if(client_prod == TRUE){amp_url <- "https://amp.monsanto.com/as/token.oauth2"}
  if(!is.null(client_id) | !is.null(client_secret)){
     ping <- httr::POST(
      amp_url,
      httr::add_headers('Content-Type'= 'application/x-www-form-urlencoded'),
      body = list(
        grant_type = "client_credentials",
        client_id = client_id,
        client_secret = client_secret
      ),
      encode = 'form'
    )
  }
  print(ping$status_code)
  if(ping$status_code==200){return(httr::content(ping)$access_token)}
  ping_token = get_ping_token(client_id,client_secret,FALSE)
  return(ping_token)
}


call_API <- function(outputID, access_token){
  ##  use for non-prod
  api_url = 'https://product360-np.ag/job-outputs/v1alpha/'
  # api_url = "https://velocity-np.ag/designanalysis/details/"
  api_call <- httr::GET(
    paste0(api_url, outputID),
    httr::add_headers(
      'Content-Type' = 'application/json',
      'authorization' = paste0("Bearer ", access_token)
    )
    #verbose(TRUE,TRUE)
  )
  ###print(api_call$status_code)
  if(api_call$status_code==200){return(httr::content(api_call))}
  return(results)
}

compareResults <- function(zResults){

  input.json <- zResults$modelOutputs[[1]]$rcbBlue$input$data
  input.list <- lapply(input.json, unlist)
  input.df   <- data.frame(do.call(rbind, input.list))

  input.df$repId                 <- as.character(input.df$repId)
  input.df$value                 <- as.numeric(as.character(input.df$value))
  # input.df$blockNum              <- as.character(input.df$blockNum)
  input.df$subSiteId             <- as.character(input.df$subSiteId)
  input.df$factorLevelId         <- as.character(input.df$factorLevelId)
  if(exists("entryId", where=input.df) == TRUE){
    input.df$entryId <- as.character(input.df$entryId)
  }else{
    input.df$entryId <- paste0('e',1:nrow(input.df))
  }
  input.df$isAnswerDeactivated   <- as.logical(input.df$isAnswerDeactivated)
  input.df$isSetEntryDeactivated <- as.logical(input.df$isSetEntryDeactivated)
  input.df$isPlaceHolder         <- as.logical(input.df$isPlaceHolder)
  input.df$questionCode          <- as.character(input.df$questionCode)
  input.df$locationId            <- as.character(input.df$locationId)
  # input.df$dsrOverride           <- as.logical(input.df$dsrOverride)

  output.json <- zResults$modelOutputs[[1]]$rcbBlue$results

  params.json <- zResults$modelOutputs[[1]]$rcbBlue$parameters

  param.test <- list(
    value = "value",
    setEntryId = "entryId",
    blockNum = "blockNum",
    subSiteId = "subSiteId",
    factorLevelId = "factorLevelId",
    repId = "repId",
    locationId = "locationId",
    isPlaceHolder = "isPlaceHolder",
    isDsrDeactivated = "isDsrDeactivated",
    isAnswerDeactivated = "isAnswerDeactivated",
    isSetEntryDeactivated = "isSetEntryDeactivated",
    alpha                   = 0.10,
    sufficientDataThreshold = 20)

  local.test <- RCB_ModelFittingFunction(input.df, param.test)

  merge.df <- merge(output.df, local.test, by = "entryId")

  merge.df <- merge.df[,order(names(merge.df))]

  return(merge.df)
}

get_API_Data <- function(zResults){
  input.json <- zResults$modelOutputs[[1]]$rcbBlue$input$data
  input.list <- lapply(input.json, unlist)
  input.df   <- data.frame(do.call(rbind, input.list))
  input.df$repId                 <- as.character(input.df$repId)
  input.df$value                 <- as.numeric(as.character(input.df$value))
  input.df$subSiteId             <- as.character(input.df$subSiteId)
  input.df$factorLevelId         <- as.character(input.df$factorLevelId)
  input.df$isAnswerDeactivated   <- as.logical(input.df$isAnswerDeactivated)
  input.df$isSetEntryDeactivated <- as.logical(input.df$isSetEntryDeactivated)
  input.df$isPlaceHolder         <- as.logical(input.df$isPlaceHolder)
  input.df$questionCode          <- as.character(input.df$questionCode)
  input.df$locationId            <- as.character(input.df$locationId)

  output.json <- zResults$modelOutputs[[1]]$rcbBlue$results

  params.json <- zResults$modelOutputs[[1]]$rcbBlue$parameters

  param.test <- list(
    value = "value",
    setEntryId = "entryId",
    blockNum = "blockNum",
    subSiteId = "subSiteId",
    factorLevelId = "factorLevelId",
    repId = "repId",
    locationId = "locationId",
    isPlaceHolder = "isPlaceHolder",
    isDsrDeactivated = "isDsrDeactivated",
    isQaqcDeactivated = "isQaqcDeactivated",
    isAnswerDeactivated = "isAnswerDeactivated",
    isSetEntryDeactivated = "isSetEntryDeactivated",
    alpha                   = 0.10,
    sufficientDataThreshold = 20)
  olst <- list(inputData   = input.df,
               outputData  = output.json,
               API_params  = params.json,
               Test_params = param.test)
  return(olst)
}

extractJobIds <- function(jobIdDf, jobIdColumnName){
  # In their raw form, job IDs are strings with invisible characters as their first and
  # last characters. This function takes an input data frame and a column name as a string.
  # It then converts the data frame column into a single vector of character strings, and
  # counts the number of characters in each string.
  # For all non-zero length strings, the 2nd through (nchar -1)th characters are extracted
  # as the actual job ID.
  rawJobIds          <- as.character(jobIdDf[, jobIdColumnName])
  rawJobIds          <- gsub(rawJobIds, pattern="(", replacement="", fixed=TRUE)
  rawJobIds          <- gsub(rawJobIds, pattern=")", replacement="", fixed=TRUE)
  numChars           <- nchar(rawJobIds)  # Number of character composing each string
  wNonMissing        <- which(numChars == 29)  # Find which strings have 29 characters
  # Extract the actual job ID from each string.
  if(length(wNonMissing) > 0){
    jobIdsToOutput <- unlist(lapply(as.list(wNonMissing),
                                    function(zi){substr(rawJobIds[zi], 2, numChars[zi] - 1)}))
  }else{
    jobIdsToOutput <- NA
  }

  return(jobIdsToOutput)
}

get_API_LsmTable <- function(zResults){
  # The following comments show the code trail to getting the data for this function.
  # The last two lines are what this function does.
  # anovaGlobalJobData <- read.csv("/repos/RCB4Cloud/Reference/Data/ValidationData/anova_global_job_output_ids.csv")
  # anovaGlobalJobIds  <- extractJobIds(anovaGlobalJobData, "job_output_id")
  # aG_results         <- call_API(anovaGlobalJobIds[1], ping_token)
  # agData01           <- get_API_Data(aG_results)
  # aglsm              <- as.data.frame(do.call(rbind, lapply(agData01$outputData$leastSquaredMeans, unlist)))  # Table of least-squares means for each level of factorLevelId
  Data01      <- get_API_Data(zResults)
  listOfLSMs  <- lapply(Data01$outputData$leastSquaredMeans, unlist)
  lsmDf       <- as.data.frame(do.call(rbind, listOfLSMs))
  lsmDf[,2:7] <- lapply(2:7, function(zi){as.numeric(as.character(lsmDf[,zi]))})
  return(lsmDf)
}

get_RCB_LsmTable <- function(zRcbOutput){
  rlsm <- zRcbOutput$blueTable
  rlsm[,2:7] <- lapply(2:7, function(zi){as.numeric(as.character(rlsm[,zi]))})
  return(rlsm)
}

get_API_DeltasTable <- function(zResults){
  # The following comments show the code trail to getting the data for this function.
  # The last two lines are what this function does plus rectify plus rectify output from treatment means minus themselves.
  # anovaGlobalJobData <- read.csv("/repos/RCB4Cloud/Reference/Data/ValidationData/anova_global_job_output_ids.csv")
  # anovaGlobalJobIds  <- extractJobIds(anovaGlobalJobData, "job_output_id")
  # aG_results         <- call_API(anovaGlobalJobIds[1], ping_token)
  # agData01           <- get_API_Data(aG_results)
  # agdlt              <- as.data.frame(do.call(rbind, lapply(agData01$outputData$deltas, unlist)))  # Table of least-squares means for each level of factorLevelId
  Data01       <- get_API_Data(zResults)
  listOfDeltas <- lapply(Data01$outputData$deltas, unlist)
  deltaDf      <- as.data.frame(do.call(rbind, listOfDeltas))
  # Begin rectificaiton of self-differences
  dltw0 <- which(deltaDf$head == deltaDf$comparison)
  # Ensure needed columns are numeric
  n_col <- ncol(deltaDf)
  deltaDf[,3:n_col] <- lapply(3:n_col, function(zi){as.numeric(as.character(deltaDf[,zi]))})
  # Correct output from self-differences
  deltaDf$pValueDifference[dltw0]        <- 1
  deltaDf$standardErrorDifference[dltw0] <- 0
  deltaDf$degreesOfFreedom[dltw0]        <- 0
  deltaDf$tValue[dltw0]                  <- 0
  deltaDf$lowerConfidenceInterval[dltw0] <- 0
  deltaDf$upperConfidenceInterval[dltw0] <- 0
  return(deltaDf)
}

get_RCB_DeltasTable <- function(zRcbOutput){
 deltaDf <- zRcbOutput$deltas
 dltw0   <- which(deltaDf$head == deltaDf$comparison)
 # Ensure needed columns are numeric
 n_col             <- ncol(deltaDf)
 deltaDf[,3:n_col] <- lapply(3:n_col, function(zi){as.numeric(as.character(deltaDf[,zi]))})
 # Correct output from self-differences
 deltaDf$probabilityDifferences[dltw0]   <- 1
 deltaDf$standardErrorDifferences[dltw0] <- 0
 deltaDf$degreesFreedom[dltw0]           <- 0
 deltaDf$t[dltw0]                        <- 0
 deltaDf$lowerConfidenceInterval[dltw0]  <- 0
 deltaDf$upperConfidenceInterval[dltw0]  <- 0
 return(deltaDf)
}

lsmCompare <- function(zLsm1, zLsm2, ndigits = 10){
  zcomp        <- lapply(2:7, function(zi){sum(round(abs(as.numeric(zLsm1[, zi]) -
                                                         as.numeric(zLsm2[, zi])), ndigits))})
  names(zcomp) <- names(zLsm1)[2:7]
  return(unlist(zcomp))
}

deltasCompare <- function(zDeltas1, zDeltas2, ndigits = 10){
  n_cols       <- ncol(zDeltas1)
  zcomp        <- lapply(3:n_cols, function(zi){sum(round(abs(as.numeric(as.character(zDeltas1[, zi])) -
                                                              as.numeric(as.character(zDeltas2[, zi]))), ndigits))})
  names(zcomp) <- names(zDeltas1)[3:n_cols]
  return(unlist(zcomp))
}

get_API_VarCompTable <- function(zResults){
  zInitialTable    <- zResults$modelOutputs[[1]]$rcbBlue$results$varianceComponents
  zInitialTable2   <- as.data.frame(do.call(rbind, lapply(zInitialTable, unlist)))
  zFinalTable      <- zInitialTable2[, c(3,1,2)]
  zFinalTable[, 2] <- as.numeric(as.character(zFinalTable[, 2]))
  return(zFinalTable)
}

get_RCB_VarCompTable <- function(zResults){
  zInitialTable                   <- zResults$varianceComposition
  zInitialTable$varianceComponent <- rownames(zInitialTable)
  rownames(zInitialTable)         <- NULL
  zFinalTable                     <- zInitialTable[, c(3,1,2)]
  zFinalTable$varianceEstimates   <- round(as.numeric(zFinalTable$varianceEstimates), 10)
  return(zFinalTable)
}

varianceCompare <- function(zVarComp1, zVarComp2, ndigits=10){
  return(sum(round(abs(as.numeric(zVarComp1$varianceEstimates) -
                       as.numeric(zVarComp2$varianceEstimates)), ndigits)))
}

get_API_AovTable <- function(zResults){
  zRawTable               <- zResults$modelOutputs[[1]]$rcbBlue$results$analysisOfVariance
  zFinalTable             <- as.data.frame(do.call(rbind, lapply(zRawTable, unlist)))
  zFinalTable[, c(1:4,6)] <- lapply(c(1:4,6), function(zi){as.numeric(as.character(zFinalTable[, zi]))})
  rownames(zFinalTable)   <- NULL
  return(zFinalTable)
}

get_RCB_AovTable <- function(zResults){
  zFinalTable             <- zResults$anova
  zFinalTable[, c(1:4,6)] <- lapply(c(1:4,6), function(zi){as.numeric(zFinalTable[, zi])})
  zFinalTable$row         <- rownames(zFinalTable)
  rownames(zFinalTable)   <- NULL
  return(zFinalTable)
}

aovCompare <- function(zAov1, zAov2, ndigits = 10){
  zcomp        <- lapply(c(1:4,6), function(zi){sum(round(abs(as.numeric(zAov1[, zi]) -
                                                         as.numeric(zAov2[, zi])), ndigits))})
  names(zcomp) <- names(zAov1)[c(1:4,6)]
  return(unlist(zcomp))
}

get_API_AnalysisType <- function(API_varCompTable){
  nr <- nrow(API_varCompTable)
  if(nr > 2){  # Two blocking factor RCBD1
    analysisType <- "P4"
  }else{       # One blocking factor RCBD1: name of blocking factor determines the analysis type
    if(       API_varCompTable$row[1] == "repId"){
      analysisType <- "P1"
    }else{  # API_varCompTable$row[1] == "subSiteId"
      analysisType <- "P2"
    }
  }
  return(analysisType)
}

aovAPIvsRCB <- function(zResults, ndigits=10){
  apiData         <- get_API_Data(zResults)
  apiVarComp      <- get_API_VarCompTable(zResults)
  apiAnalysisType <- get_API_AnalysisType(apiVarComp)
  RCB_Output      <- RCB_ModelFittingFunction(apiData$inputData,
                                              apiData$Test_params,
                                              apiAnalysisType)
  apiLSM     <- get_API_LsmTable(zResults)
  rcbLSM     <- get_RCB_LsmTable(RCB_Output)
  compareLSM <- lsmCompare(apiLSM, rcbLSM, ndigits=ndigits)

  apiDeltas     <- get_API_DeltasTable(zResults)
  rcbDeltas     <- get_RCB_DeltasTable(RCB_Output)
  compareDeltas <- deltasCompare(apiDeltas, rcbDeltas, ndigits=ndigits)

  apiVarComp     <- get_API_VarCompTable(zResults)
  rcbVarComp     <- get_RCB_VarCompTable(RCB_Output)
  compareVarComp <- varianceCompare(apiVarComp, rcbVarComp, ndigits=ndigits)

  apiAnova     <- get_API_AovTable(zResults)
  rcbAnova     <- get_RCB_AovTable(RCB_Output)
  compareAnova <- aovCompare(apiAnova, rcbAnova, ndigits=ndigits)

  apiLSD     <- as.numeric(apiData$outputData$leastSignificantDifference)
  rcbLSD     <- as.numeric(RCB_Output$leastSignificantDifference)
  compareLSD <- round(abs(apiLSD-rcbLSD), ndigits)

  olst <- list(apiLSM         = apiLSM,
               rcbLSM         = rcbLSM,
               compareLSM     = compareLSM,
               apiDeltas      = apiDeltas,
               rcbDeltas      = rcbDeltas,
               compareDeltas  = compareDeltas,
               apiVarComp     = apiVarComp,
               rcbVarComp     = rcbVarComp,
               compareVarComp = compareVarComp,
               apiAnova       = apiAnova,
               rcbAnova       = rcbAnova,
               compareAnova   = compareAnova,
               compareLSD     = compareLSD)

}

compareAPIandRCBoutputs <- function(zResults, RCB_Output, ndigits=10){
  # apiData         <- get_API_Data(zResults)
  # apiVarComp      <- get_API_VarCompTable(zResults)
  # apiAnalysisType <- get_API_AnalysisType(apiVarComp)
  reorderApiColNames <- c("treatment", "numValue", "standardError", "degreesOfFreedom",
                   "sampleSize", "lowerConfidenceInterval", "upperConfidenceInterval",
                   "meanSeparationGroup")
  apiLSM     <- zResults$leastSquaredMeans[[1]][, reorderApiColNames]
  rcbLSM     <- get_RCB_LsmTable(RCB_Output)
  compareLSM <- lsmCompare(apiLSM, rcbLSM, ndigits=ndigits)

  apiDeltasColOrder <- c("head", "comparison", "differences", "standardErrorDifference",
                         "lowerConfidenceInterval", "upperConfidenceInterval",
                           "tValue", "degreesOfFreedom", "pValueDifference")
  rcbDeltasColOrder <- c("head", "comparison", "differences", "standardErrorDifferences",
                         "lowerConfidenceInterval", "upperConfidenceInterval",
                         "t", "degreesFreedom", "probabilityDifferences")
  apiDeltas     <- as.data.frame(do.call(cbind, lapply(zResults$deltas[[1]][, apiDeltasColOrder], as.numeric)))
  rcbDeltas     <- as.data.frame(do.call(cbind, lapply(RCB_Output$deltas[, rcbDeltasColOrder], as.numeric)))
  compareDeltas <- apply(round(apiDeltas[, 3:9] - rcbDeltas[, 3:9], ndigits), 2, function(zx){sum(zx, na.rm=TRUE)})
  #compareDeltas <- deltasCompare(apiDeltas, rcbDeltas, ndigits=ndigits)

  apiVarComp     <- zResults$varianceComponents[[1]][, c(3,1,2)]
  rcbVarComp     <- RCB_Output$varianceComposition
  rcbVarComp$row <- rownames(rcbVarComp)
  rcbVarComp           <- rcbVarComp[, c(3,1,2)]
  rownames(rcbVarComp) <- NULL
  compareVarComp       <- varianceCompare(apiVarComp, rcbVarComp, ndigits=ndigits)

  apiAnova     <- do.call(cbind, lapply(zResults$analysisOfVariance[[1]][, c(5, 2, 7, 4, 6)], as.numeric))
  rcbAnova     <- do.call(cbind, lapply(RCB_Output$anova[, c(1:4, 6)], as.numeric))
  compareAnova <- apply(round(apiAnova - rcbAnova, ndigits), 2, function(zx){sum(zx, na.rm=TRUE)})
  #compareAnova <- aovCompare(apiAnova, rcbAnova, ndigits=8)

  apiLSD     <- as.numeric(zResults$leastSignificantDifference)
  rcbLSD     <- as.numeric(RCB_Output$leastSignificantDifference[1])
  compareLSD <- round(abs(apiLSD-rcbLSD), ndigits)

  olst <- list(apiLSM         = apiLSM,
               rcbLSM         = rcbLSM,
               compareLSM     = compareLSM,
               apiDeltas      = apiDeltas,
               rcbDeltas      = rcbDeltas,
               compareDeltas  = compareDeltas,
               apiVarComp     = apiVarComp,
               rcbVarComp     = rcbVarComp,
               compareVarComp = compareVarComp,
               apiAnova       = apiAnova,
               rcbAnova       = rcbAnova,
               compareAnova   = compareAnova,
               compareLSD     = compareLSD)

}

summaryCompare <- function(zAovComp){
  return( sapply(lapply(zAovComp[grep("^compare",names(zAovComp))], unlist), sum) )
}

#
##
###
####
#####







