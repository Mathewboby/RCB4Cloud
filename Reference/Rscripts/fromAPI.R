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
  print(api_call$status_code)
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
  # Intheir raw form, job IDs are strings with invisible characters as their first and
  # last characters. This function takes an input data frame and a column name as a string.
  # It then converts the data frame column into a single vector of character strings, and
  # counts the number of characters in each string.
  # For all non-zero length strings, the 2nd through (nchar -1)th characters are extracted
  # as the actual job ID.
  rawJobIds          <- as.character(jobIdDf[, jobIdColumnName])
  numChars           <- nchar(rawJobIds)  # Number of character composing each string
  wNonMissing        <- which(numChars >= 1)  # Find which strings have 1 or more characters
  # Extract the actual job ID from each string.
  if(length(wNonMissing) > 0){
    jobIdsToOutput     <- unlist(lapply(as.list(wNonMissing),
                                        function(zi){substr(rawJobIds[zi], 2, numChars[zi] - 1)}))
  }else{
    jobIdsToOutput <- NA
  }

  return(jobIdsToOutput)
}








