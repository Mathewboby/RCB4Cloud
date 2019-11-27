RCB_Fitter <- function(DataIn, params.input, analysisType){

  # Either read in the parameters from a file or use the default inputs from above.
  params.list <- params.input
  mapply(assign, names(params.list), params.list, MoreArgs = list(envir = .GlobalEnv))
  print(paste0('input ', nrow(DataIn), ' rows'))

  DataIn <- DataIn[(DataIn[,params.list$isQaqcDeactivated]       == FALSE &
                      DataIn[,params.list$isDsrDeactivated]      == FALSE &
                      DataIn[,params.list$isAnswerDeactivated]   == FALSE &
                      DataIn[,params.list$isSetEntryDeactivated] == FALSE),]

  print(paste0('analyzing ', nrow(DataIn), ' rows'))
  # create unique repId
  DataIn$holdRepId <- DataIn[params.list$repId]
  temp <- paste(DataIn[,params.list$repId], DataIn[,params.list$subSiteId], sep = "_")
  DataIn[,params.list$repId] <- temp
  DataIn$errorMessage <- ""

  obs.name <-  unique(DataIn[,params.list$questionCode])

  print(paste0('current question code ', obs.name))

  data <- DataIn
  # This is currently a fixed limit determined by commitee.  TODO: It should be based on the number of treatment levels and blocks
  if(nrow(data) < sufficientDataThreshold){
    DataIn$errorMessage        <- "Insufficient data"
    DataIn[,params.list$repId] <- DataIn$holdRepId
    DataIn$holdRepId           <- NULL
    DataIn[]                   <- lapply(DataIn, as.character)
    return(DataIn)
  }
  if(length(unique(DataIn[, params.list$factorLevelId]))<2){
    DataIn[,params.list$repId] <- DataIn$holdRepId
    DataIn$holdRepId           <- NULL
    DataIn[]                   <- lapply(DataIn, as.character)
    DataIn$errorMessage        <- "Single factor level"
    return(DataIn)
  }
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
  checkData(data, data_fields)

  # Add the data fields to the global environment
  lapply(seq_along(data_fields),
         function(x) assign(names(data_fields)[[x]], value = data_fields[[x]], envir = .GlobalEnv))

  # Manipulate data
  data <- reformatData(data, data_fields)
  # Set fixed and random effects from analysis type
  setFixedRandomEffects(analysisType)
  # for timing
  time.scale <- ifelse(nrow(data) < 4000, "secs", "mins")
  start      <- Sys.time()
  message(paste("Running ASReml using analysis type:", analysisType), appendLF = TRUE )
  # Run R-ASReml, capture output
  RCB_asr  <-  asreml::asreml(
    fixed     = fixed_formula,
    random    = random_formula,
    tolerance = 1E-10,
    data      = data,
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
    workspace  = 1000 * 1e6 / 8, # approximately 125 million bytes
    pworkspace = 1000 * 1e6 / 8, # approximately 125 million bytes
    trace      = FALSE,
    na.method.Y = "omit",
    na.method.X = "omit")

  message("finished in ", round(difftime(Sys.time(), start, units = time.scale[1]), digits = 2), " ", time.scale)
  message("Creating output files...", appendLF = TRUE)
  return(RCB_asr)
}