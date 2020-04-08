library(jsonlite)
library(asreml)

source("/repos/RCB4Cloud/R/RCB_MainFunction.R")
#source("/repos/RCB4Cloud/R/RCB_SupportFunctions.R") # supperceded by next line of code
source("/repos/RCB4Cloud/Reference/Rscripts/RCB_SupportFunctionsWithasremlPlus.R")
source("/repos/RCB4Cloud/Reference/Rscripts/fromAPI.R")

zdir  <- "/repos/RCB4Cloud/Reference/newValidation2020_04_07/"
zfile <- "ANOVA_GLOBAL_BLUE_845-31555-EHT-outputToP360.json"
zjsn  <- fromJSON(paste0(zdir, zfile))
zdat  <- zjsn$modelOutputs$rcbBlue$input$data[[1]]
zpar  <- zjsn$modelOutputs$rcbBlue$parameters
zrslt <- zjsn$modelOutputs$rcbBlue$results

zdat$factorLevelId         <- as.character(zdat$factorLevelId)
zdat$isAnswerDeactivated   <- as.logical(zdat$isAnswerDeactivated)
zdat$isDsrDeactivated      <- as.logical(zdat$isDsrDeactivated)
zdat$isPlaceHolder         <- as.logical(zdat$isPlaceHolder)
zdat$isQaqcDeactivated     <- as.logical(zdat$isQaqcDeactivated)
zdat$isSetEntryDeactivated <- as.logical(zdat$isSetEntryDeactivated)
zdat$locationId            <- as.character(zdat$locationId)
zdat$questionCode          <- as.character(zdat$questionCode)
zdat$repId                 <- as.character(zdat$repId)
zdat$subSiteId             <- as.character(zdat$subSiteId)
zdat$value                 <- as.numeric(zdat$value)

zpar$factorLevelId         <- "factorLevelId"
zpar$isAnswerDeactivated   <- "isAnswerDeactivated"
zpar$isDsrDeactivated      <- "isDsrDeactivated"
zpar$isPlaceHolder         <- "isPlaceHolder"
zpar$isQaqcDeactivated     <- "isQaqcDeactivated"
zpar$isSetEntryDeactivated <- "isSetEntryDeactivated"
zpar$locationId            <- "locationId"
zpar$questionCode          <- "questionCode"
zpar$repId                 <- "repId"
zpar$subSiteId             <- "subSiteId"
zpar$value                 <- "value"

zRCB_Output <- RCB_ModelFittingFunction(zdat, zpar, "P2")

zzcmp <- compareAPIandRCBoutputs(zrslt, zRCB_Output, ndigits=10)

