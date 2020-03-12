library(readxl)
library(jsonlite)
library(asreml)

source("/repos/RCB4Cloud/R/RCB_MainFunction.R")
source("/repos/RCB4Cloud/Reference/Rscripts/RCB_SupportFunctionsWithasremlPlus.R")

inputPath            <- "/repos/RCB4Cloud/Reference/Data/CE_Data/"
inputFile            <- "RESULTS-CE_INJURY-20200103194610.xlsx"
inputFilePath        <- paste0(inputPath, inputFile)
ceData               <- as.data.frame(read_xlsx(inputFilePath, sheet="DATA"))
ceData$RepId         <- paste0("rep",ceData$Rep)
#ceData$Injury        <- asin(sqrt(ceData$Injury/100))
ceData$isQaqcDeactivated     <- FALSE
ceData$isDsrDeactivated      <- FALSE
ceData$isAnswerDeactivated   <- FALSE
ceData$isSetEntryDeactivated <- FALSE
ceData$isPlaceHolder         <- FALSE
ceData$subSiteId             <- "Site01"

dim(table(ceData$Trt, ceData$Rep))
# [1] 35  6

moduleParams <- list(alpha                   = 0.1,
                     value                   = "Injury",
                     factorLevelId           = "Trt",
                     repId                   = "RepId",
                     subSiteId               = "subSiteId",
                     isQaqcDeactivated       = "isQaqcDeactivated",
                     isDsrDeactivated        = "isDsrDeactivated",
                     isAnswerDeactivated     = "isAnswerDeactivated",
                     isSetEntryDeactivated   = "isSetEntryDeactivated",
                     isPlaceHolder           = "isPlaceHolder",
                     sufficientDataThreshold = 35+6-1)  # num treats + num blocks - 1

zaovP1 <- RCB_ModelFittingFunction(ceData, moduleParams, "P1")

ceData$subSiteId <- ceData$RepId

zaovP2 <- RCB_ModelFittingFunction(ceData, moduleParams, "P2")
zaovP3 <- RCB_ModelFittingFunction(ceData, moduleParams, "P3")

ceData$subSiteId <- paste0("Site",(ceData$Rep <= 3) + 1)

zaovP4 <- RCB_ModelFittingFunction(ceData, moduleParams, "P4")
zaovP5 <- RCB_ModelFittingFunction(ceData, moduleParams, "P5")


write_json(zaovP1, "/repos/RCB4Cloud/Reference/Data/CE_Data/Phase1_RCB_P1_Output.json",
           pretty=TRUE, autounbox=TRUE, digits=12, na="string")
#
##
###
####

