library(jsonlite)
library(readxl)
library(asreml)

source("/repos/RCB4Cloud/R/RCB_MainFunction.R")
source("/repos/RCB4Cloud/Reference/Rscripts/RCB_SupportFunctionsWithasremlPlus.R")


# fsdat   <- as.data.frame(read_xlsx("/repos/RCB4Cloud/Reference/Data/prod_out/Statistics - FR08FRAC20THO1 disease + yield.xlsx", sheet="RawPlotData_"))
fsdat2  <- as.data.frame(read_xlsx("/repos/RCB4Cloud/Reference/Data/prod_out/Statistics - FR08FRAC20 trial series analysis of yield - source data.xlsx", sheet="PLOT_DATA_USED"))

fsdat2$Block     <- as.factor(paste0("Blk", fsdat2$Block))
fsdat2$Treatment <- as.factor(fsdat2$Treatment)
fsdat2$repId     <- "r1"
fsdat2$isPlaceHolder         <- FALSE
fsdat2$isDsrDeactivated      <- FALSE
fsdat2$isQaqcDeactivated     <- FALSE
fsdat2$isAnswerDeactivated   <- FALSE
fsdat2$isSetEntryDeactivated <- FALSE
fsdat2$questionCode          <- "stuff"

fs2tsp <- split(fsdat2, fsdat2$Trial)

RCBparams4P2 <- list(
  value         = "Plot_mean",
  setEntryId    = "Trial",
  blockNum      = "Block",
  subSiteId     = "Block",
  factorLevelId = "Treatment",
  repId         = "repId",
  locationId    = "Trial",
  questionCode  = "questionCode",
  isPlaceHolder = "isPlaceHolder",
  isDsrDeactivated = "isDsrDeactivated",
  isQaqcDeactivated = "isQaqcDeactivated",
  isAnswerDeactivated = "isAnswerDeactivated",
  isSetEntryDeactivated = "isSetEntryDeactivated",
  alpha                   = 0.10,
  sufficientDataThreshold = 14)

rcbo   <- RCB_ModelFittingFunction(fs2tsp[[1]], RCBparams4P2, "P2")
rcboP3 <- RCB_ModelFittingFunction(fs2tsp[[1]], RCBparams4P2, "P3")

rcblo <- lapply(fs2tsp, function(zx){RCB_ModelFittingFunction(zx, RCBparams4P2, "P2")})

RCBparams4P4 <- list(
  value         = "Plot_mean",
  setEntryId    = "Trial",
  blockNum      = "Trial",
  subSiteId     = "Trial",
  factorLevelId = "Treatment",
  repId         = "repId",
  locationId    = "Trial",
  questionCode  = "questionCode",
  isPlaceHolder = "isPlaceHolder",
  isDsrDeactivated = "isDsrDeactivated",
  isQaqcDeactivated = "isQaqcDeactivated",
  isAnswerDeactivated = "isAnswerDeactivated",
  isSetEntryDeactivated = "isSetEntryDeactivated",
  alpha                   = 0.10,
  sufficientDataThreshold = 14)

rcbP4o <- RCB_ModelFittingFunction(fsdat2, RCBparams4P4, "P4")

rcbo$blueTable
head(rcbo$deltas)
dim(rcbo$deltas)
rcbo$anova
rcbo$varianceComposition
rcbo$leastSignificantDifference
as.numeric(rcbo$leastSignificantDifference)/qt(0.95, 33) * qt(c(0.90, 0.95, 0.975, 0.995), 33)  # 0.80, 0.9, 0.95 and 0.99 LSDs
as.numeric(rcbo$blueTable$standardError[1])*qtukey(c(0.90, 0.95, 0.975, 0.995),33, 11)  # 0.80, 0.9, 0.95 and 0.99 HSDs


rcboP3$blupTable
rcboP3$varianceAnalysis
#
##
###
####


