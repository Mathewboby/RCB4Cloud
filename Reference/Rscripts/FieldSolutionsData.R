library(jsonlite)
library(readxl)
library(lattice)
library(asreml)

source("/repos/RCB4Cloud/R/RCB_MainFunction.R")
source("/repos/RCB4Cloud/Reference/Rscripts/RCB_SupportFunctionsWithasremlPlus.R")


fsdat   <- as.data.frame(read_xlsx("/repos/RCB4Cloud/Reference/Data/prod_out/Statistics - FR08FRAC20THO1 disease + yield.xlsx", sheet="RawPlotData_"))
fsdat2  <- as.data.frame(read_xlsx("/repos/RCB4Cloud/Reference/Data/prod_out/Statistics - FR08FRAC20 trial series analysis of yield - source data.xlsx", sheet="PLOT_DATA_USED"))


zmv     <- mean(fsdat$Value)
zmvbt   <- aggregate(data=fsdat, Value~         Treat, function(zx){mean(zx, na.rm=TRUE)})
zmvac   <- aggregate(data=fsdat, Value~AssCol        , function(zx){mean(zx, na.rm=TRUE)})
zmvacbt <- aggregate(data=fsdat, Value~AssCol + Treat, function(zx){mean(zx, na.rm=TRUE)})

zmv2     <- mean(fsdat2$Plot_mean)
zmvbt2   <- aggregate(data=fsdat2, Plot_mean~          Treat, function(zx){mean(zx, na.rm=TRUE)})
zmvac2   <- aggregate(data=fsdat2, Plot_mean~Ass_col        , function(zx){mean(zx, na.rm=TRUE)})
zmvacbt2 <- aggregate(data=fsdat2, Plot_mean~Ass_col + Treat, function(zx){mean(zx, na.rm=TRUE)})

fsdat2$Block     <- as.factor(paste0("Blk", fsdat2$Block))
fsdat2$Treatment <- as.factor(fsdat2$Treatment)
fsdat2$repId     <- "r1"
fsdat2$FIELD_ID  <- fsdat2$Block
fsdat2$FACTOR_1  <- fsdat2$fsdat2$Treatment
fsdat2$REP_ID    <- fsdat2$repId
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
  # CROP_OBSRVTN_DETAIL_ID = "Plot_mean",
  # FACTOR_1      = "Treatment",
  # FIELD_ID      = "Block",
  # REP_ID        = "repId",
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

rcbo <- RCB_ModelFittingFunction(fs2tsp[[1]], RCBparams4P2, "P2")

rcblo <- lapply(fs2tsp, function(zx){RCB_ModelFittingFunction(zx, RCBparams4P2, "P2")})
#
##
###
####


