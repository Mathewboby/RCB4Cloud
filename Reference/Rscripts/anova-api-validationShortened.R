library(dplyr)
library(lattice)
library(asreml)
library(jsonlite)
# Default git repository branch is "Mi-RCB-dev"
# Use "git chechout master" in the terminal shell to switch to the master branch

source("/repos/RCB4Cloud/R/RCB_MainFunction.R")
source("/repos/RCB4Cloud/Reference/Rscripts/RCB_SupportFunctionsWithasremlPlus.R")
source("/repos/RCB4Cloud/Reference/Rscripts/fromAPI.R")

# enter P360 access credentials before running get_ping_token

ping_token = get_ping_token(client_id, client_secret,FALSE)

#  1Y0ePbjERgputHcELdhVlNhYtgd
# 123456789012345678901234567890
#
# All needed job ids are strings 29 characters long similar to the above.
# The first and last characters confuse the
# data quiry, so extract just the 2nd through 28th characters.  However, some job IDs are missing
# so skip those.  The function, 'extractJobIds', does all of this and is found in fromAPI.R.

anovaGlobalJobData <- read.csv("/repos/RCB4Cloud/Reference/Data/ValidationData/anova_global_job_output_ids.csv")
anovaGlobalJobIds  <- extractJobIds(anovaGlobalJobData, "job_output_id")

anovaLocalJobData <- read.csv("/repos/RCB4Cloud/Reference/Data/ValidationData/anova_local_job_output_ids.csv")
anovaLocalJobIds  <- extractJobIds(anovaLocalJobData, "job_output_id")

sMeansBySubsiteJobData <- read.csv("/repos/RCB4Cloud/Reference/Data/ValidationData/simple_means_by_subsite_job_output_ids.csv")
sMeansBySubsiteJobIds  <- extractJobIds(sMeansBySubsiteJobData, "job_output_id")

sMeansJobData <- read.csv("/repos/RCB4Cloud/Reference/Data/ValidationData/simple_means_job_output_ids.csv")
sMeansJobIds  <- extractJobIds(sMeansJobData, "job_output_id")

# Compare the number of jobs run to the number of non-missing job IDs
c(nrow(anovaGlobalJobData), length(anovaGlobalJobIds))          # reports 2005 1160
c(nrow(anovaLocalJobData), length(anovaLocalJobIds))            # reports 6587 2484
c(nrow(sMeansBySubsiteJobData), length(sMeansBySubsiteJobIds))  # reports 4101 3924
c(nrow(sMeansJobData), length(sMeansJobIds))                    # reports 2322   47

#############################################################################################$
zmG0          <- sapply(anovaGlobalJobIds,
                        function(zn){summaryCompare(aovAPIvsRCB(call_API(zn, ping_token)))})
zmG           <- as.data.frame(t(zmG0))
zmG$JobId     <- rownames(zmG)
rownames(zmG) <- NULL
# write_json(zmG, "/repos/RCB4Cloud/Reference/Data/GlobalDataValidationResults.json", pretty=TRUE, auto_unbox="TRUE", digits=12, dataframe='rows')
# apply(zmG[,1:5], 2, sum)
# compareLSM  compareDeltas compareVarComp   compareAnova     compareLSD
# 0              0              0              0              0
# zmG <- fromJSON("/repos/RCB4Cloud/Reference/Data/GlobalDataValidationResults.json")
###########
zmL02          <- sapply(anovaLocalJobIds,
                        function(zn){summaryCompare(aovAPIvsRCB(call_API(zn, ping_token)))})
zmL2           <- as.data.frame(t(zmL02))
zmL2$JobId     <- rownames(zmL2)
rownames(zmL2) <- NULL
# write_json(zmL2, "/repos/RCB4Cloud/Reference/Data/LocalDataValidationResults2.json", pretty=TRUE, auto_unbox="TRUE", digits=12, dataframe='rows')
# apply(zmL2[,1:5], 2, sum)
# compareLSM  compareDeltas compareVarComp   compareAnova     compareLSD
# 0              0              0              0              0
# zmL2 <- fromJSON("/repos/RCB4Cloud/Reference/Data/LocalDataValidationResults2.json")

zsm <- call_API(sMeansJobIds[1], ping_token)
zd1 <- lapply(zsm$modelOutputs[[1]]$simpleMeans$input$data,
              function(zx){unlist(zx[c(1,3,7,8,10,11)])})
zd2                     <- as.data.frame(do.call(rbind, zd1))
zd2$dsrOverride         <- as.logical( as.character(zd2$dsrOverride        ) )
zd2$isAnswerDeactivated <- as.logical( as.character(zd2$isAnswerDeactivated) )
zd2$questionCode        <-             as.character(zd2$questionCode       )
zd2$value               <- as.numeric( as.character(zd2$value              ) )

zsm21 <- call_API(sMeansJobIds[21], ping_token)
zd21 <- lapply(zsm21$modelOutputs[[1]]$simpleMeans$input$data,
              function(zx){unlist(zx[c(1,3,7,8,10,11)])})
zd22                     <- as.data.frame(do.call(rbind, zd21))
zd22$dsrOverride         <- as.logical( as.character(zd22$dsrOverride        ) )
zd22$isAnswerDeactivated <- as.logical( as.character(zd22$isAnswerDeactivated) )
zd22$questionCode        <-             as.character(zd22$questionCode       )
zd22$value               <- as.numeric( as.character(zd22$value              ) )

# refresh the ping token and then read in the 47 simple global means data sets which are
# organized in an odd way.
ping_token = get_ping_token(client_id, client_secret,FALSE)
zjl <- lapply(sMeansJobIds, function(zi){call_API(zi, ping_token)})

zsmout <- lapply(1:length(zjl),
                 function(zi){
                   as.data.frame(do.call(rbind,
                                         lapply(zjl[[zi]]$modelOutputs[[1]]$simpleMeans$means$global$factors, unlist)))})

zsmbs1 <- call_API(sMeansBySubsiteJobIds[1], ping_token)

#zsmbsData <- do.call(rbind, lapply(zsmbs1$modelOutputs[[1]]$simpleMeans$means$local$subsites[[1]]$factors, unlist))
zsmbsData <- data.frame(do.call(rbind, zsmbs1$modelOutputs[[1]]$simpleMeans$input$data)[, c(1,2,3,7,8,9,10,11)])

zd47 <- data.frame(do.call(rbind, lapply(zjl[[47]]$modelOutputs[[1]]$simpleMeans$means$global$factors, unlist)))

zd47inputData <- data.frame(do.call(rbind, zjl[[47]]$modelOutputs[[1]]$simpleMeans$input$data)[, c(1,2,3,7,8,9,10,11)])
######################### $$$$
sm1        <- fromJSON("/repos/RCB4Cloud/Reference/Data/prod_out/p360/SIMPLEMEANS8957-18367-STLP-outputToP360.json")
idat       <- fromJSON("/repos/RCB4Cloud/Reference/Data/prod_out/p360/DSR_4757-18402-SC-outputToP360.json")
sm1Data    <- sm1$modelOutputs$simpleMeans$input$data[[1]][,c(3, 4, 8, 9)]
sm1gfactor <- sm1$modelOutputs$simpleMeans$means$global$factors[[1]]
sm1ffactor <- sm1$modelOutputs$simpleMeans$means$local$subsites[[1]]$factors[[1]]
zmerg      <- merge(sm1gfactor,sm1ffactor, by='id', all=T)

zmerg$Multiplier <- paste0(round(zmerg$value.y/zmerg$value.x))

zmerg$Multiplier[which(zmerg$Multiplier == "1e+10")] <- paste(c(1,rep(0,10)), sep="",collapse="")
zmerg$Multiplier[which(zmerg$Multiplier == "1e+09")] <- paste(c(1,rep(0, 9)), sep="",collapse="")
zmerg$Multiplier[which(zmerg$Multiplier == "1e+08")] <- paste(c(1,rep(0, 8)), sep="",collapse="")
zmerg$Multiplier[which(zmerg$Multiplier == "1e+06")] <- paste(c(1,rep(0, 6)), sep="",collapse="")
zmerg$Multiplier[which(zmerg$Multiplier == "1e+05")] <- paste(c(1,rep(0, 5)), sep="",collapse="")

idat  <- fromJSON("/repos/RCB4Cloud/Reference/Data/prod_out/p360/DSR_4757-18402-SC-outputToP360.json")
adat  <- fromJSON("/repos/RCB4Cloud/Reference/Data/prod_out/p360/ANOVA_GLOBAL_BLUE_696-18402-SC-outputToP360.json")
agdat <- adat$modelOutputs$rcbBlue$input$data[[1]]
table(agdat$subSiteId, agdat$repId)
#                                       1
# 4bcb4652-860d-4398-9475-156d15f0a224 36
# 90ea907c-ce78-41b9-8600-9b86d8846edc 36
# a35a1343-5b8d-4ee1-a1b8-b546016ab0d8 32
# e8e9e9e6-028e-427d-8dcd-c18fa24fd9f8 36

idatu <- idat$modelOutputs$dsr$input$data[[1]]
table(idatu$subSiteId, idatu$repId)
#                                       1
# 4bcb4652-860d-4398-9475-156d15f0a224 36
# 90ea907c-ce78-41b9-8600-9b86d8846edc 36
# a35a1343-5b8d-4ee1-a1b8-b546016ab0d8 32
# e8e9e9e6-028e-427d-8dcd-c18fa24fd9f8 36
mga      <- aggregate(value~factorLevelId, data=agdat, function(zx){mean(zx, na.rm=T)}) # ANOVA inputdata
mgi      <- aggregate(value~factorLevelId, data=idatu, function(zx){mean(zx, na.rm=T)}) # DSR inputdata
mmg      <- merge(mga,mgi, by="factorLevelId")
mmg$diff <- mmg$value.y-mmg$value.x

smbs9.402    <- fromJSON("/repos/RCB4Cloud/Reference/Data/prod_out/p360/SIMPLEMEANSBYSUBSITE9-18402-SC-outputToP360.json")
id9.402 <- smbs9.402$modelOutputs$simpleMeans$input$data[[1]]
if9.402 <- smbs9.402$modelOutputs$simpleMeans$means$local$subsites[[1]]$factors[[1]]
names(if9.402) <- c('id', 'value.z')

smbs8.402    <- fromJSON("/repos/RCB4Cloud/Reference/Data/prod_out/p360/SIMPLEMEANSBYSUBSITE8-18402-SC-outputToP360.json")
id8.402 <- smbs8.402$modelOutputs$simpleMeans$input$data[[1]]
if8.402 <- smbs8.402$modelOutputs$simpleMeans$means$local$subsites[[1]]$factors[[1]]
names(if8.402) <- c('id', 'value.y')

smbs3.402    <- fromJSON("/repos/RCB4Cloud/Reference/Data/prod_out/p360/SIMPLEMEANSBYSUBSITE3-18402-SC-outputToP360.json")
id3.402 <- smbs3.402$modelOutputs$simpleMeans$input$data[[1]]
if3.402 <- smbs3.402$modelOutputs$simpleMeans$means$local$subsites[[1]]$factors[[1]]
names(if3.402) <- c('id', 'value.x')

smbs2.402    <- fromJSON("/repos/RCB4Cloud/Reference/Data/prod_out/p360/SIMPLEMEANSBYSUBSITE2-18402-SC-outputToP360.json")
id2.402 <- smbs2.402$modelOutputs$simpleMeans$input$data[[1]]
if2.402 <- smbs2.402$modelOutputs$simpleMeans$means$local$subsites[[1]]$factors[[1]]
names(if2.402) <- c('id', 'value.w')

mi23       <- merge(if2.402, if3.402, by='id', all=T)
mi89       <- merge(if8.402, if9.402, by='id', all=T)
mi2389     <- merge(mi23, mi89, by='id', all=T)
mi2389$ave <- apply(mi2389[, 2:5], 1, function(zx){mean(zx, na.rm=TRUE)}) # the average of the 4 subsites' data equals the average in mmg which where calculated from the DSR and ANOVA input datasets.
mimmg      <-  merge(mi2389, mmg, by.x='id', by.y='factorLevelId', all=T)


#
##
###
####


