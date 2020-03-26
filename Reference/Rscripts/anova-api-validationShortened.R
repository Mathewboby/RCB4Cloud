library(dplyr)
library(lattice)
library(asreml)

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

#
##
###
####


