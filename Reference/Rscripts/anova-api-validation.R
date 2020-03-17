# suppressMessages(library(jsonlite, quietly=TRUE))
# suppressMessages(library(aws.s3,   quietly=TRUE))
 suppressMessages(library(dplyr,   quietly=TRUE))
# suppressMessages(library(testthat, quietly = TRUE))
library(lattice)
library(asreml)
library(ggplot2)
daeURL <- "http://cran.wustl.edu//src/contrib/Archive/dae/dae_2.7-20.tar.gz"
suppressMessages(install.packages(daeURL,repos=NULL,type="source"))
suppressMessages(library(dae, quietly=TRUE))
asremlPlusURL <- "http://cran.wustl.edu//src/contrib/Archive/asremlPlus/asremlPlus_2.0-12.tar.gz"
suppressMessages(install.packages(asremlPlusURL,repos=NULL,type="source"))
suppressMessages(library(asremlPlus, quietly=TRUE))


# Default git repository branch is "Mi-RCB-dev"
# Use "git chechout master" in the terminal shell to switch to the master branch

source("/repos/RCB4Cloud/R/RCB_MainFunction.R")
# source("/repos/RCB4Cloud/R/RCB_SupportFunctions.R")
source("/repos/RCB4Cloud/Reference/Rscripts/RCB_SupportFunctionsWithasremlPlus.R")
source("/repos/RCB4Cloud/Reference/Rscripts/fromAPI.R")

ping_token = get_ping_token(client_id, client_secret,FALSE)

#  1Y0ePbjERgputHcELdhVlNhYtgd
# 123456789012345678901234567890
#
# All needed job ids are strings 29 characters long.  The first and last characters confuse the
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

get_output_ID <- "1XzA27OTh2oNS02m47pDqjZiRYL"
results       <- call_API(get_output_ID, ping_token)

aG_results   <- call_API(anovaGlobalJobIds[1]    , ping_token)
aL_results   <- call_API(anovaLocalJobIds[1]     , ping_token)
smbs_results <- call_API(sMeansBySubsiteJobIds[1], ping_token)
sm_results   <- call_API(sMeansJobIds[1]         , ping_token)

aG_results31   <- call_API(anovaGlobalJobIds[31]    , ping_token)
aL_results31   <- call_API(anovaLocalJobIds[31]     , ping_token)
smbs_results31 <- call_API(sMeansBySubsiteJobIds[31], ping_token)
sm_results31   <- call_API(sMeansJobIds[31]         , ping_token)

# Compare the number of jobs run to the number of non-missing job IDs
c(nrow(anovaGlobalJobData), length(anovaGlobalJobIds))          # reports 2005 1160
c(nrow(anovaLocalJobData), length(anovaLocalJobIds))            # reports 6587 2484
c(nrow(sMeansBySubsiteJobData), length(sMeansBySubsiteJobIds))  # reports 4101 3924
c(nrow(sMeansJobData), length(sMeansJobIds))                    # reports 2322   47
# 1YNFDd6VVnNR0wm6hTdfw6yOth7
# 123456789012345678901234567890
"1YNFDd6VVnNR0wm6hTdfw6yOth7"
#test <- compareResults(results)

agData01   <- get_API_Data(aG_results)
alData01   <- get_API_Data(aL_results)
smbsData01 <- get_API_Data(smbs_results)
smData01   <- get_API_Data(sm_results)

aglsm <- as.data.frame(do.call(rbind, lapply(agData01$outputData$leastSquaredMeans, unlist)))  # Table of least-squares means for each level of factorLevelId
allsm <- as.data.frame(do.call(rbind, lapply(alData01$outputData$leastSquaredMeans, unlist)))  # Table of least-squares means for each level of factorLevelId
agdlt <- as.data.frame(do.call(rbind, lapply(agData01$outputData$deltas, unlist)))  # Table of differences between the means in the lsm tables
aldlt <- as.data.frame(do.call(rbind, lapply(alData01$outputData$deltas, unlist)))  # Table of differences between the means in the lsm tables

## There are differences of 0 for the cases wherein a mean estimate is subtracted from itself.
# In these cases the answer is known, so the difference has no distributional properties.
# Therefore, for these cases, set the values in the corresponding rows to 0 except for the p-value
agw0 <- which(agdlt$differences == 0)
alw0 <- which(aldlt$differences == 0)

agdlt[,3:ncol(agdlt)] <- lapply(3:ncol(agdlt), function(zx){as.numeric(as.character(agdlt[,zx]))})
aldlt[,3:ncol(aldlt)] <- lapply(3:ncol(aldlt), function(zx){as.numeric(as.character(aldlt[,zx]))})

agdlt$pValueDifference[agw0]        <- 1
agdlt$standardErrorDifference[agw0] <- 0
agdlt$degreesOfFreedom[agw0]        <- 0
agdlt$tValue[agw0]                  <- 0
agdlt$lowerConfidenceInterval[agw0] <- 0
agdlt$upperConfidenceInterval[agw0] <- 0

aldlt$pValueDifference[alw0]        <- 1
aldlt$standardErrorDifference[alw0] <- 0
aldlt$degreesOfFreedom[alw0]        <- 0
aldlt$tValue[alw0]                  <- 0
aldlt$lowerConfidenceInterval[alw0] <- 0
aldlt$upperConfidenceInterval[alw0] <- 0

aglsmtbl    <- apiLsmTablePrep(aG_results)
agdeltastbl <- apiDeltasTablePrep(aG_results)

zaG01 <- aovAPIvsRCB(aG_results)
zaG31 <- aovAPIvsRCB(aG_results31)
zaL01 <- aovAPIvsRCB(aL_results)
zaL31 <- aovAPIvsRCB(aL_results31)

zaG01[grep("^compare",names(zaG01))]
zaG31[grep("^compare",names(zaG31))]
zaL01[grep("^compare",names(zaL01))]
zaL31[grep("^compare",names(zaL31))]

sapply(lapply(zaL31[grep("^compare",names(zaL31))], unlist), sum)
summaryCompare(zaG31)

zmG0          <- sapply(anovaGlobalJobIds[1:3],
                        function(zn){summaryCompare(aovAPIvsRCB(call_API(zn, ping_token)))})
zmG           <- as.data.frame(t(zmG0))
zmG$JobId     <- rownames(zmG)
rownames(zmG) <- NULL
write_json(zmG, "/repos/RCB4Cloud/Reference/Data/GlobalDataValidationResults.json", pretty=TRUE, auto_unbox="TRUE", digits=12, dataframe='rows')

###########
# zmL0          <- sapply(anovaLocalJobIds,
#                         function(zn){summaryCompare(aovAPIvsRCB(call_API(zn, ping_token)))})
# zmL           <- as.data.frame(t(zmL0))
# zmL$JobId     <- rownames(zmL)
# rownames(zmL) <- NULL
# write_json(zmL, "/repos/RCB4Cloud/Reference/Data/LocalDataValidationResults.json", pretty=TRUE, auto_unbox="TRUE", digits=12, dataframe='rows')
## After changing the code
zmL02          <- sapply(anovaLocalJobIds,
                        function(zn){summaryCompare(aovAPIvsRCB(call_API(zn, ping_token)))})
zmL2           <- as.data.frame(t(zmL02))
zmL2$JobId     <- rownames(zmL2)
rownames(zmL2) <- NULL
write_json(zmL2, "/repos/RCB4Cloud/Reference/Data/LocalDataValidationResults2.json", pretty=TRUE, auto_unbox="TRUE", digits=12, dataframe='rows')
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


