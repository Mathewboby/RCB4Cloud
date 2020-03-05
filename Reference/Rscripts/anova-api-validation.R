# suppressMessages(library(jsonlite, quietly=TRUE))
# suppressMessages(library(aws.s3,   quietly=TRUE))
 suppressMessages(library(dplyr,   quietly=TRUE))
# suppressMessages(library(testthat, quietly = TRUE))
library(lattice)
library(asreml)

# Default git repository branch is "Mi-RCB-dev"
# Use "git chechout master" in the terminal shell to switch to the master branch

source("/repos/RCB4Cloud/R/RCB_MainFunction.R")
source("/repos/RCB4Cloud/R/RCB_SupportFunctions.R")
source("/repos/RCB4Cloud/Reference/Rscripts/fromAPI.R")

ping_token = get_ping_token(client_id,client_secret,FALSE)

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
c(nrow(anovaGlobalJobData), length(anovaGlobalJobIds))          # reports 2005 1630
c(nrow(anovaLocalJobData), length(anovaLocalJobIds))            # reports 6587 2484
c(nrow(sMeansBySubsiteJobData), length(sMeansBySubsiteJobIds))  # reports 4101 3924
c(nrow(sMeansJobData), length(sMeansJobIds))                    # reports 2322   47

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

#
##
###
####


