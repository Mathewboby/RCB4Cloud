
# Load needed packages
suppressMessages(library(jsonlite, quietly=TRUE))
suppressMessages(library(aws.s3,   quietly=TRUE))
install.packages("dplyr")
devtools::install_version("dae", version = "3.0-23", repos = "https://cran.r-project.org")
suppressMessages(library(asreml,     quietly=TRUE))
asremlPlusURL <- "http://cran.wustl.edu//src/contrib/Archive/asremlPlus/asremlPlus_2.0-12.tar.gz"
suppressMessages(install.packages(asremlPlusURL,repos=NULL,type="source"))
suppressMessages(library(asremlPlus, quietly=TRUE))
#options(digits = 20)
# Source code to be tested.
source('/repos/RCB4Cloud/R/RCB_SupportFunctions.R')
source('/repos/RCB4Cloud/R/RCB_MainFunction.R')

params.in                 <- fromJSON('Reference/Data/parameters-rcb-md.json')

data.mlmr                 <- fromJSON('Reference/Data/input-rcb-md.json')
# data.mlmr$SeedProductName <- as.factor(data.mlmr$SeedProductName)
# data.mlmr$FieldName       <- as.factor(data.mlmr$FieldName)
# data.mlmr$RepNumber       <- as.factor(data.mlmr$RepNumber)
#
# mlmrcc                    <- complete.cases(data.mlmr[,c("ObservationValueNumeric","FieldName","RepNumber","SeedProductName")])
# mlmrdup                   <- duplicated(data.mlmr[,c("ObservationValueNumeric","FieldName","RepNumber","SeedProductName")])
# data.mlmr <- droplevels(data.mlmr[mlmrcc == TRUE & mlmrdup == FALSE, ])
# data.slmr <- droplevels(data.mlmr[data.mlmr$FieldName == data.mlmr$FieldName[1], ])
# data.mlsr <- droplevels(data.mlmr[data.mlmr$RepNumber == data.mlmr$RepNumber[1], ])

dim(data.mlmr) # returns 162 28

length(unique(data.mlmr$SeedProductName)) # returns 6
length(unique(data.mlmr$FieldName))       # returns 3
length(unique(data.mlmr$RepNumber))       # returns 3

### MLMR model fit (P4)
resultsmlmr    <- RCB_ModelFittingFunction(data.mlmr, params.in, analysisType = 'P4')
# MLMR LSD using params.in$alpha (default is 0.1)
lsdmlmrasreml  <- as.numeric(resultsmlmr$leastSignificantDifference)
## degreess freedom
DDoFmlmr       <- as.numeric(resultsmlmr$deltas$degreesFreedom[is.na(resultsmlmr$deltas$degreesFreedom) == FALSE])[1]
##  make T values 2 sided confidence interval
tvalmlmr       <- qt(1 - params.in$alpha/2, DDoFmlmr)
##  get standard error
deltaSEmlmr_Mean <- mean(as.numeric(resultsmlmr$deltas$standardErrorDifferences),na.rm=TRUE)
##  multiply by T value
lsdmlmrcalc        <- tvalmlmr * deltaSEmlmr_Mean
##  compare diff
round(lsdmlmrasreml[1] - lsdmlmrcalc, 3)  # Returns 0 0
##  calculate multiple least significant values from corresponding alphas
lsdTableMLMR <- outer(qt(1 - c(0.2, 0.1, 0.05)/2, DDoFmlmr), deltaSEmlmr_Mean, "*")
#
rownames(lsdTableMLMR) <- c("80%LSD","90%LSD","95%LSD")
lsdTableMLMR
# [,1]
# 80%LSD 2.747335
# 90%LSD 3.628843
# 95%LSD 4.461097

# MLMR MSE
## retrieve value from data
mlmrVCs   <- resultsmlmr$varianceComposition$varianceEstimates
mlmrVCnms <- rownames(resultsmlmr$varianceComposition)
mlmrMSE   <- as.numeric(mlmrVCs[mlmrVCnms == "residual"])
mlmrMSE # returns 4.492342
# MLMR CV - calculate coefficient of variation
## average of data
datMean <- mean(data.mlmr[,params.in$value], na.rn=TRUE)
##  average of lsmeans
lsmMean <- mean(as.numeric(resultsmlmr$blueTable$value), na.rm=TRUE)
##  "raw" CV standard deviation divided by mean
CVmlmr_SdMean    <- sd(data.mlmr[,params.in$value], na.rm=TRUE)/datMean
##  CV with raw mean in denom
CVmlmr_RMSEMean  <- sqrt(mlmrMSE)/datMean
##  CV from with mean of lsmeans in denom
CVmlmrRMSELSMean <- sqrt(mlmrMSE)/lsmMean

c(CVmlmr_SdMean, CVmlmr_RMSEMean, CVmlmrRMSELSMean) # returns 0.14933313 0.06496486 0.06440065

## Quick check on the term degrees-of-freedom in MLMR analysis
data.mlmrU <- unique(data.mlmr[,c("ObservationValueNumeric","FieldName","RepNumber","SeedProductName")])
dim(data.mlmrU) # returns 54 4
zlmMLMR <- lm(data=data.mlmrU,
          ObservationValueNumeric~SeedProductName + FieldName + FieldName:RepNumber +
                                  SeedProductName:FieldName + SeedProductName:FieldName:RepNumber)
anova(zlmMLMR)
# Analysis of Variance Table
#
# Response: ObservationValueNumeric
# Df Sum Sq Mean Sq F value    Pr(>F)
# SeedProductName                      5 100.48  20.096  2.6945  0.054824 .
# FieldName                            2 435.73 217.863 29.2116 2.231e-06 ***
#   FieldName:RepNumber                  3 157.19  52.397  7.0255  0.002516 **
#   SeedProductName:FieldName           10 218.33  21.833  2.9274  0.022840 *
#   SeedProductName:FieldName:RepNumber 15 227.92  15.195  2.0373  0.075882 .
# Residuals                           18 134.25   7.458
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

##### Use Randall's data ##############
#######################################
##  replicate the confidence intervals 2-sided
dataRM  <- fromJSON("/repos/RCB4Cloud/Reference/Data/md-rcb-input.json")
paramRM <- fromJSON("/repos/RCB4Cloud/Reference/Data/md-rcb-parameters.json")

rmp4o <- RCB_ModelFittingFunction(dataRM, paramRM, analysisType = 'P4')
rmp5o <- RCB_ModelFittingFunction(dataRM, paramRM, analysisType = 'P5')
##  replicate CI
blue <- rmp4o$blueTable
for(k in 2:7){blue[,k] <- as.numeric(blue[,k])}
bluetValue_ParamAlpha <- qt(1-paramRM$alpha/2, blue$degreesFreedom)
blueCiHalfWidth       <- bluetValue_ParamAlpha*blue$standardError
blueLowerCiLimit      <- blue$value - blueCiHalfWidth
blueUpperCiLimit      <- blue$value + blueCiHalfWidth

blue$mtLowerCiPa <- blueLowerCiLimit
blue$mtUpperCiPa <- blueUpperCiLimit
blue <- blue[, c(1:6,9,7,10,8)]  # reorder columns so my confidence intervals estimates are next to those calculated by RCBFittingFunction
##  replicate deltas
dlt                  <- rmp4o$deltas[complete.cases(rmp4o$deltas),]
for(k in 3:9){dlt[,k] <- as.numeric(dlt[,k])}
diftValue_ParamAlpha <- qt(1-paramRM$alpha/2, dlt$degreesFreedom)
difCiHalfWidth       <- diftValue_ParamAlpha*dlt$standardErrorDifferences
difLowerCiLimit      <- dlt$differences - difCiHalfWidth
difUpperCiLimit      <- dlt$differences + difCiHalfWidth

dlt$mtLowerCiPa <- difLowerCiLimit
dlt$mtUpperCiPa <- difUpperCiLimit
dlt <- dlt[, c(1:7,8,10,9,11)]  # reorder columns so my confidence intervals estimates are next to those calculated by RCBFittingFunction
names(dlt) <- c("head", "comparison", "difference", "ProbDiffNE0", "diffSe", "degreesFreedom", "tTest_diff/se",
                "lowerCi", "mtLowerCiPa", "upperCi",  "mtUpperCiPa")

# Calculate the LSD
lsdDif    <- mean(difCiHalfWidth)
# c(as.numeric(rmp4o$leastSignificantDifference$mean), lsdDif)
# round(as.numeric(rmp4o$leastSignificantDifference$mean)-lsdDif, 14)
lsd       <- rmp4o$leastSignificantDifference
lsd$mtLsd <- as.character(lsdDif)

outputlist <- list(blueTable=blue, deltaTable=dlt, lsdTable=lsd)
write_json(outputlist, "/repos/RCB4Cloud/Reference/Data/AlphaCalcDemo.json", pretty=TRUE, auto_unbox=TRUE, digits=12, na='string')

##### Use Jaman's data ##############
#####################################
dataJM  <- fromJSON("/repos/RCB4Cloud/Reference/Data/JamanInput.json")$data
paramJM <- fromJSON("/repos/RCB4Cloud/Reference/Data/JamanParameters.json")

jmp2o <- RCB_ModelFittingFunction(dataJM, paramJM, analysisType = 'P2')
jmp3o <- RCB_ModelFittingFunction(dataJM, paramJM, analysisType = 'P3')
