
# Load needed packages
suppressMessages(library(jsonlite, quietly=TRUE))
suppressMessages(library(aws.s3,   quietly=TRUE))
#suppressMessages(install.packages('dae',repos = "http://cran.wustl.edu/"))
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
params.in$sufficientDataThreshold <- 18 # Is 20 but for the sake of the tests below reset to 18.

data.mlmr                 <- fromJSON('Reference/Data/input-rcb-md.json')
data.mlmr$SeedProductName <- as.factor(data.mlmr$SeedProductName)
data.mlmr$FieldName       <- as.factor(data.mlmr$FieldName)
data.mlmr$RepNumber       <- as.factor(data.mlmr$RepNumber)

mlmrcc                    <- complete.cases(data.mlmr[,c("ObservationValueNumeric","FieldName","RepNumber","SeedProductName")])
mlmrdup                   <- duplicated(data.mlmr[,c("ObservationValueNumeric","FieldName","RepNumber","SeedProductName")])
data.mlmr <- droplevels(data.mlmr[mlmrcc == TRUE & mlmrdup == FALSE, ])
data.slmr <- droplevels(data.mlmr[data.mlmr$FieldName == data.mlmr$FieldName[1], ])
data.mlsr <- droplevels(data.mlmr[data.mlmr$RepNumber == data.mlmr$RepNumber[1], ])

dim(data.mlmr) # returns 54 28
dim(data.mlsr) # returns 18 28
dim(data.slmr) # returns 18 28

length(unique(data.mlmr$SeedProductName)) # returns 6
length(unique(data.mlmr$FieldName))       # returns 3
length(unique(data.mlmr$RepNumber))       # returns 3

length(unique(data.mlsr$SeedProductName)) # returns 6
length(unique(data.mlsr$FieldName))       # returns 3
length(unique(data.mlsr$RepNumber))       # returns 1

length(unique(data.slmr$SeedProductName)) # returns 6
length(unique(data.slmr$FieldName))       # returns 1
length(unique(data.slmr$RepNumber))       # returns 3

### MLMR model fit (P4)
params.in$sufficientDataThreshold <- nrow(data.mlmr)
zp4o <- RCB_ModelFittingFunction(data.mlsr, params.in, analysisType = 'P4')
zp5o <- RCB_ModelFittingFunction(data.mlsr, params.in, analysisType = 'P5')

resultsmlmr    <- RCB_ModelFittingFunction(data.mlmr, params.in, analysisType = 'P4')
# MLMR LSD using params.in$alpha (default is 0.1)
lsdmlmrasreml  <- c(3.634279, 3.693421)
DDoFmlmr       <- as.numeric(resultsmlmr$deltas$degreesFreedom[is.na(resultsmlmr$deltas$degreesFreedom) == FALSE])[1]
tvalmlmr       <- qt(1-params.in$alpha/2, DDoFmlmr)
deltaSEmlmr_MinMax <- range(as.numeric(resultsmlmr$deltas$standardErrorDifferences),na.rm=TRUE)
lsdmlmrcalc        <- tvalmlmr*deltaSEmlmr_MinMax
round(lsdmlmrasreml - lsdmlmrcalc, 3)  # Returns 0 0
lsdTableMLMR <- outer(qt(1 - c(0.2, 0.1, 0.05)/2, DDoFmlmr), deltaSEmlmr_MinMax, "*")
rownames(lsdTableMLMR) <- c("80%LSD","90%LSD","95%LSD")
colnames(lsdTableMLMR) <- c("Min","Max")
lsdTableMLMR
#            Min      Max
# 80%LSD 2.752291 2.797081
# 90%LSD 3.634279 3.693421
# 95%LSD 4.466274 4.538956

# MLMR MSE
mlmrVCs   <- resultsmlmr$varianceComposition$varianceEstimates
mlmrVCnms <- rownames(resultsmlmr$varianceComposition)
mlmrMSE   <- as.numeric(mlmrVCs[mlmrVCnms == "residual"])
mlmrMSE # returns 6.944862
# MLMR CV
datMean <- mean(data.mlmr[,params.in$value], na.rn=TRUE)
lsmMean <- mean(as.numeric(resultsmlmr$lsmTable$value), na.rm=TRUE)
CVmlmr_SdMean    <- sd(data.mlmr[,params.in$value], na.rm=TRUE)/datMean
CVmlmr_RMSEMean  <- sqrt(mlmrMSE)/datMean
CVmlmrRMSELSMean <- sqrt(mlmrMSE)/lsmMean
c(CVmlmr_SdMean, CVmlmr_RMSEMean, CVmlmrRMSELSMean) # returns 0.15026940 0.08077443 0.08009349

### MLSR model fit (P2)
params.in$sufficientDataThreshold <- nrow(data.mlsr)
zp2o <- RCB_ModelFittingFunction(data.mlsr, params.in, analysisType = 'P2')
zp3o <- RCB_ModelFittingFunction(data.mlsr, params.in, analysisType = 'P3')

resultsmlsr <- RCB_ModelFittingFunction(data.mlsr, params.in, analysisType = 'P2')
# MLSR LSD using params.in$alpha (default is 0.1)
lsdmlsrasreml  <- c(2.712476, 2.712476)
DDoFmlsr       <- as.numeric(resultsmlsr$deltas$degreesFreedom[is.na(resultsmlsr$deltas$degreesFreedom) == FALSE])[1]
tvalmlsr       <- qt(1-params.in$alpha/2, DDoFmlsr)
deltaSEmlsr_MinMax <- range(as.numeric(resultsmlsr$deltas$standardErrorDifferences),na.rm=TRUE)
lsdmlsrcalc        <- tvalmlsr*deltaSEmlsr_MinMax
round(lsdmlsrasreml - lsdmlsrcalc, 3)  # Returns 0 0
lsdTableMLSR <- outer(qt(1 - c(0.2, 0.1, 0.05)/2, DDoFmlsr), deltaSEmlsr_MinMax, "*")
rownames(lsdTableMLSR) <- c("80%LSD","90%LSD","95%LSD")
colnames(lsdTableMLSR) <- c("Min","Max")
lsdTableMLSR
#             Min       Max
# 80%LSD 2.053570 2.053570
# 90%LSD 2.712476 2.712476
# 95%LSD 3.334567 3.334567

# MLSR MSE
mlsrVCs   <- resultsmlsr$varianceComposition$varianceEstimates
mlsrVCnms <- rownames(resultsmlsr$varianceComposition)
mlsrMSE   <- as.numeric(mlsrVCs[mlsrVCnms == "residual"])
mlsrMSE # returns 3.359586
# MLSR CV
datMean <- mean(data.mlsr[,params.in$value], na.rn=TRUE)
lsmMean <- mean(as.numeric(resultsmlsr$lsmTable$value), na.rm=TRUE)
CVmlsr_SdMean    <- sd(data.mlsr[,params.in$value], na.rm=TRUE)/datMean
CVmlsr_RMSEMean  <- sqrt(mlsrMSE)/datMean
CVmlsrRMSELSMean <- sqrt(mlsrMSE)/lsmMean
c(CVmlsr_SdMean, CVmlsr_RMSEMean, CVmlsrRMSELSMean) # returns 0.09811873 0.05454209 0.05454209

### SLMR model fit (P1)
resultsslmr <- RCB_ModelFittingFunction(data.slmr, params.in, analysisType = 'P1')
# SLMR LSD using params.in$alpha (default is 0.1)
lsdslmrasreml  <- c(3.313286, 3.313286)
DDoFslmr       <- as.numeric(resultsslmr$deltas$degreesFreedom[is.na(resultsslmr$deltas$degreesFreedom) == FALSE])[1]
tvalslmr       <- qt(1-params.in$alpha/2, DDoFslmr)
deltaSEslmr_MinMax <- range(as.numeric(resultsslmr$deltas$standardErrorDifferences),na.rm=TRUE)
lsdslmrcalc        <- tvalslmr*deltaSEslmr_MinMax
round(lsdslmrasreml - lsdslmrcalc,3)  # Returns 0 0
lsdTableSLMR <- outer(qt(1 - c(0.2, 0.1, 0.05)/2, DDoFslmr), deltaSEslmr_MinMax, "*")
rownames(lsdTableSLMR) <- c("80%LSD","90%LSD","95%LSD")
colnames(lsdTableSLMR) <- c("Min","Max")
lsdTableSLMR
#            Min      Max
# 80%LSD 2.508433 2.508433
# 90%LSD 3.313286 3.313286
# 95%LSD 4.073169 4.073169

# SLMR MSE
slmrVCs   <- resultsslmr$varianceComposition$varianceEstimates
slmrVCnms <- rownames(resultsslmr$varianceComposition)
slmrMSE   <- as.numeric(slmrVCs[slmrVCnms == "residual"])
slmrMSE # returns 5.012699
# SLMR CV
datMean <- mean(data.slmr[,params.in$value], na.rn=TRUE)
lsmMean <- mean(as.numeric(resultsslmr$lsmTable$value), na.rm=TRUE)
CVslmr_SdMean    <- sd(data.slmr[,params.in$value], na.rm=TRUE)/datMean
CVslmr_RMSEMean  <- sqrt(slmrMSE)/datMean
CVslmrRMSELSMean <- sqrt(slmrMSE)/lsmMean
c(CVslmr_SdMean, CVslmr_RMSEMean, CVslmrRMSELSMean) # returns 0.07351815 0.06165048 0.06165048

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
# Df Sum Sq Mean Sq F value Pr(>F)
# SeedProductName                      5 100.48  20.096
# FieldName                            2 435.73 217.863
# FieldName:RepNumber                  6 182.31  30.384
# SeedProductName:FieldName           10 218.33  21.833
# SeedProductName:FieldName:RepNumber 30 337.05  11.235
# Residuals                            0   0.00
# Warning message:
#   In anova.lm(zlmMLMR) :
#   ANOVA F-tests on an essentially perfect fit are unreliable
zlmMLMRRefit <- lm(data=data.mlmrU,
          ObservationValueNumeric~SeedProductName + FieldName + FieldName:RepNumber +
            SeedProductName:FieldName)
anova(zlmMLMRRefit)
# Analysis of Variance Table
#
# Response: ObservationValueNumeric
# Df Sum Sq Mean Sq F value    Pr(>F)
# SeedProductName            5 100.48  20.096  1.7887   0.14540
# FieldName                  2 435.73 217.863 19.3914 3.932e-06 ***
#   FieldName:RepNumber        6 182.31  30.384  2.7044   0.03212 *
#   SeedProductName:FieldName 10 218.33  21.833  1.9433   0.07801 .
# Residuals                 30 337.05  11.235
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## Quick check on the term degrees-of-freedom in MLSR analysis
data.mlsrU <- unique(data.mlsr[,c("ObservationValueNumeric","FieldName","RepNumber","SeedProductName")])
dim(data.mlsrU) # returns 18 4
zlmMLSR <- lm(data=data.mlsrU,
          ObservationValueNumeric~SeedProductName + FieldName + SeedProductName:FieldName)
anova(zlmMLSR)
# Analysis of Variance Table
#
# Response: ObservationValueNumeric
# Df  Sum Sq Mean Sq F value Pr(>F)
# SeedProductName            5  34.472   6.894
# FieldName                  2 116.763  58.381
# SeedProductName:FieldName 10  33.596   3.360
# Residuals                  0   0.000
# Warning message:
#   In anova.lm(zlmMLSR) :
#   ANOVA F-tests on an essentially perfect fit are unreliable

zlmMLSRRefit <- lm(data=data.mlsrU,
               ObservationValueNumeric~SeedProductName + FieldName)
anova(zlmMLSRRefit)
# Analysis of Variance Table
#
# Response: ObservationValueNumeric
# Df  Sum Sq Mean Sq F value    Pr(>F)
# SeedProductName  5  34.472   6.894  2.0522 0.1559844
# FieldName        2 116.763  58.381 17.3776 0.0005569 ***
#   Residuals       10  33.596   3.360
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## Quick check on the term degrees-of-freedom in SLMR analysis
data.slmrU <- unique(data.slmr[,c("ObservationValueNumeric","FieldName","RepNumber","SeedProductName")])
dim(data.slmrU) # returns 18 4
zlmSLMR <- lm(data=data.slmrU,
              ObservationValueNumeric~SeedProductName + RepNumber + SeedProductName:RepNumber)
anova(zlmSLMR)
# Analysis of Variance Table
#
# Response: ObservationValueNumeric
# Df Sum Sq Mean Sq F value Pr(>F)
# SeedProductName            5 46.402  9.2804
# RepNumber                  2 24.653 12.3264
# SeedProductName:RepNumber 10 50.127  5.0127
# Residuals                  0  0.000
zlmSLMRRefit <- lm(data=data.slmrU,
                   ObservationValueNumeric~SeedProductName + RepNumber)
anova(zlmSLMRRefit)
# Analysis of Variance Table
#
# Response: ObservationValueNumeric
# Df Sum Sq Mean Sq F value Pr(>F)
# SeedProductName  5 46.402  9.2804  1.8514 0.1904
# RepNumber        2 24.653 12.3264  2.4590 0.1353
# Residuals       10 50.127  5.0127
#
##
###
dataRM  <- fromJSON("/repos/RCB4Cloud/Reference/Data/md-rcb-input.json")
paramRM <- fromJSON("/repos/RCB4Cloud/Reference/Data/md-rcb-parameters.json")

rmp4o <- RCB_ModelFittingFunction(dataRM, paramRM, analysisType = 'P4')
rmp5o <- RCB_ModelFittingFunction(dataRM, paramRM, analysisType = 'P5')

dataJM  <- fromJSON("/repos/RCB4Cloud/Reference/Data/JamanInput.json")$data
paramJM <- fromJSON("/repos/RCB4Cloud/Reference/Data/JamanParameters.json")

jmp4o <- RCB_ModelFittingFunction(dataJM, paramJM, analysisType = 'P2')
jmp5o <- RCB_ModelFittingFunction(dataJM, paramJM, analysisType = 'P3')
