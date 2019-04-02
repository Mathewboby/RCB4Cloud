######### Unit Test of the RCB Model ############

# Load needed packages
suppressMessages(library(jsonlite, quietly=TRUE))
suppressMessages(library(aws.s3,   quietly=TRUE))
suppressMessages(install.packages('dae',repos = "http://cran.wustl.edu/"))
asremlPlusURL <- "http://cran.wustl.edu//src/contrib/Archive/asremlPlus/asremlPlus_2.0-12.tar.gz"
suppressMessages(install.packages(asremlPlusURL,repos=NULL,type="source"))
suppressMessages(library(asremlPlus,    quietly=TRUE))

# Source code to be tested.
source('/repos/RCB4Cloud/R/RCB_SupportFunctions.R')
source('/repos/RCB4Cloud/R/RCB_MainFunction_DEV.R')

# Set the path to a nice RSA yield data set in S3
yf<-"/projects/smartQAQC/business-funcs/breeding/regions/rsa/crops/corn/seasons/2017_10/programs/a4/sets/ncb_baab01/observations/yld/runs/1553272601/input.json"

# Set AWS credentials to access S3
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIAIQTLWIZT2Q5NYYOA",
"AWS_SECRET_ACCESS_KEY" = "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",
"AWS_DEFAULT_REGION" = "us-east-1")

# Read in the JSON-data file from S3
dfyld <- fromJSON(rawToChar(get_object(object = yf,bucket="trait-analytics-np")))

# Read in RCB default parameter list from Dominio
RCB_DefaultParameterList <- fromJSON("/repos/RCB4Cloud/Reference/Data/RCB_DefaultParameterList.json")
# Echo guts of RCB_DefaultParameterList
RCB_DefaultParameterList

# Run RCB model with updated code

ro1 <- RCB_ModelFittingFunction(dfyld) # Only input data frame and use function defaults of nine individual parameters
ro2 <- RCB_ModelFittingFunction(dfyld,RCB_DefaultParameterList) # Only input data frame and parameter list
ro3 <- RCB_ModelFittingFunction(dfyld,
                                ListOfRCBParameters         = NULL,
                                analysis_type               = "P2",
                                alpha                       = 0.1,
                                ResponseVariableColumnName  = "numValue",
                                FieldIDColumnName           = "fieldId",
                                TreatmentFactorColumnName   = "factor1",
                                ReplicateIDColumnName       = "repId",
                                FactorTypeColumnName        = "experimentalUnitId",
                                SufficientDataThreshold     = 20,
                                ResponseVariableShouldBeGT0 = TRUE) # Input data frame and other parameters
ro4 <- RCB_ModelFittingFunction(dfyld,
                                analysis_type               = "P2",
                                alpha                       = 0.1,
                                ResponseVariableColumnName  = "numValue",
                                FieldIDColumnName           = "fieldId",
                                TreatmentFactorColumnName   = "factor1",
                                ReplicateIDColumnName       = "repId",
                                FactorTypeColumnName        = "experimentalUnitId",
                                SufficientDataThreshold     = 20,
                                ResponseVariableShouldBeGT0 = TRUE) # Input data frame and other parameters excluding paramter list
# The next three chunks of code verify that all 4 outputs match.
# Compare output ro2 to ro1. Note there are some NA in the Deltas so the extra comparisons verify they are the same
mapply(function(zx,zy){all(zx==zy)},ro1,ro2)
sapply(ro1$Deltas,function(zx){sum(is.na(zx))})
sapply(ro2$Deltas,function(zx){sum(is.na(zx))})
mapply(function(zx,zy){all(zx[complete.cases(zx),]==zy[complete.cases(zy),])},ro1,ro2)

# Compare output ro3 to ro1. Note there are some NA in the Deltas so the extra comparisons verify they are the same
mapply(function(zx,zy){all(zx==zy)},ro1,ro3)
sapply(ro1$Deltas,function(zx){sum(is.na(zx))})
sapply(ro3$Deltas,function(zx){sum(is.na(zx))})
mapply(function(zx,zy){all(zx[complete.cases(zx),]==zy[complete.cases(zy),])},ro1,ro3)

# Compare output ro4 to ro1. Note there are some NA in the Deltas so the extra comparisons verify they are the same
mapply(function(zx,zy){all(zx==zy)},ro1,ro4)
sapply(ro1$Deltas,function(zx){sum(is.na(zx))})
sapply(ro4$Deltas,function(zx){sum(is.na(zx))})
mapply(function(zx,zy){all(zx[complete.cases(zx),]==zy[complete.cases(zy),])},ro1,ro4)

# Run a case wherien one parameter is changed.
ro5 <- RCB_ModelFittingFunction(dfyld,
                                analysis_type               = "P2",
                                alpha                       = 0.05,
                                ResponseVariableColumnName  = "numValue",
                                FieldIDColumnName           = "fieldId",
                                TreatmentFactorColumnName   = "factor1",
                                ReplicateIDColumnName       = "repId",
                                FactorTypeColumnName        = "experimentalUnitId",
                                SufficientDataThreshold     = 20,
                                ResponseVariableShouldBeGT0 = TRUE) 
# # compare results with ro1
# mapply(function(zx,zy){all(zx==zy)},ro1,ro5)
# sapply(ro1$Deltas,function(zx){sum(is.na(zx))})
# sapply(ro5$Deltas,function(zx){sum(is.na(zx))})
# mapply(function(zx,zy){all(zx[complete.cases(zx),]==zy[complete.cases(zy),])},ro1,ro5)
# R has a difficult doing the above so do it manually.
# Changing the alphs level should affect the fit or estimates of means or their standard errors.
# The residuals whould be the same.  Only confidence intervals and tests for differences should differ.
all(ro1$resid==ro5$resid)                     # Is TRUE as expected
all(ro1$varcomp==ro5$varcomp)                 # Is TRUE as expected
all(ro1$aov==ro5$aov)                         # Is TRUE as expected
all(ro1$Deltas[complete.cases(ro1$Deltas),1:6]==ro5$Deltas[complete.cases(ro5$Deltas),1:6]) # Is TRUE as expected: there 74 NAs
all(ro1$Deltas[,7:9]==ro5$Deltas[,7:9])       # Is FALSE as expected
all(ro1$LSM_TABLE[,1:5]==ro5$LSM_TABLE[,1:5]) # Is TRUE as expected. Columns 7-9 differ as expected because they are the lower nad upper confidence limits and the compact-letter-display indicating which means cluster together.


ro6 <- RCB_ModelFittingFunction(dfyld[1:19,]) # Throws out the following message and return the input data as input in ro6.
#Error: there are too few data (n<SufficientDataThreshold) to run an RCB model. Data frame has n = 19 rows and SufficientDataThreshold = 20




