######### Unit Test of the RCB Model ############

# Load needed packages
suppressMessages(library(jsonlite, quietly=TRUE))
suppressMessages(library(aws.s3,   quietly=TRUE))
suppressMessages(install.packages('dae',repos = "http://cran.wustl.edu/"))
asremlPlusURL <- "http://cran.wustl.edu//src/contrib/Archive/asremlPlus/asremlPlus_2.0-12.tar.gz"
suppressMessages(install.packages(asremlPlusURL,repos=NULL,type="source"))
suppressMessages(library(asreml,     quietly=TRUE))
suppressMessages(library(asremlPlus, quietly=TRUE))

# Source code to be tested.
source('/repos/RCB4Cloud/R/RCB_SupportFunctions.R')
source('/repos/RCB4Cloud/R/RCB_MainFunction.R')

# Set the path to a nice RSA yield data set in S3
# yf1 <- "/projects/qaqc/business-funcs/breeding/regions/rsa/crops/corn/seasons/2017_10/"
# yf2 <- "programs/a4/sets/ncb_baab01/observations/yld/runs/9876543210/input.json"
# yf <- paste0(yf1,yf2)

# # Set AWS credentials to access S3
# Sys.setenv("AWS_ACCESS_KEY_ID" = "XXXXXXXXXXXXXXXXXXXX",
#            "AWS_SECRET_ACCESS_KEY" = "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy",
#            "AWS_DEFAULT_REGION" = "bbbbbbbbb")

# Read in the JSON-data file from S3, which needs S3 credentials made system variables first
# Old way
# dfyld <- fromJSON(rawToChar(get_object(object = yf,bucket="trait-analytics-np")))
# New way
# dfyld <- as.data.frame(s3read_using(fromJSON,object=paste0("s3://trait-analytics-np",yf)))
# Write out this file as a JSON-file locally for easier access and to avoid s3 authentication
# write_json(dfyld,"/repos/RCB4Cloud/Reference/Data/RCB_input_data.json",
#                  digits=12, pretty=TRUE,auto_unbox=TRUE)

# Read in RCB default parameter list from Dominio
RCB_DefaultParameterList <- fromJSON("/repos/RCB4Cloud/Reference/Data/RCB_DefaultParameterList.json")
# Echo guts of RCB_DefaultParameterList
RCB_DefaultParameterList
# Read in S3-YLD-data-file for local Domino directory
dfyld <- as.data.frame(fromJSON("/repos/RCB4Cloud/Reference/Data/RCB_input_data.json"))
# For testing purposes create the following columns in dfyld
dfyld$numValue <- dfyld$NUM_VALUE
dfyld$fieldId  <- dfyld$FIELD_NAME
dfyld$repId    <- dfyld$BR_REP_ID
dfyld$factor1  <- dfyld$GERMPLASM_ID # can also use dfyld$TEST_SET_ENTRY_ID or dfyld$entryId
dfyld$experimentalUnitId <- as.character(dfyld$PLOT_ID)


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
                                ResponseVariableColumnName  = "NUM_VALUE",
                                FieldIDColumnName           = "FIELD_NAME",
                                TreatmentFactorColumnName   = "GERMPLASM_ID",
                                ReplicateIDColumnName       = "BR_REP_ID",
                                FactorTypeColumnName        = "PLOT_ID",
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


ro6 <- RCB_ModelFittingFunction(dfyld[1:19,]) # Throws out the following message and return the input data in ro6.
#Error: there are too few data (n<SufficientDataThreshold) to run an RCB model. Data frame has n = 19 rows and SufficientDataThreshold = 20
all(ro6==dfyld[1:19,]) # TRUE as expected.



