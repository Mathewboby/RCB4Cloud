library(RCB)
context("Check that the deltas are correct against known data")

test_that("Checking the ouput against RCBInputExample for path P4", {
  # Prep data
  library(openxlsx)
  data <- read.xlsx( file.path(system.file("data", package = "RCB"), "RCBInputExample.xlsx"), sheet = "RCB Input" )
  test_output <- read.xlsx( file.path(system.file("data", package = "RCB"), "RCBInputExample.xlsx"), sheet = "metric" )

  # Compare output deltas with the output in test_output
  package_test <- R.ASReml_RCB_Return(data, analysis_type = "P4")
  test_output <- test_output[, c("HEAD_ANALYSIS_ENTRY_CONCAT",
                                  "COMPARE_ANALYSIS_ENTRY_CONCAT_1",
                                  "MEAN_DELTA",
                                  "TRIM(TO_CHAR(ROUND(GRP_COMP_ACSS_LOC.P_VALUE,4),999990.9999))",
                                  "SE_DELTA") ]
  comp_table <- merge( test_output, package_test$Deltas,
                        by.x = c("HEAD_ANALYSIS_ENTRY_CONCAT", "COMPARE_ANALYSIS_ENTRY_CONCAT_1"),
                        by.y = c( "head", "comp" ),
                        all.x = FALSE,
                        all.y = FALSE )
  comp_table[, c("diff", "p.diff", "sed")] <- round(comp_table[, c("diff", "p_diff", "sed")], 4)

  expect_equal(comp_table$MEAN_DELTA, comp_table$diff, tolerance = 1.0e-4)
})

##  compare each element of the anova output to another version
BLUEcomp <- function(xout,yout){
  xlsm <- xout$LSM_TABLE
  ylsm <- yout$LSM_TABLE
  c1   <- all(paste0(xlsm[,1])==paste0(ylsm[,1]))
  c8   <- all(paste0(xlsm[,8])==paste0(ylsm[,8]))
  c27  <- apply(xlsm[,2:7]-ylsm[2:7],2,function(zx){max(abs(zx),na.rm=TRUE)})
  lso  <- data.frame(c1=c1,c2=c27[1],c3=c27[2],c4=c27[3],c5=c27[4],c6=c27[5],c7=c27[6],c8=c8)

  xdlt <- xout$Deltas
  ydlt <- yout$Deltas
  c1   <- all(paste0(xdlt[,1])==paste0(ydlt[,1]))
  c2   <- all(paste0(xdlt[,2])==paste0(ydlt[,2]))
  c38  <- apply(xdlt[,3:9]-ydlt[,3:9],2,function(zx){max(abs(zx),na.rm=TRUE)})
  dto  <- data.frame(c1=c1,c2=c2,c3=c38[1],c3=c27[2],c4=c38[3],c5=c38[4],c6=c38[5],c7=c38[6],c8=c38[7],c9=c38[8])

  xaov <- xout$aov
  yaov <- yout$aov
  c1   <- all(rownames(xaov)==rownames(yaov))
  c26  <- apply(xaov[,1:6]-yaov[,1:6],2,function(zx){max(abs(zx))})
  avo  <- data.frame(c1=c1,c2=c26[1],c3=c26[2],c4=c26[3],c5=c26[4],c6=c26[5],c7=c26[6])

  xvrc <- xout$varcomp
  yvrc <- yout$varcomp
  vro  <- data.frame(c1=all(rownames(xvrc)==rownames(yvrc)),
                     c2=max(abs(xvrc$variance_estimates-yvrc$variance_estimates),na.rm=TRUE),
                     c3=all(xvrc$constraint==yvrc$constraint))
  xrsd <- xout$resid
  yrsd <- yout$resid
  rdt  <- apply(xrsd-yrsd,2,function(zx){max(abs(zx),na.rm=TRUE)})
  rso  <- data.frame(c1=rdt[1],c2=rdt[2],c3=rdt[3])

  lsto <- list(LSM_TABLE=lso,Deltas=dto,aov=avo,varcomp=vro,resid=rso)
  return(lsto)
}

TestRCBOnS3InputParametersYLDFiles <- function(xxx){

  source('/repos/RCB4Cloud/R/RCB_SupportFunctions.R')
  source('/repos/RCB4Cloud/R/RCB_MainFunction.R')

  # Input aws credentials
  Sys.setenv("AWS_ACCESS_KEY_ID" = "XXXXXXXXXXXXXXXXXXXX",
             "AWS_SECRET_ACCESS_KEY" = "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy",
             "AWS_DEFAULT_REGION" = "bbbbbbbbb")

  S3BucketName <- "trait-analytics-np"
  #RCBdataList  <- rawToChar(get_object(object="/projects/RecordsOfFilesWritten/YLDTestFiles.csv",bucket=S3BucketName))
  zrcb <- aws.s3::s3read_using(read.csv, object = "s3://trait-analytics-np/projects/RecordsOfFilesWritten/YLDTestFiles.csv")
  RCBdataList <- as.character(zrcb[,1])
  # Can't csv files from S3 yet, but can read a JSON-file.
  RCBdataListJ  <- fromJSON(rawToChar(get_object(object="/projects/RecordsOfFilesWritten/YLDTestFiles.json",bucket=S3BucketName)))

  rcblisti <- c("/projects/rcb/business-funcs/breeding/regions/rsa/crops/corn/seasons/2017_10/programs/a4/sets/ncb_baab01/observations/yld/runs/1554826526/input.json",
                "/projects/rcb/business-funcs/breeding/regions/rsa/crops/corn/seasons/2017_10/programs/a4/sets/ncb_baab02/observations/yld/runs/1554826526/input.json",
                "/projects/rcb/business-funcs/breeding/regions/rsa/crops/corn/seasons/2017_10/programs/a1/sets/ncb_1aa001/observations/yld/runs/1554826526/input.json",
                "/projects/rcb/business-funcs/breeding/regions/rsa/crops/corn/seasons/2017_10/programs/a1/sets/ncb_1aa002/observations/yld/runs/1554826526/input.json",
                "/projects/rcb/business-funcs/breeding/regions/rsa/crops/corn/seasons/2017_10/programs/a1/sets/ncb_1aa003/observations/yld/runs/1554826526/input.json",
                "/projects/rcb/business-funcs/breeding/regions/rsa/crops/corn/seasons/2017_10/programs/a1/sets/ncb_1aa004/observations/yld/runs/1554826526/input.json",
                "/projects/rcb/business-funcs/breeding/regions/rsa/crops/corn/seasons/2017_10/programs/a1/sets/ncb_1aa005/observations/yld/runs/1554826526/input.json",
                "/projects/rcb/business-funcs/breeding/regions/rsa/crops/corn/seasons/2017_10/programs/a1/sets/ncb_1aa006/observations/yld/runs/1554826526/input.json",
                "/projects/rcb/business-funcs/breeding/regions/rsa/crops/corn/seasons/2017_10/programs/a1/sets/ncb_1aa007/observations/yld/runs/1554826526/input.json",
                "/projects/rcb/business-funcs/breeding/regions/rsa/crops/corn/seasons/2017_10/programs/a1/sets/ncb_1aa008/observations/yld/runs/1554826526/input.json",
                "/projects/rcb/business-funcs/breeding/regions/rsa/crops/corn/seasons/2017_10/programs/a1/sets/ncb_1aa009/observations/yld/runs/1554826526/input.json",
                "/projects/rcb/business-funcs/breeding/regions/rsa/crops/corn/seasons/2017_10/programs/a1/sets/ncb_1aa010/observations/yld/runs/1554826526/input.json",
                "/projects/rcb/business-funcs/breeding/regions/rsa/crops/corn/seasons/2017_10/programs/a1/sets/ncb_1aa011/observations/yld/runs/1554826526/input.json",
                "/projects/rcb/business-funcs/breeding/regions/rsa/crops/corn/seasons/2017_10/programs/a1/sets/ncb_1aa012/observations/yld/runs/1554826526/input.json",
                "/projects/rcb/business-funcs/breeding/regions/rsa/crops/corn/seasons/2017_10/programs/a1/sets/ncb_1aa013/observations/yld/runs/1554826526/input.json",
                "/projects/rcb/business-funcs/breeding/regions/rsa/crops/corn/seasons/2017_10/programs/a1/sets/ncb_1aa014/observations/yld/runs/1554826526/input.json",
                "/projects/rcb/business-funcs/breeding/regions/rsa/crops/corn/seasons/2017_10/programs/a1/sets/ncb_1aa015/observations/yld/runs/1554826526/input.json",
                "/projects/rcb/business-funcs/breeding/regions/rsa/crops/corn/seasons/2017_10/programs/a1/sets/ncb_1aa016/observations/yld/runs/1554826526/input.json")

  rcblistp <- gsub("input","parameters",rcblisti)
  # all(rcblisti==RCBdataList) returns TRUE
  # all(rcblisti==RCBdataListJ) returns TRUE
  rcbdata <- vector(length(rcblistp),mode='list')

  # it <- 1:length(rcblisti)
  # for(kk in it){
  #   rcbdata[[kk]] <- fromJSON(rawToChar(get_object(object=rcblisti[kk],bucket=S3BucketName)))
  # }

  rcbdpl <- fromJSON(rawToChar(get_object(object=rcblistp[1],bucket=S3BucketName)))

  rcbout <- RCB_ModelFittingFunction(rcbdata[[1]],rcbdpl)

  #zkk <- aws.s3::s3read_using(fromJSON, object = paste0("s3://trait-analytics-np",rcblisti[kk]))

  RCBoutputs <- vector(length(rcblistp),mode='list')
  for(kk in it){
    RCBoutputs[[kk]] <- RCB_ModelFittingFunction(rcbdata[[kk]],rcbdpl)
  }
  rcbdata2 <- vector(length(rcblistp),mode='list')
  it <- 1:length(rcblisti)
  for(kk in it){
    rcbdata2[[kk]] <- aws.s3::s3read_using(fromJSON, object=paste0(paste0("s3://",S3BucketName),rcblisti[kk]))
  }
  RCBoutputs <- vector(length(rcblistp),mode='list')
  for(kk in it){
    RCBoutputs[[kk]] <- RCB_ModelFittingFunction(rcbdata2[[kk]],rcbdpl)
  }

  MaxRCBoutputs <- vector(length(rcblistp),mode='list')
  rcblisto <- gsub("input","output",rcblisti)
  for(kk in it){
    MaxRCBoutputs[[kk]] <- aws.s3::s3read_using(fromJSON, object=paste0(paste0("s3://",S3BucketName),rcblisto[kk]))
  }
 return(???)
}

brokenUnitTests() <- function(xxx){

  ######### Unit Test of the RCB Model ############
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

  # R has a difficult doing the above so do it manually.
  # Changing the alphs level should affect the fit or estimates of means or their standard errors.
  # The residuals whould be the same.  Only confidence intervals and tests for differences should differ.
  all(ro1$resid==ro5$resid)                     # Is TRUE as expected
  all(ro1$varcomp==ro5$varcomp)                 # Is TRUE as expected
  all(ro1$aov==ro5$aov)                         # Is TRUE as expected
  all(ro1$Deltas[complete.cases(ro1$Deltas),1:6]==ro5$Deltas[complete.cases(ro5$Deltas),1:6]) # Is TRUE as expected: there 74 NAs
  all(ro1$Deltas[,7:9]==ro5$Deltas[,7:9])       # Is FALSE as expected
  all(ro1$LSM_TABLE[,1:5]==ro5$LSM_TABLE[,1:5]) # Is TRUE as expected. Columns 7-9 differ as expected because they are the lower nad upper confidence limits and the compact-letter-display indicating which means cluster together.

  return(???)
}



