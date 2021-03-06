

S3BucketName      <- "trait-analytics-np"
s3AnovaRunPathsDF <- read.csv("/repos/RCB4Cloud/Reference/AnovaTesting/anova-global-19-12-2019.csv")
s3AnovaRunPaths   <- paste0(s3AnovaRunPathsDF[ ,1])
zpath0            <- s3AnovaRunPaths[1]

s3FileReader <- function(zS3BucketName, zpath0){
  zpath              <- trimws(gsub("input.json","",zpath0))
  bucket.names       <- get_bucket(zS3BucketName, prefix = zpath)
  s3filePaths        <- sapply(bucket.names,function(zt){zt$Key})
  names(s3filePaths) <- NULL
  s3.data <- lapply(s3filePaths, function(zt){
    curr.name <- paste("s3:/", zS3BucketName, zt, sep = "/")
    curr.data <- s3read_using(fromJSON, object = URLencode(curr.name))
    return(curr.data)
  })
  names(s3.data) <- c("anovaInputData","anovaOutputList","anovaParameters")
  return(s3.data)
}

z3o <- s3FileReader(S3BucketName, s3AnovaRunPaths[1])
# Need to source from master branch of TADS/RCB4Cloud
# /repos/RCB4Cloud/R/RCB_MainFunction.R
# /repos/RCB4Cloud/R/RCB_SupportFunctions.R
z3o$anovaParameters$isPlaceHolder     <- "isPlaceHolder"
z3o$anovaInputData$data$isPlaceHolder <- FALSE
ro3 <- RCB_ModelFittingFunction(z3o$anovaInputData$data,
                                z3o$anovaParameters,
                                z3o$anovaInputData$analysisType)  # Can also use z3o$anovaParameters$analysisType

MD_P4data   <- fromJSON("/repos/RCB4Cloud/Reference/AnovaTesting/input-rcb-md.json")
MD_P4params <- fromJSON("/repos/RCB4Cloud/Reference/AnovaTesting/parameters-rcb-md.json")

MD_P4data$FieldName <- paste0("Field",as.character(MD_P4data$FieldName))  # Originally is a character string number
MD_P4data$RepNumber <- paste0("Rep",as.character(MD_P4data$RepNumber))    # Originally is an integer

MD_P4data$isPlaceHolder   <- FALSE
MD_P4params$isPlaceHolder <- "isPlaceHolder"

table(MD_P4data$FieldName, MD_P4data$RepNumber)
# 1  2  3
# 0009 18 18 18
# 0011 18 18 18
# 0024 18 18 18

rMDo <- RCB_ModelFittingFunction(MD_P4data, MD_P4params, "P4")


readFit <- function(zS3BucketName, zAddress){
  zJdat         <- s3FileReader(zS3BucketName, zAddress)
  zDataIn       <- zJdat$anovaInputData$data
  zAnalysisType <- zJdat$anovaInputData$analysisType
  zJOutput      <- zJdat$anovaOutputList
  zJparams      <- zJdat$anovaParameters
  zDataIn[,zJparams$subSiteId]     <- paste0(    zDataIn[, zJparams$subSiteId])
  zDataIn[,zJparams$repId]         <- paste0(    zDataIn[, zJparams$repId])
  zDataIn[,zJparams$factorLevelId] <- paste0(    zDataIn[, zJparams$factorLevelId])
  zDataIn[,zJparams$value]         <- as.numeric(zDataIn[, zJparams$value])
  zDataIn$isPlaceHolder   <- FALSE
  zJparams$isPlaceHolder  <- "isPlaceHolder"
  rcbFit                  <- RCB_ModelFittingFunction(zDataIn, zJparams, zAnalysisType)

  if(zAnalysisType == "P1" |  zAnalysisType == "P2" | zAnalysisType == "P4")
  {
    cbm <- round(as.numeric(zJOutput$blueTable$value) -
                 as.numeric(rcbFit$blueTable$value),9)
    cbs <- round(as.numeric(zJOutput$blueTable$standardError) -
                 as.numeric(rcbFit$blueTable$standardError),9)
    cbd <- round(as.numeric(zJOutput$blueTable$degreesFreedom) -
                 as.numeric(rcbFit$blueTable$degreesFreedom),9)
    cbo <- cbind(cbm,cbs,cbd)
  }
  if(zAnalysisType == "P3" |  zAnalysisType == "P5")
  {
    cbm <- round(as.numeric(zJOutput$blupTable$value) -
                   as.numeric(rcbFit$blupTable$value),9)
    cbs <- round(as.numeric(zJOutput$blupTable$standardError) -
                   as.numeric(rcbFit$blupTable$standardError),9)
    cbc <- round(as.numeric(zJOutput$blupTable$count) -
                   as.numeric(rcbFit$blupTable$count),9)
    cbo <- cbind(cbm,cbs,cbc)
  }
  return(cbo)
}

chksa <- unlist(lapply(s3AnovaRunPaths, function(zt){sum(abs(readFit(S3BucketName, zt)))}))
chkdc <- do.call(rbind,lapply(s3AnovaRunPaths, function(zt){apply(readFit(S3BucketName, zt),2,function(zzz){sum(abs(zzz))})}))

readFitData <- function(zS3BucketName, zAddress){
  zJdat         <- s3FileReader(zS3BucketName, zAddress)
  zDataIn       <- zJdat$anovaInputData$data
  zAnalysisType <- zJdat$anovaInputData$analysisType
  zJOutput      <- zJdat$anovaOutputList
  zJparams      <- zJdat$anovaParameters
  zDataIn[,zJparams$subSiteId]     <- paste0(    zDataIn[, zJparams$subSiteId])
  zDataIn[,zJparams$repId]         <- paste0(    zDataIn[, zJparams$repId])
  zDataIn[,zJparams$factorLevelId] <- paste0(    zDataIn[, zJparams$factorLevelId])
  zDataIn[,zJparams$value]         <- as.numeric(zDataIn[, zJparams$value])
  zDataIn$isPlaceHolder   <- FALSE
  zJparams$isPlaceHolder  <- "isPlaceHolder"
  rcbFit                  <- RCB_ModelFittingFunction(zDataIn, zJparams, zAnalysisType)
  olst <- list(dataIn        = zDataIn,
               output        = zJOutput,
               parms         = zJparams,
               analysisiType = zAnalysisType,
               rcbFit        = rcbFit)
  return(olst)
}

modelCheck <- function(zS3BucketName, zAddress){
  zJdat         <- s3FileReader(zS3BucketName, zAddress)
  zDataIn       <- zJdat$anovaInputData$data
  zAnalysisType <- zJdat$anovaInputData$analysisType
  zJOutput      <- zJdat$anovaOutputList
  zJparams      <- zJdat$anovaParameters
  zDataIn[,zJparams$subSiteId]     <- paste0(    zDataIn[, zJparams$subSiteId])
  zDataIn[,zJparams$repId]         <- paste0(    zDataIn[, zJparams$repId])
  zDataIn[,zJparams$factorLevelId] <- paste0(    zDataIn[, zJparams$factorLevelId])
  zDataIn[,zJparams$value]         <- as.numeric(zDataIn[, zJparams$value])
  agFormula <- paste0(zJparams$repId, "~", zJparams$subSiteId)
  repsPerSubSite <- aggregate(as.formula(agFormula), data=zDataIn, FUN=function(zc){length(unique(zc))})
  # print(agFormula)
  # print(repsPerSubSite)
  numberOfSubSites <- nrow(repsPerSubSite)
  maxNumberOfReps  <- max(repsPerSubSite[, zJparams$repId])
  if(numberOfSubSites == 1 & maxNumberOfReps >  1) {PredictedAnalysisType <- "P1"}
  if(numberOfSubSites >  1 & maxNumberOfReps == 1) {PredictedAnalysisType <- "P2_or_P3"}
  if(numberOfSubSites >  1 & maxNumberOfReps >  1) {PredictedAnalysisType <- "P4_or_P5"}
  output <- list(AnalysisTypePredicted = PredictedAnalysisType,
                 AnalysisTypeUsed      = zAnalysisType)
  return(output)
}

lapModelCheck   <- lapply(s3AnovaRunPaths, function(zt){modelCheck(S3BucketName, zt)})
lapModelCheck   <- as.data.frame(do.call(rbind, lapModelCheck))
colnames(chkdc) <- c("NumMismatchMeans","NumMismatchSE","NumMismatchDF")
zdfo            <- as.data.frame(cbind(lapModelCheck, chkdc))

####
####
#####