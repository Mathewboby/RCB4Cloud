suppressMessages(library(jsonlite, quietly=TRUE))
suppressMessages(library(aws.s3,   quietly=TRUE))
suppressMessages(install.packages('dae',repos = "http://cran.wustl.edu/"))
asremlPlusURL <- "http://cran.wustl.edu//src/contrib/Archive/asremlPlus/asremlPlus_2.0-12.tar.gz"
suppressMessages(install.packages(asremlPlusURL,repos=NULL,type="source"))
suppressMessages(library(asreml,     quietly=TRUE))
suppressMessages(library(asremlPlus, quietly=TRUE))

source('/repos/RCB4Cloud/R/RCB_SupportFunctions.R')
source('/repos/RCB4Cloud/R/RCB_MainFunction.R')

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
ro3 <- RCB_ModelFittingFunction(z3o$anovaInputData$data,
                                z3o$anovaParameters,
                                z3o$anovaInputData$analysisType)  # Can also use z3o$anovaParameters$analysisType

####
####
#####