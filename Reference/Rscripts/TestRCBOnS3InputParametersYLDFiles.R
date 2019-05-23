suppressMessages(library(jsonlite, quietly=TRUE))
suppressMessages(library(aws.s3,   quietly=TRUE))
suppressMessages(install.packages('dae',repos = "http://cran.wustl.edu/"))
asremlPlusURL <- "http://cran.wustl.edu//src/contrib/Archive/asremlPlus/asremlPlus_2.0-12.tar.gz"
suppressMessages(install.packages(asremlPlusURL,repos=NULL,type="source"))
suppressMessages(library(asreml,     quietly=TRUE))
suppressMessages(library(asremlPlus, quietly=TRUE))

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
