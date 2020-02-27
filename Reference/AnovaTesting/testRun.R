# Load needed packages
suppressMessages(library(jsonlite, quietly=TRUE))
suppressMessages(library(aws.s3,   quietly=TRUE))
daeURL <- "http://cran.wustl.edu//src/contrib/Archive/dae/dae_2.7-20.tar.gz"
suppressMessages(install.packages(daeURL,repos=NULL,type="source"))
suppressMessages(library(dae, quietly=TRUE))
#install.packages("dplyr")
#devtools::install_version("dae", version = "3.0-23", repos = "https://cran.r-project.org")
suppressMessages(library(asreml,     quietly=TRUE))
asremlPlusURL <- "http://cran.wustl.edu//src/contrib/Archive/asremlPlus/asremlPlus_2.0-12.tar.gz"
suppressMessages(install.packages(asremlPlusURL,repos=NULL,type="source"))
suppressMessages(library(asremlPlus, quietly=TRUE))
#options(digits = 20)
# Source code to be tested.

source('/repos/RCB4Cloud/R/RCB_SupportFunctions.R')
source('/repos/RCB4Cloud/R/RCB_MainFunction.R')

params.in                 <- fromJSON('Reference/AnovaTesting/parameters-rcb-md.json')
data.mlmr                 <- fromJSON('Reference/AnovaTesting/input-rcb-md.json')

data.mlmr$isPlaceHolder <- FALSE
params.in$isPlaceHolder <- "isPlaceHolder"

data.mlmr[, params.in$value]         <- as.numeric(data.mlmr[, params.in$value])
data.mlmr[, params.in$factorLevelId] <- as.character(data.mlmr[, params.in$factorLevelId])
data.mlmr[, params.in$subSiteId]     <- as.character(data.mlmr[, params.in$subSiteId])
data.mlmr[, params.in$repId]         <- as.character(data.mlmr[, params.in$repId])


resultsmlmr    <- RCB_ModelFittingFunction(data.mlmr, params.in, analysisType = 'P4')
