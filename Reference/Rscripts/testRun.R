# Load needed packages
suppressMessages(library(jsonlite, quietly=TRUE))
suppressMessages(library(aws.s3,   quietly=TRUE))
#suppressMessages(install.packages('dae',repos = "http://cran.wustl.edu/"))
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

resultsmlmr    <- RCB_ModelFittingFunction(data.mlmr, params.in, analysisType = 'P4')
