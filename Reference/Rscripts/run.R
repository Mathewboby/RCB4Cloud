
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

data.in <- fromJSON('Reference/Data/input-broke.json')
params.in <- fromJSON('Reference/Data/parameters.json')

results <- RCB_ModelFittingFunction(data.in, params.in)

write_json(results, 'Reference/Data/output.json')
