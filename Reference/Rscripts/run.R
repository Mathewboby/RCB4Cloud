library(jsonlite)
source("/repos/RCB4Cloud/R/RCB_SupportFunctions.R")
source("/repos/RCB4Cloud/R/RCB_MainFunction.R")

data.in <- fromJSON('Reference/Data/input.json')
params.in <- fromJSON('Reference/Data/parameters.json')

results <- RCB_ModelFittingFunction(data.in, params.in)

