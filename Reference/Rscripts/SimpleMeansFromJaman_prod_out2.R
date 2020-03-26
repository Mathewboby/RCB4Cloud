library(jsonlite)
library(readxl)

zdir <- "/repos/RCB4Cloud/Reference/Data/prod_out/p360/"
lstfls <- list.files("/repos/RCB4Cloud/Reference/Data/prod_out/p360/")
smFileId   <- c(which(substr(lstfls, 11, 12) == "S8"),
                which(substr(lstfls, 11, 12) == "S9"))
smbsFileId <-   which(substr(lstfls, 11, 12) == "SB")

SMsummaryFunc <- function(zi){
  smi                <- fromJSON(paste0(zdir,lstfls[zi]))
  smigsmmf           <- (as.data.frame(do.call(cbind,
                                               lapply(smi$modelOutputs$simpleMeans$means$global$factors[[1]], unlist))))
  smimosm_input_data <- smi$modelOutputs$simpleMeans$input$data[[1]][, 1:10]

  smilsmms  <-            smi$modelOutputs$simpleMeans$means$local$subsites[[1]]
  smilsmmsm <- as.numeric(smi$modelOutputs$simpleMeans$means$local$subsites[[1]]$value)
  smilsmmsf <-            smi$modelOutputs$simpleMeans$means$local$subsites[[1]]$factors
  smilsmmsa <-            smi$modelOutputs$simpleMeans$means$local$subsites[[1]]$answers
  smilsmmsc <-            smi$modelOutputs$simpleMeans$means$local$subsites[[1]]$controls
  zms <- c(mean(           smilsmmsm               , na.rm=TRUE),
           mean(           smimosm_input_data$value, na.rm=TRUE),
           mean(as.numeric(smilsmmsf[[1]]$value)   , na.rm=TRUE))
  return(list(means=zms, differences=diff(round(zms, 10))))
}

SMBSsummaryFunc <- function(zi){
  smbsi                <- fromJSON(paste0(zdir,lstfls[zi]))
  smbsigsmmf           <- (as.data.frame(do.call(cbind, lapply(smbsi$modelOutputs$simpleMeans$means$global$factors[[1]], unlist))))
  smbsimosm_input_data <- smbsi$modelOutputs$simpleMeans$input$data[[1]][, 1:10]

  smbsilsmms  <-            smbsi$modelOutputs$simpleMeans$means$local$subsites[[1]]
  smbsilsmmsm <- as.numeric(smbsi$modelOutputs$simpleMeans$means$local$subsites[[1]]$value)
  smbsilsmmsf <-            smbsi$modelOutputs$simpleMeans$means$local$subsites[[1]]$factors
  smbsilsmmsa <-            smbsi$modelOutputs$simpleMeans$means$local$subsites[[1]]$answers
  smbsilsmmsc <-            smbsi$modelOutputs$simpleMeans$means$local$subsites[[1]]$controls
  zms <- c(mean(           smbsilsmmsm               , na.rm=TRUE),
           mean(           smbsimosm_input_data$value, na.rm=TRUE),
           mean(as.numeric(smbsilsmmsf[[1]]$value    , na.rm=TRUE)))
  return(list(means=zms, differences=diff(round(zms, 10))))
}

smm <- do.call(rbind, lapply(smFileId, function(zi){SMsummaryFunc(zi)$means}))
smd <- do.call(rbind, lapply(smFileId, function(zi){SMsummaryFunc(zi)$differences}))

smbsm <- do.call(rbind, lapply(smbsFileId, function(zi){SMBSsummaryFunc(zi)$means}))
smbsd <- do.call(rbind, lapply(smbsFileId, function(zi){SMBSsummaryFunc(zi)$differences}))

#
##
###
####


