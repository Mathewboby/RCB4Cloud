library(jsonlite)
library(readxl)

zdir <- "/repos/RCB4Cloud/Reference/Data/prod_out/p360/"
lstfls <- list.files("/repos/RCB4Cloud/Reference/Data/prod_out/p360/")
smFileId   <- c(which(substr(lstfls, 11, 12) == "S8"),
                which(substr(lstfls, 11, 12) == "S9"))
smbsFileId <-   which(substr(lstfls, 11, 12) == "SB")

sm1 <- fromJSON("/repos/RCB4Cloud/Reference/Data/prod_out/p360/SIMPLEMEANS8957-18367-STLP-outputToP360.json")
sm2 <- fromJSON("/repos/RCB4Cloud/Reference/Data/prod_out/p360/SIMPLEMEANS8966-18354-STLC-outputToP360.json")
sm3 <- fromJSON("/repos/RCB4Cloud/Reference/Data/prod_out/p360/SIMPLEMEANS8976-18337-STLC-outputToP360.json")

sm1gsmmf           <- (as.data.frame(do.call(cbind, lapply(sm1$modelOutputs$simpleMeans$means$global$factors[[1]], unlist))))
sm1mosm_input_data <- sm1$modelOutputs$simpleMeans$input$data[[1]][, 1:10]

sm2gsmmf           <- (as.data.frame(do.call(cbind, lapply(sm2$modelOutputs$simpleMeans$means$global$factors[[1]], unlist))))
sm2mosm_input_data <- sm2$modelOutputs$simpleMeans$input$data[[1]][, 1:10]

sm3gsmmf           <- (as.data.frame(do.call(cbind, lapply(sm3$modelOutputs$simpleMeans$means$global$factors[[1]], unlist))))
sm3mosm_input_data <- sm3$modelOutputs$simpleMeans$input$data[[1]][, 1:10]

sm1lsmms  <- sm1$modelOutputs$simpleMeans$means$local$subsites[[1]]
sm1lsmmsf <- sm1$modelOutputs$simpleMeans$means$local$subsites[[1]]$factors
sm1lsmmsa <- sm1$modelOutputs$simpleMeans$means$local$subsites[[1]]$answers
sm1lsmmsc <- sm1$modelOutputs$simpleMeans$means$local$subsites[[1]]$controls

sm2lsmms  <- sm2$modelOutputs$simpleMeans$means$local$subsites[[1]]
sm2lsmmsf <- sm2$modelOutputs$simpleMeans$means$local$subsites[[1]]$factors
sm2lsmmsa <- sm2$modelOutputs$simpleMeans$means$local$subsites[[1]]$answers
sm2lsmmsc <- sm2$modelOutputs$simpleMeans$means$local$subsites[[1]]$controls

sm3lsmms  <- sm3$modelOutputs$simpleMeans$means$local$subsites[[1]]
sm3lsmmsf <- sm3$modelOutputs$simpleMeans$means$local$subsites[[1]]$factors
sm3lsmmsa <- sm3$modelOutputs$simpleMeans$means$local$subsites[[1]]$answers
sm3lsmmsc <- sm3$modelOutputs$simpleMeans$means$local$subsites[[1]]$controls


smbs1                <- fromJSON(paste0(zdir,lstfls[smbsFileId[1]]))
smbs1gsmmf           <- (as.data.frame(do.call(cbind, lapply(smbs1$modelOutputs$simpleMeans$means$global$factors[[1]], unlist))))
smbs1mosm_input_data <- smbs1$modelOutputs$simpleMeans$input$data[[1]][, 1:10]

smbs1lsmms  <-            smbs1$modelOutputs$simpleMeans$means$local$subsites[[1]]
smbs1lsmmsm <- as.numeric(smbs1$modelOutputs$simpleMeans$means$local$subsites[[1]]$value)
smbs1lsmmsf <-            smbs1$modelOutputs$simpleMeans$means$local$subsites[[1]]$factors
smbs1lsmmsa <-            smbs1$modelOutputs$simpleMeans$means$local$subsites[[1]]$answers
smbs1lsmmsc <-            smbs1$modelOutputs$simpleMeans$means$local$subsites[[1]]$controls

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

#cbind(sm1bslsmmsa[[1]], sm1bslsmmsf[[1]])

do.call(rbind, lapply(smFileId, function(zi){SMsummaryFunc(zi)$means}))
do.call(rbind, lapply(smFileId, function(zi){SMsummaryFunc(zi)$differences}))

do.call(rbind, lapply(smbsFileId, function(zi){SMBSsummaryFunc(zi)$means}))
do.call(rbind, lapply(smbsFileId, function(zi){SMBSsummaryFunc(zi)$differences}))

#
##
###
####


