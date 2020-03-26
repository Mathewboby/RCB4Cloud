######################### $$$$
sm1        <- fromJSON("/repos/RCB4Cloud/Reference/Data/prod_out/p360/SIMPLEMEANS8957-18367-STLP-outputToP360.json")
idat       <- fromJSON("/repos/RCB4Cloud/Reference/Data/prod_out/p360/DSR_4757-18402-SC-outputToP360.json")
sm1Data    <- sm1$modelOutputs$simpleMeans$input$data[[1]][,c(3, 4, 8, 9)]
sm1gfactor <- sm1$modelOutputs$simpleMeans$means$global$factors[[1]]
sm1ffactor <- sm1$modelOutputs$simpleMeans$means$local$subsites[[1]]$factors[[1]]
zmerg      <- merge(sm1gfactor,sm1ffactor, by='id', all=T)

zmerg$Multiplier <- paste0(round(zmerg$value.y/zmerg$value.x))

zmerg$Multiplier[which(zmerg$Multiplier == "1e+10")] <- paste(c(1,rep(0,10)), sep="",collapse="")
zmerg$Multiplier[which(zmerg$Multiplier == "1e+09")] <- paste(c(1,rep(0, 9)), sep="",collapse="")
zmerg$Multiplier[which(zmerg$Multiplier == "1e+08")] <- paste(c(1,rep(0, 8)), sep="",collapse="")
zmerg$Multiplier[which(zmerg$Multiplier == "1e+06")] <- paste(c(1,rep(0, 6)), sep="",collapse="")
zmerg$Multiplier[which(zmerg$Multiplier == "1e+05")] <- paste(c(1,rep(0, 5)), sep="",collapse="")

idat  <- fromJSON("/repos/RCB4Cloud/Reference/Data/prod_out/p360/DSR_4757-18402-SC-outputToP360.json")
adat  <- fromJSON("/repos/RCB4Cloud/Reference/Data/prod_out/p360/ANOVA_GLOBAL_BLUE_696-18402-SC-outputToP360.json")
agdat <- adat$modelOutputs$rcbBlue$input$data[[1]]
table(agdat$subSiteId, agdat$repId)
#                                       1
# 4bcb4652-860d-4398-9475-156d15f0a224 36
# 90ea907c-ce78-41b9-8600-9b86d8846edc 36
# a35a1343-5b8d-4ee1-a1b8-b546016ab0d8 32
# e8e9e9e6-028e-427d-8dcd-c18fa24fd9f8 36

idatu <- idat$modelOutputs$dsr$input$data[[1]]
table(idatu$subSiteId, idatu$repId)
#                                       1
# 4bcb4652-860d-4398-9475-156d15f0a224 36
# 90ea907c-ce78-41b9-8600-9b86d8846edc 36
# a35a1343-5b8d-4ee1-a1b8-b546016ab0d8 32
# e8e9e9e6-028e-427d-8dcd-c18fa24fd9f8 36
mga      <- aggregate(value~factorLevelId, data=agdat, function(zx){mean(zx, na.rm=T)}) # ANOVA inputdata
mgi      <- aggregate(value~factorLevelId, data=idatu, function(zx){mean(zx, na.rm=T)}) # DSR inputdata
mmg      <- merge(mga,mgi, by="factorLevelId")
mmg$diff <- mmg$value.y-mmg$value.x

smbs9.402    <- fromJSON("/repos/RCB4Cloud/Reference/Data/prod_out/p360/SIMPLEMEANSBYSUBSITE9-18402-SC-outputToP360.json")
id9.402 <- smbs9.402$modelOutputs$simpleMeans$input$data[[1]]
if9.402 <- smbs9.402$modelOutputs$simpleMeans$means$local$subsites[[1]]$factors[[1]]
names(if9.402) <- c('id', 'value.z')

smbs8.402    <- fromJSON("/repos/RCB4Cloud/Reference/Data/prod_out/p360/SIMPLEMEANSBYSUBSITE8-18402-SC-outputToP360.json")
id8.402 <- smbs8.402$modelOutputs$simpleMeans$input$data[[1]]
if8.402 <- smbs8.402$modelOutputs$simpleMeans$means$local$subsites[[1]]$factors[[1]]
names(if8.402) <- c('id', 'value.y')

smbs3.402    <- fromJSON("/repos/RCB4Cloud/Reference/Data/prod_out/p360/SIMPLEMEANSBYSUBSITE3-18402-SC-outputToP360.json")
id3.402 <- smbs3.402$modelOutputs$simpleMeans$input$data[[1]]
if3.402 <- smbs3.402$modelOutputs$simpleMeans$means$local$subsites[[1]]$factors[[1]]
names(if3.402) <- c('id', 'value.x')

smbs2.402    <- fromJSON("/repos/RCB4Cloud/Reference/Data/prod_out/p360/SIMPLEMEANSBYSUBSITE2-18402-SC-outputToP360.json")
id2.402 <- smbs2.402$modelOutputs$simpleMeans$input$data[[1]]
if2.402 <- smbs2.402$modelOutputs$simpleMeans$means$local$subsites[[1]]$factors[[1]]
names(if2.402) <- c('id', 'value.w')

mi23       <- merge(if2.402, if3.402, by='id', all=T)
mi89       <- merge(if8.402, if9.402, by='id', all=T)
mi2389     <- merge(mi23, mi89, by='id', all=T)
mi2389$ave <- apply(mi2389[, 2:5], 1, function(zx){mean(zx, na.rm=TRUE)}) # the average of the 4 subsites' data equals the average in mmg which where calculated from the DSR and ANOVA input datasets.
mimmg      <-  merge(mi2389, mmg, by.x='id', by.y='factorLevelId', all=T)


#
##
###
####


