BLUEcomp <- function(xout,yout){
  xlsm <- xout$LSM_TABLE
  ylsm <- yout$LSM_TABLE
  c1   <- all(paste0(xlsm[,1])==paste0(ylsm[,1]))
  c8   <- all(paste0(xlsm[,8])==paste0(ylsm[,8]))
  c27  <- apply(xlsm[,2:7]-ylsm[2:7],2,function(zx){max(abs(zx),na.rm=TRUE)})
  lso  <- data.frame(c1=c1,c2=c27[1],c3=c27[2],c4=c27[3],c5=c27[4],c6=c27[5],c7=c27[6],c8=c8)

  xdlt <- xout$Deltas
  ydlt <- yout$Deltas
  c1   <- all(paste0(xdlt[,1])==paste0(ydlt[,1]))
  c2   <- all(paste0(xdlt[,2])==paste0(ydlt[,2]))
  c38  <- apply(xdlt[,3:9]-ydlt[,3:9],2,function(zx){max(abs(zx),na.rm=TRUE)})
  dto  <- data.frame(c1=c1,c2=c2,c3=c38[1],c3=c27[2],c4=c38[3],c5=c38[4],c6=c38[5],c7=c38[6],c8=c38[7],c9=c38[8])

  xaov <- xout$aov
  yaov <- yout$aov
  c1   <- all(rownames(xaov)==rownames(yaov))
  c26  <- apply(xaov[,1:6]-yaov[,1:6],2,function(zx){max(abs(zx))})
  avo  <- data.frame(c1=c1,c2=c26[1],c3=c26[2],c4=c26[3],c5=c26[4],c6=c26[5],c7=c26[6])

  xvrc <- xout$varcomp
  yvrc <- yout$varcomp
  vro  <- data.frame(c1=all(rownames(xvrc)==rownames(yvrc)),
                     c2=max(abs(xvrc$variance_estimates-yvrc$variance_estimates),na.rm=TRUE),
                     c3=all(xvrc$constraint==yvrc$constraint))
  xrsd <- xout$resid
  yrsd <- yout$resid
  rdt  <- apply(xrsd-yrsd,2,function(zx){max(abs(zx),na.rm=TRUE)})
  rso  <- data.frame(c1=rdt[1],c2=rdt[2],c3=rdt[3])

  lsto <- list(LSM_TABLE=lso,Deltas=dto,aov=avo,varcomp=vro,resid=rso)
  return(lsto)
}