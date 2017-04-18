modelLogic <-function(data,CROP_OBSRVTN_DETAIL_ID,FIELD_ID,FACTOR_1,REP_ID,Abs_R, Abs_C, output.path){
}

R.ASReml_RCB<-function(data,path){
  
  CROP_OBSRVTN_DETAIL_ID="response"
  FIELD_ID="locationID"
  FACTOR_1="factor1"
  REP_ID="repId"
  Abs_R = "absoluteRange"
  Abs_C = "absoluteColumn"
  output.path="./"
  
  out = R.ASReml_RCB_Return(data,CROP_OBSRVTN_DETAIL_ID,FIELD_ID,FACTOR_1,REP_ID,Abs_R, Abs_C)
  #write.csv(out$DIFF_TABLE,file=paste(output.path,"/DIFF_TABLE.csv",sep=""),row.names=F)
  
  #write.table("----------------Modeling output----------------",
  #file=paste(output.path,"/ASReml_output.txt",sep=""),
  #append=FALSE,quote=FALSE,row.names=FALSE,col.names=FALSE)
  #write.table(out$console$modeling, file=paste(output.path,"/ASReml_output.txt",sep=""),
  #append=TRUE,quote=FALSE,row.names=FALSE,col.names=FALSE)
  return (out)
}

R.ASReml_RCB_Return<-function(data,path){
  
  check.path <- c("P1", "P2", "P3", "P4", "P5")
  
  if (!path %in% check.path) stop("Invalid 'PATH' value")
  path
  
  
  require(asreml)      #NOT on CRAN
  require(asremlPlus)  #on CRAN
  require(Matrix)      #on CRAN
  
  if (Sys.getenv("ASREML_LICENSE_FILE") == "") {
    Sys.setenv("ASREML_LICENSE_FILE"="/usr/local/asreml3/bin/asreml.lic")
  }
  
  # hard code parameter values
  CROP_OBSRVTN_DETAIL_ID="response"
  FIELD_ID="locationID"
  FACTOR_1="factor1"
  REP_ID="repId"
  Abs_R = "absoluteRange"
  Abs_C = "absoluteColumn"
  output.path="./"
  alpha=.05
  start.all=Sys.time()
  
  Out.return<-NULL
  Out.return$message<-NULL
  
  ### Ensure that all variable names are in the dataset.
  all.var.names<-c(CROP_OBSRVTN_DETAIL_ID,FIELD_ID,REP_ID,FACTOR_1,Abs_R, Abs_C)
  if(sum(all.var.names %in% colnames(data))!=length(all.var.names)){
    not.in<-all.var.names[which(!(all.var.names %in% colnames(data)))]
    message("ERROR: Variable(s) ",not.in," not in data header")
    Out.return$message<-paste("ERROR: Variable(s)",not.in,"not in data header",sep="")
    return(Out.return)
  }
  #keep only needed variables
  data<-data[,c(CROP_OBSRVTN_DETAIL_ID,FIELD_ID,REP_ID,FACTOR_1)]
  
  ### In each column, replace each of the following with "-" for the specified reason
  ##  1)"," because the code needs to parse entry and covariate value by "," later in the code
  data<-apply(data,2,gsub,pattern=",",replacement="-")
  ##  2)" " because ASReml does not like spaces
  ## data<-apply(data,2,gsub,pattern=" ",replacement="-")
  ## only replace spaces for Entry Category
  # data[,Factortype.name]<-gsub(" ","-",data[,Factortype.name])
  ##  3)":" because this character is reserved for specifying an interaction terms in formulas
  data<-apply(data,2,gsub,pattern=":",replacement="-")
  data<-data.frame(data)
  
  ### prepend "AAAA" to the beginning of the (previously specified)
  ### control to ensure it is the first entry in the sorted list,
  ### so that ASReml considers it as the control.
  data[,FACTOR_1]<-as.character(data[,FACTOR_1])                #convert factor to character
  #control.indecies<-which(data[,Factortype.name]=="CONTROL")  #records that are control
  
  #  if(length(unique(data[control.indecies,FACTOR_1]))>1){
  #   message(paste(FACTOR_1,
  #                " has more than one level specified as control, only the first will be returned.",
  #               sep=""))}
  ### take the first(and hopefully only) unique control factor name
  #Control.level<-unique(data[control.indecies,FACTOR_1])[1]
  #adjusted.control.name<-paste("AAAA",Control.level,sep="")   #prepend A's
  #data[data[,FACTOR_1]==Control.level,FACTOR_1]<-adjusted.control.name ##Change name in data
  
  #Convert data types
  data[,CROP_OBSRVTN_DETAIL_ID]<-as.numeric(as.character(data[,CROP_OBSRVTN_DETAIL_ID]))
  data[,FIELD_ID]<-as.factor(data[,FIELD_ID])
  data[,FACTOR_1]<-as.factor(data[,FACTOR_1])
  data[,REP_ID]<-as.factor(data[,REP_ID])
  
  
  if(nrow(data)<4000){time.scale<-c("secs"," seconds")}else{time.scale<-c("mins"," minutes")} #for timing
  #######################################################################################################
  ########################################## Modeling/prediction Stage #############################################
  #######################################################################################################
  start=Sys.time()
  message(paste("Running ASReml using:",sep=""),appendLF = TRUE )
  
  ###Specify the fixed, sparse and random formulas:
  switch(path,
         P1= {
           assign("fixed.formula",
                  formula(paste(CROP_OBSRVTN_DETAIL_ID,"~ 1 + ",FACTOR_1, sep="")),
                  envir = .GlobalEnv)
         },
         P2= {
           assign("fixed.formula",
                  formula(paste(CROP_OBSRVTN_DETAIL_ID,"~ 1 + ",FACTOR_1, sep="")),
                  envir = .GlobalEnv)
         },
         P3= {
           assign("fixed.formula",
                  formula(paste(CROP_OBSRVTN_DETAIL_ID,"~ 1 ", sep="")),
                  envir = .GlobalEnv)
         },
         P4= {
           assign("fixed.formula",
                  formula(paste(CROP_OBSRVTN_DETAIL_ID,"~ 1 + ",FACTOR_1, sep="")),
                  envir = .GlobalEnv)
         },
         P5= {
           assign("fixed.formula",
                  formula(paste(CROP_OBSRVTN_DETAIL_ID,"~ 1 ", sep="")),
                  envir = .GlobalEnv)
         }
         
  )
  
  switch(path,
         P1={
           print (" Model1: Single LOC Multi REP BLUE")
           assign("random.formula",
                  formula(paste("~",REP_ID,sep="")),
                  envir = .GlobalEnv)
         },
         P2= {
           print (" Model2: Multi LOC Single REP BLUE")
           assign("random.formula",
                  formula(paste("~ ",FIELD_ID,sep="")),
                  envir = .GlobalEnv)
         },
         P3= {
           print (" Model3: Multi LOC SINGLE REP BLUP")
           assign("random.formula",
                  formula(paste("~ ", FIELD_ID," + ", FACTOR_1,sep="")),
                  envir = .GlobalEnv)
         },
         P4={
           print (" Model4: Multi LOC MULTI REP BLUE")
           assign("random.formula",
                  formula(paste("~",FIELD_ID,"+",FIELD_ID,":",REP_ID,"+", FIELD_ID,":",FACTOR_1,sep="")),
                  envir = .GlobalEnv)
         },
         P5={
           print (" Model5: Multi LOC MULTI REP BLLUP")
           assign("random.formula",
                  formula(paste("~",FIELD_ID,"+",FIELD_ID,":",REP_ID,"+",FACTOR_1,"+","FIELD_ID",":","FACTOR_1", sep="")),
                  envir = .GlobalEnv)
         }
  )
  
  ############################################################################################
  # Run R-ASReml, capture output
  ## modeling.output <- capture.output(
  RCB.asr <- asreml(fixed = fixed.formula,
                    random = random.formula,
                    tolerance=1E-10,
                    data=data,
                    predict=list(classify=paste(FACTOR_1),
                                 ## levels=pred.input,
                                 present=list(),
                                 ignore=character(0),
                                 except=character(0),
                                 only=character(0),
                                 associate=list(),
                                 margin=FALSE,
                                 average=list(),
                                 as.average=list(),
                                 vcov=FALSE,
                                 sed = list(dotstring=T),
                                 parallel=FALSE,
                                 inrandom=TRUE,
                                 exrandom=logical(0),
                                 aliased=FALSE,
                                 estimable=FALSE,
                                 xform=list()),workspace=1000*1e6/8,pworkspace=1000*1e6/8)
  ## )
  modeling.output <- RCB.asr
  message("finished in ",round(difftime(Sys.time(),start,units=time.scale[1]),digits=2),time.scale[2])
  
  #######################################################################################################
  #######################################################################################################
  start=Sys.time()
  message("Creating output files...",appendLF = FALSE)
  #######################################################################################################
  ############################################ LS Means #################################################
  #######################################################################################################
  
  ##residual degrees of freedom
  dm<-Matrix::sparse.model.matrix(fixed.formula,data) #sparse design matrix
  ##        n   -               # missing_obs              -              rank(X)
  DDoF<-nrow(dm)-sum(is.na(data[,CROP_OBSRVTN_DETAIL_ID]))- Matrix::rankMatrix(x=dm,method="qr.R")[[1]]
  
  ###IGNORE THE VARIABLE NAMES HERE: IT IS ALL INTERNAL
  ### --they all get renamed later!
  LSM<-data.frame(
    Entry=RCB.asr$predictions$pvals[[FACTOR_1]],
    Yield=RCB.asr$predictions$pvals$predicted.value,
    SE=RCB.asr$predictions$pvals$standard.error)
  
  #create the CI
  LSM$CI_L<-LSM$Yield-qt(1-alpha/2,DDoF)*LSM$SE
  LSM$CI_U<-LSM$Yield+qt(1-alpha/2,DDoF)*LSM$SE
  ##replace adjusted control name with actual control name
  #levels(LSM$Entry)[levels(LSM$Entry)==adjusted.control.name] <- Control.level
  #write.csv(LSM,"LSMEANS.csv")
  #######################################################################################################
  
  
  Out.return$LSM_TABLE<-LSM
  Out.return$console$modeling<-modeling.output
  
  library(jsonlite)
  
  
  
  return(Out.return)
}


