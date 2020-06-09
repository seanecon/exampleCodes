# a formula with to alternative specific variables (price and catch) and
# an intercept

#adjustedVars <- c('race','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','surv10YrProb3Cat','registry')

# refLevel <- 'observation'
# adjustedVars <- 'grdCat'

gen.mlogitFit <- function(subData, adjustedVars, refLevel){
  depVar <- 'trtType'
  tmpAna <- dummyMat.yz(subset(subData,select=adjustedVars),adjustedVars)
  tmpAnaDf <- data.frame(subData[,'trtType'],tmpAna)
  names(tmpAnaDf) <- c('trtType',colnames(tmpAna))
  rm(tmpAna)
  anaDf <- mlogit.data(tmpAnaDf, shape="wide", choice="trtType")
  #head(anaDf)
  #names(tmpAnaDf)
  fitObj <- mlogit(formula(paste('trtType',paste('0',
                                                 PlusSignInbetween(setdiff(names(anaDf),c('trtType','alt','chid')), factor_wrapper=0)
                                                 ,'0',sep="|"),sep='~')), data=anaDf, reflevel=refLevel)
  return(fitObj)
}

#nrow(subData)



# coef(fitObj)[1:4]
genPredictProb.ovtrt <- function(fitObj){
  outMat <- array(NA, c(3,4))
  studyYears <- seq(2004,2007,1)
  for (i in 1:length(studyYears) ){
    nchoice=length(dimnames(fitObj$probabilities)[[2]])
    chosenRows <- (seq(nrow(fitObj$model)/nchoice)-1)*nchoice+1 
    X.choiceInvariant <- as.matrix(cbind(intercept=1,fitObj$model[,-1][chosenRows,]))
    if (i==1){X.choiceInvariant[,c("indexyeardummy.2005","indexyeardummy.2006","indexyeardummy.2007")] <- 0}
    if (i==2){X.choiceInvariant[,c("indexyeardummy.2005")] <- 1;X.choiceInvariant[,c("indexyeardummy.2006","indexyeardummy.2007")] <- 0}
    if (i==3){X.choiceInvariant[,c("indexyeardummy.2006")] <- 1;X.choiceInvariant[,c("indexyeardummy.2005","indexyeardummy.2007")] <- 0}
    if (i==4){X.choiceInvariant[,c("indexyeardummy.2007")] <- 1;X.choiceInvariant[,c("indexyeardummy.2005","indexyeardummy.2006")] <- 0}
    XB.Jminus1 <- X.choiceInvariant %*% matrix(coef(fitObj),ncol=2,byrow=TRUE)
    EXPXB=cbind(1,exp(XB.Jminus1)) #reference level is the first one
    Pmat <- EXPXB/rowSums(EXPXB)
    outMat[,i] <- apply(Pmat,2,mean)
  }
  rownames(outMat) <- dimnames(fitObj$probabilities)[[2]] #reference level is the first one
  colnames(outMat) <- studyYears
  return(outMat)
}


#want age 65-74 prob
# (load('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.lowDamicoRisk.low10YrSurv.RData'))
# str(subData.mlogitFit)
# fitObj <- subData.mlogitFit



genPredictProb.ovtrt.ageGrp <- function(fitObj, ageGrp){
  outMat <- array(NA, c(3,4))
  studyYears <- seq(2004,2007,1)
  for (i in 1:length(studyYears) ){
    nchoice=length(dimnames(fitObj$probabilities)[[2]])
    chosenRows <- (seq(nrow(fitObj$model)/nchoice)-1)*nchoice+1 
    X.choiceInvariant <- as.matrix(cbind(intercept=1,fitObj$model[,-1][chosenRows,]))
    
    if ('ageCatdummy....85..Inf.' %in% names(fitObj$model)){
    if (ageGrp=='65-70') { X.choiceInvariant[,'ageCatdummy....70...75.'] <-  X.choiceInvariant[,'ageCatdummy....75...80.'] <-  X.choiceInvariant[,'ageCatdummy....80...85.'] <-  X.choiceInvariant[,'ageCatdummy....85..Inf.'] <- 0}
    if (ageGrp=='70-75') { X.choiceInvariant[,'ageCatdummy....70...75.'] <- 1;  X.choiceInvariant[,'ageCatdummy....80...85.']  <-  X.choiceInvariant[,'ageCatdummy....75...80.'] <-  X.choiceInvariant[,'ageCatdummy....85..Inf.'] <-  0}
    } else {
      
      if (ageGrp=='65-70') { X.choiceInvariant[,'ageCatdummy....70...75.'] <-  X.choiceInvariant[,'ageCatdummy....75...80.'] <-  X.choiceInvariant[,'ageCatdummy....80...85.'] <- 0}
      if (ageGrp=='70-75') { X.choiceInvariant[,'ageCatdummy....70...75.'] <- 1;  X.choiceInvariant[,'ageCatdummy....80...85.']  <-  X.choiceInvariant[,'ageCatdummy....75...80.']  <-  0}
      
    }

    if (i==1){X.choiceInvariant[,c("indexyeardummy.2005","indexyeardummy.2006","indexyeardummy.2007")] <- 0}
    if (i==2){X.choiceInvariant[,c("indexyeardummy.2005")] <- 1;X.choiceInvariant[,c("indexyeardummy.2006","indexyeardummy.2007")] <- 0}
    if (i==3){X.choiceInvariant[,c("indexyeardummy.2006")] <- 1;X.choiceInvariant[,c("indexyeardummy.2005","indexyeardummy.2007")] <- 0}
    if (i==4){X.choiceInvariant[,c("indexyeardummy.2007")] <- 1;X.choiceInvariant[,c("indexyeardummy.2005","indexyeardummy.2006")] <- 0}
    XB.Jminus1 <- X.choiceInvariant %*% matrix(coef(fitObj),ncol=2,byrow=TRUE)
    EXPXB=cbind(1,exp(XB.Jminus1)) #reference level is the first one
    Pmat <- EXPXB/rowSums(EXPXB)
    outMat[,i] <- apply(Pmat,2,mean)
  }
  rownames(outMat) <- dimnames(fitObj$probabilities)[[2]] #reference level is the first one
  colnames(outMat) <- studyYears
  return(outMat)
}



# data=finalSample1
# riskGrps <- c('hig | surv10Yr.low','mid | surv10Yr.low','low | surv10Yr.low')
# adjustedVars= c('raceCat','.damicorisk','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','registry')
#adjustedVars= c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','registry')
genFitSubData <- function(riskGrps, adjustedVars, data, saveObjPathObjName){
  subData <- subset(data, riskStrata %in% riskGrps,select=c('trtType',adjustedVars))
  base::cat('nobs=', nrow(subData),'\n')
  subData.mlogitFit <- gen.mlogitFit(droplevels(subData),adjustedVars, refLevel='observation')
  save(subData.mlogitFit, file=saveObjPathObjName)
}

# count(finalSample1,'riskStrata')[,1]
# count(finalSample1,'.stggrp.simple')[,1]
# count(finalSample1,'.stggrp.detailed')[,1]
#count(finalSample1,'.damicorisk')[,1]


fitObj2Output <- function(saveObjPathObjName){
  (load(saveObjPathObjName))
  predProb <- genPredictProb.ovtrt(subData.mlogitFit)
  #95% interval of odds ratio
  vcovmat <- vcov(subData.mlogitFit)
  coefVec <- coef(subData.mlogitFit)
  #model summary
  summary <- summary(subData.mlogitFit)
  rm(subData.mlogitFit)
  set.seed(1)
  coefSampleMat <- rmnorm(n = 1000, mean =coefVec, vcovmat) 
  betaCI <- matrix(unlist(alply(coefSampleMat,2,function(x){quantile(x,probs=c(0.025,0.975))})), ncol=2, byrow=TRUE)
  betaORCI <- cbind(coefVec, betaCI, exp(coefVec), exp(betaCI))
  rownames(betaORCI) <- names(coefVec)
  colnames(betaORCI) <- c('beta','95LB.beta','95UB.beta','OR','95LB.OR','95UB.OR')
  #collect output
  re=list(summary=summary, predProb=predProb, or=betaORCI)
  return(re)
}

#inmat <- (low10yrSurv.res$predProb)

rescale.basedOnTwoChoice <- function(inmat){
  
  outList <- list()
  for (i in 1:length(rownames(inmat))){
  rmCat <- rownames(inmat)[i]
  useMat <- inmat[setdiff(rownames(inmat),rmCat),]
  colSumVec <- colSums(useMat)
  outMat <- useMat/matrix(rep(colSumVec,length.out=2*length(colSumVec)),nrow=2,byrow=T)
  outList <- lappend.yz(outList,outMat)
  }
  return(outList)
}
