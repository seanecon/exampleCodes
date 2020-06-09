
n.sims=50

genPredictProb.ovtrt.CI <- function(fitObj, n.sims){
set.seed(1)
nchoice=length(dimnames(fitObj$probabilities)[[2]])
outMat <- array(NA, c(nchoice,6))
studyYears <- seq(2004,2009,1)
betaMat = mnormt::rmnorm(n=n.sims,mean=coef(fitObj),varcov=vcov(fitObj))
outList=list()

for (j in 1:nrow(betaMat)){
  cat('j=',j,'\n')
for (i in 1:length(studyYears) ){
  chosenRows <- (seq(nrow(fitObj$model)/nchoice)-1)*nchoice+1 
  X.choiceInvariant <- as.matrix(cbind(intercept=1,fitObj$model[,-1][chosenRows,]))
  if (i==1){X.choiceInvariant[,c("indexyeardummy.2005","indexyeardummy.2006","indexyeardummy.2007","indexyeardummy.2008","indexyeardummy.2009")] <- 0}
  if (i==2){X.choiceInvariant[,c("indexyeardummy.2005")] <- 1;X.choiceInvariant[,c("indexyeardummy.2006","indexyeardummy.2007","indexyeardummy.2008","indexyeardummy.2009")] <- 0}
  if (i==3){X.choiceInvariant[,c("indexyeardummy.2006")] <- 1;X.choiceInvariant[,c("indexyeardummy.2005","indexyeardummy.2007","indexyeardummy.2008","indexyeardummy.2009")] <- 0}
  if (i==4){X.choiceInvariant[,c("indexyeardummy.2007")] <- 1;X.choiceInvariant[,c("indexyeardummy.2005","indexyeardummy.2006","indexyeardummy.2008","indexyeardummy.2009")] <- 0}
  if (i==5){X.choiceInvariant[,c("indexyeardummy.2008")] <- 1;X.choiceInvariant[,c("indexyeardummy.2005","indexyeardummy.2006","indexyeardummy.2007","indexyeardummy.2009")] <- 0}
  if (i==6){X.choiceInvariant[,c("indexyeardummy.2009")] <- 1;X.choiceInvariant[,c("indexyeardummy.2005","indexyeardummy.2006","indexyeardummy.2007","indexyeardummy.2008")] <- 0}
  XB.Jminus1 <- X.choiceInvariant %*% matrix(betaMat[j,],ncol=nchoice-1,byrow=TRUE)
  
  troubleMaker=which(XB.Jminus1>10)
  if (length(troubleMaker)>0)  {XB.Jminus1[which(XB.Jminus1>10,arr=T)]=quantile(XB.Jminus1,probs=0.99)}
  EXPXB=cbind(1,exp(XB.Jminus1)) #reference level is the first one
  Pmat <- EXPXB/rowSums(EXPXB)
  outMat[,i] <- apply(Pmat,2,mean)
}
rownames(outMat) <- dimnames(fitObj$probabilities)[[2]] #reference level is the first one
colnames(outMat) <- studyYears

outList=lappend.yz(outList,outMat)
}

#remove these with NaN due to wacky variance...big variance
usefulIndex= seq(length(outList))[!unlist(lapply(outList,function(x){any(is.na(x))}))]
outList=outList[usefulIndex]

return(outList)
}


Pmat[3015,3]

Pmat[which(is.na(Pmat),arr=T)]

probMatList=outList



getConfidenceInterval = function(probMatList){
  set.seed(1) 
  lbMat=ubMat=array(NA,dim(probMatList[[1]]),dimnames=dimnames(probMatList[[1]]))
  for(i in 1:nrow(probMatList[[1]])){
    for(j in 1:ncol(probMatList[[1]])){
      
      bdVec= quantile(unlist(lapply(probMatList,function(x){x[i,j]})),probs=c(0.025,0.975))
      lbMat[i,j]=bdVec[1]
      ubMat[i,j]=bdVec[2]
    }
  }
  return(list(lbMat=lbMat,ubMat=ubMat))
}


getConfidenceInterval.imrtRobtSum = function(probMatList){
  set.seed(1)
  lbMat=ubMat=matrix(NA,nrow=1,ncol=ncol(probMatList[[1]]))
  colnames(lbMat)=colnames(ubMat)=dimnames(probMatList[[1]])[[2]]
    for(j in 1:ncol(probMatList[[1]])){
      bdVec= quantile(unlist(lapply(probMatList,function(x){sum(x[c('imrt','robot_radpromy'),j])})),probs=c(0.025,0.975))
      lbMat[1,j]=bdVec[1]
      ubMat[1,j]=bdVec[2]
    }
  
  return(list(lbMat=lbMat,ubMat=ubMat))
}

getConfidenceInterval.ebrtORapSum = function(probMatList){
  set.seed(1)
  lbMat=ubMat=matrix(NA,nrow=1,ncol=ncol(probMatList[[1]]))
  colnames(lbMat)=colnames(ubMat)=dimnames(probMatList[[1]])[[2]]
  for(j in 1:ncol(probMatList[[1]])){
    bdVec= quantile(unlist(lapply(probMatList,function(x){sum(x[c('ebrt','abdomen_radpromy'),j])})),probs=c(0.025,0.975))
    lbMat[1,j]=bdVec[1]
    ubMat[1,j]=bdVec[2]
  }
  
  return(list(lbMat=lbMat,ubMat=ubMat))
}


rm(subData.mlogitFit)
(load( 'Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.low10yrSurv.RData'))
fitObj <- subData.mlogitFit
probMatList = genPredictProb.ovtrt.CI(fitObj, 100)
output.lowSurv=list(
  probMatList=probMatList
  ,ci.byTreatment=getConfidenceInterval(probMatList)
  ,ci.imrtRobotSum=getConfidenceInterval.imrtRobtSum(probMatList)
  ,ci.ebrtOpenRapSum=getConfidenceInterval.ebrtORapSum(probMatList)
)


(load( 'Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.lowDamicoRisk.RData'))
fitObj <- subData.mlogitFit
probMatList = genPredictProb.ovtrt.CI(fitObj, 100)
output.lowDamico=list(
 probMatList=probMatList
,ci.byTreatment=getConfidenceInterval(probMatList)
,ci.imrtRobotSum=getConfidenceInterval.imrtRobtSum(probMatList)
,ci.ebrtOpenRapSum=getConfidenceInterval.ebrtORapSum(probMatList)
)

(load( 'Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.lowDamicoRisk.low10YrSurv.RData'))

fitObj <- subData.mlogitFit
probMatList = genPredictProb.ovtrt.CI(fitObj, 500)
output.lowSurvLowDamico=list(
  probMatList=probMatList
  ,ci.byTreatment=getConfidenceInterval(probMatList)
  ,ci.imrtRobotSum=getConfidenceInterval.imrtRobtSum(probMatList)
  ,ci.ebrtOpenRapSum=getConfidenceInterval.ebrtORapSum(probMatList)
)

output.lowSurvLowDamico$ci.byTreatment
output.lowSurvLowDamico$ci.imrtRobotSum
output.lowSurvLowDamico$ci.ebrtOpenRapSum

output.lowSurv$ci.byTreatment
output.lowSurv$ci.imrtRobotSum
output.lowSurv$ci.ebrtOpenRapSum

output.lowDamico$ci.byTreatment
output.lowDamico$ci.imrtRobotSum
output.lowDamico$ci.ebrtOpenRapSum


#save(output.lowSurvLowDamico,output.lowSurv,output.lowDamico,file='Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/ci_JAMA_Revision_partA.RData')
(load(file='Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/ci_JAMA_Revision_partA.RData'))


