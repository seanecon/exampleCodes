#each provider will have an outcome gain vector

gen.g.list = function(  inDf
                      , providerIdVn
                      , modelFit
                      , transfVolumeMatrix #already transformed (if bs operatation needed, then they are already in bs matrix)
                        #number of columns are number of bs
                        #each row is a specific raw volume
                      ){

  providerVec=unique(inDf[,providerIdVn])
  g.list=list() 
  for(i in 1:length(providerVec)){
    g.list=lappend.yz(g.list, get.FE.model.indivLearnCurve(modelFit, providerVec[i], inDf, transfVolumeMatrix))
  }
  names(g.list)=providerVec
  return(g.list)
}




get.FE.model.indivLearnCurve = function(modelFit, providerIdInterest, newData,  transfVolumeMatrix){
  #basically score each surgeon for all the patients belong to a region (e.g., a state, or a county or something) he resides.  
  J=length(modelFit$y.levels) #number of ordered choices
  cuts=modelFit$alpha; betaVec=modelFit$beta
  formu=as.list(modelFit$call)$formula
  rhs.formu=paste('~',strsplit(as.character(formu), "~")[[3]],sep='')
  #providerIdVn='mdnum1.r'
  #just change newData's md into the md of interest
  newData[,providerIdVn]=providerIdInterest
  dataMat=model.matrix(as.formula(rhs.formu),data=newData)
  meanLosVec=rep(NA,length(lagVol.vec))

  for(i in 1:nrow(transfVolumeMatrix)){
  
    #XB=dataMat[,-c(1,ncol(dataMat))] %*% matrix(betaVec,ncol=1) #this is for random effect model
    XB=dataMat[,-1] %*% matrix(betaVec,ncol=1) 
    #now we change eblupDf eblup into the picked surgeon
    XBPlusRi=XB+0
    #I did a check on the correlation between eblup and outcome and confirm that it should be PLUS not minus
    cumProbMat=matrix(NA,nrow=length(XBPlusRi),ncol=J)
    for (j in 1:(J-1)){
      cumProbMat[,j]=pnorm(cuts[j]-XBPlusRi)
    }
    cumProbMat[,J]=1
    probMat=array(NA,dim=dim(cumProbMat))
    probMat[,1]=cumProbMat[,1]
    for(j in 2:J){
      probMat[,j]=cumProbMat[,j]-cumProbMat[,j-1]
    }
    
    colnames(probMat)=modelFit$y.levels
    meanLosVec[i]=mean(probMat %*% matrix(seq(1:7), ncol=1))
  }
  
  outmat=cbind(lagVol.vec, meanLosVec)
  colnames(outmat)=c('lag.vol','mean.los')
  return(outmat)
}