
# newData=nomiss.lag6
# lagvol.vec=seq(min(nomiss.lag6[,'n.lag1.lap']),max(nomiss.lag6[,'n.lag1.lap']),1)
# n.sims=10
# 
# knots.list=cleaned.obj$knots 
# bdknots.list=cleaned.obj$bdknots
# knots=knots.list$lag1
# bdknots=bdknots.list$lag1
# lagvol.vn='n.lag1.lap'
# ran.seed=1
# degree=2 
genAverageLearningCurve = function(modelFit
                                   , newData
                                   , lagvol.vec
                                   , n.sims
                            ,lagvol.vn #'n.lag1.lap' should be in newData
                            ,knots
                            ,bdknots
                            ,vnStem='lag1.nlap.bs'
                            ,degree=2 #bspline degree
                            ,ran.seed=1
){
  #basically score each surgeon for all the patients belong to a region (e.g., a state, or a county or something) he resides.  
  J=length(modelFit$y.levels) #number of ordered choices
  betaVec=modelFit$beta
  coef=modelFit$coefficients
  covmat=vcov(modelFit)
  set.seed(ran.seed)
  coefMat=mnormt::rmnorm(n = n.sims, mean=modelFit$coefficients, vcov(modelFit))
    
  formu=as.list(modelFit$call)$formula
  rhs.formu=paste('~',strsplit(as.character(formu), "~")[[3]],sep='')

  dataMat=model.matrix(as.formula(rhs.formu),data=newData)
  
  #next I need to regenerate the basis-splines
  bsDf=gen.bs(log1p(lagvol.vec)
              , knots
              , bdknots
              , degree
              , vnStem #e.g., 'lag6.nlap.bs'   
  )          
  meanLosMat=matrix(NA,nrow=length(lagvol.vec), ncol=n.sims)
  
  for(s in 1:n.sims){
    cat('simulation = ',s,'\n')
    cuts.s= coefMat[s,names(modelFit$alpha)]
    betaVec.s=coefMat[s,names(modelFit$beta)]
    for(i in 1:length(lagvol.vec)){
      #cat('vol',lagVol.vec[i], '\n')
      #XB=dataMat[,-c(1,ncol(dataMat))] %*% matrix(betaVec.s,ncol=1)
      dataMat[,paste(vnStem,seq(3),sep='')]=as.matrix(bsDf[i,][rep(1,nrow(dataMat)),]) #3 is bs1 bs2 bs3
      XB=dataMat[,-1] %*% matrix(betaVec.s,ncol=1)
      #cbind(colnames(dataMat[,-c(1,ncol(dataMat))]),names(betaVec.s))
      #now we change eblupDf eblup into the picked surgeon
      XBPlusRi=XB+0 #average one 
      #I did a check on the correlation between eblup and outcome and confirm that it should be PLUS not minus
      cumProbMat=matrix(NA,nrow=length(XBPlusRi),ncol=J)
      for (j in 1:(J-1)){
        cumProbMat[,j]=pnorm(cuts.s[j]-XBPlusRi)
      }
      cumProbMat[,J]=1
      probMat=array(NA,dim=dim(cumProbMat))
      probMat[,1]=cumProbMat[,1]
      for(j in 2:J){
        probMat[,j]=cumProbMat[,j]-cumProbMat[,j-1]
      }
      colnames(probMat)=modelFit$y.levels
      meanLosMat[i,s]=mean(probMat %*% matrix(seq(1:J), ncol=1))
      #print(meanLosMat[i,s])
    } #i loop
    
  } #s loop
  
  colnames(meanLosMat)=paste('sim', seq(n.sims),sep='.')
  #print(meanLosMat)
  outDf=data.frame(lag.vol=lagvol.vec,meanLosMat)
  return(outDf)
}