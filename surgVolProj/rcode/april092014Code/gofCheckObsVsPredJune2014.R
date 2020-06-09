#we first get a plot to see wehther the fit is ok
dim(meanLosMat)

predVec=apply(meanLosMat,1,mean)

obsDf=ddply(anaDf.raw.1,'n.lag1.lap',function(x){c(obs=mean(as.numeric(as.character(x[,'los']))))})

predDf=data.frame(n.lag1.lap=seq(0,50,1), pred=predVec)
gofCheckDf.obsVsPred=join(obsDf,predDf)
#pdf()
plot(gofCheckDf.obsVsPred[,c(1,2)],type='p',ylim=c(1.5,2.5))
lines(gofCheckDf.obsVsPred[,c(1,3)])
#dev.off()


#next, I like to use predicted instead of observed

#I used the observed allocation to get predicted LOS they should be very close to the observed. We can verify on this.

say there are M providers, 
say w, one beta alpha parameter vector, M leanring cruves, using the M learning curves, we can solve an optimal alloction optAll(w) 
then based on the optAll(w) and obsAll we solve the optLos(w) and nonOptLos(w) 

Then we basically, need to loop over w




names(fit$model)

surgeonIdVn='mdnum1.r'
n.bt=10


k=15
fit=fitList[[k]]


(load(file="Z:/j_scrdata/lapLearn/bsplineSensitivity_feb052014.RData"))

fit2parameterMatrix=function(fit,n.bt,surgeonIdVn, seed){
  set.seed(seed)
  coeffMatrixList=list()
  surgeonIds = levels(fit$model[,surgeonIdVn])

  vcov=solve(fit$Hessian) #this should be inv of negative hessian. but the output is already negative hessian...so I do not use negative
  
  foundLoc=match(paste(surgeonIdVn,surgeonIds,sep=''),names(coef(fit)))
  foundLoc.naNA=foundLoc[!is.na(foundLoc)]
  
  for(i in 1:length(surgeonIds)){
    cat('processing i=', i, '\n')
    
    coefVec= coef(fit)
    
    if (i==1){
      coefVec[foundLoc.naNA]=0
    } else {
     loc.one =match(paste(surgeonIdVn,surgeonIds[i],sep=''),names(coef(fit)))
     coefVec[foundLoc.naNA]=0
     coefVec[loc.one]=1
    }
    coeffMatrixList=lappend.yz(coeffMatrixList, mnormt::rmnorm(n=n.bt,mean=coefVec, varcov=vcov))
  }
  
  names(coeffMatrixList)=surgeonIds
  return(coeffMatrixList)
}

seed=1
parameterMatrix.eachProvider=fit2parameterMatrix(fit,n.bt,surgeonIdVn,seed)
dim(parameterMatrix.eachProvider[[1]])

for(j in seq(1:n.bt)){
  parameter.j=lapply(parameterMatrix.eachProvider,function(x){x[j,]})
}




matList=fit2parameterMatrix(fit,10)
betaMat=matList$betaMat
head(betaMat)
alphaMat=matList$cutsMat


names(fit$data)
levels(fit$model$mdnum1.r)



learningCurveList

function(allocation, fit, n.bt){
  
  #there are M providers.
  #there are n.bt betaVetors
  #say provider m, then that provider would have a n.bt learning curve.....
  #so in total there are M times n.bt learning curves.
  
  #step one is to create a learning curve list, each element is one provider
  
  
  
  k=15
  fit=fitList[[k]]
  length(fitList)
  nbt=200
  
  lagvol.bs=bsWrapper.yz(log1p(lagvolvec) #this is the x in bs function
                         , inner.knots.list[[k]] #inner knots, if it contains boundary knots, function will stop and issue erro message
                         , 'n.lag1.lap.log.bs' #output data's columen name stem
                         , degree=bsDegree
                         , boundary.knots= boundary.knots.list[[k]] #boundary.knots
                         , intercept=FALSE
                         ,  dataType='data.frame'
                         #or ='data.frame'
                         , closenessCutoffPct=0.02 #closeness of inner knots to bdnots, lower than this number means it is too close.. and not good, 0.02 means two percent of range
  )$bsdata
  
  nobsformu=passVarToFormula.yz('', c('age4cat', 'race5cat','comorbcat','pay1','year','mdnum1.r'))
  n.basis=ncol(lagvol.bs)
  bsformu=passVarToFormula.yz('', paste('n.lag1.lap.log.bs',seq(n.basis),sep=''))
  
  
  meanLosMat=matrix(NA,ncol=nbt,nrow=length(lagvolvec))
  rownames(meanLosMat)=lagvolvec
  colnames(meanLosMat)=seq(nbt)
  
  
  noLagBsDf=deldfcols.yz(anaDf.raw.1,c(paste('n.lag1.lap.log.bs',seq(n.basis),sep='')))
  x.noBs=model.matrix(nobsformu,noLagBsDf)
  
  xbout.bs=xb.matched.yz(betaMat,x.bs)
  dim(x.noBs)
  colnames(x.noBs)

  
  
  
  for (r in 1:nrow(coeffMatrix)){
    cuts=coeffMatrix[r,1:nCuts] 
    beta=coeffMatrix[r,(nCuts+1):ncol(coeffMatrix)]
    
    
    #the following block actually be replaced by getAvgLearnCurve(), if you have time
    noLagBsDf=deldfcols.yz(anaDf.raw.1,c(paste('n.lag1.lap.log.bs',seq(n.basis),sep='')))
    x.noBs=model.matrix(nobsformu,noLagBsDf)
    xbout.noBs=xb.matched.yz(beta,x.noBs)
    #xbout.noBs$unmatched
    xb.noBs=xbout.noBs$xb.matched
    
    for(vi in 1:length(lagvolvec)){
      print(vi)
      newDf.vi = as.data.frame(lagvol.bs[vi,,drop=F][rep(1,nrow(anaDf.raw.1)),])
      x.bs=model.matrix(bsformu, data = newDf.vi)
      xbout.bs=xb.matched.yz(beta,x.bs) #xb.matched.yz(b,x.bs)
      xb.bs=xbout.bs$xb.matched
      #xbout.bs$matched
      xb=xb.bs+xb.noBs
      cuts=fit$alpha
      probMat=xbCuts2probmat.orderedprobit.yz(xb,cuts)
      meanLosMat[vi,r]=mean(probMat %*% matrix(seq(1:(length(cuts)+1)), ncol=1)) 
    }
    #the above block actually be replaced by getAvgLearnCurve(), if you have time
    
  }
  
  
  
  
  return(btLosVec)
}

