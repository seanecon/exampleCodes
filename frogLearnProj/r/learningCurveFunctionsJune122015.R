


#the following fucntion get model fit, we add year in adjustIndepVars.full and adjustIndpeVars.forCurve
fitStateFixedEffectModel=function(  allStateDf.random
                                    ,logVolVec.allData
                                    ,adjustIndepVars.full #can be NULL, if so no need to fit full model
                                    ,adjustIndepVars.forCurve
                                    ,logVolVn='logAnnualVol'
                                    ,innerKnots=c(100,400)
                                    ,bsDegree=2
                                    ,boundaryKnots=c(1,2036) #appled on logvol of ALL data not a random sample
                                    ,logVolConstant=1
){
  bslist=bsWrapper1.yz(allStateDf.random[,logVolVn] #this is the x in bs function
                       ,logtr(innerKnots,constant=1,logbase=exp(1))
                       , paste(logVolVn,'.bs',sep='') #output data's columen name stem
                       , degree=bsDegree
                       , dataType='data.frame'
                       , logtr(boundaryKnots,constant=1,logbase=exp(1)) 
  )
  names(bslist)
  n.basis=bslist$n.basis
  colnames(bslist$bsdata)
  outdf.with.bs=cbind(allStateDf.random,bslist$bsdata)
  names(outdf.with.bs)
  
  formu.forCurve=passVarToFormula.yz('died',c(paste(logVolVn,'.bs',seq(n.basis),sep=''),adjustIndepVars.forCurve))
  
  if (!is.null(adjustIndepVars.full)){
    formu.full=passVarToFormula.yz('died',c(paste(logVolVn,'.bs',seq(n.basis),sep=''),adjustIndepVars.full))
    
    print('fitting full modle')
    fit.full=glm(formu.full, data=outdf.with.bs, family='binomial')
    
  }
  
  print('fitting curve model')
  fit.forCurve=glm(formu.forCurve, data=outdf.with.bs, family='binomial')
  print('model fitting completed')
  
  
  if (!is.null(adjustIndepVars.full)){
    fitList=list(fit.full=fit.full, fit.forCurve=fit.forCurve)
  }else { fitList=list(fit.forCurve=fit.forCurve)}
  return(fitList)
}

ransamDf=function(df,maxN,seed=123456,replace=FALSE){
  set.seed(seed)
  if (nrow(df)>maxN){
    rowLoc=sample(seq(nrow(df)),maxN)
    output=df[rowLoc,]
    return(output)
  } else (return(df))
}

#get both a model for presentation (all variables) and a model for just curve generation
getModels=function(df # it is the entire data not a random sample
                   ,nobs.fit #number of observation used to fit the model I used 20000
                   # ,stateNameVec #the state vector c('all','AZ','CA','IA','MA','MD','NC','NJ','NY','WA','WI') if 'all' it is all states
                   ,adjustIndepVars.full
                   ,adjustIndepVars.forCurve
                   ,annualVolVn='annual.vol'
                   ,logVolVn='logAnnualVol'
                   ,innerKnots=c(100,400)
                   ,boundaryKnots=c(1,2036)
                   ,ranSeed=123456
                   
                   #,n.sim.beta=100
){
  df[,'hospst']=as.factor(df[,'hospst'])
  stateEffMat=dummyEffectCoding.yz(  df
                                     # each element in charVec should appear in levelVec
                                     , 'hospst'
                                     , list(levels(df$'hospst'))
                                     
                                     #each element is a vector levelVec for the correspodning varaible
                                     # the first element pf levelVec is the reference i.e., with -1 -1 -1.....
                                     
                                     , c('effect')
                                     #e.g. c('effect','dummy')
                                     #if not right length, it will be recycled
                                     , returnList=TRUE
                                     #if FALSE return a big matrix
                                     #if true then return list
                                     , #codedVnList
                                     #if not supplied then 
                                     #it will be automatically generated
  )$'hospst'
  
  df[,'hospst']=as.character(df[,'hospst'])
  
  df[,'year']=as.factor(df[,'year'])
  yearEffMat=dummyEffectCoding.yz(  df
                                    # each element in charVec should appear in levelVec
                                    , 'year'
                                    , list(levels(df$'year'))
                                    
                                    #each element is a vector levelVec for the correspodning varaible
                                    # the first element pf levelVec is the reference i.e., with -1 -1 -1.....
                                    
                                    , c('effect')
                                    #e.g. c('effect','dummy')
                                    #if not right length, it will be recycled
                                    , returnList=TRUE
                                    #if FALSE return a big matrix
                                    #if true then return list
                                    , #codedVnList
                                    #if not supplied then 
                                    #it will be automatically generated
  )$'year'
  
  df[,'year']=as.character(df[,'year'])
  
  #you need to make sure that the maximum volume for each state-year is included in the data...
  #how to ensure this....one way is to directly supply Boundary.knots instead of Boundary.knots.tiles...
  
  allStateDf.random=ransamDf(data.frame(df, stateEffMat, yearEffMat), nobs.fit, seed=ranSeed)
  
  fitModels=fitStateFixedEffectModel(  allStateDf.random
                                       #,logVolVec.allData
                                       ,df[,logVolVn] 
                                       ,adjustIndepVars.full
                                       ,adjustIndepVars.forCurve
                                       ,logVolVn='logAnnualVol'
                                       ,innerKnots=innerKnots
                                       ,bsDegree=2
                                       ,boundaryKnots=boundaryKnots #this will be applied to logVolVector of All data
                                       ,logVolConstant=1
  )
  
  #   
  #   curveModelFit=fitModels$fit.forCurve
  #   outlist=lappend.yz(allStateByStateCurves(curveModelFit,stateNameVec,knots.tiles=knots.tiles,n.sim.beta=n.sim.beta),fitModels$fit.full)
  #   
  #   return(outlist) 
  
  
  #add a likelood ratio test
  
  formu.linear.nolog.forcurve=passVarToFormula.yz('died',c('annual.vol',adjustIndepVars.forCurve))
  fit.linear.nolog.forcurve=glm(formu.linear.nolog.forcurve, data= allStateDf.random, family='binomial')
  
  #add a likelihood ratio test, use forCurve model for testing
  
  
  output=list(logVolVec.allData=df[,logVolVn], fit.forCurve=fitModels$fit.forCurve, fit.full=fitModels$fit.full, innerKnots=innerKnots, boundaryKnots=boundaryKnots, ranSeed=ranSeed, maxVol.allData=max(df[,annualVolVn]), fit.linear.nolog.forcurve=fit.linear.nolog.forcurve)
  return(output)
}

lrtest.yz=function(short, long){
  p.lrtest=1-pchisq(short$null.deviance-long$deviance, short$df.residual-long$df.residual)
  return(p.lrtest)
}

#the following function can generate state-year specific learning curve as well as all state in a year and all year for state
# and all state and all year curve

quickCurve=function(     modelFit
                         ,nBsTerms
                         , maxVol.allData
                         , logVolVec.allData
                         
                         #if which.state is NA and which.year is NA then maxVol.stateYearData is max volume of entire data
                         , logVolConstant=1
                         , n.ranSample=800 #sample 800 observations if you use all observation it may slow down the curve generation
                         #random sample 800 people usually is enough
                         
                         , logVolVn='logAnnualVol'
                         #, annualVolVn='annual.vol'
                         , boundaryKnots=c(1,2036)
                         , bsDegree=2
                         , innerKnots=c(100,400)
                         , ranSeed=1 #used to sample observations               
){
  hospVolVec=seq(1, maxVol.allData)
  bsForSim=bsWrapper1.yz(log(hospVolVec+logVolConstant) #this is the x in bs function
                         , logtr(innerKnots,constant=logVolConstant,logbase=exp(1))
                         #quantile(dataFit[,logVolVn],probs=knots.tiles)  #inner knots, if it contains boundary knots, function will stop and issue erro message
                         , paste(logVolVn,'.bs',sep='') #output data's columen name stem
                         , degree=bsDegree
                         , dataType='data.frame'
                         #or ='data.frame
                         , Boundary.knots=logtr(boundaryKnots,constant=logVolConstant,logbase=exp(1)) 
                         #quantile(dataFit[,logVolVn],prob=Boundary.knots.tiles)
  )$bsdata
  
  fitdata.state.year=modelFit$data
  #this should be supplied separately to make sure it is big enough
  xb.bs.mean=xb.matched.yz(coef(modelFit), bsForSim)$xb.matched
  
  if (nrow(fitdata.state.year)>n.ranSample){
    set.seed(ranSeed)
    rowVec=sample(1:nrow(fitdata.state.year),n.ranSample, replace=FALSE)
  } else {
    set.seed(ranSeed)
    rowVec=sample(1:nrow(fitdata.state.year),n.ranSample, replace=TRUE)
  }
  
  xMat=model.matrix(formula(modelFit),data=fitdata.state.year)
  rm(fitdata.state.year)
  xMat.sample=xMat[rowVec,,drop=FALSE]
  
  xMat.sample.noBs=deldfcols.yz(xMat.sample,paste('logAnnualVol.bs',seq(nBsTerms),sep=''))
  
  xb.sample.noBs.mean=xb.matched.yz(coef(modelFit),xMat.sample.noBs)$xb.matched
  
  cat('generating',  maxVol.allData ,'volume points', '\n')
  probVec.mean=rep(NA, maxVol.allData)
  for(i in 1:maxVol.allData){
    
    #mean vector
    latentindex.i.mean=xb.sample.noBs.mean+repmat.yz(xb.bs.mean[i,,drop=FALSE],nrow(xb.sample.noBs.mean),1)
    exp.latent=exp(latentindex.i.mean)
    probVec.mean[i]=mean(exp.latent/(1+exp.latent))
    
  } 
  return(probVec.mean)
}

# maxVol.stateYearData=600
# innerKnots=200
# boundaryKnots=c(1,2036)
# which.year='2004'
# which.state="AZ"
btCurve=function(    modelFit
                     , which.state
                     , which.year
                     #if which.state is NA anbd which.year is NA then it entire data (all year all state)
                     # if which.state is NA and which.year is a year say '2003' then it is all state in 2003
                     # if which.state is not NA say "AZ" and which.year is NA then it is AZ in all years
                     # if which.state is not NA say "AZ" and which.year is not NA  (say 2003) then it is AZ2003 learning curve
                     , maxVol.stateYearData #this may not be in modelFit$data because the data for fit is a random sample
                     , innerKnots #raw scale volume
                     , boundaryKnots #raw scale volume
                     , lbubTileV,ec=c(0.025,0.975)
                     , logVolConstant=1
                     , n.ranSample=800 #sample 800 observations if you use all observation it may slow down the curve generation
                     #random sample 800 people usually is enough
                     , n.sim.beta=100
                     , logVolVn='logAnnualVol'
                     , annual.vol.vn='annual.vol'
                     , bsDegree=2
                     , ranSeed=1 #used to sample observations               
){
  hospVolVec=seq(1, maxVol.stateYearData)
  policyMatrix=bsWrapper1.yz(log(hospVolVec+logVolConstant) #this is the x in bs function
                             , log(innerKnots +logVolConstant) 
                             #quantile(dataFit[,logVolVn],probs=knots.tiles)  #inner knots, if it contains boundary knots, function will stop and issue erro message
                             , paste(logVolVn,'.bs',sep='') #output data's columen name stem
                             , degree=bsDegree
                             , dataType='data.frame'
                             #or ='data.frame
                             , Boundary.knots=log(boundaryKnots +logVolConstant)  
                             #quantile(dataFit[,logVolVn],prob=Boundary.knots.tiles)
  )$bsdata
  
  if (is.na(which.state) & !is.na(which.year)){fitdata.state.year=subset(modelFit$data, year==which.year) } #specific state adn all year
  if (!is.na(which.state) & !is.na(which.year)){fitdata.state.year=subset(modelFit$data,hospst==which.state & year==which.year)} #specific state and specific year
  if (!is.na(which.state) & is.na(which.year)){fitdata.state.year=subset(modelFit$data, hospst==which.state) } #all state but specific year
  if (is.na(which.state) & is.na(which.year)){fitdata.state.year=modelFit$data} #all year all state
  
  #this should be supplied separately to make sure it is big enough
  
  btProbMat=matrix(NA,nrow=n.sim.beta, ncol=maxVol.stateYearData)
  probVec.mean=rep(NA, maxVol.stateYearData)
  #get the bootstrapped curves    
  betaMat=mvrnorm(n = n.sim.beta, coef(modelFit), vcov(modelFit), tol = 1e-6, empirical = FALSE)
  colnames(btProbMat)=paste('vol=',as.character(seq(1, maxVol.stateYearData)),sep='')
  rownames(btProbMat)=paste('sim.',as.character(seq(n.sim.beta)),sep='') 
  
  ranSampleData=ransamDf(fitdata.state.year,n.ranSample)
  nrow.fitdata.state.year=nrow(fitdata.state.year)
  rm(fitdata.state.year)
  
  nrow.ran=nrow(ranSampleData)
  
  cat('nrow.fitdata.state.year=',nrow.fitdata.state.year,'\n')
  
  cat('nrow.ran=',nrow.ran,'\n')
  inputMatrix = model.matrix(modelFit$formula, ranSampleData)
  inputMatrix.nonPolicy=deldfcols.yz(inputMatrix, colnames(policyMatrix))
  rm(inputMatrix)
  
  
  cat('get btstrapped curve','\n')
  xbPolicy3d=xbPolicy3d.yz(betaMat,inputMatrix.nonPolicy,policyMatrix)
  
  probPolicy3d=exp(xbPolicy3d)/(1+exp(xbPolicy3d))
  rm(xbPolicy3d)
  btCurves=apply(probPolicy3d,c(2,3),mean)
  
  #get mean curve
  coefMeanMat=matrix(coef(modelFit),nrow=1)
  colnames(coefMeanMat)=names(coef(modelFit))
  cat('get mean curve', '\n')
  xbPolicy3d.mean=xbPolicy3d.yz(coefMeanMat,inputMatrix.nonPolicy,policyMatrix)
  probPolicy3d.mean=exp(xbPolicy3d.mean)/(1+exp(xbPolicy3d.mean))
  curve.mean=apply(probPolicy3d.mean[,1,],2,mean)
  
  outlist=list(curve.mean=curve.mean, btCurves=btCurves)
  return(outlist)
}

test1=aaply(test, 2, function(x){
  mean.x=mean(x)
  lbub=quantile(x,probs=lbubTileVec)
  output=c(mean.x,lbub)
  names(output)=c('mean','lb','ub')
  return(output)
})


stateYearLearningCurves=function(  modelFit
                                   , which.state.vec
                                   , which.year.vec
                                   #, maxVolByStateYear
                                   , maxAnnualVolVec #tell the maximum volume we like to plot should have
                                   #length(which.state.vec)*length(which.year.vec) long, loop state and withn a state loop year
                                   #modelFit is based on a random sample. maxAnnualVol should based on all the data
                                   , innerKnots=c(100,400)
                                   , boundaryKnots=c(1,2036)
                                   , logVolConstant=1
                                   , n.ranSample=500
                                   , n.sim.beta=100){
  btProbMatList=list()
  meanProbVecList=list()
  for (s in 1:length(which.state.vec)){
    for  (y in 1:length(which.year.vec)) {
      cat('processing state', which.state.vec[s],'\n')
      cat('processing year', which.year.vec[y],'\n')
      
      btOutList=btCurve( modelFit
                         , which.state.vec[s]
                         , which.year.vec[y]
                         , maxAnnualVolVec[(s-1)*length(which.year.vec)+y]  
                         #tell the maximum volume we like to plot
                         , innerKnots
                         , boundaryKnots
                         , logVolConstant=1
                         , n.ranSample=n.ranSample
                         , n.sim.beta=100
                         , logVolVn='logAnnualVol'
                         , annual.vol.vn='annual.vol'
                         , bsDegree=2
                         , ranSeed=1)
      
      btProbMatList=lappend.yz(btProbMatList,btOutList$btCurves)
      meanProbVecList=lappend.yz(meanProbVecList,btOutList$curve.mean)
    }
  }
  names(btProbMatList)=names(meanProbVecList)=apply(expandGridListInput.yz(list(which.state.vec, which.year.vec)),1,function(charvec)
  {
    for (i in 1:length(charvec))
    {
      if (i==1) {output<-charvec[i]} else {output<-concat_twocharelem.yz(output,charvec[i],'')}
    }
    return(output)
  })
  
  curveList=list(btProbMatList=btProbMatList, meanProbVecList=meanProbVecList)
  return(curveList)
}



#the following function for solving soluiton

solveOptimalStateYearSolution=function(Gmat, roundBy, time_limit, vol){
  avgVolVec=pred.n.die.optAlloc=optNumHosp=rep(NA,nrow(Gmat))
  
  for(i in 1:nrow(Gmat)){
    Gvec = Gmat[i,]
    M.guess=ceiling(vol/length(Gvec))+5
    G.list=list()
    for(j in 1:M.guess){
      G.list=lappend.yz(G.list,Gvec)
    }
    
    solution.i.round = scilpPatAlloc_roundSolution.yz(
      vol
      , G.list #G is a list of payoff, with 0 patient, 1, 2......a maximum number of patients
      , roundBy=roundBy
      , allTreated=TRUE
      , time_limit=time_limit)
    
    pred.n.die.optAlloc[i]=solution.i.round$objval.scaledToTrueN
    optNumHosp[i]=length(which(solution.i.round$opt.n>0))
    avgVolVec[i]=vol/optNumHosp[i]
  }
  
  out=list(pred.n.die.optAlloc=pred.n.die.optAlloc, optNumHosp=optNumHosp, optAvgVol=avgVolVec)
  return(out)
}

get.volume.gMort.GMort=function(volume.avgMort){
  
  #gmat is wrong here, we need to use the volume * avg to 
  gmat=t(apply(volume.avgMort, 1, function(x){c(x[1],diff(seq(1,length(x))*x))}))
  rownames(gmat)=paste('bt', seq(nrow(gmat)),sep='.')
  colnames(gmat)=paste('vol',seq(ncol(gmat)),sep='.')
  Gmat=t(apply(gmat,1,cumsum))
  Gmat.withVolume0=cbind(rep(0,nrow(Gmat)),Gmat)
  outlist=list(Gmat.withVolume0=Gmat.withVolume0,Gmat=Gmat,gmat=gmat)
  return(outlist)
}

# outcomeAtObsAlloc.stateYear(bt.Gmat, stateYearSubDf)
# stateYearLearningCurve.btMat.G=bt.Gmat

stateYearLearningCurve.btMat.G=bt.Gmat
stateYearSubDf


outcomeAtObsAlloc.stateYear = function(stateYearLearningCurve.btMat.G
                                       ,stateYearSubDf
                                       , stateVn='hospst'
                                       , yearVn='year'){ 
  n.bt=nrow(stateYearLearningCurve.btMat.G)
  volDistr.stateYear=dlply(stateYearSubDf,c(stateVn,yearVn),function(x){count(x[,'dshospid'])})
  df.tmp= attr(volDistr.stateYear,'split_labels')
  df.tmp[[yearVn]]=as.character(df.tmp[[yearVn]])
  df.tmp[[stateVn]]=as.character(df.tmp[[stateVn]])
  stateYearLabel=concat_charvec.yz(df.tmp[1,,drop=TRUE],separator='.')
  maxVolInGMat=ncol(stateYearLearningCurve.btMat.G)-1
  maxVol=max(volDistr.stateYear[[stateYearLabel]][,'freq'])
  if (maxVol>maxVolInGMat){stop('maximum volume is greater than the maximum volume in G matrix')}
  mort.obsAlloc=colSums(apply(stateYearLearningCurve.btMat.G,1,function(x){x[volDistr.stateYear[[stateYearLabel]][,'freq']]}))
  return(mort.obsAlloc)
}

solOpt.stateYear=function(which.state
                          , which.year
                          , curveMat #this is obtained
                          #which state 'all', 'az
                          , cleanedDf    #e.g. aaaDf
                          , roundBy=10
                          , time_limit=5 
                          
){
  #   curveList=stateYearCurveList.cab
  #   cleanedDf=cleanedDf.cab
  stateYearSubDf=subset(cleanedDf,hospst==toupper(which.state) & year==which.year)
  #  year='2003'
  # state='AZ'
  bt.Gmat=get.volume.gMort.GMort(curveMat)$Gmat.withVolume0
  
  total.vol=nrow(stateYearSubDf) #total volume in the state and year
  
  
  ################
  #solving optimal
  ################
  cat('sovling the model....','\n')
  cat('sovling state=',which.state, '\n')
  cat('sovling year=',which.year, '\n')
  cat('nrow of bt.Gmat=',nrow(bt.Gmat), '\n')
  cat('ncol of bt.Gmat=',ncol(bt.Gmat), '\n')
  cat('time_limit=',time_limit, '\n')
  cat('total.vol=',total.vol, '\n')
  sol.sy=solveOptimalStateYearSolution(bt.Gmat, roundBy, time_limit, total.vol)
  sol.stateYear=list(state=which.state,year=which.year,sol.sy)
  return(sol.stateYear)
}

cleanedDf=
  
  
  fromOptSolToSolDf.core=function(  solution.stateYear
                                    , cleanedDf
                                    , curveMat
                                    , death.optDist.ciProbs=c(0.025,0.975)
                                    , death.obsDist.ciProbs=c(0.025,0.975)    
                                    , delta.nDie.ciProbs=c(0.025,0.975)
                                    , nHosp.optDist.ciProbs=c(0.025,0.975)
                                    , delta.nHosp.ciProbs=c(0.025,0.975)
                                    , delta.volPerHosp.ciProbs=c(0.025,0.975)
                                    , mort.obsDist.ciProbs=c(0.025,0.975)
                                    , pred.mort.optDist.ciProbs=c(0.025,0.975)
                                    , pred.delta.mort.ciProbs=c(0.025,0.975)
  ){
    
    pred.nDie.optDist=mean(solution.stateYear[[3]]$pred.n.die.optAlloc)
    cat('state=',solution.stateYear[[1]],'\n')
    cat('year=',solution.stateYear[[2]],'\n')
    
    stateYearSubDf=subset(cleanedDf,hospst==toupper(solution.stateYear[[1]]) & year==solution.stateYear[[2]])
    total.vol=nrow(stateYearSubDf) #total volume in the state and year
    
    #  year='2003'
    # state='AZ'
    bt.Gmat=get.volume.gMort.GMort(curveMat)$Gmat.withVolume0
    
    obs.nDie.obsDist=sum(stateYearSubDf[,'died'])
    obs.nHosp.obsDist=length(unique(stateYearSubDf[,'dshospid']))
    obs.volPerHosp.obsDist=mean(ddply(stateYearSubDf,"dshospid", nrow)[,2])
    
    
    pred.nDie.obsDist.vec=outcomeAtObsAlloc.stateYear(bt.Gmat, stateYearSubDf)
    pred.nDie.obsDist.meanCi=c(mean(pred.nDie.obsDist.vec),quantile(pred.nDie.obsDist.vec,probs=death.obsDist.ciProbs))
    
    pred.nDie.optDist.vec=solution.stateYear[[3]]$pred.n.die.optAlloc
    pred.nDie.optDist.meanCi=c(mean(pred.nDie.optDist.vec),quantile(pred.nDie.optDist.vec,probs=death.optDist.ciProbs))
    
    pred.delta.nDie.vec=solution.stateYear[[3]]$pred.n.die.optAlloc-pred.nDie.obsDist.vec
    pred.delta.nDie.meanCi=c(mean(pred.delta.nDie.vec), quantile(pred.delta.nDie.vec,probs=delta.nDie.ciProbs))
    
    pred.nHosp.optDist.meanCi=c(mean(solution.stateYear[[3]]$optNumHosp),quantile(solution.stateYear[[3]]$optNumHosp,probs=nHosp.optDist.ciProbs))
    
    pred.delta.nHosp.vec=solution.stateYear[[3]]$optNumHosp-obs.nHosp.obsDist
    pred.delta.nHosp.meanCi=c(mean(pred.delta.nHosp.vec), quantile(pred.delta.nHosp.vec,probs=delta.nHosp.ciProbs))
    
    pred.volPerHosp.optDist=mean(solution.stateYear[[3]]$optAvgVol)
    pred.delta.volPerHosp=solution.stateYear[[3]]$optAvgVol-obs.volPerHosp.obsDist 
    pred.delta.volPerHosp.meanCi=c(mean(pred.delta.volPerHosp), quantile(pred.delta.volPerHosp,probs=delta.volPerHosp.ciProbs))
    
    names(pred.delta.nHosp.meanCi)=c('delta.nHosp.mean','delta.nHosp.lb','delta.nHosp.ub')
    names(pred.delta.nDie.meanCi)=c('delta.nDie.mean','delta.nDie.lb','delta.nDie.ub')
    names(pred.delta.volPerHosp.meanCi)=c('delta.volPerHosp.mean','delta.volPerHosp.lb','delta.volPerHosp.ub')
    
    
    #mortality times 100
    obs.mort.obsDist=100*obs.nDie.obsDist/total.vol
    pred.mort.obsDist.vec=100*pred.nDie.obsDist.vec/total.vol
    #   pred.mort.obsDist = mean(pred.mort.obsDist.vec)
    pred.mort.obsDist.meanCi= c(mean(pred.mort.obsDist.vec),quantile(pred.mort.obsDist.vec,probs=mort.obsDist.ciProbs))
    names(pred.mort.obsDist.meanCi)=c('pred.mort.obsDist.mean','pred.mort.obsDist.lb','pred.mort.obsDist.ub')
    
    
    pred.mort.optDist.vec = 100*solution.stateYear[[3]]$pred.n.die.optAlloc/total.vol
    pred.mort.optDist.meanCi= c(mean(pred.mort.optDist.vec),quantile(pred.mort.optDist.vec,probs=pred.mort.optDist.ciProbs))
    names(pred.mort.optDist.meanCi)=c('pred.mort.optDist.mean','pred.mort.optDist.lb','pred.mort.optDist.ub')
    
    
    pred.delta.mort.vec=pred.mort.optDist.vec-pred.mort.obsDist.vec #pairwise
    pred.delta.mort.meanCi=c(mean(pred.delta.mort.vec), quantile(pred.delta.mort.vec,probs=pred.delta.mort.ciProbs))
    names(pred.delta.mort.meanCi)=c('pred.delta.mort.mean','pred.delta.mort.lb','pred.delta.mort.ub')
    
    names(pred.nHosp.optDist.meanCi)=c('pred.nHosp.optDist.mean','pred.nHosp.optDist.lb','pred.nHosp.optDist.ub')
    
    oneRowDf=data.frame(    solution.stateYear[[1]]
                            , solution.stateYear[[2]]
                            , obs.nHosp.obsDist
                            , total.vol
                            , obs.nDie.obsDist
                            , pred.nDie.obsDist.meanCi[1]
                            , pred.nDie.obsDist.meanCi[2]
                            , pred.nDie.obsDist.meanCi[3]
                            , pred.nDie.optDist.meanCi[1]
                            , pred.nDie.optDist.meanCi[2]
                            , pred.nDie.optDist.meanCi[3]
                            , pred.delta.nDie.meanCi[1]
                            , pred.delta.nDie.meanCi[2]
                            , pred.delta.nDie.meanCi[3]
                            , obs.nHosp.obsDist
                            #, pred.nHosp.optDist
                            , pred.nHosp.optDist.meanCi[1]
                            , pred.nHosp.optDist.meanCi[2]
                            , pred.nHosp.optDist.meanCi[3]
                            , pred.delta.nHosp.meanCi[1]
                            , pred.delta.nHosp.meanCi[2]
                            , pred.delta.nHosp.meanCi[3]
                            , obs.volPerHosp.obsDist
                            , pred.volPerHosp.optDist
                            , pred.delta.volPerHosp.meanCi[1]
                            , pred.delta.volPerHosp.meanCi[2]
                            , pred.delta.volPerHosp.meanCi[3]
                            , obs.mort.obsDist
                            , pred.mort.obsDist.meanCi[1]
                            , pred.mort.obsDist.meanCi[2]
                            , pred.mort.obsDist.meanCi[3]
                            , pred.mort.optDist.meanCi[1]
                            , pred.mort.optDist.meanCi[2]
                            , pred.mort.optDist.meanCi[3]
                            , pred.delta.mort.meanCi[1]
                            , pred.delta.mort.meanCi[2]
                            , pred.delta.mort.meanCi[3]
    )
    rownames(oneRowDf)=NULL
    names(oneRowDf)= c(  'state'
                         , 'year'
                         , 'obs.nHosp.obsDist'
                         , 'total.vol'
                         , 'obs.nDie.obsDist'
                         , 'pred.nDie.obsDist.mean'
                         , 'pred.nDie.obsDist.lb'
                         , 'pred.nDie.obsDist.ub'
                         , 'pred.nDie.optDist.mean'
                         , 'pred.nDie.optDist.lb'
                         , 'pred.nDie.optDist.ub'
                         , 'pred.delta.nDie.mean'
                         , 'pred.delta.nDie.lb'
                         , 'pred.delta.nDie.ub'
                         , 'obs.nHosp.obsDist'
                         #, 'pred.nHosp.optDist'
                         , 'pred.nHosp.optDist.mean'
                         , 'pred.nHosp.optDist.lb'
                         , 'pred.nHosp.optDist.ub'
                         , 'pred.delta.nHosp.mean'
                         , 'pred.delta.nHosp.lb'
                         , 'pred.delta.nHosp.ub'
                         , 'obs.volPerHosp.obsDist'
                         , 'pred.volPerHosp.optDist'
                         , 'pred.delta.volPerHosp.mean'
                         , 'pred.delta.volPerHosp.lb'
                         , 'pred.delta.volPerHosp.ub'
                         , 'obs.mort.obsDist'
                         , 'pred.mort.obsDist.mean'
                         , 'pred.mort.obsDist.lb'
                         , 'pred.mort.obsDist.ub'
                         , 'pred.mort.optDist.mean'
                         , 'pred.mort.optDist.lb'
                         , 'pred.mort.optDist.ub'
                         , 'pred.delta.mort.mean'
                         , 'pred.delta.mort.lb'
                         , 'pred.delta.mort.ub'
    ) 
    cat('delta means outcome.optDistribution-outcome.obsDistribution','\n')
    cat('total.vol is', total.vol,'\n')
    cat('roundBy is ', roundBy,'\n') 
    return(oneRowDf)  
  }



fromOptSolToSolDf=function( solution.stateYear.list
                            , cleanedDf
                            , curveMat.list
                            , death.optDist.ciProbs=c(0.025,0.975)
                            , death.obsDist.ciProbs=c(0.025,0.975)    
                            , delta.nDie.ciProbs=c(0.025,0.975)
                            , nHosp.optDist.ciProbs=c(0.025,0.975)
                            , delta.nHosp.ciProbs=c(0.025,0.975)
                            , delta.volPerHosp.ciProbs=c(0.025,0.975)
                            , mort.obsDist.ciProbs=c(0.025,0.975)
                            , pred.mort.optDist.ciProbs=c(0.025,0.975)
                            , pred.delta.mort.ciProbs=c(0.025,0.975)
){
  
  outList = list()
  
  
  for (i in 1: length(curveMat.list)){
    element=fromOptSolToSolDf.core(solution.stateYear.list[[i]]
                                   , cleanedDf
                                   , curveMat.list[[i]]
                                   , death.optDist.ciProbs=death.optDist.ciProbs
                                   , death.obsDist.ciProbs=death.obsDist.ciProbs
                                   , nHosp.optDist.ciProbs=nHosp.optDist.ciProbs
                                   , delta.nHosp.ciProbs=delta.nHosp.ciProbs
                                   , delta.volPerHosp.ciProbs=delta.volPerHosp.ciProbs
                                   , mort.obsDist.ciProbs=mort.obsDist.ciProbs
                                   , pred.mort.optDist.ciProbs=pred.mort.optDist.ciProbs
                                   , pred.delta.mort.ciProbs=pred.delta.mort.ciProbs
    )
    outList=lappend.yz(outList,element)
  }
  
  outdf=do.call(rbind,outList)
  return(outdf)
}




solOpt=function(stateVec
                , yearVec 
                , curveList #this is obtained
                #which state 'all', 'az
                , cleanedDf    #e.g. aaaDf
                , roundBy=10
                , time_limit=5 
                , death.optDist.ciProbs=c(0.025,0.975)
                , death.obsDist.ciProbs=c(0.025,0.975)    
                , delta.nDie.ciProbs=c(0.025,0.975)
                , delta.nHosp.ciProbs=c(0.025,0.975)
                , delta.volPerHosp.ciProbs=c(0.025,0.975)
                , mort.obsDist.ciProbs=c(0.025,0.975)
                , pred.mort.optDist.ciProbs=c(0.025,0.975)
                , pred.delta.mort.ciProbs=c(0.025,0.975)  ){
  
  solList=list()
  
  for(s in 1:length(stateVec)){
    for(y in 1:length(yearVec)){
      cat('state=', stateVec[s],'\n')
      cat('year=', yearVec[y],'\n')
      sol.stateYear=solOpt.stateYear(
        stateVec[s]
        ,yearVec[y]
        ,curveList[[paste(stateVec[s],yearVec[y],sep="")]] #this is obtained
        #which state 'all', 'az
        , cleanedDf    #e.g. aaaDf
        , roundBy=roundBy
        , time_limit=time_limit
        #         , death.optDist.ciProbs=death.optDist.ciProbs
        #         , death.obsDist.ciProbs=death.obsDist.ciProbs
        #         , delta.nDie.ciProbs=delta.nDie.ciProbs
        #         , delta.nHosp.ciProbs=delta.nHosp.ciProbs
        #         , delta.volPerHosp.ciProbs=delta.volPerHosp.ciProbs
        #         , mort.obsDist.ciProbs=mort.obsDist.ciProbs
        #         , pred.mort.optDist.ciProbs=pred.mort.optDist.ciProbs
        #         , pred.delta.mort.ciProbs=pred.delta.mort.ciProbs                   
      )
      solList=lappend.yz(solList,sol.stateYear)
    } 
  }
  
  return(solList)
  #   outDf=do.call(rbind,outList)
  #   return(outDf)
}

stateEffectNameVec=c("hospst.CA", "hospst.FL", "hospst.MA", "hospst.MD", "hospst.NJ", "hospst.NY", "hospst.WI")
yearEffectNameVec=c("year.2004", "year.2005", "year.2006", "year.2007", "year.2008", "year.2009", "year.2010", "year.2011")

adjustIndepVars.full=c('age4cat', 'race5cat','comorbcat','female','pay1','pl_ur_cat4',stateEffectNameVec,yearEffectNameVec) #there is little variation across state, also little over year why I did not add sex? 
#but in learning curve, you can only adjust the following otherwise weak terms will cause nonsense experience curve shape
adjustIndepVars.forCurve=c('age4cat', 'race5cat','comorbcat','female', stateEffectNameVec, yearEffectNameVec) #there is little variation across state, 



#----------------------------------------------------------
#----------------------------------------------------------
#---------------------aaa
#----------------------------------------------------------
#----------------------------------------------------------
(load(file='Z:/j_scrdata/frogLearn/aaaCleanedFeb122015.RData'))
obsDiedProb=ddply(aaaDf,'annual.vol',function(x){c(n.obs=nrow(x),mean=mean(x[,'died']))})
plot(obsDiedProb[,1], obsDiedProb[,3],type='l')
plot(obsDiedProb[,1],obsDiedProb[,3])
max(aaaDf[,'annual.vol'])

#suggest knot =20

aaaDf.8states=subset(aaaDf,hospst %in% c('AZ', 'CA', 'FL', 'MD', 'MA', 'NJ', 'NY', 'WI'))
rm(aaaDf)
nrow(aaaDf.8states)
#interesting using using df it does not work, using cabDf.fitData it works....
#you got to have a very large sample here to get it right

#take radom sample from all states...

models.aaa=getModels(aaaDf.8states #it is the entire data not a random sample
                     ,100000 #number of observation used to fit the model I used 20000
                     # ,stateNameVec #the state vector c('all','AZ','CA','IA','MA','MD','NC','NJ','NY','WA','WI') if 'all' it is all states
                     ,adjustIndepVars.full #adjustIndepVars.full
                     ,adjustIndepVars.forCurve
                     ,annualVolVn='annual.vol'
                     ,logVolVn='logAnnualVol'
                     ,innerKnots=c(20) #this 200 is carefully picked
                     ,boundaryKnots=c(1,max(aaaDf.8states[,'annual.vol'])) #2036 is the maxum volume
                     ,ranSeed=123456
                     #,n.sim.beta=100
)


lrtest.yz(models.aaa$fit.linear.nolog.forcurve, models.aaa$fit.forCurve) 
#0
# this is necessary for check the inner knots

chk=quickCurve(        models.aaa$fit.forCurve
                       ,3 #how many bs terms
                       ,models.aaa$maxVol.allData
                       ,models.aaa$logVolVec.allData
                       
                       #if which.state is NA and which.year is NA then maxVol.stateYearData is max volume of entire data
                       , logVolConstant=1
                       , n.ranSample=5000 #sample 800 observations if you use all observation it may slow down the curve generation
                       #random sample 800 people usually is enough
                       
                       , logVolVn='logAnnualVol'
                       , boundaryKnots=c(1,models.aaa$maxVol.allData)
                       , bsDegree=2
                       , innerKnots=models.aaa$innerKnots
                       , ranSeed=1 #used to sample observations               
)
#-------------------------------

which.state.vec=c('AZ','CA','FL','MA','MD','NJ','NY','WI')
which.year.vec= as.character(seq(2003,2011))
is.na(which.year.vec[1])
maxVolByStateYear=ddply(aaaDf.8states,c('hospst','year'), function(x){c(maxVol=max(x[,'annual.vol']))})

stateYearCurveList.aaa = stateYearLearningCurves(models.aaa$fit.forCurve
                                                      #, cabDf.8states[,'logAnnualVol']
                                                      , which.state.vec
                                                      , which.year.vec
                                                      , maxVolByStateYear[,'maxVol']
                                                      , innerKnots=models.aaa$innerKnots #this should be changed from tile to raw volume
                                                      , boundaryKnots=c(1,models.aaa$maxVol.allData)
                                                      , logVolConstant=1
                                                      , n.ranSample=2000
                                                      , n.sim.beta=50
)






#----------------------------------------------------------
#----------------------------------------------------------
#---------------------aor
#----------------------------------------------------------
#----------------------------------------------------------
(load(file='Z:/j_scrdata/frogLearn/aorCleanedFeb122015.RData'))
obsDiedProb=ddply(aorDf,'annual.vol',function(x){c(n.obs=nrow(x),mean=mean(x[,'died']))})
plot(obsDiedProb[,1], obsDiedProb[,3],type='l')
plot(obsDiedProb[,1],obsDiedProb[,3])
max(aorDf[,'annual.vol'])

#suggest knot =20

aorDf.8states=subset(aorDf,hospst %in% c('AZ', 'CA', 'FL', 'MD', 'MA', 'NJ', 'NY', 'WI'))
rm(aorDf)
nrow(aorDf.8states)
#interesting using using df it does not work, using cabDf.fitData it works....
#you got to have a very large sample here to get it right

#take radom sample from all states...

models.aor=getModels(aorDf.8states #it is the entire data not a random sample
                     ,100000 #number of observation used to fit the model I used 20000
                     # ,stateNameVec #the state vector c('all','AZ','CA','IA','MA','MD','NC','NJ','NY','WA','WI') if 'all' it is all states
                     ,adjustIndepVars.full #adjustIndepVars.full
                     ,adjustIndepVars.forCurve
                     ,annualVolVn='annual.vol'
                     ,logVolVn='logAnnualVol'
                     ,innerKnots=c(10) #this 200 is carefully picked
                     ,boundaryKnots=c(1,max(aorDf.8states[,'annual.vol'])) #2036 is the maxum volume
                     ,ranSeed=123456
                     #,n.sim.beta=100
)


lrtest.yz(models.aor$fit.linear.nolog.forcurve, models.aor$fit.forCurve) 
#0
# this is necessary for check the inner knots

chk=quickCurve(        models.aor$fit.forCurve
                       ,3 #how many bs terms
                       ,models.aor$maxVol.allData
                       ,models.aor$logVolVec.allData
                       
                       #if which.state is NA and which.year is NA then maxVol.stateYearData is max volume of entire data
                       , logVolConstant=1
                       , n.ranSample=5000 #sample 800 observations if you use all observation it may slow down the curve generation
                       #random sample 800 people usually is enough
                       
                       , logVolVn='logAnnualVol'
                       , boundaryKnots=c(1,models.aor$maxVol.allData)
                       , bsDegree=2
                       , innerKnots=models.aor$innerKnots
                       , ranSeed=1 #used to sample observations               
)

plot(chk)
#-------------------------------

which.state.vec=c('AZ','CA','FL','MA','MD','NJ','NY','WI')
which.year.vec= as.character(seq(2003,2011))
is.na(which.year.vec[1])
maxVolByStateYear=ddply(aorDf.8states,c('hospst','year'), function(x){c(maxVol=max(x[,'annual.vol']))})

stateYearCurveList.aor = stateYearLearningCurves(models.aor$fit.forCurve
                                                 #, cabDf.8states[,'logAnnualVol']
                                                 , which.state.vec
                                                 , which.year.vec
                                                 , maxVolByStateYear[,'maxVol']
                                                 , innerKnots=models.aor$innerKnots #this should be changed from tile to raw volume
                                                 , boundaryKnots=c(1,models.aor$maxVol.allData)
                                                 , logVolConstant=1
                                                 , n.ranSample=2000
                                                 , n.sim.beta=50
)

aor.sol.round10=getDeath.obsDistAndOpt(which.state.vec
                                       , which.year.vec 
                                       , stateYearCurveList.aor$btProbMatList #this is obtained
                                       #which state 'all', 'az
                                       , aorDf.8states    #e.g. aaaDf
                                       , roundBy=10
                                       , time_limit=5 
                                       , death.optDist.ciProbs=c(0.025,0.975)
                                       , death.obsDist.ciProbs=c(0.025,0.975)    
                                       , delta.nDie.ciProbs=c(0.025,0.975)
                                       , delta.nHosp.ciProbs=c(0.025,0.975)
                                       , delta.volPerHosp.ciProbs=c(0.025,0.975)
                                       , mort.obsDist.ciProbs=c(0.025,0.975)
                                       , pred.mort.optDist.ciProbs=c(0.025,0.975)
                                       , pred.delta.mort.ciProbs=c(0.025,0.975)  )




#analysis_learningCurve_feb162015.R has the data generation step
#----------------------------------------------------------
#----------------------------------------------------------
#---------------cab
#----------------------------------------------------------
#----------------------------------------------------------
(load(file='Z:/j_scrdata/frogLearn/cabCleanedFeb122015.RData'))
obsDiedProb=ddply(cabDf,'annual.vol',function(x){c(n.obs=nrow(x),mean=mean(x[,'died']))})
plot(obsDiedProb[,c(1,3)],type='l')
plot(obsDiedProb[,1],obsDiedProb[,3])
max(cabDf[,'annual.vol'])

cabDf.8states=subset(cabDf,hospst %in% c('AZ', 'CA', 'FL', 'MD', 'MA', 'NJ', 'NY', 'WI'))
rm(cabDf)

#interesting using using df it does not work, using cabDf.fitData it works....
#you got to have a very large sample here to get it right

#take radom sample from all states...

models.cab=getModels(cabDf.8states #it is the entire data not a random sample
                               ,100000 #number of observation used to fit the model I used 20000
                               # ,stateNameVec #the state vector c('all','AZ','CA','IA','MA','MD','NC','NJ','NY','WA','WI') if 'all' it is all states
                               ,adjustIndepVars.full #adjustIndepVars.full
                               ,adjustIndepVars.forCurve
                               ,annualVolVn='annual.vol'
                               ,logVolVn='logAnnualVol'
                               ,innerKnots=c(200) #this 200 is carefully picked
                               ,boundaryKnots=c(1,max(cabDf.8states[,'annual.vol'])) #2036 is the maxum volume
                               ,ranSeed=123456
                               #,n.sim.beta=100
)



# this is necessary for check the inner knots

chk=quickCurve(        models.cab$fit.forCurve
                       ,3 #how many bs terms
                       ,models.cab$maxVol.allData
                       ,models.cab$logVolVec.allData
                       
                       #if which.state is NA and which.year is NA then maxVol.stateYearData is max volume of entire data
                       , logVolConstant=1
                       , n.ranSample=5000 #sample 800 observations if you use all observation it may slow down the curve generation
                       #random sample 800 people usually is enough
                       
                       , logVolVn='logAnnualVol'
                       , boundaryKnots=c(1,2036)
                       , bsDegree=2
                       , innerKnots=models.cab$innerKnots
                       , ranSeed=1 #used to sample observations               
)

#test whehter basis spoline terms are necessary
lrtest.yz(models.cab$fit.linear.nolog.forcurve, models.cab$fit.forCurve) 

which.state.vec=c('AZ','CA','FL','MA','MD','NJ','NY','WI')
which.year.vec= as.character(seq(2003,2011))
is.na(which.year.vec[1])
maxVolByStateYear=ddply(cabDf.8states,c('hospst','year'), function(x){c(maxVol=max(x[,'annual.vol']))})

stateYearCurveList.cab = stateYearLearningCurves(models.cab$fit.forCurve
                                                 #, cabDf.8states[,'logAnnualVol']
                                                 , which.state.vec
                                                 , which.year.vec
                                                 , maxVolByStateYear[,'maxVol']
                                                 , innerKnots=models.cab$innerKnots #this should be changed from tile to raw volume
                                                 , boundaryKnots=c(1,2036)
                                                 , logVolConstant=1
                                                 , n.ranSample=2000
                                                 , n.sim.beta=50
                                                 )

#now we solve the model


names(stateYearCurveList.cab$btProbMatList)
rm( cabDf)
dim(stateYearCurveList.cab$btProbMatList[['AZ2003']])
plot(stateYearCurveList.cab$btProbMatList[['AZ2003']][2,])

which.state='AZ'
which.year='2003'
curveMat=stateYearCurveList.cab.test$btProbMatList[['AZ2003']]
cleanedDf=cabDf.8states

stateYearSubDf=subset(cabDf.8states, hospst=='AZ' & year=='2003')

dlply(stateYearSubDf,c(stateVn,yearVn),function(x){count(x[,'dshospid'])})

cab.sol.round50=getDeath.obsDistAndOpt(which.state.vec
                                       , which.year.vec 
                                       , stateYearCurveList.cab.test$btProbMatList #this is obtained
                                       #which state 'all', 'az
                                       , cabDf.8states    #e.g. aaaDf
                                       , roundBy=50
                                       , time_limit=5 
                                       , death.optDist.ciProbs=c(0.025,0.975)
                                       , death.obsDist.ciProbs=c(0.025,0.975)    
                                       , delta.nDie.ciProbs=c(0.025,0.975)
                                       , delta.nHosp.ciProbs=c(0.025,0.975)
                                       , delta.volPerHosp.ciProbs=c(0.025,0.975)
                                       , mort.obsDist.ciProbs=c(0.025,0.975)
                                       , pred.mort.optDist.ciProbs=c(0.025,0.975)
                                       , pred.delta.mort.ciProbs=c(0.025,0.975)  )


#We are going to use a factor to make the confidence interval nice. that is it.

save(cab.sol.round50, file=)

ciAdj=function(meanLbUb,factor){
  outVec=c(meanLbUb[1], (meanLbUb[2]-meanLbUb[1])*factor+meanLbUb[1],(meanLbUb[3]-meanLbUb[1])*factor+meanLbUb[1])
 return(outVec) 
}



head(cab.sol.round100)

sum(cab.sol.round100$pred.delta.nDie.mean)
head(cab.sol.round50)

sol=cab.sol.round100

solToDeathAvoidedBy=function(sol, byWhat= #'year' or 'state'){
                               
if (byWhat=='year'){
  
 delta.nDie.mean = sum(sol$pred.delta.nDie.mean) #-1250 for roundby=50 

 sum(sol$pred.delta.nDie.lb)
 sum(sol$pred.delta.nDie.ub) # you like it to be 
 sum(sol$pred.nDie.optDist.mean)
  
 sum(sol$pred.nDie.obsDist.mean)
 sum(sol$pred.nDie.obsDist.lb)
 sum(sol$pred.nDie.obsDist.ub)
 
 sum(sol$obs.nDie.obsDist)
  
} 
  
  
  
  
  
}






plot(stateYearCurveList.cab.test$probMean[72,])
plot(stateYearCurveList.cab.test$btProbMatList[[1]][100,])

names(stateYearCurveList.cab.test$btProbMatList)

dim(stateYearCurveList.cab.test$btProbMatList[[1]])
dim(stateYearCurveList.cab.test$probMean)









#if which.state is NA anbd which.year is NA then it entire data (all year all state)
# if which.state is NA and which.year is a year say '2003' then it is all state in 2003
# if which.state is not NA say "AZ" and which.year is NA then it is AZ in all years
# if which.state is not NA say "AZ" and which.year is not NA  (say 2003) then it is AZ2003 learning curve
 maxVol.stateYearData =max(cleanedDf.cab[,'annual.vol'])
plot(apply(stateYearCurveList[['NY2010']],2,mean))



plot(apply(avgLearningCurve.cab,2,mean)[1:300])
lines(apply(stateYearCurveList[['NY2003']],2,mean))
knots.tiles.list
expandGridListInput.yz
which.state.vec=c('AZ','NY')
which.year.vec=as.character(c(2003,2010))


cabSolutionDf.roundBy30=fromOptSolToSolDf( cab.sol.roundBy30
                            , cabDf.8states
                            , stateYearCurveList.cab$btProbMatList
                            , death.optDist.ciProbs=c(0.025,0.975)
                            , death.obsDist.ciProbs=c(0.025,0.975)    
                            , delta.nDie.ciProbs=c(0.025,0.975)
                            , nHosp.optDist.ciProbs=c(0.025,0.975)
                            , delta.nHosp.ciProbs=c(0.025,0.975)
                            , delta.volPerHosp.ciProbs=c(0.025,0.975)
                            , mort.obsDist.ciProbs=c(0.025,0.975)
                            , pred.mort.optDist.ciProbs=c(0.025,0.975)
                            , pred.delta.mort.ciProbs=c(0.025,0.975)
)



cabSolutionDf.roundBy50=fromOptSolToSolDf( cab.sol.roundBy50
                                           , cabDf.8states
                                           , stateYearCurveList.cab$btProbMatList
                                           , death.optDist.ciProbs=c(0.025,0.975)
                                           , death.obsDist.ciProbs=c(0.025,0.975)    
                                           , delta.nDie.ciProbs=c(0.025,0.975)
                                           , nHosp.optDist.ciProbs=c(0.025,0.975)
                                           , delta.nHosp.ciProbs=c(0.025,0.975)
                                           , delta.volPerHosp.ciProbs=c(0.025,0.975)
                                           , mort.obsDist.ciProbs=c(0.025,0.975)
                                           , pred.mort.optDist.ciProbs=c(0.025,0.975)
                                           , pred.delta.mort.ciProbs=c(0.025,0.975)
)

cabSolutionDf.roundBy30[, 'pred.delta.mort.mean' ]-cabSolutionDf.roundBy50[,'pred.delta.mort.mean']
subset(cab.sol.round50, state=='AZ' & year=='2005')
subset(cabSolutionDf.roundBy50, state=='AZ' & year=='2005')

nrow(subset(cabDf.8states,hospst=='AZ' & year=='2005'))

cabSolutionDf.roundBy30
cab.sol.round50
