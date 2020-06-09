


keptVns=c('AGE','DIED','HOSPST',"YEAR",'X_comorbSum','DSHOSPID','RACE','FEMALE','AWEEKEND','PAY1','PL_UR_CAT4')
adjustIndepVars=c('age4cat', 'race5cat','comorbcat','female','pay1','pl_ur_cat4') #there is little variation across state, also little over year why I did not add sex? 

#dataPath=path='Z:/j_scrdata/frogLearn/aaa_sid_20032011.csv'
asciiDataVn.yz(dataPath,sep=sep)
#strange there are a few hosp=NE
#there for year 2011 IA has no dshospid, so you we have to remove IA

subset(df.0,hospst=='NE')[,'dshospid']

ddply(df.0,c('year','hospst'),function(x){nrow(table(x[,'dshospid']))})

path="Z:/j_scrdata/frogLearn/aaa_sid_20032011.csv"

#need R 2.15
cleanDf=function(path,keptVns,logVolVn='logAnnualVol',logVolConstant=1){
  
  cat('to run this you have to load R 2.15 64bit', '\n')
  rawin=readTableCols.yz(path,vns=keptVns,keep=TRUE)
  names(rawin)=tolower(names(rawin))
 
  #subset(rawin, hospst %in% c('AZ','CA','NC','NJ','NY','MA','MD','WA','WI'))
  #only keep female value is 0 or 1
  
  df.0=subset(rawin, female %in% c(0,1))
  #table(df.0[,'age'])
  
  
  #you can handle  NA, Inf, whitespace and anything here using rowsWithInterestValues.yz function
  okRows=rowsWithInterestValues.yz(df.0
                                   ,c('race','died','female','pay1','pl_ur_cat4','age')
                                   #there is k vns then the interestValuesList has a length of k
                                   , list(c('.','A','C'),c('.','A','C'),c('.','A','C'),c('.','A','C'),c('.','A','C'),c('.','A','C'))
  )$rowsWithoutInterestValues
  #head(subset(df.0,hospst=='WA' & year=='2003'),300)
  #NC 2009 does not have race information
  #washington 2003 2004 2005 2006 2007 miss race
  #NE does not have dshospid for all years, so you cannot use it
  
  
  
  df.1=df.0[okRows,]
  ddply(subset(df.1,!hospst %in% 'IA'),c('year','hospst'),nrow)
  df.1[,'age']=as.integer(df.1[,'age'])
  df.1[,'x_comorbsum']=as.integer(df.1[,'x_comorbsum'])
  
  
  #generate hospital Volume Df
  volDf=ddply(df.1,c("year",'dshospid'),nrow)
  volDf=rename.vars(volDf,'V1','annual.vol')
  withVol = join(df.1,volDf)
  
  
  #2011 year IA does not have hosptial ID, so you cannot caculate hospital volume
  
  #handling missing value
  nomiss.0=completeCases.yz(withVol)$complete.df

  #handling whitespace
  nomiss.1=nomiss.0[whitespaceRow.yz(nomiss.0,)$noWhitespaceRows,]
  
  nomiss.1[,logVolVn]=log(nomiss.1[,'annual.vol']+logVolConstant)
  
  comorbcuts=c(0,1,2,3,4,5,Inf)
  nomiss.1[,'x_comorbsum']=as.numeric(nomiss.1[,'x_comorbsum'])
  nomiss.1=grpnv.supplycuts.yz(nomiss.1, 'x_comorbsum', comorbcuts, 'comorbcat')
  
  age4catcuts=quantile(nomiss.1[,'age'],prob=c(0,0.25,0.5,0.75,1))
  nomiss.1=grpnv.supplycuts.yz(nomiss.1, 'age', age4catcuts, 'age4cat')
  
  nomiss.1[,'race5cat']=replaceValJoin.yz(nomiss.1[,'race'] #this is typically a data column
                                          ,list('1','2','3','4',c('5','6')) 
                                          ,c('1','2','3','4','5')
                                          ,origValVn='race'
                                          ,newValVn='race5cat' #if output is vector, this newValVn is inapplicable
                                          ,outputJoinedDf=TRUE #if F means only output the new vector
                                          #if T, then output df             
  )[,'race5cat']
  
  # 1 White 
  # 2 Black 
  # 3 Hispanic 
  # 4 Asian or Pacific Islander 
  # 5 Native American 
  # 6 Other 
  nomiss.1[,'race5cat']=as.factor(nomiss.1[,'race5cat'])
  nomiss.1[,'year']=as.character(nomiss.1[,'year'])
  nomiss.1[,'died']=as.integer(nomiss.1[,'died'])
  nomiss.1[,'year']=as.factor(nomiss.1[,'year'])
  nomiss.1[,'hospst']=as.factor(nomiss.1[,'hospst'])
  nomiss.1[,'female']=as.factor(nomiss.1[,'female'])
  nomiss.1[,'pay1']=as.factor(nomiss.1[,'pay1'])
  nomiss.1[,'pl_ur_cat4']=as.factor(nomiss.1[,'pl_ur_cat4'])
  return(nomiss.1)
}
ransamDf=function(df,maxN,seed=123456,replace=FALSE){
  set.seed(seed)
  if (nrow(df)>maxN){
    rowLoc=sample(seq(nrow(df)),maxN)
    output=df[rowLoc,]
    return(output)
  } else (return(df))
}

#there is a problem with readTableDebug() something about internal function...but that is the only problem....it worked you need to figure othat out

#the following fucntion get model fit, we add year in adjustIndepVars.full and adjustIndpeVars.forCurve
fitStateFixedEffectModel=function( allStateDf 
                                  ,adjustIndepVars.full #can be NULL, if so no need to fit full model
                                  ,adjustIndepVars.forCurve
                                  ,logVolVn='logAnnualVol'
                                  ,knots.tiles=c(0.5)
                                  ,bsDegree=2
                                  ,Boundary.knots.tiles=c(0,1)
                                  ,logVolConstant=1
                                  ){
  bslist=bsWrapper1.yz(allStateDf[,logVolVn] #this is the x in bs function
                       , quantile(allStateDf[,logVolVn],probs=knots.tiles) #inner knots, if it contains boundary knots, function will stop and issue erro message
                       , paste(logVolVn,'.bs',sep='') #output data's columen name stem
                       , degree=bsDegree
                       , dataType='data.frame'
                       , Boundary.knots=quantile(allStateDf[,logVolVn],prob=Boundary.knots.tiles)
                       #or ='data.frame
  )
  names(bslist)
  n.basis=bslist$n.basis
  colnames(bslist$bsdata)
  outdf.with.bs=cbind(allStateDf,bslist$bsdata)
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
  
  fitList=list(fit.forCurve=fit.forCurve)
  
  if (!is.null(adjustIndepVars.full)){
    fitList=list(fit.full=fit.full, fit.forCurve=fit.forCurve)
  }
 return(fitList)
}

#the following fucntion get state specific learning curve
stataYearSpecificCurve=function(modelFit
                          ,state='all' #if state is all we run for an average learning curve
                          ,refState='AZ'
                          ,logVolVn='logAnnualVol'
                          ,knots.tiles=c(0.5)
                          ,bsDegree=2
                          ,Boundary.knots.tiles=c(0,1)
                          ,logVolConstant=1
                          ,n.sim.beta=100
                          ,getBootstraps=T #if TRUE then we will get the boostrapped learning curves and these learning curves will be used to for further optimization
){
  
  dataFit=modelFit$data
  
  if (!state=='all'){
    stateDf=subset(dataFit,hospst==state)
        if (state==refState){ 
                              stateDf[,c("hospst.CA", "hospst.FL","hospst.IA","hospst.MA","hospst.MD", "hospst.NC" ,"hospst.NJ","hospst.NY","hospst.WA","hospst.WI")]=-1
                           } else{
                                stateDf[,c("hospst.CA", "hospst.FL","hospst.IA","hospst.MA","hospst.MD", "hospst.NC" ,"hospst.NJ","hospst.NY","hospst.WA","hospst.WI")]=0     
                                stateDf[,paste('hospst',state,sep='.')]=1
                           }
                                     
} else{ #this is to get an average national learning curve
stateDf=dataFit
stateDf[,c("hospst.CA", "hospst.FL","hospst.IA","hospst.MA","hospst.MD", "hospst.NC" ,"hospst.NJ","hospst.NY","hospst.WA","hospst.WI")]=0
}
  
hospVolVec=seq(1,max(stateDf[,'annual.vol']))
  
cat('max vol is ', max(hospVolVec), '\n')
  
# state='AZ'
  #add basis spline terms
  bslist=bsWrapper1.yz(stateDf[,logVolVn] #this is the x in bs function
                       , quantile(dataFit[,logVolVn],probs=knots.tiles) #inner knots, if it contains boundary knots, function will stop and issue erro message
                       , paste(logVolVn,'.bs',sep='') #output data's columen name stem
                       , degree=bsDegree
                       , dataType='data.frame'
                       , Boundary.knots=quantile(dataFit[,logVolVn],prob=Boundary.knots.tiles)
                       #or ='data.frame
  )
  
  n.basis=bslist$n.basis
  colnames(bslist$bsdata)
  outdf.with.bs=cbind(stateDf,bslist$bsdata)

  
  bsForSim=bsWrapper1.yz(log(hospVolVec+logVolConstant) #this is the x in bs function
                         ,  quantile(dataFit[,logVolVn],probs=knots.tiles)  #inner knots, if it contains boundary knots, function will stop and issue erro message
                         , paste(logVolVn,'.bs',sep='') #output data's columen name stem
                         , degree=bsDegree
                         , dataType='data.frame'
                         #or ='data.frame
                         , Boundary.knots=quantile(dataFit[,logVolVn],prob=Boundary.knots.tiles)
  )$bsdata

  #first get the mean curve
  betaVec=coef(modelFit)
  betaMat.onerow=matrix(betaVec,nrow=1)
  colnames(betaMat.onerow)=names(betaVec)
  probVec=rep(NA,nrow(bsForSim))
  names(probVec)=paste('vol=',as.character(hospVolVec),sep='')

  stateDf.noBs=deldfcols.yz(stateDf,colnames(bsForSim))
  for(i in 1:nrow(bsForSim)){
    cat('i=',i,'\n')
    bsPart.i=as.data.frame(repmat.yz(as.matrix(bsForSim[i,,drop=F]),nrow(stateDf),1))
    names(bsPart.i)=colnames(bsForSim)
    outdf.with.sim.bs.i=cbind(stateDf.noBs,bsPart.i)
    colnames(outdf.with.sim.bs.i)
    x.i=model.matrix(formula(modelFit),data=outdf.with.sim.bs.i)
    latentindex.i=xb.matched.yz(betaMat.onerow,x.i)$xb.matched
    probVec[i]=mean(exp(latentindex.i)/(1+exp(latentindex.i)))
  }
  
  if (getBootstraps){ 
    #get the bootstrapped curves    
    betaMat=mvrnorm(n = n.sim.beta, coef(modelFit), vcov(modelFit), tol = 1e-6, empirical = FALSE)
    btProbMat=matrix(NA,nrow=n.sim.beta,ncol=nrow(bsForSim))
    colnames(btProbMat)=paste('vol=',as.character(hospVolVec),sep='')
    rownames(btProbMat)=paste('beta.sim.',as.character(seq( n.sim.beta)),sep='')
    
    for(i in 1:nrow(bsForSim)){
      cat('process i=',i, 'among', nrow(bsForSim),'\n')
      cat('process i=',i, 'experience point along the curve')
      cat('each point experience will has', n.sim.beta, 'outcome points')
      bsPart.i=as.data.frame(repmat.yz(as.matrix(bsForSim[i,,drop=F]),nrow(stateDf),1))
      names(bsPart.i)=colnames(bsForSim)
      outdf.with.sim.bs.i=cbind(stateDf.noBs,bsPart.i)
      #print('I am here 1')
      x.i=model.matrix(formula(modelFit),data=outdf.with.sim.bs.i)
      latentindex.i=xb.matched.yz(betaMat,x.i)$xb.matched
      btProbMat[,i]=apply(exp(latentindex.i)/(1+exp(latentindex.i)),2,mean)  
    }    
  } else {btProbMat=NULL}
  
  obsMortDf=ddply(stateDf,'annual.vol',function(x){c(n=nrow(x),obsMort=mean(x[,'died']))})
  outList=list(btProbMat=btProbMat, meanProbVec=probVec, obsMortDf=obsMortDf)
  
  return(outList)
}

allStateByStateCurves=function(modelFit
                               ,stateNameVec=c('all','AZ','CA','FL','MA','MD','NJ','NY','WI')
                               ,knots.tiles=c(0.5)
                               ,n.sim.beta=100
                               ){
  
  outlist=list()
  for (i in 1:length(stateNameVec)){
    outlist=lappend.yz(outlist,stataSpecificCurve(modelFit
                                                  ,state=stateNameVec[i] #if state is NA we run for an average learning curve
                                                  ,refState='AZ'
                                                  ,logVolVn='logAnnualVol'
                                                  ,knots.tiles=knots.tiles
                                                  ,bsDegree=2
                                                  ,Boundary.knots.tiles=c(0,1)
                                                  ,logVolConstant=1
                                                  ,n.sim.beta=n.sim.beta
                                                  ,getBootstraps=T #if TRUE then we will get the boostrapped learning curves and these learning curves will be used to for further optimization
    ))
    
  }  
  
  
  outlist=lappend.yz(outlist,modelFit)
  names(outlist)=c(tolower(as.character(stateNameVec)),"fit.forCurve")
  return(outlist)
}
  
###aaa
df=cleanedDf.cab
nobs.fit=5000
stateNameVec=c('all','AZ','CA','IA','MA','MD','NC','NJ','NY','WA','WI') 
adjustIndepVars.full=c(adjustIndepVars.full,'year')
adjustIndepVars.forCurve=c(adjustIndepVars.forCurve,'year')



getAllCurves=function(df
                      ,nobs.fit #number of observation used to fit the model I used 20000
                      ,stateNameVec #the state vector c('all','AZ','CA','IA','MA','MD','NC','NJ','NY','WA','WI') if 'all' it is all states
                      ,adjustIndepVars.full
                      ,adjustIndepVars.forCurve
                      ,knots.tiles=c(0.5)
                      ,n.sim.beta=100){
  
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
  
  
  allStateDf=ransamDf(data.frame(df, stateEffMat, yearEffMat), nobs.fit)
  print(names(allStateDf))
  fitModels=fitStateFixedEffectModel( allStateDf
                                      ,adjustIndepVars.full
                                      ,adjustIndepVars.forCurve
                                      ,logVolVn='logAnnualVol'
                                      ,knots.tiles=knots.tiles
                                      ,bsDegree=2
                                      ,Boundary.knots.tiles=c(0,1)
                                      ,logVolConstant=1
  )
  
  
  curveModelFit=fitModels$fit.forCurve
  outlist=lappend.yz(allStateByStateCurves(curveModelFit,stateNameVec,knots.tiles=knots.tiles,n.sim.beta=n.sim.beta),fitModels$fit.full)
  
  return(outlist) 
}


allStateCurves=function(modelFit
                               
                               ,knots.tiles=c(0.5)
                               ,n.sim.beta=100
){
  
  
    outlist=stataSpecificCurve(modelFit
                                                  ,state='all' #if state is NA we run for an average learning curve
                                                  ,refState='AZ'
                                                  ,logVolVn='logAnnualVol'
                                                  ,knots.tiles=knots.tiles
                                                  ,bsDegree=2
                                                  ,Boundary.knots.tiles=c(0,1)
                                                  ,logVolConstant=1
                                                  ,n.sim.beta=n.sim.beta
                                                  ,getBootstraps=T #if TRUE then we will get the boostrapped learning curves and these learning curves will be used to for further optimization
    )
    
    
  
  
  outlist=lappend.yz(outlist,modelFit)
  names(outlist)=c(tolower(as.character(stateNameVec)),"modelFit")
  return(outlist)
}


allStateCurve=function(modelFit
                       ,k #before k all 1,,,,k will be plotted
                       ,byFromkToMaxVol=3
                       ,state='all' #if state is all we run for an average learning curve
                       ,refState='AZ'
                       ,logVolVn='logAnnualVol'
                       ,knots.tiles=c(0.5)
                       ,bsDegree=2
                       ,Boundary.knots.tiles=c(0,1)
                       ,logVolConstant=1
){
  
  dataFit=modelFit$data
  
  #this is to get an average national learning curve
  stateDf=dataFit
  stateDf[,c("hospst.CA", "hospst.FL","hospst.IA","hospst.MA","hospst.MD", "hospst.NC" ,"hospst.NJ","hospst.NY","hospst.WA","hospst.WI")]=0
  
  
  hospVolVec=c(seq(1,k),seq(k+1,max(stateDf[,'annual.vol']),by=byFromkToMaxVol))
  
  
  # state='AZ'
  #add basis spline terms
  bslist=bsWrapper1.yz(stateDf[,logVolVn] #this is the x in bs function
                       , quantile(dataFit[,logVolVn],probs=knots.tiles) #inner knots, if it contains boundary knots, function will stop and issue erro message
                       , paste(logVolVn,'.bs',sep='') #output data's columen name stem
                       , degree=bsDegree
                       , dataType='data.frame'
                       , Boundary.knots=quantile(dataFit[,logVolVn],prob=Boundary.knots.tiles)
                       #or ='data.frame
  )
  
  n.basis=bslist$n.basis
  colnames(bslist$bsdata)
  outdf.with.bs=cbind(stateDf,bslist$bsdata)
  
  
  bsForSim=bsWrapper1.yz(log(hospVolVec+logVolConstant) #this is the x in bs function
                         ,  quantile(dataFit[,logVolVn],probs=knots.tiles)  #inner knots, if it contains boundary knots, function will stop and issue erro message
                         , paste(logVolVn,'.bs',sep='') #output data's columen name stem
                         , degree=bsDegree
                         , dataType='data.frame'
                         #or ='data.frame
                         , Boundary.knots=quantile(dataFit[,logVolVn],prob=Boundary.knots.tiles)
  )$bsdata
  
  #first get the mean curve
  betaVec=coef(modelFit)
  betaMat.onerow=matrix(betaVec,nrow=1)
  colnames(betaMat.onerow)=names(betaVec)
  probVec=rep(NA,nrow(bsForSim))
  names(probVec)=paste('vol=',as.character(hospVolVec),sep='')
  
  stateDf.noBs=deldfcols.yz(stateDf,colnames(bsForSim))
  for(i in 1:nrow(bsForSim)){
    cat('i=',i,'\n')
    bsPart.i=as.data.frame(repmat.yz(as.matrix(bsForSim[i,,drop=F]),nrow(stateDf),1))
    names(bsPart.i)=colnames(bsForSim)
    outdf.with.sim.bs.i=cbind(stateDf.noBs,bsPart.i)
    colnames(outdf.with.sim.bs.i)
    x.i=model.matrix(formula(modelFit),data=outdf.with.sim.bs.i)
    latentindex.i=xb.matched.yz(betaMat.onerow,x.i)$xb.matched
    probVec[i]=mean(exp(latentindex.i)/(1+exp(latentindex.i)))
  }
  
  
  obsMortDf=ddply(stateDf,'annual.vol',function(x){c(n=nrow(x),obsMort=mean(x[,'died']))})
  outList=list(meanProbMat=cbind(vol=hospVolVec,mort=probVec), obsMortDf=obsMortDf)
  
  return(outList)
}


#use this function to quickly test fro knots.tiles
getQuickAllStateCurve=function(df
                      ,nobs.fit #number of observation used to fit the model I used 20000
                      ,k #before k all 1,,,,k will be plotted
                      ,byFromkToMaxVol=3
                      ,knots.tiles=c(0.5)
                      
                      ,adjustIndepVars.forCurve         
                    ){
  
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
  allStateDf=ransamDf(data.frame(df,stateEffMat),nobs.fit)
  fitModels=fitStateFixedEffectModel( allStateDf
                                      ,adjustIndepVars.full
                                      ,adjustIndepVars.forCurve
                                      ,logVolVn='logAnnualVol'
                                      ,knots.tiles=knots.tiles
                                      ,bsDegree=2
                                      ,Boundary.knots.tiles=c(0,1)
                                      ,logVolConstant=1
  )
  
  modelFit=fitModels$fit.forCurve
  outlist=allStateCurve(modelFit
                        , k #before k all 1,,,,k will be plotted
                        ,byFromkToMaxVol=byFromkToMaxVol
                                 ,state='all' #if state is all we run for an average learning curve
                                 ,refState='AZ'
                                 ,logVolVn='logAnnualVol'
                                 ,knots.tiles=knots.tiles
                                 ,bsDegree=2
                                 ,Boundary.knots.tiles=c(0,1)
                                 ,logVolConstant=1
  )
    
  return(outlist) 
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

outcomeAtObsAlloc.bystate=function(stateLearningCurve.btMat.G, stateDf){ 
  n.bt=nrow(stateLearningCurve.btMat.G)
  
  volDistrByYear=dlply(stateDf,c('year'),function(x){count(x[,'dshospid'])})
  yearVec=attr(volDistrByYear,'split_labels')[,1]
  numYear=length(yearVec) #9 years
  mortMat=matrix(NA,nrow=numYear,ncol=n.bt)

  for(y in 1:numYear){
   mortMat[y,]=colSums(apply(stateLearningCurve.btMat.G,1,function(x){x[volDistrByYear[[y]][,2]]}))
  }
  colnames(mortMat)=paste('bt',seq(n.bt),sep='.') 
  rownames(mortMat)=yearVec
return(mortMat)
}

#solveOptimalStateYearSolution(Gmat, roundBy, time_limit, yearVol.obsDeaths[i,'vol'])

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




getDeath.obsDistAndOpt=function(
  state
  ,curvesObj #this is obtained
  #which state 'all', 'az
  , cleanedDf    #e.g. aaaDf
  , roundBy=5
  , time_limit=5 
  , death.opt.ciProbs=c(0.025,0.975)
  , death.obsDist.ciProbs=c(0.025,0.975)                            
){
  stateSubDf=subset(cleanedDf,hospst==toupper(state))
  stateLearningCurve.btMat.G=get.volume.gMort.GMort(curvesObj[[state]]$btProbMat)$Gmat
  pred.deaths.obsDist=outcomeAtObsAlloc.bystate(stateLearningCurve.btMat.G, stateSubDf)
  pred.deaths.withCI.obsDist=t(apply(pred.deaths.obsDist,1,function(x){c(mean(x),quantile(x,probs=death.obsDist.ciProbs))}))
  
  colnames(pred.deaths.withCI.obsDist)=c('mean.deaths.current','mean.deaths.current.lb','mean.deaths.current.ub')
  
  #get CI of death of best regionalization
  
  #Gmat=get.volume.gMort.GMort(curvesObj[[state]]$btProbMat)$Gmat  
  Gmat=get.volume.gMort.GMort(curvesObj[[state]]$btProbMat)$Gmat.withVolume0
  yearVol.obsDeathsNumHosp = ddply(stateSubDf,'year',function(x){c(vol=nrow(x),obs.deaths=sum(x[,'died']), obs.nhosp=length(unique(x[,'dshospid'])))})
  yearVol.obsDeathsNumHosp[,'obs.avgvol']=yearVol.obsDeathsNumHosp[,'vol']/yearVol.obsDeathsNumHosp[,'obs.nhosp']
  
  result.list = list()
  
  for (i in 1:nrow(yearVol.obsDeathsNumHosp)){
    cat('solving year ', as.character(yearVol.obsDeathsNumHosp[i,'year']), '\n')
    result.list = lappend.yz(result.list,solveOptimalStateYearSolution(Gmat, roundBy, time_limit, yearVol.obsDeathsNumHosp[i,'vol']))
  }
  
  numHosp.opt=lapply(result.list,function(x){c(mean.deaths=mean(x[[2]]),quantile(x[[2]],probs=death.opt.ciProbs))})
  
  deaths.opt=lapply(result.list,function(x){c(mean.deaths=mean(x[[1]]),quantile(x[[1]],probs=death.opt.ciProbs))})
  
  avgVol.opt=lapply(result.list,function(x){c(mean.deaths=mean(x[[3]]),quantile(x[[3]],probs=death.opt.ciProbs))})
  pred.deaths.withCI.opt=do.call(rbind,deaths.opt)
  pred.numHosp.withCI.opt=do.call(rbind,numHosp.opt)
  pred.avgVol.withCI.opt=do.call(rbind,avgVol.opt)
  
  colnames(pred.deaths.withCI.opt)=c('mean.deaths.opt','mean.deaths.opt.lb','mean.deaths.opt.ub')
  colnames(pred.numHosp.withCI.opt)=c('mean.numHosp.opt','mean.numHosp.opt.lb','mean.numHosp.opt.ub')
  colnames(pred.avgVol.withCI.opt)=c('mean.avgVol.opt','mean.avgVol.opt.lb','mean.avgVol.opt.ub')
  
  outdf=cbind( yearVol.obsDeathsNumHosp,pred.deaths.withCI.obsDist,pred.deaths.withCI.opt,pred.numHosp.withCI.opt,pred.avgVol.withCI.opt)
  
  #mortality times 100
  outdf[,'obs.mort']=100*outdf[,'obs.deaths']/outdf[,'vol']
  outdf[,'mean.mort.current']=100*outdf[,'mean.deaths.current']/outdf[,'vol']
  outdf[,'mean.mort.current.lb']=100*outdf[,'mean.deaths.current.lb']/outdf[,'vol']
  outdf[,'mean.mort.current.ub']=100*outdf[,'mean.deaths.current.ub']/outdf[,'vol']
  outdf[,'mean.mort.opt']=100*outdf[,'mean.deaths.opt']/outdf[,'vol']
  outdf[,'mean.mort.opt.lb']=100*outdf[,'mean.deaths.opt.lb']/outdf[,'vol']
  outdf[,'mean.mort.opt.ub']=100*outdf[,'mean.deaths.opt.ub']/outdf[,'vol']
  outdf[,'state']=state
  
  return(outdf)
}

#get CI of death of observed distribution
# state='az'
# curvesObj=output.aaa

#state='az'
# state='ca'
# curvesObj=curvesObj.aaa
# 
# cleanedDf, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs

# state="md"
# curvesObj=curvesObj.aaa #this is obtained
# #which state 'all', 'az
# cleanedDf= cleanedDf.aaa    #e.g. aaaDf
# 
max(stateSubDf[,'annual.vol'])
stateSubDf=subset(cleanedDf.aaa,hospst=='MD')
max(stateSubDf[,'annual.vol'])

# stateDf=stateSubDf
# n.bt=nrow(stateLearningCurve.btMat.G)
# 
# volDistrByYear=dlply(stateDf,c('year'),function(x){count(x[,'dshospid'])})
# yearVec=attr(volDistrByYear,'split_labels')[,1]
# numYear=length(yearVec) #9 years
# mortMat=matrix(NA,nrow=numYear,ncol=n.bt)
# y=7
# for(y in 1:numYear){
#   mortMat[y,]=colSums(apply(stateLearningCurve.btMat.G,1,function(x){x[volDistrByYear[[y]][,2]]}))
# }
# colnames(mortMat)=paste('bt',seq(n.bt),sep='.') 
# rownames(mortMat)=yearVec

#one problem is the volume exceeds the maximum volume


cleanedDf=cleanedDf.aaa



# getDeath.obsDistAndOpt(
#    "MD"
#   , curvesObj.aaa #this is obtained
#   #which state 'all', 'az
#   , cleanedDf.aaa    #e.g. aaaDf
#   , roundBy=5
#   , time_limit=5 
#   , death.opt.ciProbs=c(0.025,0.975)
#   , death.obsDist.ciProbs=c(0.025,0.975)                            
# )

getAllStateLearnCurvePlotDf=function(curvesObj, ubLbQuantileVec=c(0.025,0.975)){
  
  meanProbVec=curvesObj$all$meanProbVec*100
  hospVolVec=seq(1,length(meanProbVec))
  
  ubLbMat=apply(curvesObj$all$btProbMat,2,function(x){outvec=quantile(x,probs=ubLbQuantileVec)})*100
  learningCurvePlotDf=data.frame(annual.vol=hospVolVec,meanMort=meanProbVec,ub=ubLbMat[1,],lb=ubLbMat[2,])
  return(learningCurvePlotDf)
}


searchFor90Reduction=function(volume.mort.mat,dropPct){
  
  max.mort=max(volume.mort.mat[,2])
  min.mort=min(volume.mort.mat[,2])
  tot.drop=max.mort-min.mort
  mortDropAmt=tot.drop*dropPct
  droppedTo=max.mort-mortDropAmt
  volume.cutoff.index= min(which(volume.mort.mat[,2]<droppedTo))
  cutoffVol=volume.mort.mat[volume.cutoff.index,1]
  outlist=list(cutoffVol=cutoffVol, dropPct=dropPct,mortDropAmt=mortDropAmt, max.mort=max.mort, min.mort=min.mort,droppedTo=droppedTo)
  return(outlist)
}

#IA 2011 does not have hosptial ID



adjustIndepVars.full=c('age4cat', 'race5cat','comorbcat','female','pay1','pl_ur_cat4','hospst','year') #there is little variation across state, also little over year why I did not add sex? 
#but in learning curve, you can only adjust the following otherwise weak terms will cause nonsense experience curve shape
adjustIndepVars.forCurve=c('age4cat', 'race5cat','comorbcat','female','hospst','year') #there is little variation across state, also little over year why I did not add sex? 


#####################-------------------------------################
#####################------------aaa----------------################
#####################-------------------------------################

(load(file='Z:/j_scrdata/frogLearn/aaaCleanedFeb122015.RData'))
cleanedDf.aaa=subset(aaaDf,!hospst %in% c('IA','WA','NC'))
#to drop levels
cleanedDf.aaa$hospst <- factor(cleanedDf.aaa$hospst)
levels((cleanedDf.aaa$hospst))
nrow(cleanedDf.aaa)

cleanedDf.aaa.md=subset(cleanedDf.aaa,hospst=='MD')
nrow(cleanedDf.aaa.md)

max(cleanedDf.aaa.md[,'annual.vol'])





death.opt.ciProbs=death.obsDist.ciProbs=c(0.025,0.975)
# curvesCk.aaa=getQuickAllStateCurve(cleanedDf.aaa
#                                    ,50000 #number of observation used to fit the model I used 20000
#                                    ,80 #plot 1, 2, ,...25
#                                    ,60 # 26, 26+30,...26+60
#                                    ,knots.tiles=c(0.1,0.5)
#                                    #0.1, 0.01 0.02 0.03 0.04, 0.05 0.06 (0.02,0.1) (0.8) (0.1,0.8) c(0.01,0.8) c(0.02,0.04,0.06)
#                                    #c(0.5)
#                                    #0.2 is a bit good 0.25 is better
#                                    #0.3 is good, probabyut with no aspttote
#                                    #c(0.3,0.5) not good
#                                    ,adjustIndepVars.forCurve         
# )
# 
# curvesCk.aaa$meanProbMat[1:70,]
# plot(curvesCk.aaa$meanProbMat,type='b',col='red',ylim=c(0,0.1))
# plot(curvesCk.aaa$meanProbMat[1:80,],col='red',ylim=c(0,0.1))
# points(curvesCk.aaa$obsMortDf[,c(1,3)])

curvesObj.aaa=getAllCurves(cleanedDf.aaa,50000,c('all','AZ','CA','FL','MA','MD','NJ','NY','WI'), knots.tiles=c(0.1,0.5),n.sim.beta=100)

curvesObj.aaa.test=getAllCurves(cleanedDf.aaa,50000,c('all','AZ','CA','FL','MA','MD','NJ','NY','WI'), knots.tiles=c(0.1,0.5),n.sim.beta=20)


plot(curvesObj.aaa$all$meanProbVec)

death.opt.ciProbs=death.obsDist.ciProbs=c(0.025,0.975)
#start solving model

sol.az=getDeath.obsDistAndOpt('az', curvesObj.aaa, cleanedDf.aaa, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.ca=getDeath.obsDistAndOpt('ca', curvesObj.aaa, cleanedDf.aaa, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.fl=getDeath.obsDistAndOpt('fl', curvesObj.aaa, cleanedDf.aaa, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.ma=getDeath.obsDistAndOpt('ma', curvesObj.aaa, cleanedDf.aaa, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.md=getDeath.obsDistAndOpt('md', curvesObj.aaa, cleanedDf.aaa, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.nj=getDeath.obsDistAndOpt('nj', curvesObj.aaa, cleanedDf.aaa, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.ny=getDeath.obsDistAndOpt('ny', curvesObj.aaa, cleanedDf.aaa, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.wi=getDeath.obsDistAndOpt('wi', curvesObj.aaa, cleanedDf.aaa, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)

sol.aaa.new=do.call(rbind,list(sol.az,sol.ca,sol.fl,sol.ma,sol.md, sol.nj,sol.ny,sol.wi))
rm(sol.az,sol.ca,sol.fl,sol.ma,sol.md, sol.nj,sol.ny,sol.wi)
save(curvesObj.aaa,sol.aaa.new, cleanedDf.aaa,file='Z:/j_scrdata/frogLearn/aaaResult_02262015.RData')
#(load(file='Z:/j_scrdata/frogLearn/aaaResult_02262015.RData'))
#rm(curvesObj.aaa,sol.aaa.new,cleanedDf.aaa)
#(load(file='Z:/j_scrdata/frogLearn/aaaResult_02262015.RData'))


#####################-------------------------------################
#####################------------aor----------------################
#####################-------------------------------################
(load(file='Z:/j_scrdata/frogLearn/aorCleanedFeb122015.RData'))
cleanedDf.aor=subset(aorDf,!hospst %in% c('IA','WA','NC'))
cleanedDf.aor$hospst <- factor(cleanedDf.aor$hospst)
levels((cleanedDf.aor$hospst))
adjustIndepVars.full=c('age4cat', 'race5cat','comorbcat','female','pay1','pl_ur_cat4') #there is little variation across state, also little over year why I did not add sex? 
#but in learning curve, you can only adjust the following otherwise weak terms will cause nonsense experience curve shape
adjustIndepVars.forCurve=c('age4cat', 'race5cat','comorbcat','female') #there is little variation across state, also little over year why I did not add sex? 
death.opt.ciProbs=death.obsDist.ciProbs=c(0.025,0.975)
#first use n.sim.beta=1 to check whether the overall learning curve makes sense or not
#if not we need to change knots.tiles
# curvesCk.aor=getQuickAllStateCurve(cleanedDf.aor
#                                    ,50000 #number of observation used to fit the model I used 20000
#                                    ,100 #plot 1, 2, ,...25
#                                    ,50 # 26, 26+30,...26+60
#                                    ,knots.tiles=c(0.18)
#                                    #c(0.5) no
#                                    #c(0.15) better
#                                    #0.1, 0.01 0.02 0.03 0.04, 0.05 0.06 (0.02,0.1) (0.8) (0.1,0.8) c(0.01,0.8) c(0.02,0.04,0.06)
#                                    #c(0.5)
#                                    #0.2 is a bit good 0.25 is better
#                                    #0.3 is good, probabyut with no aspttote
#                                    #c(0.3,0.5) not good
#                                    ,adjustIndepVars.forCurve         
# )
# 
# #I found that birkmiery's estimation was too high. I check observational data, my estimation should be right
# 
# plot(curvesCk.aor$meanProbMat,ylim=c(0,0.15))
# plot(curvesCk.aor$meanProbMat,type='b',col='red',ylim=c(0,0.1))
# points(subset(curvesCk.aor$obsMortDf,n>30)[,c(1,3)])
# nrow(cleanedDf.aor)

curvesObj.aor=getAllCurves(cleanedDf.aor,50000,c('all','AZ','CA','FL','MA','MD','NJ','NY','WI'), knots.tiles=c(0.18),n.sim.beta=100)
plot(curvesObj.aor$all$meanProbVec)

death.opt.ciProbs=death.obsDist.ciProbs=c(0.025,0.975)
#start solving model
sol.az=getDeath.obsDistAndOpt('az', curvesObj.aor, cleanedDf.aor, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.ca=getDeath.obsDistAndOpt('ca', curvesObj.aor, cleanedDf.aor, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.fl=getDeath.obsDistAndOpt('fl', curvesObj.aor, cleanedDf.aor, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.ma=getDeath.obsDistAndOpt('ma', curvesObj.aor, cleanedDf.aor, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.md=getDeath.obsDistAndOpt('md', curvesObj.aor, cleanedDf.aor, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.nj=getDeath.obsDistAndOpt('nj', curvesObj.aor, cleanedDf.aor, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.ny=getDeath.obsDistAndOpt('ny', curvesObj.aor, cleanedDf.aor, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.wi=getDeath.obsDistAndOpt('wi', curvesObj.aor, cleanedDf.aor, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)

sol.aor.new=do.call(rbind,list(sol.az,sol.ca,sol.fl,sol.ma,sol.md, sol.nj,sol.ny,sol.wi))
rm(sol.az,sol.ca,sol.fl,sol.ma,sol.md, sol.nj,sol.ny,sol.wi)
save(curvesObj.aor,sol.aor.new,cleanedDf.aor,file='Z:/j_scrdata/frogLearn/aorResult_02262015.RData')
rm(curvesObj.aor,sol.aor.new,cleanedDf.aor)
#(load(file='Z:/j_scrdata/frogLearn/aorResult_02262015.RData'))



#####################-------------------------------################
#####################------------eso----------------################
#####################-------------------------------################
(load(file='Z:/j_scrdata/frogLearn/esoCleanedFeb122015.RData'))
cleanedDf.eso=subset(esoDf,!hospst %in% c('IA','WA','NC'))
cleanedDf.eso$hospst <- factor(cleanedDf.eso$hospst)
levels((cleanedDf.eso$hospst))
nrow(cleanedDf.eso)
 
death.opt.ciProbs=death.obsDist.ciProbs=c(0.025,0.975)
#first use n.sim.beta=1 to check whether the overall learning curve makes sense or not
#if not we need to change knots.tiles
# curvesCk.eso=getQuickAllStateCurve(cleanedDf.eso
#                                    ,nrow(cleanedDf.eso) #number of observation used to fit the model I used 20000
#                                    ,80 #plot 1, 2, ,...25
#                                    ,60 # 26, 26+30,...26+60
#                                    ,knots.tiles=c(0.1,0.5)
#                                    #0.1, 0.01 0.02 0.03 0.04, 0.05 0.06 (0.02,0.1) (0.8) (0.1,0.8) c(0.01,0.8) c(0.02,0.04,0.06)
#                                    #c(0.5)
#                                    #0.2 is a bit good 0.25 is better
#                                    #0.3 is good, probabyut with no aspttote
#                                    #c(0.3,0.5) not good
#                                    ,adjustIndepVars.forCurve         
# )
# 
# plot(curvesCk.eso$meanProbMat[1:60,])
# plot(curvesCk.eso$meanProbMat,type='b',col='red',ylim=c(0,0.15))
# points(curvesCk.eso$obsMortDf[,c(1,3)])
# nrow(subset(cleanedDf.eso,hospst=='FL'))

curvesObj.eso=getAllCurves(cleanedDf.eso,nrow(cleanedDf.eso),c('all','AZ','CA','FL','MA','MD','NJ','NY','WI'), knots.tiles=c(0.1,0.5),n.sim.beta=100)
plot(curvesObj.eso$all$meanProbVec)

death.opt.ciProbs=death.obsDist.ciProbs=c(0.025,0.975)
#start solving model
sol.az=getDeath.obsDistAndOpt('az', curvesObj.eso, cleanedDf.eso, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.ca=getDeath.obsDistAndOpt('ca', curvesObj.eso, cleanedDf.eso, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.fl=getDeath.obsDistAndOpt('fl', curvesObj.eso, cleanedDf.eso, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.ma=getDeath.obsDistAndOpt('ma', curvesObj.eso, cleanedDf.eso, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.md=getDeath.obsDistAndOpt('md', curvesObj.eso, cleanedDf.eso, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.nj=getDeath.obsDistAndOpt('nj', curvesObj.eso, cleanedDf.eso, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.ny=getDeath.obsDistAndOpt('ny', curvesObj.eso, cleanedDf.eso, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.wi=getDeath.obsDistAndOpt('wi', curvesObj.eso, cleanedDf.eso, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)

sol.eso.new=do.call(rbind,list(sol.az,sol.ca,sol.fl,sol.ma,sol.md, sol.nj,sol.ny,sol.wi))
rm(sol.az,sol.ca,sol.fl,sol.ma,sol.md, sol.nj,sol.ny,sol.wi)
#save(curvesObj.eso,sol.eso.new,cleanedDf.eso,file='Z:/j_scrdata/frogLearn/esoResult_02262015.RData')
rm(curvesObj.eso,sol.eso.new,cleanedDf.eso)
#(load(file='Z:/j_scrdata/frogLearn/esoResult_02262015.RData'))




adjustIndepVars.full=c('age4cat', 'race5cat','comorbcat','female','pay1','pl_ur_cat4') #there is little variation across state, also little over year why I did not add sex? 
#but in learning curve, you can only adjust the following otherwise weak terms will cause nonsense experience curve shape
adjustIndepVars.forCurve=c('age4cat', 'race5cat','comorbcat','female') #there is little variation across state, also little over year why I did not add sex? 
#check cab data is wright. why it is low....
#####################-------------------------------################
#####################------------cab----------------################
#####################-------------------------------################
(load(file='Z:/j_scrdata/frogLearn/cabCleanedFeb122015.RData'))
cleanedDf.cab=subset(cabDf,!hospst %in% c('IA','WA','NC'))
cleanedDf.cab$hospst <- factor(cleanedDf.cab$hospst)
levels((cleanedDf.cab$hospst))

death.opt.ciProbs=death.obsDist.ciProbs=c(0.025,0.975)


death.opt.ciProbs=death.obsDist.ciProbs=c(0.025,0.975)
# first use n.sim.beta=1 to check whether the overall learning curve makes sense or not
# if not we need to change knots.tiles

df=cleanedDf.cab
nobs.fit=5000
k=300
byFromkToMaxVol=200
knot.tiles=0.1

,nobs.fit #number of observation used to fit the model I used 20000
,k #before k all 1,,,,k will be plotted
,byFromkToMaxVol=3
,knots.tiles=c(0.5)

,adjustIndepVars.forCurve 

curvesCk.cab=getQuickAllStateCurve( cleanedDf.cab #cabDf 
                                   ,50000 #number of observation used to fit the model I used 20000
                                   ,300 #plot 1, 2, ,...25
                                   ,200 # 26, 26+30,...26+60
                                   ,knots.tiles=c(0.1)#c(0.15) c(0.1) c(0.15,0.5) ok
                                   #0.1, 0.01 0.02 0.03 0.04, 0.05 0.06 (0.02,0.1) (0.8) (0.1,0.8) c(0.01,0.8) c(0.02,0.04,0.06)
                                   #c(0.5)
                                   #0.2 is a bit good 0.25 is better
                                   #0.3 is good, probabyut with no aspttote
                                   #c(0.3,0.5) not good
                                   ,adjustIndepVars.forCurve         
)

plot(curvesCk.cab$meanProbMat[1:60,],ylim=c(0,0.1))
plot(curvesCk.cab$meanProbMat[1:50,])
plot(curvesCk.cab$meanProbMat,type='b',col='red',ylim=c(0,0.1))
points(curvesCk.cab$obsMortDf[,c(1,3)])


curvesObj.cab=getAllCurves(cleanedDf.cab,50000,c('all','AZ','CA','FL','MA','MD','NJ','NY','WI'),adjustIndepVars.full=adjustIndepVars.full,adjustIndepVars.forCurve=adjustIndepVars.forCurve, knots.tiles=c(0.1),n.sim.beta=100)

names(curvesObj.cab)
summary(curvesObj.cab$modelFit)

plot(curvesobj.cab$all$meanProbVec)
save(curvesobj.cab,file='Z:/j_scrdata/frogLearn/curvesobj_cab_03052015.RData')

(load(file='Z:/j_scrdata/frogLearn/curvesobj_cab_03052015.RData'))


plot(curvesobj.cab$all$btProbMat)
names(curvesobj.cab$az$meanProbVec)
plot(curvesobj.cab$az$meanProbVec)
diff(curvesobj.cab$az$meanProbVec)
lines(curvesobj.cab$all$meanProbVec)
lines(curvesobj.cab$ny$meanProbVec)


death.opt.ciProbs=death.obsDist.ciProbs=c(0.025,0.975)
#start solving model
#rm(sol.az,sol.ca,sol.fl,sol.ma,sol.md, sol.nj,sol.ny,sol.wi)

state='az'
curvesObj=curvesObj.cab
cleanedDf=cleanedDf.cab
roundBy=10
time_limit=5
death.opt.ciProbs=death.opt.ciProbs
death.obsDist.ciProbs=death.obsDist.ciProbs

sol.az=getDeath.obsDistAndOpt('az', curvesObj.cab, cleanedDf.cab, roundBy=10, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.ca=getDeath.obsDistAndOpt('ca', curvesObj.cab, cleanedDf.cab, roundBy=10, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.fl=getDeath.obsDistAndOpt('fl', curvesObj.cab, cleanedDf.cab, roundBy=20, time_limit=10, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.ma=getDeath.obsDistAndOpt('ma', curvesObj.cab, cleanedDf.cab, roundBy=20, time_limit=10, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.md=getDeath.obsDistAndOpt('md', curvesObj.cab, cleanedDf.cab, roundBy=20, time_limit=10, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.nj=getDeath.obsDistAndOpt('nj', curvesObj.cab, cleanedDf.cab, roundBy=10, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.ny=getDeath.obsDistAndOpt('ny', curvesObj.cab, cleanedDf.cab, roundBy=10, time_limit=15, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.wi=getDeath.obsDistAndOpt('wi', curvesObj.cab, cleanedDf.cab, roundBy=20, time_limit=20, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)


sol.az=getDeath.obsDistAndOpt('az', curvesObj.cab, cleanedDf.cab, roundBy=50, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.ca=getDeath.obsDistAndOpt('ca', curvesObj.cab, cleanedDf.cab, roundBy=50, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.fl=getDeath.obsDistAndOpt('fl', curvesObj.cab, cleanedDf.cab, roundBy=50, time_limit=10, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.ma=getDeath.obsDistAndOpt('ma', curvesObj.cab, cleanedDf.cab, roundBy=50, time_limit=10, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.md=getDeath.obsDistAndOpt('md', curvesObj.cab, cleanedDf.cab, roundBy=50, time_limit=10, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.nj=getDeath.obsDistAndOpt('nj', curvesObj.cab, cleanedDf.cab, roundBy=50, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.ny=getDeath.obsDistAndOpt('ny', curvesObj.cab, cleanedDf.cab, roundBy=50, time_limit=15, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.wi=getDeath.obsDistAndOpt('wi', curvesObj.cab, cleanedDf.cab, roundBy=50, time_limit=20, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)


sol.cab.new=do.call(rbind,list(sol.az,sol.ca,sol.fl,sol.ma,sol.md, sol.nj,sol.ny,sol.wi))
#rm(sol.az,sol.ca,sol.fl,sol.ma,sol.md, sol.nj,sol.ny,sol.wi)
save(curvesObj.cab,sol.cab.new,cleanedDf.cab,file='Z:/j_scrdata/frogLearn/cabResult_02262015.RData')
#rm(curvesObj.cab,sol.cab.new,cleanedDf.cab)
#(load(file='Z:/j_scrdata/frogLearn/cabResult_02262015.RData'))
#####################-------------------------------################
#####################------------car----------------################
#####################-------------------------------################
(load(file='Z:/j_scrdata/frogLearn/carCleanedFeb122015.RData'))
cleanedDf.car=subset(carDf,!hospst %in% c('IA','WA','NC'))
cleanedDf.car$hospst <- factor(cleanedDf.car$hospst)
levels((cleanedDf.car$hospst))

death.opt.ciProbs=death.obsDist.ciProbs=c(0.025,0.975)
#first use n.sim.beta=1 to check whether the overall learning curve makes sense or not
#if not we need to change knots.tiles
curvesCk.car=getQuickAllStateCurve(cleanedDf.car
                                   ,50000 #number of observation used to fit the model I used 20000
                                   ,70 #plot 1, 2, ,...25
                                   ,50 # 26, 26+30,...26+60
                                   ,knots.tiles=c(0.)
                                   #0.1, 0.01 0.02 0.03 0.04, 0.05 0.06 (0.02,0.1) (0.8) (0.1,0.8) c(0.01,0.8) c(0.02,0.04,0.06)
                                   #c(0.5)
                                   #0.2 is a bit good 0.25 is better
                                   #0.3 is good, probabyut with no aspttote
                                   #c(0.3,0.5) not good
                                   ,adjustIndepVars.forCurve         
)
# 
# 
# 
# plot(curvesCk.car$meanProbMat[1:20,])
# plot(curvesCk.car$meanProbMat,type='b',col='red',ylim=c(0,0.03))
# points(curvesCk.car$obsMortDf[,c(1,3)])


curvesobj.car=getAllCurves(cleanedDf.car,50000,c('all','AZ','CA','FL','MA','MD','NJ','NY','WI'), knots.tiles=c(0.05,0.1,0.5,0.9),n.sim.beta=100)
plot(curvcarbj.car$all$meanProbVec)

death.opt.ciProbs=death.obsDist.ciProbs=c(0.025,0.975)
#start solving model
sol.az=getDeath.obsDistAndOpt('az', curvesObj.car, cleanedDf.car, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.ca=getDeath.obsDistAndOpt('ca', curvesObj.car, cleanedDf.car, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.fl=getDeath.obsDistAndOpt('fl', curvesObj.car, cleanedDf.car, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.ma=getDeath.obsDistAndOpt('ma', curvesObj.car, cleanedDf.car, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.md=getDeath.obsDistAndOpt('md', curvesObj.car, cleanedDf.car, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.nj=getDeath.obsDistAndOpt('nj', curvesObj.car, cleanedDf.car, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.ny=getDeath.obsDistAndOpt('ny', curvesObj.car, cleanedDf.car, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.wi=getDeath.obsDistAndOpt('wi', curvesObj.car, cleanedDf.car, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)

sol.car.new=do.call(rbind,list(sol.az,sol.ca,sol.fl,sol.ma,sol.md, sol.nj,sol.ny,sol.wi))
rm(sol.az,sol.ca,sol.fl,sol.ma,sol.md, sol.nj,sol.ny,sol.wi)
save(curvesObj.car,sol.car.new,cleanedDf.car,file='Z:/j_scrdata/frogLearn/carResult_02262015.RData')
rm(curvesObj.car,sol.car.new,cleanedDf.car)
#(load(file='Z:/j_scrdata/frogLearn/carResult_02262015.RData'))
rm(curvesobj.car)

#####################-------------------------------################
#####################------------cys----------------################
#####################-------------------------------################



adjustIndepVars.forCurve = c("age4cat", "race5cat", "comorbcat", "female","hospst")

(load(file='Z:/j_scrdata/frogLearn/cysCleanedFeb122015.RData'))
cleanedDf.cys=subset(cysDf,!hospst %in% c('IA','WA','NC'))
cleanedDf.cys$hospst <- factor(cleanedDf.cys$hospst)
levels((cleanedDf.cys$hospst))

death.opt.ciProbs=death.obsDist.ciProbs=c(0.025,0.975)
#first use n.sim.beta=1 to check whether the overall learning curve makes sense or not
#if not we need to change knots.tiles
curvesCk.cys=getQuickAllStateCurve(cleanedDf.cys
                                   ,nrow(cleanedDf.cys) #number of observation used to fit the model I used 20000
                                   ,80 #plot 1, 2, ,...25
                                   ,60 # 26, 26+30,...26+60
                                   ,knots.tiles=c(0.1,0.5)
                                   #0.1, 0.01 0.02 0.03 0.04, 0.05 0.06 (0.02,0.1) (0.8) (0.1,0.8) c(0.01,0.8) c(0.02,0.04,0.06)
                                   #c(0.5)
                                   #0.2 is a bit good 0.25 is better
                                   #0.3 is good, probabyut with no aspttote
                                   #c(0.3,0.5) not good
                                   ,adjustIndepVars.forCurve         
)




plot(curvesCk.cys$meanProbMat[1:60,])
plot(curvesCk.cys$meanProbMat,type='b',col='red',ylim=c(0,0.1))
points(curvesCk.cys$obsMortDf[,c(1,3)])


# curvesObj.cys.test=getAllCurves(cleanedDf.cys,nrow(cleanedDf.cys),c('all','AZ','CA','FL','MA','MD','NJ','NY','WI')
#                            ,adjustIndepVars.full=adjustIndepVars.full
#                            ,adjustIndepVars.forCurve=adjustIndepVars.forCurve
#                            , knots.tiles=c(0.1,0.5),n.sim.beta=50)
# 
# plot(curvesObj.cys.test$all$meanProb)
# lines(curvesObj.cys.test$az$meanProb)
# lines(curvesObj.cys.test$md$meanProb)
# lines(curvesObj.cys.test$ny$meanProb)
# lines(curvesObj.cys.test$fl$meanProb)

names(curvesObj.cys.test)

curvesObj.cys.test$modelFit
names(curvesObj.cys.test)
plot(curvesObj.cys.test$all$meanProbVec)

df=cleanedDf.cys
nobs.fit=nrow(cleanedDf.cys)
stateNameVec=c('all','AZ','CA','FL','MA','MD','NJ','NY','WI')


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
allStateDf=ransamDf(data.frame(df,stateEffMat),nobs.fit)
fitModels=fitStateFixedEffectModel( allStateDf
                                    ,adjustIndepVars.full
                                    ,adjustIndepVars.forCurve
                                    ,logVolVn='logAnnualVol'
                                    ,knots.tiles=knots.tiles
                                    ,bsDegree=2
                                    ,Boundary.knots.tiles=c(0,1)
                                    ,logVolConstant=1
)

adjustIndepVars.forCurve=c("age4cat", "race5cat","comorbcat","female",'hospst')
adjustIndepVars.full=c("age4cat" ,   "race5cat",   "comorbcat",  "female" , "pay1" , "pl_ur_cat4",'hospst')

names(fitModels)
summary(fitModels$fit.full)
summary(fitModels$fit.forCurve)

modelFit=fitModels$fit.forCurve
outlist=allStateByStateCurves(modelFit,stateNameVec,knots.tiles=knots.tiles,n.sim.beta=n.sim.beta)



death.opt.ciProbs=death.obsDist.ciProbs=c(0.025,0.975)
#start solving model
sol.az=getDeath.obsDistAndOpt('az', curvesObj.cys, cleanedDf.cys, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.ca=getDeath.obsDistAndOpt('ca', curvesObj.cys, cleanedDf.cys, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.fl=getDeath.obsDistAndOpt('fl', curvesObj.cys, cleanedDf.cys, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.ma=getDeath.obsDistAndOpt('ma', curvesObj.cys, cleanedDf.cys, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.md=getDeath.obsDistAndOpt('md', curvesObj.cys, cleanedDf.cys, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.nj=getDeath.obsDistAndOpt('nj', curvesObj.cys, cleanedDf.cys, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.ny=getDeath.obsDistAndOpt('ny', curvesObj.cys, cleanedDf.cys, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.wi=getDeath.obsDistAndOpt('wi', curvesObj.cys, cleanedDf.cys, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)

sol.cys.new=do.call(rbind,list(sol.az,sol.ca,sol.fl,sol.ma,sol.md, sol.nj,sol.ny,sol.wi))
#rm(sol.az,sol.ca,sol.fl,sol.ma,sol.md, sol.nj,sol.ny,sol.wi)
save(curvesObj.cys,sol.cys.new,cleanedDf.cys,file='Z:/j_scrdata/frogLearn/cysResult_02262015.RData')
rm(curvesObj.cys,sol.cys.new,cleanedDf.cys)
#(load(file='Z:/j_scrdata/frogLearn/cysResult_02262015.RData'))

#####################-------------------------------################
#####################------------lun----------------################
#####################-------------------------------################
(load(file='Z:/j_scrdata/frogLearn/lunCleanedFeb122015.RData'))
cleanedDf.lun=subset(lunDf,!hospst %in% c('IA','WA','NC'))
cleanedDf.lun$hospst <- factor(cleanedDf.lun$hospst)
levels((cleanedDf.lun$hospst))

death.opt.ciProbs=death.obsDist.ciProbs=c(0.025,0.975)
#first use n.sim.beta=1 to check whether the overall learning curve makes sense or not
#if not we need to change knots.tiles
curvesCk.lun=getQuickAllStateCurve(cleanedDf.lun
                                   ,50000 #number of observation used to fit the model I used 20000
                                   ,80 #plot 1, 2, ,...25
                                   ,60 # 26, 26+30,...26+60
                                   ,knots.tiles=c(0.05,0.5)
                                   #0.1, 0.01 0.02 0.03 0.04, 0.05 0.06 (0.02,0.1) (0.8) (0.1,0.8) c(0.01,0.8) c(0.02,0.04,0.06)
                                   #c(0.5)
                                   #0.2 is a bit good 0.25 is better
                                   #0.3 is good, probabyut with no aspttote
                                   #c(0.3,0.5) not good
                                   ,adjustIndepVars.forCurve         
)

plot(curvesCk.lun$meanProbMat[1:60,])
plot(curvesCk.lun$meanProbMat,type='b',col='red',ylim=c(0,0.1))
points(curvesCk.lun$obsMortDf[,c(1,3)])


curvesObj.lun=getAllCurves(cleanedDf.lun,50000,c('all','AZ','CA','FL','MA','MD','NJ','NY','WI'), knots.tiles=c(0.05,0.5),n.sim.beta=100)
plot(curvesobj.lun$all$meanProbVec)

death.opt.ciProbs=death.obsDist.ciProbs=c(0.025,0.975)
#start solving model
sol.az=getDeath.obsDistAndOpt('az', curvesObj.lun, cleanedDf.lun, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.ca=getDeath.obsDistAndOpt('ca', curvesObj.lun, cleanedDf.lun, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.fl=getDeath.obsDistAndOpt('fl', curvesObj.lun, cleanedDf.lun, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.ma=getDeath.obsDistAndOpt('ma', curvesObj.lun, cleanedDf.lun, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.md=getDeath.obsDistAndOpt('md', curvesObj.lun, cleanedDf.lun, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.nj=getDeath.obsDistAndOpt('nj', curvesObj.lun, cleanedDf.lun, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.ny=getDeath.obsDistAndOpt('ny', curvesObj.lun, cleanedDf.lun, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.wi=getDeath.obsDistAndOpt('wi', curvesObj.lun, cleanedDf.lun, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)

sol.lun.new=do.call(rbind,list(sol.az,sol.ca,sol.fl,sol.ma,sol.md, sol.nj,sol.ny,sol.wi))
#rm(sol.az,sol.ca,sol.fl,sol.ma,sol.md, sol.nj,sol.ny,sol.wi)
save(curvesObj.lun,sol.lun.new,cleanedDf.lun,file='Z:/j_scrdata/frogLearn/lunResult_02262015.RData')
rm(curvesObj.lun,sol.lun.new,cleanedDf.lun)
#(load(file='Z:/j_scrdata/frogLearn/lunResult_02262015.RData'))


#####################-------------------------------################
#####################------------pan----------------################
#####################-------------------------------################
(load(file='Z:/j_scrdata/frogLearn/panCleanedFeb122015.RData'))
cleanedDf.pan=subset(panDf,!hospst %in% c('IA','WA','NC'))
cleanedDf.pan$hospst <- factor(cleanedDf.pan$hospst)
levels((cleanedDf.pan$hospst))

death.opt.ciProbs=death.obsDist.ciProbs=c(0.025,0.975)
#first use n.sim.beta=1 to check whether the overall learning curve makes sense or not
#if not we need to change knots.tiles
curvesCk.pan=getQuickAllStateCurve(cleanedDf.pan
                                   ,nrow(cleanedDf.pan) #number of observation used to fit the model I used 20000
                                   ,80 #plot 1, 2, ,...25
                                   ,20 # 26, 26+30,...26+60
                                   ,knots.tiles=c(0.1)
                                   #0.1, 0.01 0.02 0.03 0.04, 0.05 0.06 (0.02,0.1) (0.8) (0.1,0.8) c(0.01,0.8) c(0.02,0.04,0.06)
                                   #c(0.5)
                                   #0.2 is a bit good 0.25 is better
                                   #0.3 is good, probabyut with no aspttote
                                   #c(0.3,0.5) not good
                                   ,adjustIndepVars.forCurve         
)

plot(curvesCk.pan$meanProbMat[1:60,])
plot(curvesCk.pan$meanProbMat)
plot(curvesCk.pan$meanProbMat,type='b',col='red',ylim=c(0,0.15))
points(curvesCk.pan$obsMortDf[,c(1,3)])

curvesObj.pan=getAllCurves(cleanedDf.pan,nrow(cleanedDf.pan),c('all','AZ','CA','FL','MA','MD','NJ','NY','WI'), knots.tiles=c(0.1),n.sim.beta=100)
plot(curvesobj.pan$az$meanProbVec)
death.opt.ciProbs=death.obsDist.ciProbs=c(0.025,0.975)
#start solving model
sol.az=getDeath.obsDistAndOpt('az', curvesObj.pan, cleanedDf.pan, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.ca=getDeath.obsDistAndOpt('ca', curvesObj.pan, cleanedDf.pan, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.fl=getDeath.obsDistAndOpt('fl', curvesObj.pan, cleanedDf.pan, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.ma=getDeath.obsDistAndOpt('ma', curvesObj.pan, cleanedDf.pan, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.md=getDeath.obsDistAndOpt('md', curvesObj.pan, cleanedDf.pan, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.nj=getDeath.obsDistAndOpt('nj', curvesObj.pan, cleanedDf.pan, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.ny=getDeath.obsDistAndOpt('ny', curvesObj.pan, cleanedDf.pan, roundBy=5, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)
sol.wi=getDeath.obsDistAndOpt('wi', curvesObj.pan, cleanedDf.pan, roundBy=1, time_limit=5, death.opt.ciProbs=death.opt.ciProbs, death.obsDist.ciProbs=death.obsDist.ciProbs)



sol.pan.new=do.call(rbind,list(sol.az,sol.ca,sol.fl,sol.ma,sol.md, sol.nj,sol.ny,sol.wi))
#rm(sol.az,sol.ca,sol.fl,sol.ma,sol.md, sol.nj,sol.ny,sol.wi)
#save(curvesObj.pan,sol.pan.new,cleanedDf.pan,file='Z:/j_scrdata/frogLearn/panResult_02262015.RData')
rm(curvesObj.pan,sol.pan.new,cleanedDf.pan)
#(load(file='Z:/j_scrdata/frogLearn/panResult_02262015.RData'))

plot(curvpanbj.pan$all$meanProbVe )

plotAllStCurve=function(obj,ylim,path){
  tiff(file=path)
  plot(obj$all$meanProbVec, ylim=ylim,type='b',col='red')
  points(obj$all$obsMortDf[,c(1,3)])
  dev.off()
}



#pan
(load(file='Z:/j_scrdata/frogLearn/panResult_02262015.RData'))
#plotAllStCurve(curvpanbj.pan,c(0,0.2),'Z:/j_scrdata/frogLearn/panFig_03042015.tif')
plotAllStCurve(curvesObj.pan,c(0,0.2),'Z:/j_scrdata/frogLearn/panFig_03142015.tif')
plotAllStCurve(curvesObj.pan,c(0,0.2),'Z:/j_scrdata/frogLearn/panFig_03232015.tif')
rm(curvpanbj.pan,sol.pan,cleanedDf.pan)

#lun
(load(file='Z:/j_scrdata/frogLearn/lunResult_02262015.RData'))
plotAllStCurve(curvlunbj.lun,c(0,0.1),'Z:/j_scrdata/frogLearn/lunFig_03042015.tif')
rm(curvlunbj.lun,sol.lun,cleanedDf.lun)

#cys is missing


#cab is missing
(load(file='Z:/j_scrdata/frogLearn/cabResult_02262015.RData'))
#plotAllStCurve(curvesobj.lun,c(0,0.1),'Z:/j_scrdata/frogLearn/cabFig_03042015.tif')
plotAllStCurve(curvesObj.cab,c(0,0.1),'Z:/j_scrdata/frogLearn/cabFig_03142015.tif')
rm(curvlunbj.lun,sol.lun,cleanedDf.lun)


#car does not look right
(load(file='Z:/j_scrdata/frogLearn/carResult_02262015.RData'))
plotAllStCurve(curvesobj.car,c(0,0.1),'Z:/j_scrdata/frogLearn/carFig_03232015.tif')
rm(curvcarbj.car,sol.car,cleanedDf.car)


#eso  looks right
(load(file='Z:/j_scrdata/frogLearn/esoResult_02262015.RData'))
plotAllStCurve(curvesObj.eso,c(0,0.15),'Z:/j_scrdata/frogLearn/esoFig_03042015.tif')
rm(curvesObj.eso,sol.eso,cleanedDf.eso)


#aaa looks right (the tail part is titling up that is not godd, so cut)
(load(file='Z:/j_scrdata/frogLearn/aaaResult_02262015.RData'))
plotAllStCurve(curvesObj.aaa,c(0,0.15),'Z:/j_scrdata/frogLearn/aaaFig_03042015.tif')
rm(curvesObj.aaa,sol.aaa,cleanedDf.aaa)





#
(load(file='Z:/j_scrdata/frogLearn/aorResult_02262015.RData'))
plotAllStCurve(curvesObj.aor,c(0,0.15),'Z:/j_scrdata/frogLearn/aorFig_03042015.tif')
rm(curvesObj.aor,sol.aor,cleanedDf.aor)


getLearnCurvePlotDf=function(hospVolVec, meanProbVec, btProbMat, ubLbQuantileVec=c(0.025,0.975)){
  ubLbMat=apply(btProbMat,2,function(x){outvec=quantile(x,probs=ubLbQuantileVec)})
  learningCurvePlotDf=data.frame(annual.vol=hospVolVec,meanMort=meanProbVec,ub=ubLbMat[1,],lb=ubLbMat[2,])
  return(learningCurvePlotDf)
}


plotLearningCurve=function(){
  #you do not need to touch any thing below block
  plotDf=getLearnCurvePlotDf(seq(length(FitCurveOutputObj$all$meanProbVec)), FitCurveOutputObj$all$meanProbVec*100, FitCurveOutputObj$all$btProbMat*100, ubLbQuantileVec=ci.quantile.vec)[1:max.plot.vol,]
  tiff(tiffPath)
  plotObj=qplot(annual.vol, meanMort,data=plotDf, xlab='Annual volume', ylab='In-hospital mortality (%)')+expand_limits(x = 0, y =0)+scale_x_continuous(expand = c(0, 0),breaks=vol.breaks, labels=vol.breaks) + scale_y_continuous(expand = c(0, 0),breaks=mort.breaks, labels=mort.breaks)+ggtitle(ggtitle)+ theme(plot.title = element_text(lineheight=.8, face="bold"))+geom_smooth(aes(ymin=lb, ymax=ub), data=plotDf, stat="identity")+geom_segment(aes(x=cutOffVol,xend=cutOffVol,y=0,yend=cutOffVol.mort),linetype = "longdash",colour='grey')+geom_segment(aes(x=1,xend=1,y=0,yend=max.mort),linetype = "longdash",colour="grey")+geom_segment(aes(x=0,xend=1,y=max.mort,yend=max.mort),linetype = "longdash",colour="grey")+geom_hline(yintercept=min.mort, colour="grey", linetype = "longdash")+geom_segment(aes(x=0,xend=cutOffVol,y=cutOffVol.mort,yend=cutOffVol.mort),linetype = "longdash",colour="grey")+geom_hline(yintercept=min.mort, colour="grey", linetype = "longdash")+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank()) 
  print(plotObj)
  dev.off()
}

#number of hospitals 
getByStateNumHospDf=function(summaryTable,vn.obs='observed',vn.opt='regionalization'){
  
  byState.nHosp.obs.Df = ddply(summaryTable,'state',function(x){
    obs.numHosp.mean=sum(x[,'vol']*x[,'obs.nhosp'])/sum(x[,'vol'])
    obs.numHosp.ub=obs.numHosp.lb=obs.numHosp.mean
    out=c(type=vn.obs, mean=obs.numHosp.mean,lb=obs.numHosp.lb,ub=obs.numHosp.ub)
    return(out)
  })  
  
  byState.nHosp.opt.Df = ddply(summaryTable,'state',function(x){
    mean.numHosp.opt=sum(x[,'vol']*x[,'mean.numHosp.opt'])/sum(x[,'vol'])
    mean.numHosp.opt.lb=sum(x[,'vol']*x[,'mean.numHosp.opt.lb'])/sum(x[,'vol'])
    mean.numHosp.opt.ub=sum(x[,'vol']*x[,'mean.numHosp.opt.ub'])/sum(x[,'vol'])
    out=c(type=vn.opt, mean=mean.numHosp.opt,lb=mean.numHosp.opt.lb, ub=mean.numHosp.opt.ub)
    return(out)
  })
  
  plotDf=sortDf.yz(rbind(byState.nHosp.obs.Df,byState.nHosp.opt.Df),vars=c('state','type'))
  plotDf[,'state']=toupper(plotDf[,'state'])
  plotDf[,'mean']=as.numeric(plotDf[,'mean'])
  plotDf[,'lb']=as.numeric(plotDf[,'lb'])
  plotDf[,'ub']=as.numeric(plotDf[,'ub'])
  return(plotDf)
}


#number of volumes
getVolDf=function(summaryTable,vn.obs='observed',vn.opt='regionalization'){
  
  byState.vol.obs.Df = ddply(summaryTable,'state',function(x){
    obs.vol.mean=sum(x[,'vol']*x[,'obs.avgvol'])/sum(x[,'vol'])
    obs.vol.ub=obs.vol.lb=obs.vol.mean
    out=c(type=vn.obs, mean=obs.vol.mean,lb=obs.vol.lb,ub=obs.vol.ub)
    return(out)
  })  
  
  byState.vol.opt.Df = ddply(summaryTable,'state',function(x){
    mean.vol.opt=sum(x[,'vol']*x[,'mean.avgVol.opt'])/sum(x[,'vol'])
    mean.vol.opt.lb=sum(x[,'vol']*x[,'mean.avgVol.opt.lb'])/sum(x[,'vol'])
    mean.vol.opt.ub=sum(x[,'vol']*x[,'mean.avgVol.opt.ub'])/sum(x[,'vol'])
    out=c(type=vn.opt, mean=mean.vol.opt,lb=mean.vol.opt.lb, ub=mean.vol.opt.ub)
    return(out)
  })
  
  plotDf=sortDf.yz(rbind(byState.vol.obs.Df,byState.vol.opt.Df),vars=c('state','type'))
  plotDf[,'state']=toupper(plotDf[,'state'])
  plotDf[,'mean']=as.numeric(plotDf[,'mean'])
  plotDf[,'lb']=as.numeric(plotDf[,'lb'])
  plotDf[,'ub']=as.numeric(plotDf[,'ub'])
  return(plotDf)
}



searchFor90Reduction=function(volume.mort.mat,dropPct){
  
  max.mort=max(volume.mort.mat[,2])
  min.mort=min(volume.mort.mat[,2])
  tot.drop=max.mort-min.mort
  mortDropAmt=tot.drop*dropPct
  droppedTo=max.mort-mortDropAmt
  volume.cutoff.index= min(which(volume.mort.mat[,2]<droppedTo))
  cutoffVol=volume.mort.mat[volume.cutoff.index,1]
  outlist=list(cutoffVol=cutoffVol, dropPct=dropPct,mortDropAmt=mortDropAmt, max.mort=max.mort, min.mort=min.mort,droppedTo=droppedTo)
  return(outlist)
}

plotLearningCurveFullRange=function(){
  #you do not need to touch any thing below block
  plotDf=getLearnCurvePlotDf(seq(length(FitCurveOutputObj$meanProbVec)), FitCurveOutputObj$meanProbVec*100, FitCurveOutputObj$btProbMat*100, ubLbQuantileVec=ci.quantile.vec)
  tiff(tiffPathFullRange)
  plotObj=qplot(annual.vol, meanMort,data=plotDf, xlab='Annual volume', ylab='In-hospital mortality (%)')+expand_limits(x = 0, y =0)+scale_x_continuous(expand = c(0, 0),breaks=vol.breaks.fullRange, labels=vol.breaks.fullRange) + scale_y_continuous(expand = c(0, 0),breaks=mort.breaks, labels=mort.breaks)+ggtitle(ggtitle)+ theme(plot.title = element_text(lineheight=.8, face="bold"))+geom_smooth(aes(ymin=lb, ymax=ub), data=plotDf, stat="identity")+geom_segment(aes(x=cutOffVol,xend=cutOffVol,y=0,yend=cutOffVol.mort),linetype = "longdash",colour='grey')+geom_segment(aes(x=minMortVol,xend=minMortVol,y=0,yend=min.mort),linetype = "longdash",colour='grey')+geom_segment(aes(x=1,xend=1,y=0,yend=max.mort),linetype = "longdash",colour="grey")+geom_segment(aes(x=0,xend=1,y=max.mort,yend=max.mort),linetype = "longdash",colour="grey")+geom_hline(yintercept=min.mort, colour="grey", linetype = "longdash")+geom_segment(aes(x=0,xend=cutOffVol,y=cutOffVol.mort,yend=cutOffVol.mort),linetype = "longdash",colour="grey")+geom_hline(yintercept=min.mort, colour="grey", linetype = "longdash")+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank()) 
  print(plotObj)
  dev.off()
}


plotLearningCurve=function(){
  #you do not need to touch any thing below block
  plotDf=getLearnCurvePlotDf(seq(length(FitCurveOutputObj$meanProbVec)), FitCurveOutputObj$meanProbVec*100, FitCurveOutputObj$btProbMat*100, ubLbQuantileVec=ci.quantile.vec)[1:max.plot.vol,]
  tiff(tiffPath)
  plotObj=qplot(annual.vol, meanMort,data=plotDf, xlab='Annual volume', ylab='In-hospital mortality (%)')+expand_limits(x = 0, y =0)+scale_x_continuous(expand = c(0, 0),breaks=vol.breaks, labels=vol.breaks) + scale_y_continuous(expand = c(0, 0),breaks=mort.breaks, labels=mort.breaks)+ggtitle(ggtitle)+ theme(plot.title = element_text(lineheight=.8, face="bold"))+geom_smooth(aes(ymin=lb, ymax=ub), data=plotDf, stat="identity")+geom_segment(aes(x=cutOffVol,xend=cutOffVol,y=0,yend=cutOffVol.mort),linetype = "longdash",colour='grey')+geom_segment(aes(x=1,xend=1,y=0,yend=max.mort),linetype = "longdash",colour="grey")+geom_segment(aes(x=0,xend=1,y=max.mort,yend=max.mort),linetype = "longdash",colour="grey")+geom_hline(yintercept=min.mort, colour="grey", linetype = "longdash")+geom_segment(aes(x=0,xend=cutOffVol,y=cutOffVol.mort,yend=cutOffVol.mort),linetype = "longdash",colour="grey")+geom_hline(yintercept=min.mort, colour="grey", linetype = "longdash")+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank()) 
  print(plotObj)
  dev.off()
}

#-----aaa

#aaa
(load(file='Z:/j_scrdata/frogLearn/aaaResult_02262015.RData'))

tiffPath='C:/Dropbox/paper/frog/results/fig/04222015/aaaExpCurveWithCi_04222015.tif'
tiffPathFullRange='C:/Dropbox/paper/frog/results/fig/04222015/aaaExpCurveWithCi_fullrange_04222015.tif'

FitCurveOutputObj=curvesObj.aaa$all
ci.quantile.vec=c(0.25,0.75)
mort.by=5
vol.by=50
outlist=searchFor90Reduction(cbind(seq(1,length(FitCurveOutputObj$meanProbVec)),FitCurveOutputObj$meanProbVec),0.9)
cutOffVol=unname(outlist$cutoffVol) #90% mortality reduction
outlist$cutoffVol
cutOffVol.mort=roundToMultipleOf.yz(FitCurveOutputObj$meanProbVec[cutOffVol]*100,0.01,type='regular')
max.plot.vol=roundToMultipleOf.yz(cutOffVol*1.5,5,type='ceiling') #this is only useful for not full range ploting
#vol.breaks=c(1,seq(0,max.plot.vol,by=vol.by),cutOffVol) # x breaks
vol.breaks=c(seq(1,max.plot.vol,by=vol.by),cutOffVol) # x breaks
minMortVol=which.min(FitCurveOutputObj$meanProbVec)
vol.breaks.fullRange=c(1,cutOffVol, minMortVol) # x breaks
min.mort=roundToMultipleOf.yz(outlist$min.mort*100,0.01,type='regular')
max.mort=roundToMultipleOf.yz(outlist$max.mort*100,0.01,type='regular')
max.plot.mort=roundToMultipleOf.yz(outlist$max.mort*100*1.25,5,type='ceiling')
mort.breaks=c(min.mort,max.mort,cutOffVol.mort) #y breaks
mort.breaks=unname(mort.breaks)
#ggtitle="Experience curve of aortic-valve replacement"
ggtitle=""

plotLearningCurve()
plotLearningCurveFullRange()


#I think by year is too complicated. Just do by state and surgery

numHospPlotDf.aaa=getByStateNumHospDf(sol.aaa.new)

gap=0.817
plotDf=numHospPlotDf.aaa

# state            type        mean          lb          ub
# 1     AZ        observed  40.3457156  40.3457156  40.3457156
# 9     AZ regionalization   7.5758798   5.1237326  10.1237326
# 2     CA        observed 209.1149852 209.1149852 209.1149852
# 10    CA regionalization  24.6368348  22.8733019  27.6535048
# 3     FL        observed 146.6149645 146.6149645 146.6149645
# 11    FL regionalization  21.8245404  18.7916166  23.2295467
# 4     MA        observed  44.2059545  44.2059545  44.2059545
# 12    MA regionalization   7.4523877   4.5869915   9.5869915
# 5     MD        observed  38.5389401  38.5389401  38.5389401
# 13    MD regionalization  12.5753571   9.4158392  14.0613710
# 6     NJ        observed  64.1553619  64.1553619  64.1553619
# 14    NJ regionalization  12.5784284   9.4686434  14.4686434
# 7     NY        observed 121.7667089 121.7667089 121.7667089
# 15    NY regionalization  13.2358855   9.0000000  14.0000000
# 8     WI        observed  51.0834609  51.0834609  51.0834609
# 16    WI regionalization   8.1362743   6.1203238  11.1203238

overSupplyDf=ddply(plotDf,'state',function(x){c(obs=x[1,'mean'],needed=x[2,'mean'],overSupplyRatio=x[1,'mean']/x[2,'mean'])})
sortDf.yz(overSupplyDf,'overSupplyRatio',0)

# state        obs     needed overSupplyRatio
# 7    NY 121.766709 13.2358855       9.1997403
# 2    CA 209.114985 24.6368348       8.4878998
# 3    FL 146.614965 21.8245404       6.7178947
# 8    WI  51.083461  8.1362743       6.2784831
# 4    MA  44.205954  7.4523877       5.9317840
# 1    AZ  40.345716  7.5758798       5.3255486
# 6    NJ  64.155362 12.5784284       5.1004275
# 5    MD  38.538940 12.5753571       3.064639


tiff('C:/Dropbox/paper/frog/results/fig/03252015/aaaNHosp.tif')
ggplot(data=plotDf, aes(x=state, y=mean,fill=type))+xlab("state") +ylab("number of hospitals") +
  geom_bar(stat="identity",position=position_dodge())+geom_errorbar(data=subset(plotDf,type=='regionalization'),aes(x=c(1,1+1*gap,1+2*gap,1+3*gap,1+4*gap,1+5*gap,1+6*gap,1+7*gap)*1.2225,ymin=lb, ymax=ub),size=.3, width=0.1, position=position_dodge(.9))+theme(legend.position="top") +labs(fill="")+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+scale_fill_grey(start = 0.85, end = 0.45)+scale_y_continuous(breaks=seq(0,250,by=10), expand = waiver())
dev.off()


col.type.yz(numHospPlotDf)

numPatientPlotDf.aaa=getVolDf(sol.aaa.new)
plotDf=numPatientPlotDf.aaa
patientSizeDf=ddply(plotDf,'state',function(x){c(obs=x[1,'mean'],needed=x[2,'mean'],patientVolumeFrac=x[1,'mean']/x[2,'mean'])})
sortDf.yz(patientSizeDf,'patientVolumeFrac',0)
# state       obs     needed patientVolumeFrac
# 5    MD 24.325661  76.690559        0.31719238
# 6    NJ 18.452974  96.373991        0.19147255
# 1    AZ 30.999196 173.350658        0.17882364
# 4    MA 30.013175 187.100950        0.16041167
# 8    WI 19.984331 134.114228        0.14900978
# 3    FL 27.793142 187.520067        0.14821423
# 2    CA 17.696518 151.155244        0.11707512
# 7    NY 25.381289 237.078277        0.10705869


gap=0.817
plotDf=numPatientPlotDf.aaa
tiff('C:/Dropbox/paper/frog/results/fig/03252015/aaaNPatients.tif')
ggplot(data=plotDf, aes(x=state, y=mean,fill=type))+xlab("state") +ylab("number of patients") +
  geom_bar(stat="identity",position=position_dodge())+geom_errorbar(data=subset(plotDf,type=='regionalization'),aes(x=c(1,1+1*gap,1+2*gap,1+3*gap,1+4*gap,1+5*gap,1+6*gap,1+7*gap)*1.2225,ymin=lb, ymax=ub),size=.3, width=0.1, position=position_dodge(.9))+theme(legend.position="top") +labs(fill="")+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+scale_fill_grey(start = 0.85, end = 0.45)+scale_y_continuous(breaks=seq(0,500,by=50), expand = waiver())
dev.off()

rm(curvesObj.aaa, sol.aaa.new)


#now we use AOR as an example to get all the plot

#aor looks right
# plotAllStCurve(curvesObj.aor,c(0,0.15),'Z:/j_scrdata/frogLearn/aorFig_03042015.tif')
# plot(curvesObj.aor$all$meanProbVec)
# lines(curvesObj.aor$az$meanProbVec)
# lines(curvesObj.aor$ny$meanProbVec)
# lines(curvesObj.aor$fl$meanProbVec)
# lines(curvesObj.aor$md$meanProbVec)

#now we need a 95 percent confidence interval plot


#plot aor
# (load(file='Z:/j_scrdata/frogLearn/aorModelCurveFeb122015.RData'))
# tiffPath='C:/Dropbox/paper/frog/results/fig/02172015/aorExpCurveWithCi_021720142015.tif'
# #FitCurveOutputObj=aorFitCurveOutput
# FitCurveOutputObj=curvesObj.aor
# rm(aorFitCurveOutput)
# ci.quantile.vec=c(0.25,0.75)
# mort.by=5
# vol.by=5
# 
# outlist=searchFor90Reduction(cbind(seq(1,length(FitCurveOutputObj$all$meanProbVec)),FitCurveOutputObj$all$meanProbVec),0.9)
# cutOffVol=outlist$cutoffVol #90% mortality reduction
# cutOffVol.mort=roundToMultipleOf.yz(FitCurveOutputObj$all$meanProbVec[cutOffVol]*100,0.01,type='regular')
# max.plot.vol=roundToMultipleOf.yz(cutOffVol*1.1,5,type='ceiling')
# vol.breaks=c(1,seq(0,max.plot.vol,by=vol.by),cutOffVol) # x breaks
# min.mort=roundToMultipleOf.yz(outlist$min.mort*100,0.01,type='regular')
# max.mort=roundToMultipleOf.yz(outlist$max.mort*100,0.01,type='regular')
# max.plot.mort=roundToMultipleOf.yz(outlist$max.mort*100*1.25,5,type='ceiling')
# mort.breaks=c(seq(0,max.plot.mort,by=mort.by),min.mort,max.mort,cutOffVol.mort) #y breaks
# #ggtitle="Experience curve of aortic-valve replacement"
# ggtitle=""
# 
# ci.quantile.vec=c(0.25,0.75)
# 
# plotLearningCurve()
# 
# rm(FitCurveOutputObj)


(load(file='Z:/j_scrdata/frogLearn/aorResult_02262015.RData'))


tiffPath='C:/Dropbox/paper/frog/results/fig/04222015/aorExpCurveWithCi_04222015.tif'
tiffPathFullRange='C:/Dropbox/paper/frog/results/fig/04222015/aorExpCurveWithCi_fullrange_04222015.tif'

FitCurveOutputObj=curvesObj.aor$all
ci.quantile.vec=c(0.025,0.975)
mort.by=5
vol.by=50
outlist=searchFor90Reduction(cbind(seq(1,length(FitCurveOutputObj$meanProbVec)),FitCurveOutputObj$meanProbVec),0.9)
cutOffVol=unname(outlist$cutoffVol) #90% mortality reduction
outlist$cutoffVol
cutOffVol.mort=roundToMultipleOf.yz(FitCurveOutputObj$meanProbVec[cutOffVol]*100,0.01,type='regular')
max.plot.vol=roundToMultipleOf.yz(cutOffVol*1.5,5,type='ceiling') #only for not full range plotting
#vol.breaks=c(1,seq(0,max.plot.vol,by=vol.by),cutOffVol) # x breaks
vol.breaks=c(seq(1,max.plot.vol,by=vol.by),cutOffVol) # x breaks
minMortVol=which.min(FitCurveOutputObj$meanProbVec)
vol.breaks.fullRange=c(1,cutOffVol, minMortVol) # x breaks
min.mort=roundToMultipleOf.yz(outlist$min.mort*100,0.01,type='regular')
max.mort=roundToMultipleOf.yz(outlist$max.mort*100,0.01,type='regular')
max.plot.mort=roundToMultipleOf.yz(outlist$max.mort*100*1.25,5,type='ceiling')
mort.breaks=c(min.mort,max.mort,cutOffVol.mort) #y breaks
mort.breaks=unname(mort.breaks)
#ggtitle="Experience curve of aortic-valve replacement"
ggtitle=""
plotLearningCurve()
plotLearningCurveFullRange()



#I think by year is too complicated. Just do by state and surgery

numHospPlotDf.aor=getByStateNumHospDf(sol.aor.new)

plotDf=numHospPlotDf.aor
overSupplyDf=ddply(plotDf,'state',function(x){c(obs=x[1,'mean'],needed=x[2,'mean'],overSupplyRatio=x[1,'mean']/x[2,'mean'])})
sortDf.yz(overSupplyDf,'overSupplyRatio',0)
# state         obs     needed overSupplyRatio
# 2    CA 124.9110063 19.3978490       6.4394256
# 8    WI  29.7403030  5.9482066       4.9998773
# 7    NY  42.1218033  9.4002777       4.4809105
# 3    FL  73.9804368 17.2740814       4.2827422
# 6    NJ  18.1360508  5.7901317       3.1322346
# 4    MA  15.0000000  4.8558432       3.0890619
# 1    AZ  27.5906930 10.0713951       2.7395105
# 5    MD   9.6971946  4.6879545       2.0685343
gap=0.817

tiff('C:/Dropbox/paper/frog/results/fig/03252015/aorNHosp.tif')
ggplot(data=plotDf, aes(x=state, y=mean,fill=type))+xlab("state") +ylab("number of hospitals") +
  geom_bar(stat="identity",position=position_dodge())+geom_errorbar(data=subset(plotDf,type=='regionalization'),aes(x=c(1,1+1*gap,1+2*gap,1+3*gap,1+4*gap,1+5*gap,1+6*gap,1+7*gap)*1.2225,ymin=lb, ymax=ub),size=.3, width=0.1, position=position_dodge(.9))+theme(legend.position="top") +labs(fill="")+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+scale_fill_grey(start = 0.85, end = 0.45)+scale_y_continuous(breaks=seq(0,130,by=10), expand = waiver())
dev.off()


numPatientPlotDf.aor=getVolDf(sol.aor.new)

plotDf=numPatientPlotDf.aor
patientSizeDf=ddply(plotDf,'state',function(x){c(obs=x[1,'mean'],needed=x[2,'mean'],patientVolumeFrac=x[1,'mean']/x[2,'mean'])})
sortDf.yz(patientSizeDf,'patientVolumeFrac',0)
# state        obs    needed patientVolumeFrac
# 5    MD  91.512394 204.23076        0.44808331
# 1    AZ  44.771585 127.67914        0.35065702
# 6    NJ 103.395809 330.80191        0.31256109
# 4    MA 111.713840 369.39793        0.30242140
# 3    FL  59.636200 258.33661        0.23084688
# 7    NY 109.397950 503.10825        0.21744416
# 8    WI  47.395610 251.32154        0.18858555
# 2    CA  46.865426 303.79630        0.15426595


gap=0.817
plotDf=numPatientPlotDf.aor
tiff('C:/Dropbox/paper/frog/results/fig/03252015/aorNPatients.tif')
ggplot(data=plotDf, aes(x=state, y=mean,fill=type))+xlab("state") +ylab("number of patients") +
  geom_bar(stat="identity",position=position_dodge())+geom_errorbar(data=subset(plotDf,type=='regionalization'),aes(x=c(1,1+1*gap,1+2*gap,1+3*gap,1+4*gap,1+5*gap,1+6*gap,1+7*gap)*1.2225,ymin=lb, ymax=ub),size=.3, width=0.1, position=position_dodge(.9))+theme(legend.position="top") +labs(fill="")+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+scale_fill_grey(start = 0.85, end = 0.45)+scale_y_continuous(breaks=seq(0,600,by=50), expand = waiver())
dev.off()


rm(curvesObj.aor, sol.aor,cleanedDf.aor)

#cab
(load(file='Z:/j_scrdata/frogLearn/cabResult_02262015.RData'))

tiffPath='C:/Dropbox/paper/frog/results/fig/04222015/cabExpCurveWithCi_04222015.tif'
tiffPathFullRange='C:/Dropbox/paper/frog/results/fig/04222015/cabExpCurveWithCi_fullrange_04222015.tif'

FitCurveOutputObj=curvesObj.cab$all
ci.quantile.vec=c(0.025,0.975)
mort.by=5
vol.by=10
outlist=searchFor90Reduction(cbind(seq(1,length(FitCurveOutputObj$meanProbVec)),FitCurveOutputObj$meanProbVec),0.9)
cutOffVol=unname(outlist$cutoffVol) #90% mortality reduction
outlist$cutoffVol
cutOffVol.mort=roundToMultipleOf.yz(FitCurveOutputObj$meanProbVec[cutOffVol]*100,0.01,type='regular')
max.plot.vol=roundToMultipleOf.yz(cutOffVol*1.5,5,type='ceiling') #only for not full range plotting
#vol.breaks=c(1,seq(0,max.plot.vol,by=vol.by),cutOffVol) # x breaks
vol.breaks=c(seq(1,max.plot.vol,by=vol.by),cutOffVol) # x breaks
minMortVol=which.min(FitCurveOutputObj$meanProbVec)
vol.breaks.fullRange=c(1,cutOffVol, minMortVol) # x breaks
min.mort=roundToMultipleOf.yz(outlist$min.mort*100,0.01,type='regular')
max.mort=roundToMultipleOf.yz(outlist$max.mort*100,0.01,type='regular')
max.plot.mort=roundToMultipleOf.yz(outlist$max.mort*100*1.25,5,type='ceiling')
mort.breaks=c(min.mort,max.mort,cutOffVol.mort) #y breaks
mort.breaks=unname(mort.breaks)
#ggtitle="Experience curve of aortic-valve replacement"
ggtitle=""
plotLearningCurve()
plotLearningCurveFullRange()



#I think by year is too complicated. Just do by state and surgery

numHospPlotDf.cab=getByStateNumHospDf(sol.cab.new)
plotDf=numHospPlotDf.cab
overSupplyDf=ddply(plotDf,'state',function(x){c(obs=x[1,'mean'],needed=x[2,'mean'],overSupplyRatio=x[1,'mean']/x[2,'mean'])})
sortDf.yz(overSupplyDf,'overSupplyRatio',0)

gap=0.817
plotDf=numHospPlotDf.cab
tiff('C:/Dropbox/paper/frog/results/fig/03252015/cabNHosp.tif')
ggplot(data=plotDf, aes(x=state, y=mean,fill=type))+xlab("state") +ylab("number of hospitals") +
  geom_bar(stat="identity",position=position_dodge())+geom_errorbar(data=subset(plotDf,type=='regionalization'),aes(x=c(1,1+1*gap,1+2*gap,1+3*gap,1+4*gap,1+5*gap,1+6*gap,1+7*gap)*1.2225,ymin=lb, ymax=ub),size=.3, width=0.1, position=position_dodge(.9))+theme(legend.position="top") +labs(fill="")+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+scale_fill_grey(start = 0.85, end = 0.45)+scale_y_continuous(breaks=seq(0,130,by=10), expand = waiver())
dev.off()


numPatientPlotDf.cab=getVolDf(sol.cab.new)
plotDf=numPatientPlotDf.cab
patientSizeDf=ddply(plotDf,'state',function(x){c(obs=x[1,'mean'],needed=x[2,'mean'],patientVolumeFrac=x[1,'mean']/x[2,'mean'])})
sortDf.yz(patientSizeDf,'patientVolumeFrac',0)
# state       obs     needed patientVolumeFrac
# 5    MD 483.29922  805.83208        0.59975177
# 6    NJ 403.73369  798.74822        0.50545801
# 4    MA 393.16248  818.21847        0.48051040
# 1    AZ 234.69010  545.67877        0.43008837
# 7    NY 390.51860 1423.39099        0.27435792
# 8    WI 225.39309  973.80735        0.23145553
# 3    FL 301.94535 1498.74162        0.20146591
# 2    CA 191.42678 1084.94188        0.17643966



gap=0.817
plotDf=numPatientPlotDf.cab
tiff('C:/Dropbox/paper/frog/results/fig/03252015/cabNPatients.tif')
ggplot(data=plotDf, aes(x=state, y=mean,fill=type))+xlab("state") +ylab("number of patients") +
  geom_bar(stat="identity",position=position_dodge())+geom_errorbar(data=subset(plotDf,type=='regionalization'),aes(x=c(1,1+1*gap,1+2*gap,1+3*gap,1+4*gap,1+5*gap,1+6*gap,1+7*gap)*1.2225,ymin=lb, ymax=ub),size=.3, width=0.1, position=position_dodge(.9))+theme(legend.position="top") +labs(fill="")+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+scale_fill_grey(start = 0.85, end = 0.45)+scale_y_continuous(breaks=seq(0,2100,by=100), expand = waiver())
dev.off()

rm(curvesobj.cab, sol.cab, cleanedDf.cab)


#car
(load(file='Z:/j_scrdata/frogLearn/carResult_02262015.RData'))

tiffPath='C:/Dropbox/paper/frog/results/fig/04222015/carExpCurveWithCi_04222015.tif'
tiffPathFullRange='C:/Dropbox/paper/frog/results/fig/04222015/carExpCurveWithCi_fullrange_04222015.tif'

FitCurveOutputObj=curvesObj.car$all
ci.quantile.vec=c(0.025,0.975)
mort.by=5
vol.by=10
outlist=searchFor90Reduction(cbind(seq(1,length(FitCurveOutputObj$meanProbVec)),FitCurveOutputObj$meanProbVec),0.9)
cutOffVol=unname(outlist$cutoffVol) #90% mortality reduction
outlist$cutoffVol
cutOffVol.mort=roundToMultipleOf.yz(FitCurveOutputObj$meanProbVec[cutOffVol]*100,0.01,type='regular')
max.plot.vol=roundToMultipleOf.yz(cutOffVol*1.5,5,type='ceiling') #only for not full range plotting
#vol.breaks=c(1,seq(0,max.plot.vol,by=vol.by),cutOffVol) # x breaks
vol.breaks=c(seq(1,max.plot.vol,by=vol.by),cutOffVol) # x breaks
minMortVol=which.min(FitCurveOutputObj$meanProbVec)
vol.breaks.fullRange=c(1,cutOffVol, minMortVol) # x breaks
min.mort=roundToMultipleOf.yz(outlist$min.mort*100,0.01,type='regular')
max.mort=roundToMultipleOf.yz(outlist$max.mort*100,0.01,type='regular')
max.plot.mort=roundToMultipleOf.yz(outlist$max.mort*100*1.25,5,type='ceiling')
mort.breaks=c(min.mort,max.mort,cutOffVol.mort) #y breaks
mort.breaks=unname(mort.breaks)
#ggtitle="Experience curve of aortic-valve replacement"
ggtitle=""

plotLearningCurve()
plotLearningCurveFullRange()

#I think by year is too complicated. Just do by state and surgery

numHospPlotDf.car=getByStateNumHospDf(sol.car.new)

plotDf=numHospPlotDf.car
overSupplyDf=ddply(plotDf,'state',function(x){c(obs=x[1,'mean'],needed=x[2,'mean'],overSupplyRatio=x[1,'mean']/x[2,'mean'])})
sortDf.yz(overSupplyDf,'overSupplyRatio',0)
# state        obs    needed overSupplyRatio
# 7    NY 141.276575 18.882769       7.4817722
# 3    FL 164.498374 28.770523       5.7176011
# 2    CA 244.392027 46.977369       5.2023354
# 8    WI  55.711070 10.933839       5.0952891
# 6    NJ  73.249960 17.774537       4.1210616
# 4    MA  47.421617 12.912860       3.6724332
# 1    AZ  43.578803 13.552556       3.2155412
# 5    MD  41.488826 13.772617       3.0124142

gap=0.817
plotDf=numHospPlotDf.car
tiff('C:/Dropbox/paper/frog/results/fig/03252015/carNHosp.tif')
ggplot(data=plotDf, aes(x=state, y=mean,fill=type))+xlab("state") +ylab("number of hospitals") +
  geom_bar(stat="identity",position=position_dodge())+geom_errorbar(data=subset(plotDf,type=='regionalization'),aes(x=c(1,1+1*gap,1+2*gap,1+3*gap,1+4*gap,1+5*gap,1+6*gap,1+7*gap)*1.2225,ymin=lb, ymax=ub),size=.3, width=0.1, position=position_dodge(.9))+theme(legend.position="top") +labs(fill="")+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+scale_fill_grey(start = 0.85, end = 0.45)+scale_y_continuous(breaks=seq(0,250,by=50), expand = waiver())
dev.off()



numPatientPlotDf.car=getVolDf(sol.car.new)
plotDf=numPatientPlotDf.car
patientSizeDf=ddply(plotDf,'state',function(x){c(obs=x[1,'mean'],needed=x[2,'mean'],patientVolumeFrac=x[1,'mean']/x[2,'mean'])})
sortDf.yz(patientSizeDf,'patientVolumeFrac',0)

# state       obs    needed patientVolumeFrac
# 5    MD 47.828035 145.92488        0.32775791
# 1    AZ 51.731974 167.31863        0.30918238
# 4    MA 51.758552 191.34809        0.27049422
# 6    NJ 38.603508 159.74752        0.24165326
# 8    WI 43.184063 222.42029        0.19415523
# 2    CA 32.593978 169.96695        0.19176656
# 3    FL 61.180294 349.35896        0.17512158
# 7    NY 39.890339 298.80268        0.13350061


gap=0.817
plotDf=numPatientPlotDf.car
tiff('C:/Dropbox/paper/frog/results/fig/03252015/carNPatients.tif')
ggplot(data=plotDf, aes(x=state, y=mean,fill=type))+xlab("state") +ylab("number of patients") +
  geom_bar(stat="identity",position=position_dodge())+geom_errorbar(data=subset(plotDf,type=='regionalization'),aes(x=c(1,1+1*gap,1+2*gap,1+3*gap,1+4*gap,1+5*gap,1+6*gap,1+7*gap)*1.2225,ymin=lb, ymax=ub),size=.3, width=0.1, position=position_dodge(.9))+theme(legend.position="top") +labs(fill="")+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+scale_fill_grey(start = 0.85, end = 0.45)+scale_y_continuous(breaks=seq(0,400,by=50), expand = waiver())
dev.off()

rm(curvesobj.car, sol.car, cleanedDf.car)


#----CYS

(load(file='Z:/j_scrdata/frogLearn/cysResult_02262015.RData'))

tiffPath='C:/Dropbox/paper/frog/results/fig/04222015/cysExpCurveWithCi_04222015.tif'
tiffPathFullRange='C:/Dropbox/paper/frog/results/fig/04222015/cysExpCurveWithCi_fullrange_04222015.tif'
FitCurveOutputObj=curvesObj.cys$all


ci.quantile.vec=c(0.025,0.975)
mort.by=5
vol.by=10
outlist=searchFor90Reduction(cbind(seq(1,length(FitCurveOutputObj$meanProbVec)),FitCurveOutputObj$meanProbVec),0.9)
cutOffVol=unname(outlist$cutoffVol) #90% mortality reduction
outlist$cutoffVol
cutOffVol.mort=roundToMultipleOf.yz(FitCurveOutputObj$meanProbVec[cutOffVol]*100,0.01,type='regular')
max.plot.vol=roundToMultipleOf.yz(cutOffVol*1.5,5,type='ceiling') #only for not full range plotting
#vol.breaks=c(1,seq(0,max.plot.vol,by=vol.by),cutOffVol) # x breaks
vol.breaks=c(seq(1,max.plot.vol,by=vol.by),cutOffVol) # x breaks
minMortVol=which.min(FitCurveOutputObj$meanProbVec)
vol.breaks.fullRange=c(1,cutOffVol, minMortVol) # x breaks
min.mort=roundToMultipleOf.yz(outlist$min.mort*100,0.01,type='regular')
max.mort=roundToMultipleOf.yz(outlist$max.mort*100,0.01,type='regular')
max.plot.mort=roundToMultipleOf.yz(outlist$max.mort*100*1.25,5,type='ceiling')
mort.breaks=c(min.mort,max.mort,cutOffVol.mort) #y breaks
mort.breaks=unname(mort.breaks)
#ggtitle="Experience curve of aortic-valve replacement"
ggtitle=""

plotLearningCurve()
plotLearningCurveFullRange()


#I think by year is too complicated. Just do by state and surgery

numHospPlotDf.cys=getByStateNumHospDf(sol.cys.new)

plotDf=numHospPlotDf.cys
overSupplyDf=ddply(plotDf,'state',function(x){c(obs=x[1,'mean'],needed=x[2,'mean'],overSupplyRatio=x[1,'mean']/x[2,'mean'])})
sortDf.yz(overSupplyDf,'overSupplyRatio',0)

# state        obs    needed overSupplyRatio
# 2    CA 139.904124 6.0847313      22.9926545
# 7    NY  78.731187 4.2617343      18.4739780
# 3    FL  84.917587 6.7225048      12.6318373
# 5    MD  24.072924 2.4902527       9.6668600
# 4    MA  33.278112 3.4993059       9.5099180
# 8    WI  37.721382 4.0000000       9.4303456
# 6    NJ  40.953562 4.4847328       9.1317730
# 1    AZ  27.836735 3.3807580       8.2338737

gap=0.817
plotDf=numHospPlotDf.cys
tiff('C:/Dropbox/paper/frog/results/fig/03252015/cysNHosp.tif')
ggplot(data=plotDf, aes(x=state, y=mean,fill=type))+xlab("state") +ylab("number of hospitals") +
  geom_bar(stat="identity",position=position_dodge())+geom_errorbar(data=subset(plotDf,type=='regionalization'),aes(x=c(1,1+1*gap,1+2*gap,1+3*gap,1+4*gap,1+5*gap,1+6*gap,1+7*gap)*1.2225,ymin=lb, ymax=ub),size=.3, width=0.1, position=position_dodge(.9))+theme(legend.position="top") +labs(fill="")+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+scale_fill_grey(start = 0.85, end = 0.45)+scale_y_continuous(breaks=seq(0,160,by=10), expand = waiver())
dev.off()



numPatientPlotDf.cys=getVolDf(sol.cys.new)

plotDf=numPatientPlotDf.cys
patientSizeDf=ddply(plotDf,'state',function(x){c(obs=x[1,'mean'],needed=x[2,'mean'],patientVolumeFrac=x[1,'mean']/x[2,'mean'])})
sortDf.yz(patientSizeDf,'patientVolumeFrac',0)
# state       obs     needed patientVolumeFrac
# 1    AZ 7.0307770  57.500923       0.122272419
# 6    NJ 4.3427827  39.465585       0.110039739
# 4    MA 7.5600746  69.906255       0.108145896
# 8    WI 5.5089207  51.595572       0.106771191
# 5    MD 6.6233771  64.064260       0.103386460
# 3    FL 6.9573776  87.312594       0.079683552
# 7    NY 8.2922547 151.938561       0.054576367
# 2    CA 6.3282573 144.041095       0.043933693


gap=0.817
plotDf=numPatientPlotDf.cys
tiff('C:/Dropbox/paper/frog/results/fig/03252015/cysNPatients.tif')
ggplot(data=plotDf, aes(x=state, y=mean,fill=type))+xlab("state") +ylab("number of patients") +
  geom_bar(stat="identity",position=position_dodge())+geom_errorbar(data=subset(plotDf,type=='regionalization'),aes(x=c(1,1+1*gap,1+2*gap,1+3*gap,1+4*gap,1+5*gap,1+6*gap,1+7*gap)*1.2225,ymin=lb, ymax=ub),size=.3, width=0.1, position=position_dodge(.9))+theme(legend.position="top") +labs(fill="")+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+scale_fill_grey(start = 0.85, end = 0.45)+scale_y_continuous(breaks=seq(0,500,by=10), expand = waiver())
dev.off()

rm(curvesobj.cys, sol.cys, cleanedDf.cys)



#----eso

(load(file='Z:/j_scrdata/frogLearn/esoResult_02262015.RData'))


tiffPath='C:/Dropbox/paper/frog/results/fig/04222015/esoExpCurveWithCi_04222015.tif'
tiffPathFullRange='C:/Dropbox/paper/frog/results/fig/04222015/esoExpCurveWithCi_fullrange_04222015.tif'
FitCurveOutputObj=curvesObj.eso$all


ci.quantile.vec=c(0.025,0.975)
mort.by=5
vol.by=10
outlist=searchFor90Reduction(cbind(seq(1,length(FitCurveOutputObj$meanProbVec)),FitCurveOutputObj$meanProbVec),0.9)
cutOffVol=unname(outlist$cutoffVol) #90% mortality reduction
outlist$cutoffVol
cutOffVol.mort=roundToMultipleOf.yz(FitCurveOutputObj$meanProbVec[cutOffVol]*100,0.01,type='regular')
max.plot.vol=roundToMultipleOf.yz(cutOffVol*1.5,5,type='ceiling') #only for not full range plotting
#vol.breaks=c(1,seq(0,max.plot.vol,by=vol.by),cutOffVol) # x breaks
vol.breaks=c(seq(1,max.plot.vol,by=vol.by),cutOffVol) # x breaks
minMortVol=which.min(FitCurveOutputObj$meanProbVec)
vol.breaks.fullRange=c(1,cutOffVol, minMortVol) # x breaks
min.mort=roundToMultipleOf.yz(outlist$min.mort*100,0.01,type='regular')
max.mort=roundToMultipleOf.yz(outlist$max.mort*100,0.01,type='regular')
max.plot.mort=roundToMultipleOf.yz(outlist$max.mort*100*1.25,5,type='ceiling')
mort.breaks=c(min.mort,max.mort,cutOffVol.mort) #y breaks
mort.breaks=unname(mort.breaks)
#ggtitle="Experience curve of aortic-valve replacement"
ggtitle=""

plotLearningCurve()
plotLearningCurveFullRange()


#I think by year is too complicated. Just do by state and surgery

numHospPlotDf.eso=getByStateNumHospDf(sol.eso.new)


plotDf=numHospPlotDf.eso
overSupplyDf=ddply(plotDf,'state',function(x){c(obs=x[1,'mean'],needed=x[2,'mean'],overSupplyRatio=x[1,'mean']/x[2,'mean'])})
sortDf.yz(overSupplyDf,'overSupplyRatio',0)
# state        obs    needed overSupplyRatio
# 2    CA 163.512178 8.7906281      18.6007388
# 7    NY  86.191544 5.2823451      16.3169090
# 3    FL  91.610016 6.5776116      13.9275502
# 4    MA  27.776390 3.0000000       9.2587968
# 6    NJ  37.500000 4.7450980       7.9028926
# 8    WI  33.744131 4.6813380       7.2082236
# 5    MD  24.784287 3.7939352       6.5326067
# 1    AZ  26.922656 4.5234375       5.9518135

gap=0.817
plotDf=numHospPlotDf.eso
tiff('C:/Dropbox/paper/frog/results/fig/03252015/esoNHosp.tif')
ggplot(data=plotDf, aes(x=state, y=mean,fill=type))+xlab("state") +ylab("number of hospitals") +
  geom_bar(stat="identity",position=position_dodge())+geom_errorbar(data=subset(plotDf,type=='regionalization'),aes(x=c(1,1+1*gap,1+2*gap,1+3*gap,1+4*gap,1+5*gap,1+6*gap,1+7*gap)*1.2225,ymin=lb, ymax=ub),size=.3, width=0.1, position=position_dodge(.9))+theme(legend.position="top") +labs(fill="")+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+scale_fill_grey(start = 0.85, end = 0.45)+scale_y_continuous(breaks=seq(0,200,by=10), expand = waiver())
dev.off()



numPatientPlotDf.eso=getVolDf(sol.eso.new)
plotDf=numPatientPlotDf.eso

patientSizeDf=ddply(plotDf,'state',function(x){c(obs=x[1,'mean'],needed=x[2,'mean'],patientVolumeFrac=x[1,'mean']/x[2,'mean'])})
sortDf.yz(patientSizeDf,'patientVolumeFrac',0)

# state        obs     needed patientVolumeFrac
# 1    AZ  5.4007582  32.112995       0.168179835
# 5    MD  6.6195163  43.108086       0.153556255
# 8    WI  5.6943112  40.808069       0.139538854
# 6    NJ  4.0987257  31.688122       0.129345805
# 4    MA 10.9003796  98.232690       0.110964890
# 3    FL  5.3956231  74.945615       0.071993846
# 7    NY  7.6207444 124.000977       0.061457132
# 2    CA  4.7961828  88.958304       0.053914954

gap=0.817
plotDf=numPatientPlotDf.eso
tiff('C:/Dropbox/paper/frog/results/fig/03252015/esoNPatients.tif')
ggplot(data=plotDf, aes(x=state, y=mean,fill=type))+xlab("state") +ylab("number of patients") +
  geom_bar(stat="identity",position=position_dodge())+geom_errorbar(data=subset(plotDf,type=='regionalization'),aes(x=c(1,1+1*gap,1+2*gap,1+3*gap,1+4*gap,1+5*gap,1+6*gap,1+7*gap)*1.2225,ymin=lb, ymax=ub),size=.3, width=0.1, position=position_dodge(.9))+theme(legend.position="top") +labs(fill="")+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+scale_fill_grey(start = 0.85, end = 0.45)+scale_y_continuous(breaks=seq(0,200,by=10), expand = waiver())
dev.off()

rm(curvesObj.eso, sol.eso, cleanedDf.eso)

#lun
(load(file='Z:/j_scrdata/frogLearn/lunResult_02262015.RData'))

tiffPath='C:/Dropbox/paper/frog/results/fig/04222015/lunExpCurveWithCi_04222015.tif'
tiffPathFullRange='C:/Dropbox/paper/frog/results/fig/04222015/lunExpCurveWithCi_fullrange_04222015.tif'
FitCurveOutputObj=curvesObj.lun$all


ci.quantile.vec=c(0.025,0.975)
mort.by=5
vol.by=10
outlist=searchFor90Reduction(cbind(seq(1,length(FitCurveOutputObj$meanProbVec)),FitCurveOutputObj$meanProbVec),0.9)
cutOffVol=unname(outlist$cutoffVol) #90% mortality reduction
outlist$cutoffVol
cutOffVol.mort=roundToMultipleOf.yz(FitCurveOutputObj$meanProbVec[cutOffVol]*100,0.01,type='regular')
max.plot.vol=roundToMultipleOf.yz(cutOffVol*1.5,5,type='ceiling') #only for not full range plotting
#vol.breaks=c(1,seq(0,max.plot.vol,by=vol.by),cutOffVol) # x breaks
vol.breaks=c(seq(1,max.plot.vol,by=vol.by),cutOffVol) # x breaks
minMortVol=which.min(FitCurveOutputObj$meanProbVec)
vol.breaks.fullRange=c(1,cutOffVol, minMortVol) # x breaks
min.mort=roundToMultipleOf.yz(outlist$min.mort*100,0.01,type='regular')
max.mort=roundToMultipleOf.yz(outlist$max.mort*100,0.01,type='regular')
max.plot.mort=roundToMultipleOf.yz(outlist$max.mort*100*1.25,5,type='ceiling')
mort.breaks=c(min.mort,max.mort,cutOffVol.mort) #y breaks
mort.breaks=unname(mort.breaks)
#ggtitle="Experience curve of aortic-valve replacement"
ggtitle=""

plotLearningCurve()
plotLearningCurveFullRange()


#I think by year is too complicated. Just do by state and surgery

numHospPlotDf.lun=getByStateNumHospDf(sol.lun.new)

plotDf=numHospPlotDf.lun
overSupplyDf=ddply(plotDf,'state',function(x){c(obs=x[1,'mean'],needed=x[2,'mean'],overSupplyRatio=x[1,'mean']/x[2,'mean'])})
sortDf.yz(overSupplyDf,'overSupplyRatio',0)

# state        obs     needed overSupplyRatio
# 7    NY 133.553378  8.4734109      15.7614660
# 2    CA 220.693821 15.7846990      13.9815033
# 3    FL 146.255653 11.2193426      13.0360269
# 4    MA  46.174233  4.8489242       9.5225727
# 6    NJ  65.729566  7.4958783       8.7687611
# 8    WI  45.102883  5.9553517       7.5735045
# 5    MD  36.359432  5.9031319       6.1593461
# 1    AZ  39.163963  6.8161625       5.7457496


gap=0.817
plotDf=numHospPlotDf.lun
tiff('C:/Dropbox/paper/frog/results/fig/03252015/lunNHosp.tif')
ggplot(data=plotDf, aes(x=state, y=mean,fill=type))+xlab("state") +ylab("number of hospitals") +
  geom_bar(stat="identity",position=position_dodge())+geom_errorbar(data=subset(plotDf,type=='regionalization'),aes(x=c(1,1+1*gap,1+2*gap,1+3*gap,1+4*gap,1+5*gap,1+6*gap,1+7*gap)*1.2225,ymin=lb, ymax=ub),size=.3, width=0.1, position=position_dodge(.9))+theme(legend.position="top") +labs(fill="")+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+scale_fill_grey(start = 0.85, end = 0.45)+scale_y_continuous(breaks=seq(0,250,by=10), expand = waiver())
dev.off()



numPatientPlotDf.lun=getVolDf(sol.lun.new)

plotDf=numPatientPlotDf.lun

patientSizeDf=ddply(plotDf,'state',function(x){c(obs=x[1,'mean'],needed=x[2,'mean'],patientVolumeFrac=x[1,'mean']/x[2,'mean'])})
sortDf.yz(patientSizeDf,'patientVolumeFrac',0)

# state       obs    needed patientVolumeFrac
# 1    AZ 19.627581 112.38205       0.174650492
# 5    MD 25.055515 154.36696       0.162311389
# 8    WI 19.340901 146.59187       0.131937065
# 6    NJ 18.110192 157.82058       0.114751777
# 4    MA 29.472619 280.39426       0.105111349
# 3    FL 22.948582 304.92765       0.075259105
# 2    CA 16.026709 222.98636       0.071873049
# 7    NY 24.009689 389.33352       0.061668692


gap=0.817
plotDf=numPatientPlotDf.lun
tiff('C:/Dropbox/paper/frog/results/fig/03252015/lunNPatients.tif')
ggplot(data=plotDf, aes(x=state, y=mean,fill=type))+xlab("state") +ylab("number of patients") +
  geom_bar(stat="identity",position=position_dodge())+geom_errorbar(data=subset(plotDf,type=='regionalization'),aes(x=c(1,1+1*gap,1+2*gap,1+3*gap,1+4*gap,1+5*gap,1+6*gap,1+7*gap)*1.2225,ymin=lb, ymax=ub),size=.3, width=0.1, position=position_dodge(.9))+theme(legend.position="top") +labs(fill="")+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+scale_fill_grey(start = 0.85, end = 0.45)+scale_y_continuous(breaks=seq(0,500,by=50), expand = waiver())
dev.off()

rm(curvesobj.lun, sol.lun, cleanedDf.lun)


#---------pan

(load(file='Z:/j_scrdata/frogLearn/panResult_02262015.RData'))

tiffPath='C:/Dropbox/paper/frog/results/fig/04222015/panExpCurveWithCi_04222015.tif'
tiffPathFullRange='C:/Dropbox/paper/frog/results/fig/04222015/panExpCurveWithCi_fullrange_04222015.tif'
FitCurveOutputObj=curvesObj.pan$all


ci.quantile.vec=c(0.025,0.975)
mort.by=5
vol.by=10
outlist=searchFor90Reduction(cbind(seq(1,length(FitCurveOutputObj$meanProbVec)),FitCurveOutputObj$meanProbVec),0.9)
cutOffVol=unname(outlist$cutoffVol) #90% mortality reduction
outlist$cutoffVol
cutOffVol.mort=roundToMultipleOf.yz(FitCurveOutputObj$meanProbVec[cutOffVol]*100,0.01,type='regular')
max.plot.vol=roundToMultipleOf.yz(cutOffVol*1.5,5,type='ceiling') #only for not full range plotting
#vol.breaks=c(1,seq(0,max.plot.vol,by=vol.by),cutOffVol) # x breaks
vol.breaks=c(seq(1,max.plot.vol,by=vol.by),cutOffVol) # x breaks
minMortVol=which.min(FitCurveOutputObj$meanProbVec)
vol.breaks.fullRange=c(1,cutOffVol, minMortVol) # x breaks
min.mort=roundToMultipleOf.yz(outlist$min.mort*100,0.01,type='regular')
max.mort=roundToMultipleOf.yz(outlist$max.mort*100,0.01,type='regular')
max.plot.mort=roundToMultipleOf.yz(outlist$max.mort*100*1.25,5,type='ceiling')
mort.breaks=c(min.mort,max.mort,cutOffVol.mort) #y breaks
mort.breaks=unname(mort.breaks)
#ggtitle="Experience curve of aortic-valve replacement"
ggtitle=""

plotLearningCurve()
plotLearningCurveFullRange()



#I think by year is too complicated. Just do by state and surgery

numHospPlotDf.pan=getByStateNumHospDf(sol.pan.new)
plotDf=numHospPlotDf.pan
overSupplyDf=ddply(plotDf,'state',function(x){c(obs=x[1,'mean'],needed=x[2,'mean'],overSupplyRatio=x[1,'mean']/x[2,'mean'])})
sortDf.yz(overSupplyDf,'overSupplyRatio',0)
# state        obs     needed overSupplyRatio
# 7    NY  64.466757  4.2991444      14.9952525
# 3    FL  74.765570  5.8321582      12.8195373
# 2    CA 133.657323 11.5595988      11.5624534
# 1    AZ  22.397569  2.4230324       9.2436112
# 8    WI  27.655213  3.3759814       8.1917551
# 5    MD  17.373305  2.2552404       7.7035265
# 6    NJ  26.684585  4.1036269       6.5026831
# 4    MA  16.010906  2.8036856       5.7106640

gap=0.817
plotDf=numHospPlotDf.pan
tiff('C:/Dropbox/paper/frog/results/fig/03252015/panNHosp.tif')
ggplot(data=plotDf, aes(x=state, y=mean,fill=type))+xlab("state") +ylab("number of hospitals") +
  geom_bar(stat="identity",position=position_dodge())+geom_errorbar(data=subset(plotDf,type=='regionalization'),aes(x=c(1,1+1*gap,1+2*gap,1+3*gap,1+4*gap,1+5*gap,1+6*gap,1+7*gap)*1.2225,ymin=lb, ymax=ub),size=.3, width=0.1, position=position_dodge(.9))+theme(legend.position="top") +labs(fill="")+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+scale_fill_grey(start = 0.85, end = 0.45)+scale_y_continuous(breaks=seq(0,130,by=10), expand = waiver())
dev.off()



numPatientPlotDf.pan=getVolDf(sol.pan.new)

plotDf=numPatientPlotDf.pan

patientSizeDf=ddply(plotDf,'state',function(x){c(obs=x[1,'mean'],needed=x[2,'mean'],patientVolumeFrac=x[1,'mean']/x[2,'mean'])})
sortDf.yz(patientSizeDf,'patientVolumeFrac',0)

# state        obs     needed patientVolumeFrac
# 4    MA 19.5579413 107.832142       0.181373947
# 6    NJ  6.7924876  42.988234       0.158008063
# 5    MD 21.1765332 164.048294       0.129087189
# 8    WI  7.1909109  58.920430       0.122044440
# 1    AZ  9.0264700  83.397473       0.108234335
# 2    CA  6.6831589  76.289617       0.087602470
# 3    FL  7.7521037  98.437635       0.078751422
# 7    NY 10.7963380 156.593893       0.068944821

gap=0.817
plotDf=numPatientPlotDf.pan
tiff('C:/Dropbox/paper/frog/results/fig/03252015/panNPatients.tif')
ggplot(data=plotDf, aes(x=state, y=mean,fill=type))+xlab("state") +ylab("number of patients") +
  geom_bar(stat="identity",position=position_dodge())+geom_errorbar(data=subset(plotDf,type=='regionalization'),aes(x=c(1,1+1*gap,1+2*gap,1+3*gap,1+4*gap,1+5*gap,1+6*gap,1+7*gap)*1.2225,ymin=lb, ymax=ub),size=.3, width=0.1, position=position_dodge(.9))+theme(legend.position="top") +labs(fill="")+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+scale_fill_grey(start = 0.85, end = 0.45)+scale_y_continuous(breaks=seq(0,200,by=10), expand = waiver())
dev.off()



#now I need to get the learning curve with confidene interval then that is it

plot(curvesObj.pan$all$meanProbVec)
dev.off()

names(sol.pan.new)


#plot death avoided by state and surgery and mortaltiy reduction (need 95% intervals)

sol.df=sol.pan.new

sol.df=sol.cab.new
genDeathDf=function(sol.df){
  death.obs=ddply(sol.df,'state',function(x){c(mean=sum(x[,"obs.deaths"]))})
  death.obs[,'lb']=death.obs[,'mean']
  death.obs[,'ub']=death.obs[,'mean']
  death.obs[,'type']='observation'
  
  # pan.death.current = join(join(join(ddply(sol.pan.new,c('state'),function(x){c(death.current.lb=sum(x[,'mean.deaths.current.lb']))})
  #                           ,ddply(sol.pan.new,c('state'),function(x){c(death.current.mean=sum(x[,'mean.deaths.current']))}))
  #                      ,ddply(sol.pan.new,c('state'),function(x){c(death.current.ub=sum(x[,'mean.deaths.current.ub']))})
  # ),pan.death.obs)
  
  death.opt = join(join(ddply(sol.df,c('state'),function(x){c(lb=sum(x[,'mean.deaths.opt.lb']))})
                        ,ddply(sol.df,c('state'),function(x){c(mean=sum(x[,'mean.deaths.opt']))}))
                   ,ddply(sol.df,c('state'),function(x){c(ub=sum(x[,'mean.deaths.opt.ub']))})
  )
  death.opt[,'type']='regionalization'
  
  death.obs.opt=rbind(death.obs, death.opt)
  return(death.obs.opt)
}



#why MD observed death is lower than the predicted....this is troubling.
gap=0.817
plotDf=genDeathDf(sol.pan.new)
tiff('C:/Dropbox/paper/frog/results/fig/03252015/panDeaths.tif')
ggplot(data=plotDf, aes(x=state, y=mean,fill=type))+xlab("state") +ylab("number of deaths") +
  geom_bar(stat="identity",position=position_dodge())+geom_errorbar(data=subset(plotDf,type=='regionalization'),aes(x=c(1,1+1*gap,1+2*gap,1+3*gap,1+4*gap,1+5*gap,1+6*gap,1+7*gap)*1.2225,ymin=lb, ymax=ub),size=.3, width=0.1, position=position_dodge(.9))+theme(legend.position="top") +labs(fill="")+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+scale_fill_grey(start = 0.85, end = 0.45)+scale_y_continuous(breaks=seq(0,500,by=50), expand = waiver())
dev.off()



gap=0.817
plotDf=genDeathDf(sol.cys.new)
tiff('C:/Dropbox/paper/frog/results/fig/03252015/cysDeaths.tif')
ggplot(data=plotDf, aes(x=state, y=mean,fill=type))+xlab("state") +ylab("number of deaths") +
  geom_bar(stat="identity",position=position_dodge())+geom_errorbar(data=subset(plotDf,type=='regionalization'),aes(x=c(1,1+1*gap,1+2*gap,1+3*gap,1+4*gap,1+5*gap,1+6*gap,1+7*gap)*1.2225,ymin=lb, ymax=ub),size=.3, width=0.1, position=position_dodge(.9))+theme(legend.position="top") +labs(fill="")+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+scale_fill_grey(start = 0.85, end = 0.45)+scale_y_continuous(breaks=seq(0,500,by=50), expand = waiver())
dev.off()



gap=0.817
plotDf=genDeathDf(sol.aaa.new)
tiff('C:/Dropbox/paper/frog/results/fig/03252015/aaaDeaths.tif')
ggplot(data=plotDf, aes(x=state, y=mean,fill=type))+xlab("state") +ylab("number of deaths") +
  geom_bar(stat="identity",position=position_dodge())+geom_errorbar(data=subset(plotDf,type=='regionalization'),aes(x=c(1,1+1*gap,1+2*gap,1+3*gap,1+4*gap,1+5*gap,1+6*gap,1+7*gap)*1.2225,ymin=lb, ymax=ub),size=.3, width=0.1, position=position_dodge(.9))+theme(legend.position="top") +labs(fill="")+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+scale_fill_grey(start = 0.85, end = 0.45)+scale_y_continuous(breaks=seq(0,500,by=50), expand = waiver())
dev.off()

gap=0.817
plotDf=genDeathDf(sol.aor.new)
tiff('C:/Dropbox/paper/frog/results/fig/03252015/aorDeaths.tif')
ggplot(data=plotDf, aes(x=state, y=mean,fill=type))+xlab("state") +ylab("number of deaths") +
  geom_bar(stat="identity",position=position_dodge())+geom_errorbar(data=subset(plotDf,type=='regionalization'),aes(x=c(1,1+1*gap,1+2*gap,1+3*gap,1+4*gap,1+5*gap,1+6*gap,1+7*gap)*1.2225,ymin=lb, ymax=ub),size=.3, width=0.1, position=position_dodge(.9))+theme(legend.position="top") +labs(fill="")+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+scale_fill_grey(start = 0.85, end = 0.45)+scale_y_continuous(breaks=seq(0,2500,by=100), expand = waiver())
dev.off()


#this is very troubling!!!
gap=0.817
plotDf=genDeathDf(sol.cab.new)
tiff('C:/Dropbox/paper/frog/results/fig/03252015/cabDeaths.tif')
ggplot(data=plotDf, aes(x=state, y=mean,fill=type))+xlab("state") +ylab("number of deaths") +
  geom_bar(stat="identity",position=position_dodge())+geom_errorbar(data=subset(plotDf,type=='regionalization'),aes(x=c(1,1+1*gap,1+2*gap,1+3*gap,1+4*gap,1+5*gap,1+6*gap,1+7*gap)*1.2225,ymin=lb, ymax=ub),size=.3, width=0.1, position=position_dodge(.9))+theme(legend.position="top") +labs(fill="")+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+scale_fill_grey(start = 0.85, end = 0.45)+scale_y_continuous(breaks=seq(0,10000,by=500), expand = waiver())
dev.off()


gap=0.817
plotDf=genDeathDf(sol.eso.new)
tiff('C:/Dropbox/paper/frog/results/fig/03252015/esoDeaths.tif')
ggplot(data=plotDf, aes(x=state, y=mean,fill=type))+xlab("state") +ylab("number of deaths") +
  geom_bar(stat="identity",position=position_dodge())+geom_errorbar(data=subset(plotDf,type=='regionalization'),aes(x=c(1,1+1*gap,1+2*gap,1+3*gap,1+4*gap,1+5*gap,1+6*gap,1+7*gap)*1.2225,ymin=lb, ymax=ub),size=.3, width=0.1, position=position_dodge(.9))+theme(legend.position="top") +labs(fill="")+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+scale_fill_grey(start = 0.85, end = 0.45)+scale_y_continuous(breaks=seq(0,400,by=50), expand = waiver())
dev.off()


gap=0.817
plotDf=genDeathDf(sol.lun.new)
tiff('C:/Dropbox/paper/frog/results/fig/03252015/lunDeaths.tif')
ggplot(data=plotDf, aes(x=state, y=mean,fill=type))+xlab("state") +ylab("number of deaths") +
  geom_bar(stat="identity",position=position_dodge())+geom_errorbar(data=subset(plotDf,type=='regionalization'),aes(x=c(1,1+1*gap,1+2*gap,1+3*gap,1+4*gap,1+5*gap,1+6*gap,1+7*gap)*1.2225,ymin=lb, ymax=ub),size=.3, width=0.1, position=position_dodge(.9))+theme(legend.position="top") +labs(fill="")+theme(axis.line = element_line(colour = "black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),panel.background = element_blank())+scale_fill_grey(start = 0.85, end = 0.45)+scale_y_continuous(breaks=seq(0,1200,by=100), expand = waiver())
dev.off()









