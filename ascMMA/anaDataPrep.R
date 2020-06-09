#the analysis is the HRR level.


#get POS file mean
posVarsHrr=xpt2r.yz("Z:/j_scrdata/ascMMA", "posVars_2006")
posVarsDf2006 = ddply(posVarsHrr,'hrrnum',function(x){c(n.hosp=sum(x[,'hospind']),n.oprooms=sum(x[,'numoprooms']), n.fasc=sum(x[,'fascind']),n.hospasc=sum(x[,'hospascind']))})

posVarsHrr=xpt2r.yz("Z:/j_scrdata/ascMMA", "posVars_2007")
posVarsDf2007 = ddply(posVarsHrr,'hrrnum',function(x){c(n.hosp=sum(x[,'hospind']),n.oprooms=sum(x[,'numoprooms']), n.fasc=sum(x[,'fascind']),n.hospasc=sum(x[,'hospascind']))})

posVarsHrr=xpt2r.yz("Z:/j_scrdata/ascMMA", "posVars_2008")
posVarsDf2008 = ddply(posVarsHrr,'hrrnum',function(x){c(n.hosp=sum(x[,'hospind']),n.oprooms=sum(x[,'numoprooms']), n.fasc=sum(x[,'fascind']),n.hospasc=sum(x[,'hospascind']))})

posVarsHrr=xpt2r.yz("Z:/j_scrdata/ascMMA", "posVars_2009")
posVarsDf2009 = ddply(posVarsHrr,'hrrnum',function(x){c(n.hosp=sum(x[,'hospind']),n.oprooms=sum(x[,'numoprooms']), n.fasc=sum(x[,'fascind']),n.hospasc=sum(x[,'hospascind']))})

posVarsHrr=xpt2r.yz("Z:/j_scrdata/ascMMA", "posVars_2010")
posVarsDf2010 = ddply(posVarsHrr,'hrrnum',function(x){c(n.hosp=sum(x[,'hospind']),n.oprooms=sum(x[,'numoprooms']), n.fasc=sum(x[,'fascind']),n.hospasc=sum(x[,'hospascind']))})

posVarsDf2011=posVarsDf2010
posVarsDf.20062011 = do.call(rbind, list(data.frame(posVarsDf2006,year=2006)
                                         , data.frame(posVarsDf2007,year=2007)
                                         , data.frame(posVarsDf2008,year=2008)
                                         , data.frame(posVarsDf2009,year=2009)
                                         , data.frame(posVarsDf2010,year=2010)
                                         , data.frame(posVarsDf2011,year=2011)))

posVarsDf.mean= ddply(posVarsDf.20062010, 'hrrnum', function(x){c(n.hosp=mean(x[,'n.hosp']),n.oprooms=mean(x[,'n.oprooms']), n.fasc=mean(x[,'n.fasc']),n.hospasc=mean(x[,'n.hospasc']))})



#next we will get denominator file mean
hist(posVarsDf.mean[,'n.hosp'],30, main='number of hospitals')
box()
hist(posVarsDf.mean[,'n.oprooms'],30, main='number of oprooms')
box()
hist(posVarsDf.mean[,'n.fasc'],30, main='number of fasc')
box()
hist(posVarsDf.mean[,'n.hospasc'],30, main='number of hospasc')
box()

#next, get denominator file var

asciiDataVn.yz("Z:/j_scrdata/ascMMA/denVarsHrr_2006.csv")
#den means denominator file

denAggByHrr(denDataPath)


denAggByHrr=function(denDataPath,year){
indf = readTableCols.yz(
  denDataPath
  #"Z:/j_scrdata/ascMMA/denVarsHrr_2006.csv"
                 , c("AGE", "RACE","SEX","hrrnum")
                 )
# asciiDataVn.yz("Z:/j_scrdata/ascMMA/denVarsHrr_2006.csv")
names(indf)=tolower(names(indf))
indf[,'age']=as.numeric(indf[,'age'])

okRows=rowsWithInterestValues.yz(indf
                                 ,c('sex')
                                 #there is k vns then the interestValuesList has a length of k
                                 , list(c(0))
)$rowsWithoutInterestValues

aggHrrList = aggregateVarsBy.yz( indf[okRows,]
                               ,'hrrnum' #by variable
                               ,c("race", "sex")
                               ,'percent'
                               #either 'freq' or 'percent'
                               ,'age' #these are numerical vars, will generate mean.varname, sd.varname, '.' is the vnSep
                               #meanSdVars can be empty
                               ,
                               #these are numerical vars, will generate median.varname
                               #medianVars can be empty
                               ,tabNA=TRUE
                               ,na.rm.quantile=TRUE
)

denVarsDf = data.frame(join(aggHrrList[[1]],aggHrrList[[2]],by='hrrnum'),year=year)
return(denVarsDf)
}

indf = readTableCols.yz(
  "Z:/j_scrdata/ascMMA/denVarsHrr_2010.csv"
  #"Z:/j_scrdata/ascMMA/denVarsHrr_2006.csv"
  , c("AGE", "RACE","SEX","hrrnum")
)
count(indf,'SEX')

denHrrLevel.2006 = denAggByHrr("Z:/j_scrdata/ascMMA/denVarsHrr_2006.csv",2006)
denHrrLevel.2007 = denAggByHrr("Z:/j_scrdata/ascMMA/denVarsHrr_2007.csv",2007)
denHrrLevel.2008 = denAggByHrr("Z:/j_scrdata/ascMMA/denVarsHrr_2008.csv",2008)
denHrrLevel.2009 = denAggByHrr("Z:/j_scrdata/ascMMA/denVarsHrr_2009.csv",2009)
denHrrLevel.2010 = denAggByHrr("Z:/j_scrdata/ascMMA/denVarsHrr_2010.csv",2010)
denHrrLevel.2011 = denAggByHrr("Z:/j_scrdata/ascMMA/denVarsHrr_2011.csv",2011)

denHrr.20062011=do.call(rbind,list(denHrrLevel.2006,denHrrLevel.2007,denHrrLevel.2008,denHrrLevel.2009,denHrrLevel.2010,denHrrLevel.2011))
#sapply(list(denHrrLevel.2006,denHrrLevel.2007,denHrrLevel.2008,denHrrLevel.2009,denHrrLevel.2010,denHrrLevel.2011),names)

head(denHrr.20062011)

#comorbidity 
hrrMeanComorb=xpt2r.yz("Z:/j_scrdata/ascMMA/",'ran_meanComorbHrr_2010')[,c('hrrnum','.avg.comorbsum')]

hrrMeanComorb.2006=data.frame(hrrMeanComorb,year=2006)
hrrMeanComorb.2007=data.frame(hrrMeanComorb,year=2007)
hrrMeanComorb.2008=data.frame(hrrMeanComorb,year=2008)
hrrMeanComorb.2009=data.frame(hrrMeanComorb,year=2009)
hrrMeanComorb.2010=data.frame(hrrMeanComorb,year=2010)
hrrMeanComorb.2011=data.frame(hrrMeanComorb,year=2011)


hrrMeanComorb.20062011=do.call(rbind,list(hrrMeanComorb.2006,hrrMeanComorb.2007,hrrMeanComorb.2008,hrrMeanComorb.2009, hrrMeanComorb.2010,hrrMeanComorb.2011))

indepVarDf.20062011=join(posVarsDf.20062011,join(denHrr.20062011,hrrMeanComorb.20062011))

sapply(list(posVarsDf.20062011,denHrr.20062011,hrrMeanComorb.20062011),nrow)

  dep2006=xpt2r.yz("Z:/j_scrdata/ascMMA/",'fascOfOpVol_2006')
  dep2007=xpt2r.yz("Z:/j_scrdata/ascMMA/",'fascOfOpVol_2007')
  dep2008=xpt2r.yz("Z:/j_scrdata/ascMMA/",'fascOfOpVol_2008')
  dep2009=xpt2r.yz("Z:/j_scrdata/ascMMA/",'fascOfOpVol_2009')
  dep2010=xpt2r.yz("Z:/j_scrdata/ascMMA/",'fascOfOpVol_2010')
  dep2011=xpt2r.yz("Z:/j_scrdata/ascMMA/",'fascOfOpVol_2011')

depVarDf.20062011 = do.call(rbind,list(dep2006,dep2007,dep2008,dep2009,dep2010,dep2011))
nrow(depVarDf)
nrow(indepVarDf)


anaDf.raw=join(depVarDf.20062011, indepVarDf.20062011, by=c('hrrnum','year'))
anaDf.raw[,'entday.opFascOf']=anaDf.raw[,'entday.op']+anaDf.raw[,'entday.of']+anaDf.raw[,'entday.fasc']
head(anaDf.raw)
names(anaDf.raw)

vec=anaDf.raw[,'.qtr.since2006']

get4quarters=function(vec){
  remainderVec=vec%%4
  zeroLoc=which(remainderVec==0)
  remainderVec[zeroLoc]=4
  return(remainderVec)
}

anaDf.raw[,'qtr']=as.factor(get4quarters(anaDf.raw[,'.qtr.since2006']))
anaDf.raw[,'MMA2008.impact']=as.integer(anaDf.raw[,'year']>=2008)
anaDf.raw[,'year']=as.factor(anaDf.raw[,'year'])

xtabs(~MMA2008.impact+year,data=anaDf.raw)

#knots.tiles=c(0.2,0.4,0.8)

knots.tiles=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)

#knots.tiles=c(0.5)
knots.tiles=c(0.2,0.4,0.6,0.8)
knots.tiles=c(0.2)

bslist=bsWrapper1.yz(anaDf.raw[,'.qtr.since2006'] #this is the x in bs function
                     , quantile(anaDf.raw[,'.qtr.since2006'],probs=knots.tiles) #inner knots, if it contains boundary knots, function will stop and issue erro message
                     , paste('.qtr.since2006','.bs',sep='') #output data's columen name stem
                     , degree=2
                     , dataType='data.frame'
                     , Boundary.knots=quantile(anaDf.raw[,'.qtr.since2006'],prob=c(0,1))
                     #or ='data.frame
)

names(bslist)
n.basis=bslist$n.basis
names(bslist$bsdata)
outdf.with.bs=cbind(anaDf.raw,bslist$bsdata)
names(outdf.with.bs)

adjustVars=c('MMA2008.impact','mean.age', '.avg.comorbsum','n.hosp','n.oprooms', 'n.fasc','n.hospasc','race.0','race.1','race.2','race.3','race.4','race.5','sex.1'
             ,'qtr')

fit.fasc=glm(passVarToFormula.yz('entday.fasc',c(adjustVars,paste('.qtr.since2006','.bs',seq(n.basis),sep='')))
           , offset=log(n.bene)
           , family = poisson
           , data=outdf.with.bs)

summary(fit.fasc)
count(outdf.with.bs,'MMA2008.impact')

fit.op=glm(passVarToFormula.yz('entday.op',c(adjustVars,paste('.qtr.since2006','.bs',seq(n.basis),sep='')))
             , offset=log(n.bene)
             , family = poisson
             , data=outdf.with.bs)
summary(fit.op)

fit.of=glm(passVarToFormula.yz('entday.of',c(adjustVars,paste('.qtr.since2006','.bs',seq(n.basis),sep='')))
                 , offset=log(n.bene)
                 , family = poisson
                 , data=outdf.with.bs)
summary(fit.of)

col.type.yz(outdf.with.bs)

fit.opFascOf=glm(passVarToFormula.yz('entday.opFascOf',c(adjustVars,paste('.qtr.since2006','.bs',seq(n.basis),sep='')))
                 , offset=log(n.bene)
                 , family = poisson
                 , data=outdf.with.bs)
summary(fit.opFascOf)


denomDf=ddply(anaDf.raw,'.qtr.since2006',function(x){sum(x[,'n.bene'])})



interest.factorVecList=list(year=rep(year.val,nrow(outdf.with.bs)),qtr=rep(qtr.val,nrow(outdf.with.bs)))


# qtr.val=4
# year.val=2011

trendPredFunc=function(year.val, qtr.val, modelFit, otherIndepVarsDf, quarterInnerKnots, Boundary.knots){
  print(year.val)
  print(qtr.val)

  nobs=nrow(otherIndepVarsDf)
  cat('generate basis spline','\n')
  bslist=bsWrapper1.yz(rep(4*(year.val-2006)+qtr.val,nobs) #this is the x in bs function
                       , quarterInnerKnots #inner knots, if it contains boundary knots, function will stop and issue erro message
                       , paste('.qtr.since2006','.bs',sep='') #output data's columen name stem
                       , degree=2
                       , dataType='data.frame'
                       , Boundary.knots
                       #or ='data.frame
  )
  n.basis = bslist$n.basis
  otherIndepVarsDf[,paste(paste('.qtr.since2006','.bs',sep=''),seq(n.basis),sep='')]=bslist$bsdata
  
  
  interest.factorVecList=list(year=rep(year.val,nobs),qtr=rep(qtr.val,nobs))
  xMat = addFactorConsistentWithModelFit.yz(   modelFit #e.g. a model fit objective 
                            , otherIndepVarsDf #this is a new data.frame which contain all the independent variables other than factors of interests
                            ,interest.factorVecList
                             #names of list elments should be interested factor variable names
                            #the length of each element should be equal to the nrow(df)
                            ,outputType='data.matrix'
                            #either 'data.matrix' or 'data.frame'
)
  
  
xbOut=xb.matched.yz(coef(modelFit),xMat)
predExpectedRateVec=exp(xbOut$xb.matched)
volVec=modelFit$data$n.bene*predExpectedRateVec
rows.yr=which(modelFit$data[,'year']==as.character(year.val))
rows.qtr=which(modelFit$data[,'qtr']==as.character(qtr.val))
okRows=intersect.yz(rows.yr,rows.qtr)
volDf.tmp=modelFit$data[okRows,c('n.bene','hrrnum')]
volDf.tmp=rename.vars(volDf.tmp,c('n.bene'),'n.bene.chosen')
tmp=join(modelFit$data,volDf.tmp,type='left')
predVol=sum(predExpectedRateVec*tmp[,'n.bene.chosen'])
predUseRate=predVol/sum(tmp[,'n.bene.chosen'])
outVec=c(predVol=predVol,predUseRate=predUseRate, rawMean=mean(predExpectedRateVec))
return(outVec)
}

quarterInnerKnots = quantile(outdf.with.bs[,'.qtr.since2006'],probs=knots.tiles)
Boundary.knots=quantile(outdf.with.bs[,'.qtr.since2006'],prob=c(0,1))


qtrRate=function(indf,outcomeVn){
  out=ddply(indf,c('.qtr.since2006'),function(x){out=sum(x[,outcomeVn])/sum(x[,'n.bene'])})
  names(out)[2]=outcomeVn
  return(out)
}



Boundary.knots=quantile(anaDf.raw[,'.qtr.since2006'],prob=c(0,1))
quarterInnerKnots=quantile(anaDf.raw[,'.qtr.since2006'],probs=knots.tiles)
#op
obsRate.op=rename.vars(qtrRate(outdf.with.bs,'entday.op'), 'entday.op','entday.op.obs')
otherIndepVarsDf=outdf.with.bs

yearVec=seq(2006,2011)
qtrVec=seq(4)

otherIndepVarsDf=outdf.with.bs
outlist=list()
for(i in seq(length(yearVec))){ 
  for(j in seq(length(qtrVec))){
    outlist=lappend.yz(outlist,
                       trendPredFunc(yearVec[i],qtrVec[j],fit.op,otherIndepVarsDf,quarterInnerKnots, Boundary.knots)
    )
  }
}

plotdf.MMA=data.frame(obsRate.op, pred=do.call(rbind,outlist)[,2])
plot(plotdf.MMA[,2],type='b',col='red',ylim=c(0.1,0.15))
lines(plotdf.MMA[,3],type='b')

otherIndepVarsDf[,'MMA2008.impact']=0
outlist=list()
for(i in seq(length(yearVec))){ 
  for(j in seq(length(qtrVec))){
    outlist=lappend.yz(outlist,
                       trendPredFunc(yearVec[i],qtrVec[j],fit.op,otherIndepVarsDf,quarterInnerKnots, Boundary.knots)
    )
  }
}

names(otherIndepVarsDf)
plotdf.noMMA=data.frame(obsRate.op, pred=do.call(rbind,outlist)[,2])
plot(plotdf.noMMA[,2],type='b',col='red',ylim=c(0.1,0.15))
lines(plotdf.noMMA[,3],type='b')

plot(plotdf.noMMA[,3],col='red',ylim=c(0.1,0.15),type='b')
lines(plotdf.MMA[,3],type='b')


#of--------
obsRate.of=rename.vars(qtrRate(outdf.with.bs,'entday.of'), 'entday.of','entday.of.obs')

otherIndepVarsDf=outdf.with.bs

yearVec=seq(2006,2011)
qtrVec=seq(4)

otherIndepVarsDf=outdf.with.bs
outlist=list()
for(i in seq(length(yearVec))){ 
  for(j in seq(length(qtrVec))){
    outlist=lappend.yz(outlist,
                       trendPredFunc(yearVec[i],qtrVec[j],fit.of,otherIndepVarsDf,quarterInnerKnots, Boundary.knots)
    )
  }
}

plotdf.MMA=data.frame(obsRate.of, pred=do.call(rbind,outlist)[,2])
plot(plotdf.MMA[,2],type='b',col='red',ylim=c(0.1,0.15))
lines(plotdf.MMA[,3],type='b')

otherIndepVarsDf[,'MMA2008.impact']=0
outlist=list()
for(i in seq(length(yearVec))){ 
  for(j in seq(length(qtrVec))){
    outlist=lappend.yz(outlist,
                       trendPredFunc(yearVec[i],qtrVec[j],fit.of,otherIndepVarsDf,quarterInnerKnots, Boundary.knots)
    )
  }
}

names(otherIndepVarsDf)
plotdf.noMMA=data.frame(obsRate.of, pred=do.call(rbind,outlist)[,2])
plot(plotdf.noMMA[,2],type='b',col='red',ylim=c(0.1,0.15))
lines(plotdf.noMMA[,3],type='b')

plot(plotdf.noMMA[,3],col='red',ylim=c(0.15,0.3),type='b')
lines(plotdf.MMA[,3],type='b')











plot(plotdf[,2],ylim=c(0,0.2),type='l')
lines(plotdf.allvol.withEffect[,2])

otherIndepVarsDf=outdf.with.bs
otherIndepVarsDf[,'MMA2008.impact']=0
yearVec=seq(2006,2011)
qtrVec=seq(4)
outlist=list()
for(i in seq(length(yearVec))){ 
  for(j in seq(length(qtrVec))){
    outlist=lappend.yz(outlist,
                       trendPredFunc(yearVec[i],qtrVec[j],fit.op,otherIndepVarsDf)
    )
  }
}

tail(outdf.with.bs)
otherIndepVarsDf=outdf.with.bs
otherIndepVarsDf[,'MMA2008.impact']=0
names(outdf.with.bs)

yearVec=seq(2006,2011)
qtrVec=seq(4)
outlist=list()
for(i in seq(length(yearVec))){ 
  for(j in seq(length(qtrVec))){
    outlist=lappend.yz(outlist,
                       trendPredFunc(yearVec[i],qtrVec[j],modelFit)
    )
  }
}
  # n.bt.coefVec=10
  # n.bt.withinVec=5

  
  coefMatWithRowname=t(MASS::mvrnorm(n = n.bt.coefVec, coef(glmPosFit), vcov(glmPosFit)))
  
  yearQtrDf=expandGrid.yz(seq(2006,2011),seq(1,4))
  names(yearQtrDf)=c('year','qtr')
  

  policyDataWithRawOffset=outdf.with.bs
  
  policyDataWithRawOffset[,c('year','qtr')]=yearQtrDf[1,]
  policyDataWithRawOffset[,'year']=factor(policyDataWithRawOffset[,'year'], levels=sort(yearVec))
  policyDataWithRawOffset[,'qtr']=factor(policyDataWithRawOffset[,'qtr'], levels=sort(qtrVec))
  
  levels(policyDataWithRawOffset[,'year'])
  levels(outdf.with.bs[,'year'])
  levels(policyDataWithRawOffset[,'qtr'])
  levels(outdf.with.bs[,'qtr'])
  
  coef(glmPosFit)
  

  policyData=outdf.with.bs
  vn='qtr'
  policyData[,vn]=factor(1,levels=levels(modelFitObj$data[,vn]))
  
levels(policyData[,'qtr'])
  policyDfVnTypes=sapply(policyData,class)[names(glmPosFit$data)]
  modelDfVnTypes=sapply(glmPosFit$data,class)
  
  
  #first use model.matrix on original data to first obtain data matrix, then change data matrix
  #change data.frame then use model.matrix on the changed  data.frame  to get matrix
  sameColclassAsIndepVarsOfModelFitDf.yz(policyDataWithRawOffset,glmPosFit)
  policyDataWithRawOffset=outdf.with.bs
  policyDataWithRawOffset[,'qtr']=factor(1,levels=c(1,2,3,4))
  levels(glmPosFit$data[,'qtr'])
  levels(policyDataWithRawOffset[,'qtr'])
  
  is.factor(policyDataWithRawOffset[,'qtr'])
  
  policy.xMat=model.matrix(formula(glmPosFit), data=policyDataWithRawOffset)
  policy.xMat[1,]
  
  
 
  

  col.type.yz(policyDataWithRawOffset)
  
  
  
  policy.xMat=model.matrix(glmPosFit, data=outdf.with.bs)
  
  head(policy.xMat)
  
  outList=list()
  for (k in 1:n.bt.coefVec){
    policyDataWithRawOffset=outdf.with.bs
    for (year.i in 1:length(yearVec)){
      policyDataWithRawOffset[,'year']=factor(policyDataWithRawOffset[,'year'], levels=sort(yearVec))
      for(qtr.i in 1:length(qtrVec)){
        policyDataWithRawOffset[,'qtr']=factor(qtr.i, levels=sort(qtrVec))
        
      }
      #factor.levels=levels(outdf.with.bs[,'year'])
          levels(policyDataWithRawOffset[,'qtr'])
          levels(outdf.with.bs[,'qtr'])
      unique(outdf.with.bs[,'qtr'])
     
      policyDataWithRawOffset[,'.qtr.since2006']= factor(policyDataWithRawOffset[,'.qtr.since2006'], levels=sort(unique(anaDf.raw[,'.qtr.since2006'])))
      
      for(j in 1:n.bt.withinVec){
        cat('processing k=', k,'\n')
        cat('processing i=', i, '\n')
        cat('processing j=', j, '\n')
        
        rPoisDrawList = rPoisDrawGlmPoisModel.yz(modelFormula # looks like ~ a+b+c  usually use formula(glmPoissonFit)
                                                 ,coefMatWithRowname[,k] # k X n.bt  
                                                 #coefVec should have name, so it can check match with data.matrix internally
                                                 #the order of independent variable in modelFormula should match the name of coefVecWithName
                                                 #this code will examine whether it matches
                                                 ,policyDataWithRawOffset #has nobs and with all the indepent variable and raw offset (not the logged one)
                                                 #rawOffset is not the one without taking log()
                                                 #variable name of logOffset is 'logOffset'
                                                 #contains all thye indepenet variable in modelFormula
                                                 ,rawOffSetVn #raw means it is NOT logged one, it is just raw Offset eg. it is person-day not log(person-day)
        )
        
        
        volMat.k[i,j]=sum(unlist(rPoisDrawList))
      } 
    }
    outList=lappend.yz(outList,volMat.k)
  }
  return(outList)
}

mean.95ci=function(rPoisDrawList,which.qrt){
  vec=unlist(lapply(rPoisDrawList,function(x){x[which.qrt,]}))
  meanVal=mean(vec)
  ciVec=quantile(vec,probs=c(0.025,0.975))
  outmat=matrix(c(meanVal,ciVec),nrow=1)
  colnames(outmat)=c('mean','lowBd','upBd')
  return(outmat)  
}
mean.95ci.allqtrs=function(rPoisDrawList){
  outList=list()
  for (which.qrt in 1:20){
    outList=lappend.yz(outList, mean.95ci(rPoisDrawList,which.qrt))
  }
  
  outMat=matrix(unlist(outList),nrow=20,byrow=T) 
  colnames(outMat)=c('mean','lowBd','upBd')
  return(outMat)
}

plotDf.of=mean.95ci.allqtrs(
  getRanPoisResult.changeYear(glmPosFit=fit.of,n.bt.coefVec=100,n.bt.withinVec=1)
)

plotDf.op=mean.95ci.allqtrs(
  getRanPoisResult.changeYear(glmPosFit=fit.op,n.bt.coefVec=100,n.bt.withinVec=1)
)

plotDf.fasc=mean.95ci.allqtrs(
  getRanPoisResult.changeYear(glmPosFit=fit.fasc,n.bt.coefVec=100,n.bt.withinVec=1)
)

plotDf.opFascOf=mean.95ci.allqtrs(
  getRanPoisResult.changeYear(glmPosFit=fit.opFascOf,n.bt.coefVec=100,n.bt.withinVec=1)
)


denomDf=ddply(anaDf.raw,'.qtr.since2006',function(x){c(denom=sum(x[,'n.bene']))})

plotDf.of.1=cbind(denomDf,plotDf.of)
plotDf.op.1=cbind(denomDf,plotDf.op)
plotDf.fasc.1=cbind(denomDf,plotDf.fasc)
plotDf.opFascOf.1=cbind(denomDf,plotDf.opFascOf)

getRate=function(plotDf){
  plotDf[,'mean.rate']=plotDf[,'mean']/plotDf[,'denom']
  plotDf[,'lb.rate']=plotDf[,'lowBd']/plotDf[,'denom']
  plotDf[,'ub.rate']=plotDf[,'upBd']/plotDf[,'denom']
  return(plotDf)
  
  
  
}







getRanPoisResult.changeYear=function(glmPosFit,n.bt.coefVec,n.bt.withinVec){
modelFormula = formula(glmPosFit)
# n.bt.coefVec=10
# n.bt.withinVec=5
coefMatWithRowname=t(MASS::mvrnorm(n = n.bt.coefVec, coef(glmPosFit), vcov(glmPosFit)))

outList=list()
for (k in 1:n.bt.coefVec){

volMat.k=matrix(NA,nrow=20,ncol=n.bt.withinVec) #20 qarter
for (i in 1:unilen.yz(as.numeric(anaDf.raw[,'.qtr.since2006']))){
  
  policyDataWithRawOffset=anaDf.raw
  policyDataWithRawOffset[,'.qtr.since2006']=i
  policyDataWithRawOffset[,'.qtr.since2006']= factor(policyDataWithRawOffset[,'.qtr.since2006'], levels=sort(unique(anaDf.raw[,'.qtr.since2006'])))
  
  for(j in 1:n.bt.withinVec){
    cat('processing k=', k,'\n')
    cat('processing i=', i, '\n')
    cat('processing j=', j, '\n')
    
  rPoisDrawList = rPoisDrawGlmPoisModel.yz(modelFormula # looks like ~ a+b+c  usually use formula(glmPoissonFit)
                                            ,coefMatWithRowname[,k] # k X n.bt  
                                            #coefVec should have name, so it can check match with data.matrix internally
                                            #the order of independent variable in modelFormula should match the name of coefVecWithName
                                            #this code will examine whether it matches
                                            ,policyDataWithRawOffset #has nobs and with all the indepent variable and raw offset (not the logged one)
                                            #rawOffset is not the one without taking log()
                                            #variable name of logOffset is 'logOffset'
                                            #contains all thye indepenet variable in modelFormula
                                            ,rawOffSetVn #raw means it is NOT logged one, it is just raw Offset eg. it is person-day not log(person-day)
  )
  
  
 volMat.k[i,j]=sum(unlist(rPoisDrawList))
} 
}
outList=lappend.yz(outList,volMat.k)
}
return(outList)
}

mean.95ci=function(rPoisDrawList,which.qrt){
  vec=unlist(lapply(rPoisDrawList,function(x){x[which.qrt,]}))
  meanVal=mean(vec)
  ciVec=quantile(vec,probs=c(0.025,0.975))
  outmat=matrix(c(meanVal,ciVec),nrow=1)
  colnames(outmat)=c('mean','lowBd','upBd')
  return(outmat)  
}
mean.95ci.allqtrs=function(rPoisDrawList){
  outList=list()
  for (which.qrt in 1:20){
  outList=lappend.yz(outList, mean.95ci(rPoisDrawList,which.qrt))
  }

outMat=matrix(unlist(outList),nrow=20,byrow=T) 
  colnames(outMat)=c('mean','lowBd','upBd')
 return(outMat)
}

plotDf.of=mean.95ci.allqtrs(
  getRanPoisResult.changeYear(glmPosFit=fit.of,n.bt.coefVec=100,n.bt.withinVec=1)
  )

plotDf.op=mean.95ci.allqtrs(
  getRanPoisResult.changeYear(glmPosFit=fit.op,n.bt.coefVec=100,n.bt.withinVec=1)
)

plotDf.fasc=mean.95ci.allqtrs(
  getRanPoisResult.changeYear(glmPosFit=fit.fasc,n.bt.coefVec=100,n.bt.withinVec=1)
)

plotDf.opFascOf=mean.95ci.allqtrs(
  getRanPoisResult.changeYear(glmPosFit=fit.opFascOf,n.bt.coefVec=100,n.bt.withinVec=1)
)


denomDf=ddply(anaDf.raw,'.qtr.since2006',function(x){c(denom=sum(x[,'n.bene']))})

plotDf.of.1=cbind(denomDf,plotDf.of)
plotDf.op.1=cbind(denomDf,plotDf.op)
plotDf.fasc.1=cbind(denomDf,plotDf.fasc)
plotDf.opFascOf.1=cbind(denomDf,plotDf.opFascOf)

getRate=function(plotDf){
  plotDf[,'mean.rate']=plotDf[,'mean']/plotDf[,'denom']
  plotDf[,'lb.rate']=plotDf[,'lowBd']/plotDf[,'denom']
  plotDf[,'ub.rate']=plotDf[,'upBd']/plotDf[,'denom']
  return(plotDf)
}

plotDf.of.rate=getRate(plotDf.of.1)
plotDf.op.rate=getRate(plotDf.op.1)
plotDf.fasc.rate=getRate(plotDf.fasc.1)
plotDf.opFascOf.rate=getRate(plotDf.opFascOf.1)


pdf('C:/Dropbox/K/ascMMA/result/fig/03112014/outpatientVolumeByPlaceTrendWithCI.pdf')
#of
plot(seq(20),plotDf.of.rate[,'mean.rate'],type='b',ylim=c(0,5),main=c('Adjusted volume trend with CI by place of care'), xlab='quarter since 2006 January', axes=F, ylab='per capita encounters in a quarter',pch=1)
lines(seq(20),plotDf.of.rate[,'lb.rate'])
lines(seq(20),plotDf.of.rate[,'ub.rate'])
#op
lines(seq(20),plotDf.op.rate[,'mean.rate'],type='b',pch=2)
lines(seq(20),plotDf.op.rate[,'lb.rate'])
lines(seq(20),plotDf.op.rate[,'ub.rate'])
#fasc
lines(seq(20),plotDf.fasc.rate[,'mean.rate'],type='b',pch=3)
lines(seq(20),plotDf.fasc.rate[,'lb.rate'])
lines(seq(20),plotDf.fasc.rate[,'ub.rate'])
#axis
axis(1,at=seq(1,20),labels=F,cex=0.5)
text(seq(1,20),par("usr")[3] - 0.1,labels = seq(20), srt = 45, pos = 1, xpd = TRUE, cex=0.85)
axis(2,at=seq(1,5),labels=T,cex=0.5)
abline(v=9,lty=3)
legend('topleft',c('office','hospital based outpatient','freestanding ASC'),pch=c(1,2,3),cex=0.85)
box()
dev.off()


pdf('C:/Dropbox/K/ascMMA/result/fig/03112014/outpatientVolumeByPlaceTrendNoCI.pdf')
#of
plot(seq(20),plotDf.of.rate[,'mean.rate'],type='b',ylim=c(0,5),main=c('Adjusted volume trend by place of care without CI'), xlab='quarter since 2006 January', axes=F, ylab='per capita encounters in a quarter',pch=1)
# lines(seq(20),plotDf.of.rate[,'lb.rate'])
# lines(seq(20),plotDf.of.rate[,'ub.rate'])
#op
lines(seq(20),plotDf.op.rate[,'mean.rate'],type='b',pch=2)
# lines(seq(20),plotDf.op.rate[,'lb.rate'])
# lines(seq(20),plotDf.op.rate[,'ub.rate'])
#fasc
lines(seq(20),plotDf.fasc.rate[,'mean.rate'],type='b',pch=3)
# lines(seq(20),plotDf.fasc.rate[,'lb.rate'])
# lines(seq(20),plotDf.fasc.rate[,'ub.rate'])
#axis
axis(1,at=seq(1,20),labels=F,cex=0.5)
text(seq(1,20),par("usr")[3] - 0.1,labels = seq(20), srt = 45, pos = 1, xpd = TRUE, cex=0.85)
axis(2,at=seq(1,5),labels=T,cex=0.5)
abline(v=9,lty=3)
legend('topleft',c('office','hospital based outpatient','freestanding ASC'),pch=c(1,2,3),cex=0.85)
box()
dev.off()


pdf('C:/Dropbox/K/ascMMA/result/fig/03112014/totalOutpatientVolumeTrendWithCI.pdf')
plot(seq(20),plotDf.opFascOf.rate[,'mean.rate'],type='b',ylim=c(0,10),main=c('Adjusted total volume trend with CI'), xlab='quarter since 2006 January', axes=F, ylab='per capita encounters in a quarter',pch=1)
lines(seq(20),plotDf.opFascOf.rate[,'lb.rate'])
lines(seq(20),plotDf.opFascOf.rate[,'ub.rate'])
#axis
axis(1,at=seq(1,20),labels=F,cex=0.5)
text(seq(1,20),par("usr")[3] - 0.1,labels = seq(20), srt = 45, pos = 1, xpd = TRUE, cex=0.85)
axis(2,at=seq(1,10),labels=T,cex=0.5)
abline(v=9,lty=3)
box()
dev.off()

pdf('C:/Dropbox/K/ascMMA/result/fig/03112014/totalOutpatientVolumeTrendNoCI.pdf')
plot(seq(20),plotDf.opFascOf.rate[,'mean.rate'],type='b',ylim=c(0,10),main=c('Adjusted total volume trend without CI'), xlab='quarter since 2006 January', axes=F, ylab='per capita encounters in a quarter',pch=1)
#axis
axis(1,at=seq(1,20),labels=F,cex=0.5)
text(seq(1,20),par("usr")[3] - 0.1,labels = seq(20), srt = 45, pos = 1, xpd = TRUE, cex=0.85)
axis(2,at=seq(1,10),labels=T,cex=0.5)
abline(v=9,lty=3)
box()
dev.off()



#then you joint hrrPeneDf
names(hrrPeneDf)

anaDf.raw[,'.qtr.since2006']=as.numeric(anaDf.raw[,'.qtr.since2006'])

table(anaDf.raw[,'year'])

is.factor(anaDf.raw[,'hrrnum'])

head(hrrPeneDf,500)

hrrPeneDf[completeCases.yz(hrrPeneDf)$deletedRows,]

anaDf.raw.1=join(anaDf.raw,hrrPeneDf,by=c('hrrnum',".qtr.since2006"))
#then break the penetration variable int three groups.

n.grp=3
cuts=quantile(anaDf.raw.1[,'hrrPene10kRate'],seq(0,1,by=1/n.grp))[2:n.grp]
anaDf.raw.2=grpnv.supplycuts.yz(anaDf.raw.1,'hrrPene10kRate',c(-Inf,cuts,Inf),'fascPeneGrp')


enlistComb.yz(  list(fascPene=unique(anaDf.raw.2[,'fascPeneGrp']), MMA=c(0,1)) # a named list
                         , refCombList # at named list (names are the same as vecList)
                         , whichCombVn='.whichComb')



names(anaDf.raw)



# save(plotDf.opFascOf.rate, plotDf.op.rate, plotDf.fasc.rate, plotDf.of.rate, fit.opFascOf, fit.op, fit.of, fit.fasc, anaDf.raw
#      ,file='Z:/j_scrdata/ascMMA/result/mmaResult03112014.RData')

#load(file='Z:/j_scrdata/ascMMA/result/mmaResult03112014.RData')


summary(fit.of)
















