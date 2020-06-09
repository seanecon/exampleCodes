
#load(file='Z:/j_scrdata/ascMMA/result/mmaResult03112014.RData')
trendPredFunc=function(modelFit,outcomeVn){ 
  xMat=model.matrix(formula(modelFit),modelFit$data)
  xbOut=xb.matched.yz(coef(modelFit),xMat)
  predExpectedRateVec=exp(xbOut$xb.matched)
  predVol=modelFit$data$n.bene*predExpectedRateVec
  outDf=cbind(modelFit$data[,c('n.bene',outcomeVn, '.qtr.since2006','hrrnum')],predVol=predVol)
  return(outDf)
}
getSplineFit=function(
   outcomeVn
  ,analysisDf
  ,whichQtrVn='.qtr.since2006'
  ,knots.tiles=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
  ,adjustVars=c('MMA2008.impact','mean.age', '.avg.comorbsum','n.hosp','n.oprooms', 'n.fasc','n.hospasc','race.0','race.1','race.2','race.3','race.4','race.5','sex.1' ,'qtr')
){
  bslist=bsWrapper1.yz(analysisDf[,whichQtrVn] #this is the x in bs function
                       , quantile(analysisDf[,whichQtrVn],probs=knots.tiles) #inner knots, if it contains boundary knots, function will stop and issue erro message
                       , paste(whichQtrVn,'.bs',sep='') #output data's columen name stem
                       , degree=2
                       , dataType='data.frame'
                       , Boundary.knots=quantile(analysisDf[,whichQtrVn],prob=c(0,1))
  )
  n.basis=bslist$n.basis
  anaDfwithBs=cbind(analysisDf,bslist$bsdata)
  
  modelFit=glm(passVarToFormula.yz(outcomeVn,c(adjustVars,paste(whichQtrVn,'.bs',seq(n.basis),sep='')))
               , offset=log(n.bene)
               , family = poisson
               , data=anaDfwithBs)
  
  return(modelFit)
  
}
fit.fasc=getSplineFit(
  outcomeVn='entday.fasc'
  ,anaDf.raw
  ,whichQtrVn='.qtr.since2006'
  ,knots.tiles=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
  ,adjustVars=c('MMA2008.impact','mean.age', '.avg.comorbsum','n.hosp','n.oprooms', 'n.fasc','n.hospasc','race.0','race.1','race.2','race.3','race.4','race.5','sex.1' ,'qtr')
)

#knots.tiles=c(0.2,0.4,0.6,0.8)
knots.tiles=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)

bslist=bsWrapper1.yz(anaDf.raw.2[,'.qtr.since2006'] #this is the x in bs function
                      , quantile(anaDf.raw.2[,'.qtr.since2006'],probs=knots.tiles) #inner knots, if it contains boundary knots, function will stop and issue erro message
                      , paste('.qtr.since2006','.bs',sep='') #output data's columen name stem
                      , degree=2
                      , dataType='data.frame'
                      , Boundary.knots=quantile(anaDf.raw.2[,'.qtr.since2006'],prob=c(0,1))
)
n.basis=bslist$n.basis
anaDfwithBs=cbind(anaDf.raw.2,bslist$bsdata)

anaDfwithBs[,'totalOutpVol']=anaDfwithBs[,'entday.fasc']+anaDfwithBs[,'entday.of']+anaDfwithBs[,'entday.op']

names(anaDfwithBs)=tolower(names(anaDfwithBs))



modelFit.allvol=glm(
  totaloutpvol ~  mma2008.impact+fascsurgeongrp+ mma2008.impact*fascsurgeongrp+mean.age+.avg.comorbsum+n.hosp+n.oprooms+n.fasc+n.hospasc+race.0+race.1+race.2+race.3+race.4+race.5+sex.1+.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4+.qtr.since2006.bs5+.qtr.since2006.bs6,
  , offset=log(n.bene)
  , family = poisson
  , data=anaDfwithBs)




modelFit.fasc=glm(
  entday.fasc ~  mma2008.impact+fascsurgeongrp+ mma2008.impact*fascsurgeongrp+mean.age+.avg.comorbsum+n.hosp+n.oprooms+n.fasc+n.hospasc+race.0+race.1+race.2+race.3+race.4+race.5+sex.1+.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4+.qtr.since2006.bs5+.qtr.since2006.bs6,
             , offset=log(n.bene)
             , family = poisson
             , data=anaDfwithBs)
summary(modelFit.fasc)

#test


hist(anaDfwithBs[,'nfascdocper10kbene'],100)


names(anaDfwithBs)

xtabs(~mma2008.impact+year,data=anaDfwithBs)
subset(anaDf.raw.2,year==2009)[,'MMA2008.impact']


modelFit.of=glm(
  entday.of ~ mma2008.impact+fascsurgeongrp+ mma2008.impact*fascsurgeongrp+mean.age+.avg.comorbsum+n.hosp+n.oprooms+n.fasc+n.hospasc+race.0+race.1+race.2+race.3+race.4+race.5+sex.1+.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4+.qtr.since2006.bs5+.qtr.since2006.bs6+.qtr.since2006.bs7+.qtr.since2006.bs8+.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11,
  , offset=log(n.bene)
  , family = poisson
  , data=anaDfwithBs)
summary(modelFit.of)



modelFit.op=glm(
  entday.op ~ mma2008.impact+fascsurgeongrp+ mma2008.impact*fascsurgeongrp+mean.age+.avg.comorbsum+n.hosp+n.oprooms+n.fasc+n.hospasc+race.0+race.1+race.2+race.3+race.4+race.5+sex.1+.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4+.qtr.since2006.bs5+.qtr.since2006.bs6+.qtr.since2006.bs7+.qtr.since2006.bs8+.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11,
  , offset=log(n.bene)
  , family = poisson
  , data=anaDfwithBs)
summary(modelFit.op)


policyExpTest=function(mma2008.impact.val,fascpenegrp.val.vec,modelFit){
anaDfwithBs.pol=modelFit$data


for (i in 1:length(fascpenegrp.val.vec)){
  anaDfwithBs.pol[,'fascpenegrp']=factor(fascpenegrp.val.vec[i],levels=c('[-Inf, 196)','[ 196, 322)', '[ 322, Inf]'))
}


#there is no policy
anaDfwithBs.pol[,'mma2008.impact']=0

for(qtr in qtrVec[j]){
  bslist=bsWrapper1.yz(analysisDf[,whichQtrVn] #this is the x in bs function
                       , quantile(analysisDf[,whichQtrVn],probs=knots.tiles) #inner knots, if it contains boundary knots, function will stop and issue erro message
                       , paste(whichQtrVn,'.bs',sep='') #output data's columen name stem
                       , degree=2
                       , dataType='data.frame'
                       , Boundary.knots=quantile(analysisDf[,whichQtrVn],prob=c(0,1))
  )
  n.basis=bslist$n.basis
  
}

model.mat=model.matrix(formula(modelFit),data=anaDfwithBs.pol)
xbOut=xb.matched.yz(coef(modelFit),model.mat)
predExpectedRateVec=exp(xbOut$xb.matched)


out=mean(predExpectedRateVec)




return(out)
}






yy.op=matrix(c(policyExpTest(0,'[-Inf, 196)',modelFit.op),
policyExpTest(1,'[-Inf, 196)',modelFit.op),
policyExpTest(0,'[ 196, 322)',modelFit.op),
policyExpTest(1,'[ 196, 322)',modelFit.op),
policyExpTest(0,'[ 322, Inf]',modelFit.op),
policyExpTest(1,'[ 322, Inf]',modelFit.op)),nrow=2
)
rownames(yy.op)=c('no MMA','with MMA')


pdf('Z:/j_scrdata/ascMMA/result/fig/mmaFascEffectonOp.pdf')
 barplot(yy.op, beside=TRUE,col=c("white","grey"), ylim=c(0,0.16), names.arg=c('low','medium','high'), axis.lty=1, xlab="FASC penetration", ylab="Outpatient use rate")
dev.off()

tiff('Z:/j_scrdata/ascMMA/result/fig/mmaFascEffectonOp.tiff')
barplot(yy.op, beside=TRUE,col=c("white","grey"), ylim=c(0,0.16), names.arg=c('low','medium','high'), axis.lty=1, xlab="FASC penetration", ylab="Outpatient use rate",main='Outpatient volume: MMA Impact by FASC penetration')
dev.off()


yy.of=matrix(c(policyExpTest(0,'[-Inf, 196)',modelFit.of),
               policyExpTest(1,'[-Inf, 196)',modelFit.of),
               policyExpTest(0,'[ 196, 322)',modelFit.of),
               policyExpTest(1,'[ 196, 322)',modelFit.of),
               policyExpTest(0,'[ 322, Inf]',modelFit.of),
               policyExpTest(1,'[ 322, Inf]',modelFit.of)),nrow=2
)
rownames(yy.of)=c('no MMA','with MMA')

pdf('Z:/j_scrdata/ascMMA/result/fig/mmaFascEffectonOf.pdf')
barplot(yy.of, beside=TRUE,col=c("white","grey"), ylim=c(0,0.25), names.arg=c('low','medium','high'), axis.lty=1, xlab="FASC penetration", ylab="Office visit use rate")
dev.off()

tiff('Z:/j_scrdata/ascMMA/result/fig/mmaFascEffectonOf.tiff')
barplot(yy.of, beside=TRUE,col=c("white","grey"), ylim=c(0,0.25), names.arg=c('low','medium','high'), axis.lty=1, xlab="FASC penetration", ylab="Office visit use rate", main='Office visit volume: MMA Impact by FASC penetration')
dev.off()


yy.fasc=matrix(c(policyExpTest(0,'[-Inf, 196)',modelFit.fasc),
               policyExpTest(1,'[-Inf, 196)',modelFit.fasc),
               policyExpTest(0,'[ 196, 322)',modelFit.fasc),
               policyExpTest(1,'[ 196, 322)',modelFit.fasc),
               policyExpTest(0,'[ 322, Inf]',modelFit.fasc),
               policyExpTest(1,'[ 322, Inf]',modelFit.fasc)),nrow=2
)
rownames(yy.fasc)=c('no MMA','with MMA')

pdf('Z:/j_scrdata/ascMMA/result/fig/mmaFascEffectonFasc.pdf')
barplot(yy.fasc, beside=TRUE,col=c("white","grey"), ylim=c(0,0.1), names.arg=c('low','medium','high'), axis.lty=1, xlab="FASC penetration", ylab="FASC use rate")
dev.off()

tiff('Z:/j_scrdata/ascMMA/result/fig/mmaFascEffectonFasc.tiff')
barplot(yy.fasc, beside=TRUE,col=c("white","grey"), ylim=c(0,0.1), names.arg=c('low','medium','high'), axis.lty=1, xlab="FASC penetration", ylab="FASC use rate", main='FASC volume: MMA Impact by FASC penetration')
dev.off()


anaDfwithBs[,'fascpenegrp']=as.factor(anaDfwithBs[,'fascpenegrp'])



names(anaDfwithBs)

testDf=trendPredFunc(fit.fasc,'entday.fasc')
pred=ddply(testDf,'.qtr.since2006',function(x){sum(x[,'predVol'])/sum(x[,'n.bene'])})
observed=ddply(testDf,'.qtr.since2006',function(x){sum(x[,'entday.fasc'])/sum(x[,'n.bene'])})

plotDf=cbind(pred,observed[,2])

pdf('Z:/j_scrdata/ascMMA/result/fig/splineGofChk.pdf')
plot(plotDf[,1],plotDf[,2],type='b',ylim=c(0.035,0.045),pch=1,lty=1,xlab='quarter since 2006',ylab=c('fasc use rate(probablity) per beneficiary-quarter'))
points(plotDf[,1],plotDf[,3],type='b',pch=2,lty=2)
legend('topleft',c('observed','predicted'),lty=c(1,2),pch=c(1,2),cex=0.9)
dev.off()

fit.op=getSplineFit(
  outcomeVn='entday.op'
  ,anaDf.raw
  ,whichQtrVn='.qtr.since2006'
  ,knots.tiles=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
  ,adjustVars=c('MMA2008.impact','mean.age', '.avg.comorbsum','n.hosp','n.oprooms', 'n.fasc','n.hospasc','race.0','race.1','race.2','race.3','race.4','race.5','sex.1' ,'qtr')
)
head(summary(fit.op)$coefficients)


fit.of=getSplineFit(
  outcomeVn='entday.of'
  ,anaDf.raw
  ,whichQtrVn='.qtr.since2006'
  ,knots.tiles=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
  ,adjustVars=c('MMA2008.impact','mean.age', '.avg.comorbsum','n.hosp','n.oprooms', 'n.fasc','n.hospasc','race.0','race.1','race.2','race.3','race.4','race.5','sex.1' ,'qtr')
)
head(summary(fit.of)$coefficients)

fit.opFascOf=getSplineFit(
  outcomeVn='entday.opFascOf'
  ,anaDf.raw
  ,whichQtrVn='.qtr.since2006'
  ,knots.tiles=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
  ,adjustVars=c('MMA2008.impact','mean.age', '.avg.comorbsum','n.hosp','n.oprooms', 'n.fasc','n.hospasc','race.0','race.1','race.2','race.3','race.4','race.5','sex.1' ,'qtr')
)
head(summary(fit.opFascOf)$coefficients)

trendPredFunc=function(year.val, qtr.val, modelFit, otherIndepVarsDf, quarterInnerKnots, Boundary.knots){
  nobs=nrow(otherIndepVarsDf)
  cat('generate basis spline','\n')
  bslist=bsWrapper1.yz(  rep(4*(year.val-2006)+qtr.val,nobs) #this is the x in bs function
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
  #volVec=modelFit$data$n.bene*predExpectedRateVec
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


modelFit=fit.of

mmaEffectFunc=function(modelFit){
  
  fitData=modelFit$data
  xMat.1=model.matrix(formula(modelFit),fitData)
  
  
  fitData[,'MMA2008.impact']=0
  xMat.0=model.matrix(formula(modelFit),fitData)
  
  xbOut.0=xb.matched.yz(coef(modelFit),xMat.0)
  predExpectedRateVec.0=exp(xbOut.0$xb.matched)
  
  xbOut.1=xb.matched.yz(coef(modelFit),xMat.1)
  predExpectedRateVec.1=exp(xbOut.1$xb.matched)
  
  volVec.0=modelFit$data$n.bene*predExpectedRateVec.0
  volVec.1=modelFit$data$n.bene*predExpectedRateVec.1
  
  predVolDf=cbind(fitData[,c('hrrnum','.qtr.since2006','n.bene')],volumn.noMMA=volVec.0, volumn.MMA=volVec.1)
  
  mmaEffectDf=ddply(predVolDf,c('.qtr.since2006'), function(x){predUseRate.MMA=sum(x[,'volumn.MMA'])/sum(x[,'n.bene']);
                                                            predUseRate.noMMA=sum(x[,'volumn.noMMA'])/sum(x[,'n.bene'])
                                                            outVec=c(predUseRate.MMA=predUseRate.MMA,predUseRate.noMMA=predUseRate.noMMA)
                                                            return(outVec)
                                                           })
  return(mmaEffectDf)
}

mmaEffDf.fasc=mmaEffectFunc(fit.fasc)
plot(mmaEffDf.fasc[,1], mmaEffDf.fasc[,2],type='l',col=2,ylim=c(0.035,0.05))
lines(mmaEffDf.fasc[,1],mmaEffDf.fasc[,3])

mmaEffDf.of=mmaEffectFunc(fit.of)
plot(mmaEffDf.of[,1], mmaEffDf.of[,2],type='l',col=2,ylim=c(0.16,0.22))
lines(mmaEffDf.of[,1],mmaEffDf.of[,3])

mmaEffDf.op=mmaEffectFunc(fit.op)
plot(mmaEffDf.op[,1],mmaEffDf.op[,2],type='l',col=2,ylim=c(0.11,0.13))
lines(mmaEffDf.op[,1],mmaEffDf.op[,3])

mmaEffDf.opFascOf=mmaEffectFunc(fit.opFascOf)
plot(mmaEffDf.opFascOf[,1],mmaEffDf.opFascOf[,2],type='l',col=2,ylim=c(0.32,0.4))
lines(mmaEffDf.opFascOf[,1],mmaEffDf.opFascOf[,3])

