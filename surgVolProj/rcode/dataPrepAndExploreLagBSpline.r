
#in this file, I found tha lag1 is the better to any other lags.
#within lag1, I did a search for all Bpline percentile (I used only one perceitle)
#can I see that using qudratic(degree=2) and percentiel between 60-80% perceditle is good.
#I picked 75% as  the final choice knot because its assoicated prediction is close to the loess trend plot
#the minimual LOS is then 88 (22 per quater). it is easier to justify a larger number based on 
#my discussion with Montie. alhouthg 75% is not the highly likeliihood but it is very close to the highest.
#I found that you cannot drop insignicant spline terms. It is easy to argue to keep insig bspline terms
#becaue they are coming out for a basis-spline. you can not disentangle them. further more I will argue
#that terms of parametric boostrapped, so ignifcant terms will be ok.

#C:\Dropbox\K\lapLearningCurve\rcode\rFun\indivFunFolder
#generate final analysis data
source('C:/Dropbox/K/lapLearningCurve/rcode/rFun/fun.r')
anaDf.raw=genAnaDf.lapLearn(100)

library(ggplot2)

#---- the following plot is helpful for a general trend analysis
# lag4.df.raw=ddply(nomiss.lag6,'n.lag4.lap', function(x){c(meanlos=mean(as.numeric(x[,'los']),na.rm=T), freq=nrow(x))})
# lag4.df=cbind(lag4.df.raw,logLap=log1p(lag4.df.raw[,'n.lag4.lap']))
# sortDf.yz(lag4.df,'n.lag4.lap')
lag1.df.raw=ddply(nomiss.lag6,'n.lag1.lap', function(x){c(meanlos=mean(as.numeric(x[,'los']),na.rm=T), freq=nrow(x))})
plot(lag1.df.raw[,'meanlos'])
lag1.df=cbind(lag1.df.raw,logLap=log1p(lag1.df.raw[,'n.lag1.lap']))
sortDf.yz(lag1.df,'n.lag1.lap')
fit=loess(meanlos~logLap, data=lag1.df,weights=freq, span=0.9)
plot(fit$x, fit$fitted,type='b',xlab='log volume', ylab='average length of stay', ylim=c(1,2.5))
points(fit$x, fit$y)

plot(lag1.df[,'n.lag1.lap'],lag1.df[,'meanlos'])


#-----------------search for the right percentile and also generate an average learning curve (aggregate over year) ------------------
bsDegree=2
lagvolvec=seq(min(nomiss.lag6[,'n.lag1.lap']),max(nomiss.lag6[,'n.lag1.lap']))
inner.knots.list=boundary.knots.list=fitList=list()
tileList=list(seq(0.1,0.9,0.2), seq(0.1,0.9,0.1), seq(0.1,0.9,0.3),seq(0.1,0.9,0.4), seq(0.1,0.9,0.5), c(0.1,0.7), c(0.2,0.7), 
              0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
meanLosbyvolTile=matrix(NA,nrow=length(lagvolvec),ncol=length(tileList))

for(i in 1:length(tileList)){
  print(i)
  log.lag1.innerKnots=quantile(anaDf.raw[,'n.lag1.lap.log'],prob=tileList[[i]],na.rm=T)
  cleaned.obj=finalCleaning(anaDf.raw, log.lag1.innerKnots, bsDegree) #degree=2
  cleaned=cleaned.obj$df
  n.basis=cleaned.obj$n.basis
  inner.knots=cleaned.obj$inner.knots$lag1
  boundary.knots=cleaned.obj$boundary.knots$lag1
  
  inner.knots.list=lappend.yz(inner.knots.list,inner.knots)
  boundary.knots.list=lappend.yz(boundary.knots.list,boundary.knots)
  
  chkMissVars.lag6=c('n.lag6.lap','n.lag6.nonLap','n.lag6.rap','age3cat','race5cat','year','comorbcat','pay1','mdnum1.r')
  
  nomiss.lag6 = droplevels(cleaned[complete.cases(cleaned[,chkMissVars.lag6]),])
  
#   testModel.lag1bs <- clm(los ~ n.lag1.lap.log.bs1+lag1.nlap.bs2+lag1.nlap.bs3+age4cat+race5cat+comorbcat+pay1+year+mdnum1.r, data=nomiss.lag6, link=c('probit'))
  
  cat('it is running bspline fit','\n')
  formu=passVarToFormula.yz('los',c(paste('n.lag1.lap.log.bs',seq(n.basis),sep=''),'age4cat', 'race5cat','comorbcat','pay1','year','mdnum1.r'))
  
  #get basis spline fit model
  bsFit=clm(formu, data=nomiss.lag6, link=c('probit'))
  fitList=lappend.yz(fitList,bsFit)
  
  #generate basis spline terms
  lagvol.bs=bsWrapper.yz(log1p(lagvolvec) #this is the x in bs function
               , log.lag1.innerKnots #inner knots, if it contains boundary knots, function will stop and issue erro message
               , 'n.lag1.lap.log.bs' #output data's columen name stem
               , degree=bsDegree
               , boundary.knots=boundary.knots
               , intercept=FALSE
               ,  dataType='data.frame'
               #or ='data.frame'
               , closenessCutoffPct=0.02 #closeness of inner knots to bdnots, lower than this number means it is too close.. and not good, 0.02 means two percent of range
  )$bsdata
  n.basis=ncol(lagvol.bs)
  nobsformu=passVarToFormula.yz('', c('age4cat', 'race5cat','comorbcat','pay1','year','mdnum1.r'))
  bsformu=passVarToFormula.yz('', paste('n.lag1.lap.log.bs',seq(n.basis),sep=''))
  noLagBsDf=deldfcols.yz(nomiss.lag6,c(paste('n.lag1.lap.log.bs',seq(n.basis),sep='')))
  x.noBs=model.matrix(nobsformu,noLagBsDf)
  b=bsFit$beta

  xbout.noBs=xb.matched.yz(b,x.noBs)
  xbout.noBs$unmatched
  xb.noBs=xbout.noBs$xb.matched
  xb.noBsPart=xbout.noBs$xb.matched
  meanLosVec=rep(NA,length(lagvolvec))
  for(vi in 1:length(lagvolvec)){
    print(vi)
    newDf.vi = as.data.frame(lagvol.bs[vi,,drop=F][rep(1,nrow(nomiss.lag6)),])
    x.bs=model.matrix(bsformu, data = newDf.vi)
    xbout.bs=xb.matched.yz(b,x.bs)
    xb.bs=xbout.bs$xb.matched
    #xbout.bs$matched
    xb=xb.bs+xb.noBs
    cuts=bsFit$alpha
    probMat=xbCuts2probmat.orderedprobit.yz(xb,cuts)
    meanLosVec[vi]=mean(probMat %*% matrix(seq(1:(length(cuts)+1)), ncol=1)) 
  }
  meanLosbyvolTile[,i]=meanLosVec 
  }

#-----------------search for the right percentile and generate average learning curve------------------
lagvolvec=seq(0,50)
bsDegree=2
k=15
nobsformu=passVarToFormula.yz('', c('age4cat', 'race5cat','comorbcat','pay1','year','mdnum1.r'))


avgLearn.allyear=getAvgLearnCurve(fitList[[k]]$beta #beta coefficient vector
                            ,fitList[[k]]$alpha #cuts coefficient vector
                            ,nobsformu #non Bspline part formula nobsformu=passVarToFormula.yz('', c('age4cat', 'race5cat','comorbcat','pay1','year','mdnum1.r'))
                            ,nomiss.lag6 #input data, the data used to fit model
                            ,as.character(seq(2004,2010)) #the years of interest
                            ,lagvolvec# LAP volume of inerest
                            ,bsDegree #bspline degreee
                            ,inner.knots.list[[k]] #innner knots of bspline
                            ,boundary.knots.list[[k]] #bspline boundary
                            ,yearVn='year' #appear in input data
                            , bsVnStem='n.lag1.lap.log.bs')


avgLearn.2004=getAvgLearnCurve(fitList[[k]]$beta #beta coefficient vector
                                  ,fitList[[k]]$alpha #cuts coefficient vector
                                  ,nobsformu #non Bspline part formula nobsformu=passVarToFormula.yz('', c('age4cat', 'race5cat','comorbcat','pay1','year','mdnum1.r'))
                                  ,nomiss.lag6 #input data, the data used to fit model
                                  ,as.character(2004) #the years of interest
                                  ,lagvolvec# LAP volume of inerest
                                  ,bsDegree #bspline degreee
                                  ,inner.knots.list[[k]] #innner knots of bspline
                                  ,boundary.knots.list[[k]] #bspline boundary
                                  ,yearVn='year' #appear in input data
                                  , bsVnStem='n.lag1.lap.log.bs')
avgLearn.2005=getAvgLearnCurve(fitList[[k]]$beta #beta coefficient vector
                               ,fitList[[k]]$alpha #cuts coefficient vector
                               ,nobsformu #non Bspline part formula nobsformu=passVarToFormula.yz('', c('age4cat', 'race5cat','comorbcat','pay1','year','mdnum1.r'))
                               ,nomiss.lag6 #input data, the data used to fit model
                               ,as.character(2005) #the years of interest
                               ,lagvolvec# LAP volume of inerest
                               ,bsDegree #bspline degreee
                               ,inner.knots.list[[k]] #innner knots of bspline
                               ,boundary.knots.list[[k]] #bspline boundary
                               ,yearVn='year' #appear in input data
                               , bsVnStem='n.lag1.lap.log.bs')

avgLearn.2006=getAvgLearnCurve(fitList[[k]]$beta #beta coefficient vector
                               ,fitList[[k]]$alpha #cuts coefficient vector
                               ,nobsformu #non Bspline part formula nobsformu=passVarToFormula.yz('', c('age4cat', 'race5cat','comorbcat','pay1','year','mdnum1.r'))
                               ,nomiss.lag6 #input data, the data used to fit model
                               ,as.character(2006) #the years of interest
                               ,lagvolvec# LAP volume of inerest
                               ,bsDegree #bspline degreee
                               ,inner.knots.list[[k]] #innner knots of bspline
                               ,boundary.knots.list[[k]] #bspline boundary
                               ,yearVn='year' #appear in input data
                               , bsVnStem='n.lag1.lap.log.bs')
avgLearn.2007=getAvgLearnCurve(fitList[[k]]$beta #beta coefficient vector
                               ,fitList[[k]]$alpha #cuts coefficient vector
                               ,nobsformu #non Bspline part formula nobsformu=passVarToFormula.yz('', c('age4cat', 'race5cat','comorbcat','pay1','year','mdnum1.r'))
                               ,nomiss.lag6 #input data, the data used to fit model
                               ,as.character(2007) #the years of interest
                               ,lagvolvec# LAP volume of inerest
                               ,bsDegree #bspline degreee
                               ,inner.knots.list[[k]] #innner knots of bspline
                               ,boundary.knots.list[[k]] #bspline boundary
                               ,yearVn='year' #appear in input data
                               , bsVnStem='n.lag1.lap.log.bs')

avgLearn.2008=getAvgLearnCurve(fitList[[k]]$beta #beta coefficient vector
                               ,fitList[[k]]$alpha #cuts coefficient vector
                               ,nobsformu #non Bspline part formula nobsformu=passVarToFormula.yz('', c('age4cat', 'race5cat','comorbcat','pay1','year','mdnum1.r'))
                               ,nomiss.lag6 #input data, the data used to fit model
                               ,as.character(2008) #the years of interest
                               ,lagvolvec# LAP volume of inerest
                               ,bsDegree #bspline degreee
                               ,inner.knots.list[[k]] #innner knots of bspline
                               ,boundary.knots.list[[k]] #bspline boundary
                               ,yearVn='year' #appear in input data
                               , bsVnStem='n.lag1.lap.log.bs')
avgLearn.2009=getAvgLearnCurve(fitList[[k]]$beta #beta coefficient vector
                               ,fitList[[k]]$alpha #cuts coefficient vector
                               ,nobsformu #non Bspline part formula nobsformu=passVarToFormula.yz('', c('age4cat', 'race5cat','comorbcat','pay1','year','mdnum1.r'))
                               ,nomiss.lag6 #input data, the data used to fit model
                               ,as.character(2009) #the years of interest
                               ,lagvolvec# LAP volume of inerest
                               ,bsDegree #bspline degreee
                               ,inner.knots.list[[k]] #innner knots of bspline
                               ,boundary.knots.list[[k]] #bspline boundary
                               ,yearVn='year' #appear in input data
                               , bsVnStem='n.lag1.lap.log.bs')
avgLearn.2010=getAvgLearnCurve(fitList[[k]]$beta #beta coefficient vector
                               ,fitList[[k]]$alpha #cuts coefficient vector
                               ,nobsformu #non Bspline part formula nobsformu=passVarToFormula.yz('', c('age4cat', 'race5cat','comorbcat','pay1','year','mdnum1.r'))
                               ,nomiss.lag6 #input data, the data used to fit model
                               ,as.character(2010) #the years of interest
                               ,lagvolvec# LAP volume of inerest
                               ,bsDegree #bspline degreee
                               ,inner.knots.list[[k]] #innner knots of bspline
                               ,boundary.knots.list[[k]] #bspline boundary
                               ,yearVn='year' #appear in input data
                               , bsVnStem='n.lag1.lap.log.bs')

avgLearn.20062010=getAvgLearnCurve(fitList[[k]]$beta #beta coefficient vector
                               ,fitList[[k]]$alpha #cuts coefficient vector
                               ,nobsformu #non Bspline part formula nobsformu=passVarToFormula.yz('', c('age4cat', 'race5cat','comorbcat','pay1','year','mdnum1.r'))
                               ,nomiss.lag6 #input data, the data used to fit model
                               ,as.character(seq(2006,2010)) #the years of interest
                               ,lagvolvec# LAP volume of inerest
                               ,bsDegree #bspline degreee
                               ,inner.knots.list[[k]] #innner knots of bspline
                               ,boundary.knots.list[[k]] #bspline boundary
                               ,yearVn='year' #appear in input data
                               , bsVnStem='n.lag1.lap.log.bs')

count(nomiss.lag6,'year')


avgLearnCurveByYear=data.frame(lag1LapVolume=lagvolvec
                               , meanLos2004=avgLearn.2004
                               , meanLos2005=avgLearn.2005
                               , meanLos2006=avgLearn.2006
                               , meanLos2007=avgLearn.2007
                               , meanLos2008=avgLearn.2008
                               , meanLos2009=avgLearn.2009
                               , meanLos2010=avgLearn.2010
                               , meanLos20062010=avgLearn.20062010
 )


plotAvgLearnDf=subset(avgLearnCurveByYear, lag1LapVolume<=40)

cex.pch=0.5
pdf('C:/Dropbox/paper/surgVol/optimalReferral/figure/avgLearnCurveByYear.pdf')
plot(plotAvgLearnDf[,'lag1LapVolume'], plotAvgLearnDf[,'meanLos2004'], ylim=c(1.6,2.4),xlab='Number of MIRP performed during the past 3 months', ylab='Expected length of stay', type='b',pch=1,cex=cex.pch)
points(plotAvgLearnDf[,'lag1LapVolume'], plotAvgLearnDf[,'meanLos2005'],pch=2,type='b',cex=cex.pch)
points(plotAvgLearnDf[,'lag1LapVolume'], plotAvgLearnDf[,'meanLos20062010'],pch=3,type='b',cex=cex.pch)
legend('topleft',pch=1:3, legend=c('2004','2005','2006-2010'),cex=1)
dev.off()


# Next I like to create an average learning curve but with a confidence interval around it.
#parametric sample the coefficients
#the 15th inner knots choice has the best model.
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

meanLosMat=matrix(NA,ncol=nbt,nrow=length(lagvolvec))
rownames(meanLosMat)=lagvolvec
colnames(meanLosMat)=seq(nbt)

vcov=solve(fit$Hessian) #this should be inv of negative hessian. but the output is already negative hessian...so I do not use negative
coeffMatrix=mnormt::rmnorm(n=nbt,mean=fit$coefficients, varcov=vcov)

nobsformu=passVarToFormula.yz('', c('age4cat', 'race5cat','comorbcat','pay1','year','mdnum1.r'))
n.basis=ncol(lagvol.bs)
bsformu=passVarToFormula.yz('', paste('n.lag1.lap.log.bs',seq(n.basis),sep=''))

nCuts=length(fit$alpha)

for (r in 1:nrow(coeffMatrix)){
  cuts=coeffMatrix[r,1:nCuts] 
  beta=coeffMatrix[r,(nCuts+1):ncol(coeffMatrix)]
 
  
  #the following block actually be replaced by getAvgLearnCurve(), if you have time
  noLagBsDf=deldfcols.yz(nomiss.lag6,c(paste('n.lag1.lap.log.bs',seq(n.basis),sep='')))
  x.noBs=model.matrix(nobsformu,noLagBsDf)
  xbout.noBs=xb.matched.yz(beta,x.noBs)
  #xbout.noBs$unmatched
  xb.noBs=xbout.noBs$xb.matched
  
  for(vi in 1:length(lagvolvec)){
    print(vi)
    newDf.vi = as.data.frame(lagvol.bs[vi,,drop=F][rep(1,nrow(nomiss.lag6)),])
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


ubLbLearnCurveTiles=c(0.10,0.90)
lbub=apply(meanLosMat,1,function(x){quantile(x,prob=ubLbLearnCurveTiles)})
lb=lbub[1,]
ub=lbub[2,]
#we choose the 15th model as the optimal

avgLearnCurve=data.frame(lag1LapVolume=lagvolvec, meanLos=meanLosbyvolTile[,15], lb=lb, ub=ub)

(avgLearnCurve[14,'meanLos']-avgLearnCurve[1,'meanLos'])/avgLearnCurve[1,'meanLos']

library(ggplot2)


#---------------------------get by year average learning curve
k=15
fit=fitList[[k]]
tileList[[k]]
plot(unlist(lapply(fitList,logLik)))
lagvol.bs=bsWrapper.yz(log1p(lagvolvec) #this is the x in bs function
                       , inner.knots.list[[k]] #inner knots, if it contains boundary knots, function will stop and issue erro message
                       , 'n.lag1.lap.log.bs' #output data's columen name stem
                       , degree=bsDegree
                       , boundary.knots=boundary.knots.list[[k]]
                       , intercept=FALSE
                       ,  dataType='data.frame'
                       #or ='data.frame'
                       , closenessCutoffPct=0.02 #closeness of inner knots to bdnots, lower than this number means it is too close.. and not good, 0.02 means two percent of range
)$bsdata

ncol(lagvol.bs)

getAvgLearnCurveByYear= function(fit, whatYears,lagvolvec,lagvol.bs){
  meanLosVec=rep(NA,length(lagvolvec))
  names(meanLosVec)=lagvolvec
  
  cuts=fit$alpha 
  beta=fit$beta
  
  yearDf=subset(nomiss.lag6,year %in% whatYears)
#   yearDf=nomiss.lag6
#   yearDf[,'year']=factor(rep(whatYears, nrow(nomiss.lag6)),levels=levels(nomiss.lag6[,'year']))
  noLagBsDf=deldfcols.yz(yearDf,c(paste('n.lag1.lap.log.bs',seq(ncol(lagvol.bs)),sep='')))
  x.noBs=model.matrix(nobsformu,noLagBsDf)
  xbout.noBs=xb.matched.yz(beta,x.noBs)
  #xbout.noBs$unmatched
  xb.noBs=xbout.noBs$xb.matched
  
  for(vi in 1:length(lagvolvec)){
    print(vi)
    newDf.vi = as.data.frame(lagvol.bs[vi,,drop=F][rep(1,nrow(yearDf)),])
    bsformu=passVarToFormula.yz('', paste('n.lag1.lap.log.bs',seq(ncol(lagvol.bs)),sep=''))
    x.bs=model.matrix(bsformu, data = newDf.vi)
    xbout.bs=xb.matched.yz(beta,x.bs)
    xb.bs=xbout.bs$xb.matched
    #xbout.bs$matched
    xb=xb.bs+xb.noBs
    probMat=xbCuts2probmat.orderedprobit.yz(xb,cuts)
    meanLosVec[vi]=mean(probMat %*% matrix(seq(1:(length(cuts)+1)), ncol=1)) 
    
  }
  return(meanLosVec)
  
}


avgLearn.2004=getAvgLearnCurveByYear(fit, yearVec,lagvolvec,lagvol.bs)
plot(avgLearn.2004)

which.min(avgLearn.2004)
which.min(avgLearn.2004)
avgLearn.2005=getAvgLearnCurveByYear(fit, '2005',lagvolvec,lagvol.bs)
avgLearn.2006=getAvgLearnCurveByYear(fit, '2006',lagvolvec)
avgLearn.2007=getAvgLearnCurveByYear(fit, '2007',lagvolvec)
avgLearn.2008=getAvgLearnCurveByYear(fit, '2008',lagvolvec)
avgLearn.2009=getAvgLearnCurveByYear(fit, '2009',lagvolvec)
avgLearn.2010=getAvgLearnCurveByYear(fit, '2010',lagvolvec)
avgLearn.20062010=getAvgLearnCurveByYear(fit, c('2006','2007','2008','2009','2010'),lagvolvec)

avgLearnCurveByYear= data.frame(lag1LapVolume=lagvolvec
             
                                , meanlos.2004=avgLearn.2004
                                , meanlos.2005=avgLearn.2005
           ,meanlos.2006=avgLearn.2006
           ,meanlos.2007=avgLearn.2007
           ,meanlos.2008=avgLearn.2008
           ,meanlos.2009=avgLearn.2009
          ,meanlos.2010=avgLearn.2010
          ,meanlos.20062010=avgLearn.20062010)


which.min(plotPart[,'meanlos.2005']) #8
which.min(avgLearnCurve[,'meanLos']) #14

#I like to see large number i.e., 14 in the past three month...but I cannot see that once I was try to get
#year specific learning curve. so I probably do not want to present year specific leanring curve.
#this should be a concern....

plotPart=subset(avgLearnCurveByYear,lag1LapVolume<=40)

which.min(plotPart[,'meanlos.2006'])

plot(plotPart[,'lag1LapVolume'], plotPart[,'meanlos.2004'],ylim=c(1.5,2.3))
lines(plotPart[,'lag1LapVolume'], plotPart[,'meanlos.2005'])
lines(plotPart[,'lag1LapVolume'], plotPart[,'meanlos.2006'])
lines(plotPart[,'lag1LapVolume'], plotPart[,'meanlos.2007'])
lines(plotPart[,'lag1LapVolume'], plotPart[,'meanlos.2008'])
lines(plotPart[,'lag1LapVolume'], plotPart[,'meanlos.2009'])
lines(plotPart[,'lag1LapVolume'], plotPart[,'meanlos.2010'])

plot(plotPart[,'lag1LapVolume'], plotPart[,'meanlos.2004'],ylim=c(1.5,2.3),type='l')
lines(plotPart[,'lag1LapVolume'], plotPart[,'meanlos.2005'])
lines(plotPart[,'lag1LapVolume'], plotPart[,'meanlos.20062010'])


pdf('C:/Dropbox/paper/surgVol/optimalReferral/figure/avgLearnCurve.pdf')
qplot(lag1LapVolume, meanLos,data=subset(avgLearnCurve,lag1LapVolume<=40) , xlab='Number of MIRP performed during the past 3 months', ylab='Expected length of stay')+geom_smooth(aes(ymin=lb, ymax=ub), data=subset(avgLearnCurve,lag1LapVolume<=40), stat="identity")+opts(axis.line = theme_segment(colour = "black"),panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(),panel.border = theme_blank(),panel.background = theme_blank()) 
dev.off()



#the optimal is 13 RAP per quarter


dat <- data.frame(n.lag1.lap= nomiss.lag6[,'n.lag1.lap'], aboveCutoff=NA)

for(i in 1:nrow(dat)){
  
  if (dat[i,'n.lag1.lap']>13) {dat[i,'aboveCutoff']='greater than 13'} else {dat[i,'aboveCutoff']='less than 13'}
  
}

library(ggplot2)
qplot(n.lag1.lap,data=dat,geom="histogram",xlab="Surgeon's last quarter MIRP volume",ylab='Number of MIRP',fill=aboveCutoff, binwidth=0.5)+opts(axis.line = theme_segment(colour = "black"),panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(),panel.border = theme_blank(),panel.background = theme_blank()) 
library(ggplot2)

#the following is code for histogram with two colors
p=ggplot(nomiss.lag6,aes(n.lag1.lap)) +
  geom_histogram(binwidth=1,fill="white",color="black") +
  geom_histogram(data=subset(nomiss.lag6,n.lag1.lap<13),binwidth=1, 
                 colour="black", fill="grey") +opts(axis.line = theme_segment(colour = "black"),panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(),panel.border = theme_blank(),panel.background = theme_blank()) 

pdf('C:/Dropbox/paper/surgVol/optimalReferral/figure/mirpUnexperiencedSrgeon.pdf')
p+scale_x_continuous("Performing surgeons' past quarter MIRP volume")+scale_y_continuous("Count of MIRP")
dev.off()

sum(nomiss.lag6[,'n.lag1.lap']<13)/nrow(nomiss.lag6)

#[1] 0.55191965

#now I like to get the average learning curve.
#the learning curve is obtained 
# I just perturb each individual docotors past MIRP amount 

#plotCI(avgLearnCurve[,'lag1LapVolume'], avgLearnCurve[,'meanLos'], ui=avgLearnCurve[,'ub'], li=avgLearnCurve[,'lb'], lwd=0.01)


#likelihood ratio test for a linear relationship model, reject linear linear curve model, so good
bsFit.linear=clm(los ~ n.lag1.lap.log + 
            age4cat + race5cat + comorbcat + pay1 + year + mdnum1.r, data=nomiss.lag6, link=c('probit'))

lrt.yz=function(model.full,model.short){
  1-pchisq(2*(logLik(model.full)[1]-logLik(model.short)[1]), model.short$df.residual- model.full$df.residual)
}

lrt.yz(fitList[[15]],bsFit.linear)
#our test reject the null of a linear model with a pvalue of 5.4829577e-06


#--------------------need to do optimization problem and predict savings of LOS through centratlization---------------


#------first get within state individaul learning curve---------------

#genAverage Learning curve based on mean beta and mean cuts
# According to standard likelihood theory, the variance-covariance matrix of the parameters
# can be obtained as the inverse of the observed Fisher information matrix. This matrix is
# given by the negative Hessian of the log-likelihood function7 evaluated at the maximum
# likelihood estimates.




#save the results
# save(anaDf.raw, nomiss.lag6, fitList,meanLosbyvolTile, tileList, lagvolvec, file="Z:/j_scrdata/lapLearn/bsplineSensitivity_may222013.RData")
(load(file="Z:/j_scrdata/lapLearn/bsplineSensitivity_may222013.RData"))

k=15
fit=fitList[[k]]
model.matrix.Formula.rhsPatYear=passVarToFormula.yz('',c('age4cat', 'race5cat','comorbcat','pay1','year'))
inner.knots=inner.knots.list[[k]]
boundary.knots=boundary.knots.list[[k]]
degree=2 #bspline degree
lagVolSeq=seq(0,50) #the is the range of lag1 n.lap

#for a coefficient vector drawn from coefficient matrix
#for an individaul surgeon
#pull out patient data of the surgeon's residing state
#then alter the surgeon's volume to get his individaul learning curve, then spit out a g.list

# nbt=100
# coeffMatrix=mnormt::rmnorm(n=nbt,mean=fit$coefficients, varcov=vcov)
# r=1
# nCuts=length(fit$alpha)
# cuts=coeffMatrix[r,1:nCuts] 
# beta=coeffMatrix[r,(nCuts+1):ncol(coeffMatrix)]
# model.matrix.Formula.rhsPatYear=passVarToFormula.yz('',c('age4cat', 'race5cat','comorbcat','pay1','year'))
# inner.knots=inner.knot.list[[15]]
# boundary.knots= boundary.knots.list[[15]]
# degree=2


#the following are two functions used for getting within-state learning curve
indivDocWithinStateLearnCurve = function(
  cuts #cuts are extracted from fit model's mean and vcov
  , betaVec #betaVec extracted from fit model's mean and vcov
  , mdId 
  , model.matrix.Formula.rhsPatYear
  , inner.knots #inner knots
  , boundary.knots #nomiss.lag6[,'n.lap.loag1']
  , degree #degree used to generate bs
  #can be empty, if so then surgeon specific intercept is zero
  #if specified, it is a character vector
  , patientVarsYearDf #should be corresponds to the surgeon's state
  , lagVolSeq #the lag volume sequence used for generating leanring curve
  , surgeonIdFactorVn #in fit. it is 'mdnum1.r'
){
  
  meanLosVec=rep(NA,length(lagVolSeq))
  
  #basically score each surgeon for all the patients belong to a region (e.g., a state, or a county or something) he resides.  
  J=length(cuts)+1 #number of ordered choices
  
  x.patYear=model.matrix(model.matrix.Formula.rhsPatYear, data=patientVarsYearDf)
  xbout.patYear=xb.matched.yz(beta,x.patYear)
  xbout.patYear$unmatched.b
  xb.noBs=xbout.patYear$xb.matched
  #subset(nomiss.lag6,n.lag1.lap==9,select=c('n.lag1.lap.log.bs1','n.lag1.lap.log.bs2','n.lag1.lap.log.bs3'))
  for(i in 1:length(lagVolSeq)){
    cat('processing i=',i,'\n')
    
    x.volSpline=bsWrapper.yz(log1p(lagVolSeq[i]) #this is the x in bs function
                             , inner.knots #inner knots, if it contains boundary knots, function will stop and issue erro message
                             , 'n.lag1.lap.log.bs' #output data's columen name stem
                             , degree=2
                             , boundary.knots=boundary.knots
                             , intercept=FALSE
                             , dataType='matrix'
    )$bsdata[rep(1,length(xb.noBs)),]
    
    xbout.volBs=xb.matched.yz(beta,x.volSpline)
    xb.volBs=xbout.volBs$xb.matched
    xbout.volBs$unmatched.b
    #     mdId='746'
    #     surgeonIdFactorVn='mdnum1.r'
    mdDummy= paste(surgeonIdFactorVn,mdId,sep='')
    if (mdDummy %in% names(beta)){
      xb=xb.volBs+xb.noBs+beta[mdDummy]}
    else{
      xb=xb.volBs+xb.noBs
    }
    
    probMat = xbCuts2probmat.orderedprobit.yz(xb,cuts)
    meanLosVec[i]=mean(probMat %*% matrix(seq(1,J), ncol=1)) 
  }
  
  return(meanLosVec)
  
}#function

names(nomiss.lag6)


learnCurves.withinStateYear=function(cuts, betaVec, docs, stateName, indf, inner.knots, boundary.knots, degree, lagVolSeq, surgeonIdFactorVn="mdnum1.r"){
  learnList=list()
  
  for(i in 1:length(docs)){ 
    indiv.learn=indivDocWithinStateLearnCurve(
      cuts #cuts are extracted from fit model's mean and vcov
      , betaVec #betaVec extracted from fit model's mean and vcov
      , docs[i]
      , model.matrix.Formula.rhsPatYear
      , inner.knots #inner knots
      , boundary.knots #nomiss.lag6[,'n.lap.loag1']
      , degree #degree used to generate bs
      #can be empty, if so then surgeon specific intercept is zero
      #if specified, it is a character vector
      , subset(indf,hospst==stateName) #should be corresponds to the surgeon's state
      , lagVolSeq #the lag volume sequence used for generating leanring curve
      , surgeonIdFactorVn #in fit. it is 'mdnum1.r'
    )
    learnList=lappend.yz(learnList, indiv.learn)
  }
  names(learnList)=docs
  return(learnList) 
}

#extract MDs by states
ia.docs=as.character(unique(subset(nomiss.lag6,hospst=='IA')[,'mdnum1.r']))
ny.docs=as.character(unique(subset(nomiss.lag6,hospst=='NY')[,'mdnum1.r']))
md.docs=as.character(unique(subset(nomiss.lag6,hospst=='MD')[,'mdnum1.r']))



yearVec=as.character(seq(2004,2010))

ia.curves.byYear=md.curves.byYear=ny.curves.byYear=list()

for(y in 1:length(yearVec)){
  
  ia.curves.y=learnCurves.withinStateYear(cuts, betaVec, ia.docs,'IA', subset(nomiss.lag6, year==yearVec[y]), inner.knots, boundary.knots, degree, lagVolSeq, surgeonIdFactorVn="mdnum1.r")
  
  ia.curves.byYear=lappend.yz(ia.curves.byYear, ia.curves.y)

}

for(y in 1:length(yearVec)){
  
  md.curves.y=learnCurves.withinStateYear(cuts, betaVec, md.docs,'MD', subset(nomiss.lag6, year==yearVec[y]), inner.knots, boundary.knots, degree, lagVolSeq, surgeonIdFactorVn="mdnum1.r")
  
  md.curves.byYear=lappend.yz(md.curves.byYear, ia.curves.y)
 
}

for(y in 1:length(yearVec)){
  
  ny.curves.y=learnCurves.withinStateYear(cuts, betaVec, ny.docs,'NY', subset(nomiss.lag6, year==yearVec[y]), inner.knots, boundary.knots, degree, lagVolSeq, surgeonIdFactorVn="mdnum1.r")
  
  ny.curves.byYear=lappend.yz(ny.curves.byYear, ny.curves.y)

}

names(ia.curves.byYear)=names(md.curves.byYear)=names(ny.curves.byYear)=yearVec

str(ny.curves.byYear)


#next, I would then solve for the optimal solution and compare the optimal solution to observed los


#set coverage integer linear programming, patient allocation
scilpPatAlloc.yz <- function(N
                             , G.list #G is a list of payoff, with 0 patient, 1, 2......a maximum number of patients
                             , allTreated=TRUE){
  J <- length(G.list)
  #note if TM_OPTIMAL_SOLUTION_FOUND==0 means found optimal, if NULL then not found
  Mvec <- unlist(lapply(G.list,length))-1
  Mmax <- max(Mvec)
  
  if (allTreated){stopifnot(sum(Mvec)>=N)}
  
  constMat <- matrix(rep(0,(J+1)*sum(Mvec+1)),nrow=(J+1))
  constrColnameList <- list()
  for (j in 1:J){
    for (i in 0:Mvec[j]) {constrColnameList<-lappend.yz(constrColnameList, paste(j,':',i,sep=''))}
  }                   
  
  constList.Jrows <-list()
  
  for(j in 1:J){
    constrMat.Jrows.jthDoc = matrix(rep(0,J*(Mvec[j]+1)),nrow=J)
    constrMat.Jrows.jthDoc[j,] <- 1
    constList.Jrows <- lappend.yz(constList.Jrows,constrMat.Jrows.jthDoc)
  }
  
  Jplus1List <- list()
  for(j in 1:J){Jplus1List <- lappend.yz(Jplus1List,c(0,seq(Mvec[j])))}
  Jplus1Row <- unlist(Jplus1List)                     
  constMat <- rbind(do.call('cbind',constList.Jrows),Jplus1Row)
  colnames(constMat) <- unlist(constrColnameList)
  rownames(constMat) <- c(paste('doc',seq(J),sep=''),'all.doc')
  if (allTreated){constDir <- c(rep('==', J),'==')} else {constDir <- c(rep('==',J),'<=')}
  
  rhs <- c(rep(1,J),N)
  types <- rep("B", J*(Mmax+1))
  
  Gvec=unlist(G.list)
  #ensure when you want, all patints be treated it is possible
  stopifnot(length(Gvec) == sum(Mvec+1))
  
  fit <- Rsymphony_solve_LP(Gvec, constMat, constDir, rhs,type=types, max = F) #false means minimize
  opt.n <- subset(data.frame(chosenDecision=fit$solution,n.treated=Jplus1Row),chosenDecision==1)[,'n.treated']
  
  names(opt.n) <- paste('j=',seq(J),sep='')
  fit$opt.n <- opt.n
  fit$total.opt.n <- sum(opt.n)
  fit$solution <- NULL
  return(fit)
}

ia.curves.byYear[[1]]
learnCurveList=ia.curves.byYear

getGlist=function(learnCurveList){
  outlist=lapply(learnCurveList,function(x){out=cumsum(c(0,x));return(out)})
  return(outlist)
  }

G.list.ia.2004=getGlist(ia.curves.byYear$'2004')
G.list.md.2004=getGlist(md.curves.byYear$'2004')
G.list.ny.2004=getGlist(ny.curves.byYear$'2004')

G.list.ia.2005=getGlist(ia.curves.byYear$'2005')
G.list.md.2005=getGlist(md.curves.byYear$'2005')
G.list.ny.2005=getGlist(ny.curves.byYear$'2005')


G.list.ia.2006=getGlist(ia.curves.byYear$'2006')
G.list.md.2006=getGlist(md.curves.byYear$'2006')
G.list.ny.2006=getGlist(ny.curves.byYear$'2006')

G.list.ia.2007=getGlist(ia.curves.byYear$'2007')
G.list.md.2007=getGlist(md.curves.byYear$'2007')
G.list.ny.2007=getGlist(ny.curves.byYear$'2007')

G.list.ia.2008=getGlist(ia.curves.byYear$'2008')
G.list.md.2008=getGlist(md.curves.byYear$'2008')
G.list.ny.2008=getGlist(ny.curves.byYear$'2008')

G.list.ia.2009=getGlist(ia.curves.byYear$'2009')
G.list.md.2009=getGlist(md.curves.byYear$'2009')
G.list.ny.2009=getGlist(ny.curves.byYear$'2009')

G.list.ia.2010=getGlist(ia.curves.byYear$'2010')
G.list.md.2010=getGlist(md.curves.byYear$'2010')
G.list.ny.2010=getGlist(ny.curves.byYear$'2010')


#G.list=G.list.ny.2004
random.seeds=seq(30)
picked.n.md=10
n.surgery=100


solveOptAllocation=function(n.surgery, G.list, random.seeds, picked.n.md=10){
#you cannot solve the problem by feeding all the MD
#however, you can randomly pick MDs assume you have no prior information about MD quality
avgVol=optNumMd=minLos=rep(NA,length(random.seeds))
optSolution.list=list()
for(i in 1:length(random.seeds)){
  set.seed(random.seeds[i])
  pickedMdIndx = sample(seq(length(G.list)), picked.n.md ,replace=FALSE)  
  G.list.picked=G.list[pickedMdIndx]
opt.obj=scilpPatAlloc.yz(n.surgery
  , G.list.picked #G is a list of payoff, with 0 patient, 1, 2......a maximum number of patients
  , allTreated=TRUE)
minLos[i]=opt.obj$objval
optNumMd[i]=length(which(opt.obj$opt.n>0))
avgVol[i]=sum(opt.obj$opt.n)/optNumMd[i]
  optSolution.list=lappend.yz(optSolution.list,opt.obj)
}
summaryDf=cbind(random.seeds, minLos, optNumMd, avgVol)
names(summaryDf)=c('random.seeds', 'minLos', 'optNumMd', 'avgVol')
return(list(optSolution.summary=summaryDf, optSolution.list=optSolution.list))
}



solveByQuarter = function(quarterLap, n.random.seeds){
  n.md.opt=vol.mean.opt=vol.std.opt=los.opt=rep(NA,nrow(quarterLap)) 
  avgVolPerformingMdList=nMdOptList=losVecOptList=list()
  
  for (i in 1:nrow(quarterLap)){
    cat('processing i=',i, 'out of', nrow(quarterLap), 'rows', '\n')
    state=quarterLap[i,'hospst']
    year=quarterLap[i,'year']
    right.G.list=get(paste('G.list', tolower(state),year,sep='.'))
    
    tmp=solveOptAllocation(quarterLap[i,'n.lap'], right.G.list, seq(n.random.seeds), picked.n.md=10)  
    
    #record details for each random seed solution
    nMdOptVec=unlist(lapply(tmp$optSolution.list,function(x){sum(x$opt.n>0)}))
    minLosVec=unlist(lapply(tmp$optSolution.list,function(x){x$objval}))
    
    avgVolVec=unlist(lapply(tmp$optSolution.list,function(x){mean(x$opt.n[which(x$opt.n>0)])}))
    
    names(avgVolVec)=names(minLosVec)=names(nMdOptVec)=paste('seed',seq(n.random.seeds))
    
    losVecOptList=lappend.yz(losVecOptList,minLosVec)
    nMdOptList=lappend.yz(nMdOptList,nMdOptVec)
    avgVolPerformingMdList=lappend.yz(avgVolPerformingMdList,avgVolVec)
    
    n.md.opt[i]=mean(tmp$optSolution.summary[,'optNumMd'])
    vol.mean.opt[i]=mean(tmp$optSolution.summary[,'avgVol'])
    vol.std.opt[i]=sd(tmp$optSolution.summary[,'avgVol'])
    los.opt[i]=mean(tmp$optSolution.summary[,'minLos'])
  }
  detailed.losOpt=as.data.frame(do.call(rbind,losVecOptList))
  detailed.avgVolOpt=as.data.frame(do.call(rbind, avgVolPerformingMdList))
  detailed.numMdOpt=as.data.frame(do.call(rbind,nMdOptList))
  names(detailed.losOpt)=paste('optLos.','seed.',seq(n.random.seeds),sep='')
  names(detailed.numMdOpt)=paste('optNumMd.','seed.',seq(n.random.seeds),sep='')
  names(detailed.avgVolOpt)=paste('optAvgVol.','seed.',seq(n.random.seeds),sep='')
  
  summaryDf=cbind(quarterLap,n.md.opt=n.md.opt, vol.mean.opt=vol.mean.opt, vol.std.opt=vol.std.opt, los.opt=los.opt, detailed.losOpt, detailed.numMdOpt,detailed.avgVolOpt)
  
 return(summaryDf)  
}

#vol.mean.obs is the number of LAP in the quarter for surgeons who have at least one surgery in the quarter
quarterLap=ddply(nomiss.lag6,c('hospst','dqtr','year'),function(x){c(n.lap=nrow(x),los.obs=sum(as.numeric(x[,'los'])), n.md.obs=length(unique(x[,'mdnum1.r'])), vol.mean.obs=nrow(x)/length(unique(x[,'mdnum1.r'])))})

opt.by.st.quarter=solveByQuarter(quarterLap, 30)

names(opt.by.st.quarter)

(sum(opt.by.st.quarter[,'los.obs'])-sum(opt.by.st.quarter[,'los.opt']))/sum(opt.by.st.quarter[,'los.obs'])

#I like to lump them into year


error.bar <- function(x, y, upper, lower=upper, length=0.05,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
  legend('topleft',legend=c('observed','optimal'), fill=c("grey","white"), inset=c(0.01,0.01))
  box()
} 

names(opt.by.st.quarter)
head(opt.by.st.quarter)

#30 is number of random seeds
nMdPlotDf=ddply(opt.by.st.quarter,'year', function(x){
  mean.obs.n.md.perq=mean(x[,'n.md.obs'])
  mean.opt.n.md.perq=rep(NA,30)
  #get numMd opt
  opt.n.md.vns=paste('optNumMd.seed.',seq(30), sep='')
  for(i in 1:30){
  mean.opt.n.md.perq[i]=mean(x[,opt.n.md.vns[i]])
  }
 quanout= quantile(mean.opt.n.md.perq, prob=c(0.025,0.975))
  out=c(mean.obs.n.md.perq, mean(mean.opt.n.md.perq), quanout)
 names(out)=c('mean.obs.n.md.perq','mean.opt.n.md.perq','mean.opt.n.md.perq.lb','mean.opt.n.md.perq.ub')
  return(out)
})

yy.nmd=t(as.matrix(nMdPlotDf[,c(2,3)]))
lower.nmd=rbind(rep(0.0001,7),nMdPlotDf[,3]-nMdPlotDf[,4])
upper.nmd=rbind(rep(0.0001,7),nMdPlotDf[,5]-nMdPlotDf[,3])

dev.off()

barPlotWithErrorBar(  yy.nmd #can be vector, number of column or length is number of blocks #each block can have one or nrow stacked bars
                               , lower.nmd #can be vector
                               , upper.nmd #can be vector
                               , c(0,20)
                               , seq(2004,2010) #i.e., values on the x axis
                               , c("grey","white") #a vector with length of nrow ymat there are ncol block, each block has nrow stached bars
                               , "Year"
                               , "Average number of performing MIRP surgeons per quarter"
                               , 0.04
                      , 'topleft'
                      , c('observed', 'optimal')
                      , c(0.01,0.01)
                               ,'C:/Dropbox/paper/surgVol/optimalReferral/figure/numMdPlot.pdf'
                               , axis.lty=1
                               , beside=T
)

dev.off()

pdf('C:/Dropbox/paper/surgVol/optimalReferral/figure/numMdPlot.pdf')
barx.nmd <- barplot(yy.nmd, beside=TRUE,col=c("grey","white"), ylim=c(0,20), names.arg=seq(2004,2010), axis.lty=1, xlab="Year", ylab="Average number of MIRP-performing surgeons per quarter")
error.bar(barx.nmd,yy.nmd,lower.nmd,upper.nmd)
dev.off()

#next get another plot (stop here)
names(opt.by.st.quarter)
avgvolperfmdPlotDf=ddply(opt.by.st.quarter,'year', function(x){
  mean.obs.vol.perq=mean(x[,'vol.mean.obs'])
  mean.opt.vol.perq=rep(NA,30)
  #get numMd opt
  opt.volpermdf.vns=paste('optAvgVol.seed.',seq(30), sep='')
  for(i in 1:30){
    mean.opt.vol.perq[i]=mean(x[,opt.volpermdf.vns[i]])
  }
  quanout= quantile(mean.opt.vol.perq, prob=c(0.025,0.975))
  out=c(mean.obs.vol.perq, mean(mean.opt.vol.perq), quanout)
  names(out)=c('mean.obs.avgVol.perq','mean.opt.avgVol.perq','mean.opt.avgVol.perq.lb','mean.opt.avgVol.perq.ub')
  return(out)
})

yy.volpermd=t(as.matrix(avgvolperfmdPlotDf[,c(2,3)]))
lower.volpermd=rbind(rep(0.0001,7),avgvolperfmdPlotDf[,3]-avgvolperfmdPlotDf[,4])
upper.volpermd=rbind(rep(0.0001,7),avgvolperfmdPlotDf[,5]-avgvolperfmdPlotDf[,3])

pdf('C:/Dropbox/paper/surgVol/optimalReferral/figure/avgvolperfMdPlot.pdf')
barx.volpermd <- barplot(yy.volpermd, beside=TRUE,col=c("grey","white"), ylim=c(0,40), names.arg=seq(2004,2010), axis.lty=1, xlab="Year", ylab="Average number of MIRPs per performing surgeon per quarter")
error.bar(barx.volpermd,yy.volpermd,lower.volpermd,upper.volpermd)
dev.off()


#next get another plot

pdf('C:/Dropbox/paper/surgVol/optimalReferral/figure/losSavingPlot.pdf')

dev.off()


#0.23780574 so 23.8% reduction


#  
# 
# save(anaDf.raw
#      , nomiss.lag6
#       , tileList,inner.knots.list
#       , boundary.knots.list
#       , fitList,meanLosbyvolTile
#       , lagvolvec
#       ,ubLbLearnCurveTiles
#       ,avgLearnCurveByYear
#       , avgLearnCurve
#       , bsFit.linear
# , ia.docs
# , ny.docs
# , md.docs
# , ia.curves
# , md.curves
# , ny.curves
# , ia.curves.byYear
#       , md.curves.byYear
#       ,ny.curves.byYear
# ,quarterLap
# 
# ,G.list.ia.2004
# ,G.list.md.2004
# ,G.list.ny.2004
# 
# ,G.list.ia.2005
# ,G.list.md.2005
# ,G.list.ny.2005
# 
# 
# ,G.list.ia.2006
# ,G.list.md.2006
# ,G.list.ny.2006
# 
# ,G.list.ia.2007
# ,G.list.md.2007
# ,G.list.ny.2007
# 
# ,G.list.ia.2008
# ,G.list.md.2008
# ,G.list.ny.2008
# 
# ,G.list.ia.2009
# ,G.list.md.2009
# ,G.list.ny.2009
# 
# ,G.list.ia.2010
# ,G.list.md.2010
# ,G.list.ny.2010
# 
# ,quarterLap
# ,opt.by.st.quarter
# 
# ,file="Z:/j_scrdata/lapLearn/bsplineSensitivity_may232013.RData")
#  
(load(file="Z:/j_scrdata/lapLearn/bsplineSensitivity_may232013.RData"))


#get table 1

nomiss.lag6
names(nomiss.lag6)

table.yz(indep.catvns,depcat.vn,df)

tabout = table.yz(c('age4cat', 'race5cat','comorbcat','pay1','year'),'los',nomiss.lag6)

xtable(do.call('rbind',tabout$cellsize.list))

xtable(tabout$tabout.mat)


ddply(nomiss.lag6, 'los', function(x){c(mean=mean(x[,'n.lag1.lap']),sd=sd(x[,'n.lag1.lap']))})

anova(aov(n.lag1.lap ~ los, data=nomiss.lag6))



table(nomiss.lag6[,'comorbcat'])
table(nomiss.lag6[,'year'])

scilpPatAlloc.yz(50,G.list.ia.2004[10:20])
scilpPatAlloc.yz(200,G.list.ia.2008[1:10])


names(nomiss.lag6)


fitted(fit)

?VGAM::predict



names(nomiss.lag6)

xb.obj=xb.matched.yz(fit$beta,
                     model.matrix(passVarToFormula.yz('los',c(paste('n.lag1.lap.log.bs',seq(3),sep=''),'age4cat', 'race5cat','comorbcat','pay1','year','mdnum1.r'))
                                  , nomiss.lag6)
)
xb=xb.obj$xb.matched
predProbMat=xbCuts2probmat.orderedprobit.yz(xb,fit$alpha)


cbind(nomiss.lag6[,'los'],predProbMat, predict(fit, newdata=nomiss.lag6)$fit)
cbind(nomiss.lag6$los, predict(fit, newdata=nomiss.lag6)$fit)



mean(predProbMat %*% matrix(seq(ncol(predProbMat)), ncol=1)) # 1.6917993
mean(as.numeric(nomiss.lag6[,'los'])) #1.692208


#the predictions are very close, so it is good sign.

cbind(nomiss.lag6[,'los'], predProbMat)

ddply(data.frame(los=nomiss.lag6[,'los'], predProbMat),'los', function(x){mean(as.matrix(x[,2:8])%*%matrix(seq(ncol(predProbMat)), ncol=1))})

# los        V1
# 1   1 1.4139851
# 2   2 1.9248169
# 3   3 2.0889311
# 4   4 2.1296789
# 5   5 2.2418668
# 6   6 2.2674645
# 7   7 2.8553684


#generate average learning cruve

testDf=nomiss.lag6
volumeVec=range(nomiss.lag6[,'n.lag1.lap'])

i=1
bsDegree=2
boundary.knots[[15]]

for(i in 1:length(volumeVec)){
  
  
  lagvol.bs=bsWrapper.yz(log1p(volumeVec[i]) #this is the x in bs function
                         , log.lag1.innerKnots #inner knots, if it contains boundary knots, function will stop and issue erro message
                         , 'n.lag1.lap.log.bs' #output data's columen name stem
                         , degree=bsDegree
                         , boundary.knots=boundary.knots
                         , intercept=FALSE
                         ,  dataType='data.frame'
                         #or ='data.frame'
                         , closenessCutoffPct=0.02 #closeness of inner knots to bdnots, lower than this number means it is too close.. and not good, 0.02 means two percent of range
  )$bsdata
  
  bsPart=lagvol.bs[rep(1,nrow(nomiss.lag6)),]
 
cbind(deldfcols.yz(nomiss.lag6,c("n.lag1.lap.log.bs1", "n.lag1.lap.log.bs2", "n.lag1.lap.log.bs3")),)
 
  
}
names(nomiss.lag6)


xb.obj=xb.matched.yz(fit$beta,
                     model.matrix(passVarToFormula.yz('los',c(paste('n.lag1.lap.log.bs',seq(3),sep=''),'age4cat', 'race5cat','comorbcat','pay1','year','mdnum1.r'))
                                  , nomiss.lag6)
)
xb=xb.obj$xb.matched
predProbMat=xbCuts2probmat.orderedprobit.yz(xb,fit$alpha)





pred=predict(fit, newdata=nomiss.lag6)
str(pred)
str(fit)
predicted(fit)

#I chose k=15, tileList[[15]], inner knot=0.8 i.e., 80 quantile which is also the best fit
# plot(seq(length(tileList)),unlist(lapply(fitList,logLik)),type='b')
#how I decide the final model basis spline selection.
#note from tileList[[1]] and tileList[[2]] has 5 and 9 mid points, so it is overfitting.
#the valid comparison is among tileList[[3]] on, from three mid points to even single mid point.
#at last tileList[[15]] which has only one single point has the best fit, so we need to use it.
pdf('C:/Dropbox/paper/surgVol/optimalReferral/figure/splineSelectionLogLikelihood.pdf')
plot(seq(length(tileList[3:16])),unlist(lapply(fitList[3:16],logLik)),type='b', xlab='Different choice of inner knots', ylab='log-likelihood', main='Inner knots selection based on likelihoold')
dev.off()

#the 15th model is the best I would pick, so what I need is then based on the model to predict run optimization problem.
head(summary(fitList[[15]])$coefficients,12)

#----------------next is to solve the optimization problem--------------------


#the idea we have is to solve for the optimal solution






























    
    
 
 
  






















  fit.list=lappend.yz(fit.list,testModel.lag1bs)
  
  bsMat = as.matrix(gen.bs(log1p(seq(0,50)),knots.list$lag1,bdknots.list$lag1,2,'lag1.nlap.bs')[,c(1,2,3)])
  vec=bsMat %*% matrix(testModel.lag1bs$beta[1:3],ncol=1)
  minLos.vol[i]=which.min(vec)
  losbyvol[,i]=vec
}

head(summary(fit.list[[7]])$coefficients[7:9,])
plot(log1p(seq(0,50)),losbyvol[,8],type='b')
#76 per year, this meakes sense, also based on significant level of the terms
unlist(lapply(fit.list,logLik)) #8th is the best, 7th is is the second bead, but think of terms significance
#I think we need to choose 7th one.
#now we can use the final model.


#=----------fine search for the percentile

fine.fit.list=list()
fine.tileVec=seq(0.6,0.8,0.025)
fine.minLos.vol=rep(NA,length(fine.tileVec))
fine.losbyvol=matrix(NA,nrow=51,ncol=length(fine.tileVec))

for(i in 1:length(fine.tileVec)){
  print(i)
  cleaned.obj=finalCleaning(anaDf.raw, fine.tileVec[i],2)
  cleaned=cleaned.obj$df
  knots.list=cleaned.obj$knots
  bdknots.list=cleaned.obj$bdknots
  chkMissVars.lag6=c('n.lag6.lap','n.lag6.nonLap','n.lag6.rap','age3cat','race5cat','year','comorbcat','pay1','mdnum1.r')
  nomiss.lag6 = droplevels(cleaned[complete.cases(cleaned[,chkMissVars.lag6]),])
  
  testModel.lag1bs <- clm(los ~ lag1.nlap.bs1+lag1.nlap.bs2+lag1.nlap.bs3+age4cat+race5cat+comorbcat+pay1+year+mdnum1.r, data=nomiss.lag6, link=c('probit'))
  
  fine.fit.list=lappend.yz(fine.fit.list,testModel.lag1bs)
  
  bsMat = as.matrix(gen.bs(log1p(seq(0,50)),knots.list$lag1,bdknots.list$lag1,2,'lag1.nlap.bs')[,c(1,2,3)])
  vec=bsMat %*% matrix(testModel.lag1bs$beta[1:3],ncol=1)
  fine.minLos.vol[i]=which.min(vec)
  fine.losbyvol[,i]=vec
}

plot(fine.tileVec,fine.minLos.vol)
plot(unlist(lapply(fine.fit.list,logLik)))
cbind(fine.losbyvol)

#Final choice
#lag=1
#use second order B-spline 
#use percentile=0.75 for lag1 as knot

save(anaDf.raw,cleaned.obj,fine.tileVec,fine.minLos.vol,fine.fit.list,fine.losbyvol,tileVec,minLos.vol,fit.list,losbyvol
     , file='Z:/j_scrdata/lapLearn/lagKnotPctSearch_may162017.RData'
     )

load(file='Z:/j_scrdata/lapLearn/lagKnotPctSearch_may162017.RData')

plot(fine.losbyvol[,6])
#test two knots


twoKnots.fit.list=list()
#0.3 seems to good
firstKnot.tileVec=seq(0.1,0.5,0.05)
twoknots.minLos.vol=rep(NA,length(firstKnot.tileVec))
twoknots.losbyvol=matrix(NA,nrow=51,ncol=length(firstKnot.tileVec))
i=2
for(i in 1:length(firstKnot.tileVec)){
  print(i)
  cleaned.obj=finalCleaning(anaDf.raw, c(firstKnot.tileVec[i],0.75),2)
  cleaned=cleaned.obj$df
  knots.list=cleaned.obj$knots
  bdknots.list=cleaned.obj$bdknots
  
  chkMissVars.lag6=c('n.lag6.lap','n.lag6.nonLap','n.lag6.rap','age3cat','race5cat','year','comorbcat','pay1','mdnum1.r')
  
  nomiss.lag6 = droplevels(cleaned[complete.cases(cleaned[,chkMissVars.lag6]),])

  testModel.lag1bs <- clm(los ~ lag1.nlap.bs1+lag1.nlap.bs2+lag1.nlap.bs3+lag1.nlap.bs4+age4cat+race5cat+comorbcat+pay1+year+mdnum1.r, data=nomiss.lag6, link=c('probit'))
  
  
 plot(summary(testModel.lag1bs)$coefficients[7:48,1])
  
  
  testModel.lag1bs <- clm(los ~ factor(n.lag1.lap)+lag1.nlap.bs4+age4cat+race5cat+comorbcat+pay1+year+mdnum1.r, data=nomiss.lag6, link=c('probit'))
  summary(testModel.lag1bs)
  
  
  
  twoKnots.fit.list=lappend.yz(twoKnots.fit.list,testModel.lag1bs)
  
  bsMat = as.matrix(gen.bs(log1p(seq(0,50)),knots.list$lag1,bdknots.list$lag1,2,'lag1.nlap.bs')[,c(1,2,3,4)])
  
  vec=bsMat[,c(1,2,3)] %*% matrix(testModel.lag1bs$beta[1:3],ncol=1)
  twoknots.minLos.vol[i]=which.min(vec)
  twoknots.losbyvol[,i]=vec
}


plot(fine.losbyvol[,6])

head(summary(twoKnots.fit.list[[1]])$coefficients[7:10,])
plot(log1p(seq(0,50)),losbyvol[,8],type='b')
#76 per year, this meakes sense, also based on significant level of the terms
unlist(lapply(fit.list,logLik)) #8th is the best, 7th is is the second bead, but think of terms significance
#I think we need to choose 7th one.
#now we can use the final model.


