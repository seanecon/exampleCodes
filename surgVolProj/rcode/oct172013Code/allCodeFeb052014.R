#I am going to create a final copy of analysis file this file intends to modularize all the steps


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

sourceDir('C:/Dropbox/K/lapLearningCurve/rcode/rFun/indivFunFolder')

source('C:/Dropbox/K/lapLearningCurve/rcode/rFun/fun.r')
anaDf.raw = genAnaDf.lapLearn(100)
library(ggplot2)
count(anaDf.raw,'pay1')
# /*1 Medicare */
#   /*2 Medicaid */
#   /*3 Private insurance */
#   /*4 Self-pay */
#   /*5 No charge */
#   /*6 Other */
#   /*. Missing */
#   /*.A Invalid */
#   /*.B Unavailable from source (coded in 1988-1997 data only) */
#   


genIndependentVars = function(anaDf.raw){ 
  
  completeCase.vec=complete.cases(anaDf.raw[,c('age','race')])
  cat('dropped (due to missing age or race) ', length(completeCase.vec)-sum(completeCase.vec),'\n')
  anaDf.raw=anaDf.raw[completeCase.vec,]
  (losDensity=density.yz(anaDf.raw[,'los']))
  print(losDensity)
  
  age4catcuts=quantile(anaDf.raw[,'age'],prob=c(0,0.25,0.5,0.75,1))
  anaDf.raw=grpnv.supplycuts.yz(anaDf.raw, 'age', age4catcuts, 'age4cat')
  
  #comorbcuts=quantile(anaDf.raw[,'comorbSum'],prob=c(0,0.333,0.666,1))
  comorbcuts=c(0,1,2,3,Inf)
  anaDf.raw=grpnv.supplycuts.yz(anaDf.raw, 'comorbSum', comorbcuts, 'comorbcat')
  
  anaDf.raw[,'race5cat']=replaceValJoin.yz(anaDf.raw[,'race'] #this is typically a data column
                                           ,list(1,2,3,4,c(5,6)) 
                                           ,c(1,2,3,4,5)
                                           ,origValVn='race'
                                           ,newValVn='race5cat' #if output is vector, this newValVn is inapplicable
                                           ,outputJoinedDf=TRUE #if F means only output the new vector
                                           #if T, then output df             
  )[,'race5cat']
  
  
  #to run fixed effect, you have to delete physician who have only one patient record

  
  anaDf.raw=intoFac.yz(anaDf.raw,c('race','race5cat','year','age4cat','comorbcat','pay1'
                                   #,'n.lag2.nonLap.5cat','n.lag3.nonLap.5cat','n.lag4.nonLap.5cat','n.lag5.nonLap.5cat','n.lag6.nonLap.5cat'
                                   ,'hospst','mdnum1.r'))
  (losDensity=density.yz(as.numeric(anaDf.raw[,'los'])))
  print(losDensity)
  
  
  chkMissVars.lag6=c('n.lag6.lap','age4cat','race5cat','year','comorbcat','pay1','mdnum1.r')
  
  anaDf.raw = droplevels(anaDf.raw[complete.cases(anaDf.raw[,chkMissVars.lag6]),])
  
  (losDensity=density.yz(as.numeric(anaDf.raw[,'los'])))
  print(losDensity)
  
  
  anaDf.raw=subset(anaDf.raw, los<=7)
  anaDf.raw=intoFac.yz(anaDf.raw,'los')
  
  #year 2003 will be missing if we use lag6, so after running this function, you need to droplevels for year
  (losDensity=density.yz(as.numeric(anaDf.raw[,'los'])))
  print(losDensity)
  
  return(anaDf.raw)
}
#we use lag6 to restrict data. 
anaDf.raw.1=genIndependentVars(anaDf.raw)

#99.1 sample <7 days
nrow(anaDf.raw.1)



#get table 1
table.yz <- function(indep.catvns,depcat.vn,df){
  chisq.vec <- numeric(0)
  if (!is.subset.yz(c(indep.catvns,depcat.vn),names(df))){stop('table.yz: some varaibles are not in df')}
  tabout.list <- cellsize.list<- list()
  for (i in seq_along(indep.catvns)){
    tabout <- t(table(df[,depcat.vn],df[,indep.catvns[i]]))
    rownames(tabout) <- cartesianPaste.yz(indep.catvns[i],rownames(tabout),sep='=')
    cellsize.list = lappend.yz(cellsize.list,tabout)  
    chisq.vec <- c(chisq.vec, chisq.test(tabout)$p.value)
    for (j in 1:ncol(tabout)){
      
      tabout[,j] <- tabout[,j]/sum(tabout[,j])}
    tabout.list <- lappend.yz(tabout.list,tabout)
    
  }
  names(chisq.vec) <- indep.catvns
  tabout.mat <- do.call(rbind,tabout.list)
  sample.size <- table(df[,depcat.vn])
  outlist <- list(cellsize.list=cellsize.list,tabout.mat=tabout.mat, chisq.vec=chisq.vec, sample.size=sample.size)
  return(outlist)
}

table.yz(indep.catvns,depcat.vn,df)
tabout = table.yz(c('age4cat', 'race5cat','comorbcat','pay1','year'),'los',anaDf.raw.1)
str(tabout)
xtable(do.call('rbind',tabout$cellsize.list))

xtable(tabout$tabout.mat)




#run models with different lags for care experience
#I tried, you can not use both mdnum.1 and state to run the model. I think their nesting structure makes it impossible to run such model

testModel.lag1<- clm(los ~ n.lag1.lap.log+age4cat+race5cat+comorbcat+pay1+mdnum1.r+year, data=anaDf.raw.1, link=c('probit'))
testModel.lag2<- clm(los ~ n.lag2.lap.log+age4cat+race5cat+comorbcat+pay1+mdnum1.r+year, data=anaDf.raw.1, link=c('probit'))
testModel.lag3<- clm(los ~ n.lag3.lap.log+age4cat+race5cat+comorbcat+pay1+mdnum1.r+year, data=anaDf.raw.1, link=c('probit'))
testModel.lag4<- clm(los ~ n.lag4.lap.log+age4cat+race5cat+comorbcat+pay1+mdnum1.r+year, data=anaDf.raw.1, link=c('probit'))
testModel.lag5<- clm(los ~ n.lag5.lap.log+age4cat+race5cat+comorbcat+pay1+mdnum1.r+year, data=anaDf.raw.1, link=c('probit'))
testModel.lag6<- clm(los ~ n.lag6.lap.log+age4cat+race5cat+comorbcat+pay1+mdnum1.r+year, data=anaDf.raw.1, link=c('probit'))


fitList.logLike=list(testModel.lag1, testModel.lag2,testModel.lag3,testModel.lag4,testModel.lag5,testModel.lag6)


#this plot is updated on 12/20/2013
pdf(file='C:/Dropbox/paper/surgVol/optimalReferral/figure/lagSelSensitivity.pdf')
plot(unlist(lapply(fitList.logLike,logLik)),type='b', main='Log-likehood of models with different care expereience time windows', xlab='Number of lagged quarters used to measure care volume', ylab='log-likelihood')
dev.off()

#next, I am going to test different basis splines

xtable(summary(bsFit)$coefficients)
createBs = function(anaDf.raw
                  , interestVn # interestVn='n.lag1.lap.log'
                  , inner.knots #tell the knots inner.knots=tileList[[i]]
                  , outputVnStem #output data's columen name stem outputVnStem='n.lag1.lap.log.bs'
                  , bsDegree){   
  
  print('i am a')
  bslist=bsWrapper.yz(anaDf.raw[,interestVn] #this is the x in bs function
                      , inner.knots #inner knots, if it contains boundary knots, function will stop and issue erro message
                      , outputVnStem #output data's columen name stem
                      , degree=bsDegree
                      , dataType='data.frame'
                      #or ='data.frame
  )
  
  print('after')
  
  anaDf.raw=cbind(anaDf.raw,bslist$bsdata)
  boundary.knots=bslist$boundary.knots
  n.basis=bslist$n.basis
  
  inner.knots.list=list(inner.knots.lag1=inner.knots)
  boundary.knots.list=list(boundary.knots=boundary.knots)
  
  names(inner.knots.list)=paste('lag',seq(1),sep='')
  names(boundary.knots.list)=paste('lag',seq(1),sep='')

  outlist=list(inner.knots=inner.knots.list,boundary.knots=boundary.knots.list,n.basis=n.basis, df=anaDf.raw)
  
  return(outlist)
}

#Spline selection
#afterwards, we conduct an comprehensive search for various basis-splines transofrmation of the chosen lag window. This operation would maximuze our 
#ability to capture nonlinear realtionship and esnure model stability as well.
#lag selection step
#we decided lag1 is the best, so we then move on use spline technique to find the best curve.


outputVnStem='n.lag1.lap.log.bs'

#-----------------search for the right percentile and also generate an average learning curve (aggregate over year) ------------------
bsDegree=2
lagvolvec = seq(min(anaDf.raw.1[,'n.lag1.lap']),max(anaDf.raw.1[,'n.lag1.lap']))

inner.knots.list=boundary.knots.list=fitList=list()

#you cannot just use equal spaced knots. they would be overly curveline

tileList=list(seq(0.1,0.9,0.2), seq(0.1,0.9,0.1), seq(0.1,0.9,0.3),seq(0.1,0.9,0.4), seq(0.1,0.9,0.5), c(0.1,0.7), c(0.2,0.7), 
              0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)



#the following for loop would generate average learning curve

meanLosbyvolTile=matrix(NA,nrow=length(lagvolvec),ncol=length(tileList))
i=15
for(i in 1:length(tileList)){
  print(i)
  log.lag1.innerKnots=quantile(anaDf.raw[,'n.lag1.lap.log'],prob=tileList[[i]],na.rm=T)
  cleaned.obj=createBs(anaDf.raw.1, "n.lag1.lap.log",log.lag1.innerKnots,outputVnStem, bsDegree) #degree=2
  cleaned=cleaned.obj$df
  n.basis=cleaned.obj$n.basis
  inner.knots=cleaned.obj$inner.knots$lag1
  boundary.knots=cleaned.obj$boundary.knots$lag1
  
  inner.knots.list=lappend.yz(inner.knots.list,inner.knots)
  boundary.knots.list=lappend.yz(boundary.knots.list,boundary.knots)
    
  cat('it is running bspline fit','\n')
  formu=passVarToFormula.yz('los',c(paste(outputVnStem,seq(n.basis),sep=''),'age4cat', 'race5cat','comorbcat','pay1','year','mdnum1.r'))
  
  #get basis spline fit model
  bsFit = clm(formu, data=cleaned, link=c('probit'))
  fitList=lappend.yz(fitList,bsFit)
  
  #generate basis spline terms
  lagvol.bs=bsWrapper.yz(log1p(lagvolvec) #this is the x in bs function
                         , log.lag1.innerKnots #inner knots, if it contains boundary knots, function will stop and issue erro message
                         , outputVnStem #output data's columen name stem
                         , degree=bsDegree
                         , boundary.knots=boundary.knots
                         , intercept=FALSE
                         , dataType='data.frame'
                         #or ='data.frame'
                         , closenessCutoffPct=0.02 #closeness of inner knots to bdnots, lower than this number means it is too close.. and not good, 0.02 means two percent of range
  )$bsdata
  n.basis=ncol(lagvol.bs)
  nobsformu=passVarToFormula.yz('', c('age4cat', 'race5cat','comorbcat','pay1','year','mdnum1.r'))
  bsformu=passVarToFormula.yz('', paste(outputVnStem,seq(n.basis),sep=''))
  noLagBsDf=deldfcols.yz(cleaned,c(paste(outputVnStem,seq(n.basis),sep='')))
  x.noBs=model.matrix(nobsformu,noLagBsDf)
  b=bsFit$beta
  
  xbout.noBs=xb.matched.yz(b,x.noBs)
  xbout.noBs$unmatched
  xb.noBs=xbout.noBs$xb.matched
  xb.noBsPart=xbout.noBs$xb.matched
  meanLosVec=rep(NA,length(lagvolvec))
  for(vi in 1:length(lagvolvec)){
    print(vi)
    newDf.vi = as.data.frame(lagvol.bs[vi,,drop=F][rep(1,nrow(cleaned)),])
    x.bs=model.matrix(bsformu, data = newDf.vi)
    xbout.bs=xb.matched.yz(b,x.bs)
    xb.bs=xbout.bs$xb.matched
    #xbout.bs$matched
    xb=xb.bs+xb.noBs
    cuts=bsFit$alpha
    
    #prob is based on the coefficent point estimate xb
    probMat=xbCuts2probmat.orderedprobit.yz(xb,cuts)
    #then we obtained predicted length of stay based on the expected LOS based on point estiamte
    meanLosVec[vi]=mean(probMat %*% matrix(seq(1:(length(cuts)+1)), ncol=1)) 
  }
  meanLosbyvolTile[,i]=meanLosVec 
}



#this plot is updated on 12/20/2013
pdf(file='C:/Dropbox/paper/surgVol/optimalReferral/figure/bic.pdf')
plot(seq(16),unlist(lapply(fitList,BIC)),type='b', xlab='Type of inner knots', ylab='Bayesian information criterion', axes=F)
axis(1, 1:16)
axis(2)
box()
dev.off()

#-----------------search for the right percentile and generate average learning curve------------------
lagvolvec=seq(0,50)
bsDegree=2
k=15


avgLearn.allyear=getAvgLearnCurve(fitList[[k]]$beta #beta coefficient vector
                                  ,fitList[[k]]$alpha #cuts coefficient vector
                                  ,nobsformu #non Bspline part formula nobsformu=passVarToFormula.yz('', c('age4cat', 'race5cat','comorbcat','pay1','year','mdnum1.r'))
                                  ,cleaned #input data, the data used to fit model
                                  ,as.character(seq(2004,2010)) #the years of interest
                                  ,lagvolvec# LAP volume of inerest
                                  ,bsDegree #bspline degreee
                                  ,inner.knots.list[[k]] #innner knots of bspline
                                  ,boundary.knots.list[[k]] #bspline boundary
                                  ,yearVn='year' #appear in input data
                                  , bsVnStem='n.lag1.lap.log.bs')

plot(avgLearn.allyear)


avgLearn.2004=getAvgLearnCurve(fitList[[k]]$beta #beta coefficient vector
                               ,fitList[[k]]$alpha #cuts coefficient vector
                               ,nobsformu #non Bspline part formula nobsformu=passVarToFormula.yz('', c('age4cat', 'race5cat','comorbcat','pay1','year','mdnum1.r'))
                               ,anaDf.raw.1 #input data, the data used to fit model
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
                               ,anaDf.raw.1 #input data, the data used to fit model
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
                               ,anaDf.raw.1 #input data, the data used to fit model
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
                               ,anaDf.raw.1 #input data, the data used to fit model
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
                               ,anaDf.raw.1 #input data, the data used to fit model
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
                               ,anaDf.raw.1 #input data, the data used to fit model
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
                               ,anaDf.raw.1 #input data, the data used to fit model
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
                                   ,anaDf.raw.1 #input data, the data used to fit model
                                   ,as.character(seq(2006,2010)) #the years of interest
                                   ,lagvolvec# LAP volume of inerest
                                   ,bsDegree #bspline degreee
                                   ,inner.knots.list[[k]] #innner knots of bspline
                                   ,boundary.knots.list[[k]] #bspline boundary
                                   ,yearVn='year' #appear in input data
                                   , bsVnStem='n.lag1.lap.log.bs')

count(anaDf.raw.1,'year')




plotAvgLearnDf=subset(avgLearnCurveByYear, lag1LapVolume<=40)

cex.pch=0.5
pdf('C:/Dropbox/paper/surgVol/optimalReferral/figure/avgLearnCurveByYear_feb052014.pdf')
plot(plotAvgLearnDf[,'lag1LapVolume'], plotAvgLearnDf[,'meanlos.2004'], ylim=c(1.6,2.4),xlab='Number of MIRP performed during the past 3 months', ylab='Expected length of stay', type='b',pch=1,cex=cex.pch)
points(plotAvgLearnDf[,'lag1LapVolume'], plotAvgLearnDf[,'meanlos.2005'],pch=2,type='b',cex=cex.pch)
points(plotAvgLearnDf[,'lag1LapVolume'], plotAvgLearnDf[,'meanlos.20062010'],pch=3,type='b',cex=cex.pch)
legend('topleft',pch=1:3, legend=c('2004','2005','2006-2010'),cex=1)
dev.off()


#stop here
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



ubLbLearnCurveTiles=c(0.2,0.8) #
#ubLbLearnCurveTiles=c(0.025,0.975)
#ubLbLearnCurveTiles=c(0.05,0.6)

lbub=apply(meanLosMat,1,function(x){quantile(x,prob=ubLbLearnCurveTiles)})
lb=lbub[1,]
ub=lbub[2,]

medianLos=apply(meanLosMat,1,function(x){quantile(x,prob=0.5)})

avgLearnCurve=data.frame(lag1LapVolume=lagvolvec, meanLos=meanLosbyvolTile[,15], lb=lb, ub=ub)


# 
# qplot(lag1LapVolume, medianLos,data=data.frame(subset(avgLearnCurve,lag1LapVolume<=25),medianLos=medianLos[1:26]), ylim=c(1.5,2)
#       , xlab='Number of MIRP performed during the past 3 months'
#       , ylab='Expected length of stay')+geom_smooth(aes(ymin=lb, ymax=ub), data=data.frame(subset(avgLearnCurve,lag1LapVolume<=25),medianLos=medianLos[1:26]), stat="identity")+geom_vline(xintercept = 13,linetype=4)+scale_x_continuous(breaks=c(seq(0,25,by=5),14), labels=c(seq(0,25,by=5),14))+opts(axis.line = theme_segment(colour = "black"),panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(),panel.border = theme_blank(),panel.background = theme_blank()) 

#we choose the 15th model as the optimal




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
  
  yearDf=subset(anaDf.raw.1,year %in% whatYears)
  #   yearDf=anaDf.raw.1
  #   yearDf[,'year']=factor(rep(whatYears, nrow(anaDf.raw.1)),levels=levels(anaDf.raw.1[,'year']))
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


yearVec=as.character(seq(2004,2010))

avgLearn.2004=getAvgLearnCurveByYear(fit, yearVec,lagvolvec,lagvol.bs)
plot(avgLearn.2004)

avgLearn.2005=getAvgLearnCurveByYear(fit, '2005',lagvolvec,lagvol.bs)
avgLearn.2006=getAvgLearnCurveByYear(fit, '2006',lagvolvec,lagvol.bs)
avgLearn.2007=getAvgLearnCurveByYear(fit, '2007',lagvolvec,lagvol.bs)
avgLearn.2008=getAvgLearnCurveByYear(fit, '2008',lagvolvec,lagvol.bs)
avgLearn.2009=getAvgLearnCurveByYear(fit, '2009',lagvolvec,lagvol.bs)
avgLearn.2010=getAvgLearnCurveByYear(fit, '2010',lagvolvec,lagvol.bs)
avgLearn.20062010=getAvgLearnCurveByYear(fit, c('2006','2007','2008','2009','2010'),lagvolvec, lagvol.bs)

avgLearnCurveByYear= data.frame(lag1LapVolume=lagvolvec
                                
                                , meanlos.2004=avgLearn.2004
                                , meanlos.2005=avgLearn.2005
                                ,meanlos.2006=avgLearn.2006
                                ,meanlos.2007=avgLearn.2007
                                ,meanlos.2008=avgLearn.2008
                                ,meanlos.2009=avgLearn.2009
                                ,meanlos.2010=avgLearn.2010
                                ,meanlos.20062010=avgLearn.20062010)



which.min(avgLearnCurve[,'meanLos']) #14

#I like to see large number i.e., 14 in the past three month...but I cannot see that once I was try to get
#year specific learning curve. so I probably do not want to present year specific leanring curve.
#this should be a concern....

plotPart=subset(avgLearnCurveByYear,lag1LapVolume<=40)
which.min(plotPart[,'meanlos.2005']) #8

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




#update on Dec 20 2013
pdf('C:/Dropbox/paper/surgVol/optimalReferral/figure/avgLearnCurve.pdf')
qplot(lag1LapVolume, meanLos,data=subset(avgLearnCurve,lag1LapVolume<=40) , xlab='Number of MIRP performed during the past 3 months', ylab='Expected length of stay')+geom_smooth(aes(ymin=lb, ymax=ub), data=subset(avgLearnCurve,lag1LapVolume<=40), stat="identity")+geom_vline(xintercept = 14,linetype=4)+scale_x_continuous(breaks=c(seq(0,40,by=10),14), labels=c(seq(0,40,by=10),14))+opts(axis.line = theme_segment(colour = "black"),panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(),panel.border = theme_blank(),panel.background = theme_blank()) 
dev.off()




# pdf('C:/Dropbox/paper/surgVol/optimalReferral/figure/avgLearnCurveCutoff25.pdf')
# qplot(lag1LapVolume, meanLos,data=subset(avgLearnCurve,lag1LapVolume<=25), ylim=c(1.5,2) , xlab='Number of MIRP performed during the past 3 months', ylab='Expected length of stay')+geom_smooth(aes(ymin=lb, ymax=ub), data=subset(avgLearnCurve,lag1LapVolume<=25), stat="identity")+geom_vline(xintercept = 14,linetype=4)+scale_x_continuous(breaks=c(seq(0,25,by=5),14), labels=c(seq(0,25,by=5),14))+opts(axis.line = theme_segment(colour = "black"),panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(),panel.border = theme_blank(),panel.background = theme_blank()) 
# dev.off()




pdf('C:/Dropbox/paper/surgVol/optimalReferral/figure/avgLearnCurveCutoff25_april232014.pdf')
qplot(lag1LapVolume, meanLos,data=subset(avgLearnCurve,lag1LapVolume<=25), ylim=c(1.6,1.9) , xlab='Number of MIRP performed during the past 3 months', ylab='Expected length of stay')+geom_smooth(aes(ymin=lb, ymax=ub), data=subset(avgLearnCurve,lag1LapVolume<=25), stat="identity")+geom_vline(xintercept = 13,linetype=4)+scale_x_continuous(breaks=c(seq(0,25,by=5),13), labels=c(seq(0,25,by=5),13))+opts(axis.line = theme_segment(colour = "black"),panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(),panel.border = theme_blank(),panel.background = theme_blank()) 
dev.off()

tiff('C:/Dropbox/paper/surgVol/optimalReferral/figure/avgLearnCurveCutoff25_april232014.tiff')
qplot(lag1LapVolume, meanLos,data=subset(avgLearnCurve,lag1LapVolume<=25), ylim=c(1.6,1.9) , xlab='Number of MIRP performed during the past 3 months', ylab='Expected length of stay')+geom_smooth(aes(ymin=lb, ymax=ub), data=subset(avgLearnCurve,lag1LapVolume<=25), stat="identity")+geom_vline(xintercept = 13,linetype=4)+scale_x_continuous(breaks=c(seq(0,25,by=5),13), labels=c(seq(0,25,by=5),13))+opts(axis.line = theme_segment(colour = "black"),panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(),panel.border = theme_blank(),panel.background = theme_blank()) 
dev.off()

#april 23, 2014 data for avg learning curve

# ubLbLearnCurveTiles=c(0.2,0.8) 
# lbub=apply(meanLosMat,1,function(x){quantile(x,prob=ubLbLearnCurveTiles)})
# lb=lbub[1,]
# ub=lbub[2,]
# medianLos=apply(meanLosMat,1,function(x){quantile(x,prob=0.5)})
# avgLearnCurve=data.frame(lag1LapVolume=lagvolvec, meanLos=meanLosbyvolTile[,15], lb=lb, ub=ub)
# lag1LapVolume   meanLos        lb        ub
# 0              0 1.7640029 1.7287465 1.8951994
# 1              1 1.7137874 1.6755770 1.8428946
# 2              2 1.6922290 1.6561852 1.8242470
# 3              3 1.6801873 1.6396648 1.8131162
# 4              4 1.6726365 1.6314534 1.8064491
# 5              5 1.6675997 1.6274985 1.8018509
# 6              6 1.6641230 1.6229421 1.8001365
# 7              7 1.6616840 1.6148519 1.7992528
# 8              8 1.6599703 1.6159165 1.7978105
# 9              9 1.6587832 1.6187222 1.7969915
# 10            10 1.6579894 1.6210269 1.7942433
# 11            11 1.6574965 1.6174236 1.7936802
# 12            12 1.6572381 1.6145769 1.7948923
# 13            13 1.6571654 1.6150307 1.7930006
# 14            14 1.6572415 1.6149230 1.7911891
# 15            15 1.6574383 1.6163876 1.7912287
# 16            16 1.6577338 1.6159570 1.7912104
# 17            17 1.6581107 1.6147252 1.7927300
# 18            18 1.6585551 1.6168619 1.7939630
# 19            19 1.6590558 1.6168519 1.7949324
# 20            20 1.6596036 1.6173749 1.7950182
# 21            21 1.6601910 1.6178022 1.7947569
# 22            22 1.6608119 1.6189548 1.7950057
# 23            23 1.6614608 1.6221980 1.7962801
# 24            24 1.6621336 1.6246909 1.7968854
# 25            25 1.6628264 1.6258565 1.7970716


subAvgLearnData=subset(avgLearnCurve,lag1LapVolume<=25)

plot(subAvgLearnData[,'lag1LapVolume'], subAvgLearnData[,'meanLos'],data=subAvgLearnData, ylim=c(1.5,2) , xlab='Number of MIRP performed during the past 3 months', ylab='Expected length of stay', type='b')


which.min(avgLearnCurve[,'meanLos'])
#the optimal is 14 RAP per quarter

pdf('C:/Dropbox/paper/surgVol/optimalReferral/figure/obsVolHistOptimalVol.pdf')
y <- hist(anaDf.raw.1[,'n.lag1.lap'], plot=FALSE, n=50, main='')
plot(y, col=ifelse(y$mid<13,'grey','white'), xlab='Observed number of MIRPs performed during the past 3 months', main='')
box()
dev.off()
install.packages("ggplot2",type="source")
  
density.yz(anaDf.raw.1[,'n.lag1.lap'])


# dat <- data.frame(n.lag1.lap= anaDf.raw.1[,'n.lag1.lap'], aboveCutoff=NA)
# 
# for(i in 1:nrow(dat)){
#   
#   if (dat[i,'n.lag1.lap']>13) {dat[i,'aboveCutoff']='greater than 13'} else {dat[i,'aboveCutoff']='less than 13'}
#   
# }
# 
# library(ggplot2)
# qplot(n.lag1.lap,data=dat,geom="histogram",xlab="Surgeon's last quarter MIRP volume",ylab='Number of MIRP',fill=aboveCutoff, binwidth=0.5)+opts(axis.line = theme_segment(colour = "black"),panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(),panel.border = theme_blank(),panel.background = theme_blank()) 
# library(ggplot2)
# 
# #the following is code for histogram with two colors
# p=ggplot(anaDf.raw.1,aes(n.lag1.lap)) +
#   geom_histogram(binwidth=1,fill="white",color="black") +
#   geom_histogram(data=subset(anaDf.raw.1,n.lag1.lap<13),binwidth=1, 
#                  colour="black", fill="grey") +opts(axis.line = theme_segment(colour = "black"),panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(),panel.border = theme_blank(),panel.background = theme_blank()) 
# 
# pdf('C:/Dropbox/paper/surgVol/optimalReferral/figure/mirpUnexperiencedSrgeon.pdf')
# p+scale_x_continuous("Performing surgeons' past quarter MIRP volume")+scale_y_continuous("Count of MIRP")
# dev.off()

#sum(anaDf.raw.1[,'n.lag1.lap']<13)/nrow(anaDf.raw.1)
#[1] 0.55191965
sum(anaDf.raw.1[,'n.lag1.lap']<13)/nrow(anaDf.raw.1)
#[1] 0.55184705

#now I like to get the average learning curve.
#the learning curve is obtained 
# I just perturb each individual docotors past MIRP amount 

#plotCI(avgLearnCurve[,'lag1LapVolume'], avgLearnCurve[,'meanLos'], ui=avgLearnCurve[,'ub'], li=avgLearnCurve[,'lb'], lwd=0.01)


#likelihood ratio test for a linear relationship model, reject linear linear curve model, so good
bsFit.linear=clm(los ~ n.lag1.lap.log + 
                   age4cat + race5cat + comorbcat + pay1 + year + mdnum1.r, data=anaDf.raw.1, link=c('probit'))

lrt.yz=function(model.full,model.short){
  1-pchisq(2*(logLik(model.full)[1]-logLik(model.short)[1]), model.short$df.residual- model.full$df.residual)
}

lrt.yz(fitList[[15]],bsFit.linear)
#our test reject the null of a linear model with a pvalue of5.481228e-06


#--------------------need to do optimization problem and predict savings of LOS through centratlization---------------


#------first get within state individaul learning curve---------------

#genAverage Learning curve based on mean beta and mean cuts
# According to standard likelihood theory, the variance-covariance matrix of the parameters
# can be obtained as the inverse of the observed Fisher information matrix. This matrix is
# given by the negative Hessian of the log-likelihood function7 evaluated at the maximum
# likelihood estimates.




#save the results
# save(anaDf.raw, anaDf.raw.1, fitList,meanLosbyvolTile, tileList, lagvolvec, file="Z:/j_scrdata/lapLearn/bsplineSensitivity_may222013.RData")
#(load(file="Z:/j_scrdata/lapLearn/bsplineSensitivity_may222013.RData"))




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
  , boundary.knots #anaDf.raw.1[,'n.lap.loag1']
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
  #subset(anaDf.raw.1,n.lag1.lap==9,select=c('n.lag1.lap.log.bs1','n.lag1.lap.log.bs2','n.lag1.lap.log.bs3'))
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

names(anaDf.raw.1)


learnCurves.withinStateYear=function(cuts, betaVec, docs, stateName, indf, inner.knots, boundary.knots, degree, lagVolSeq, surgeonIdFactorVn="mdnum1.r"){
  learnList=list()
  
  for(i in 1:length(docs)){ 
    indiv.learn=indivDocWithinStateLearnCurve(
      cuts #cuts are extracted from fit model's mean and vcov
      , betaVec #betaVec extracted from fit model's mean and vcov
      , docs[i]
      , model.matrix.Formula.rhsPatYear
      , inner.knots #inner knots
      , boundary.knots #anaDf.raw.1[,'n.lap.loag1']
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
ia.docs=as.character(unique(subset(anaDf.raw.1,hospst=='IA')[,'mdnum1.r']))
ny.docs=as.character(unique(subset(anaDf.raw.1,hospst=='NY')[,'mdnum1.r']))
md.docs=as.character(unique(subset(anaDf.raw.1,hospst=='MD')[,'mdnum1.r']))



ia.curves.byYear=md.curves.byYear=ny.curves.byYear=list()

for(y in 1:length(yearVec)){
  
  ia.curves.y=learnCurves.withinStateYear(cuts, betaVec, ia.docs,'IA', subset(anaDf.raw.1, year==yearVec[y]), inner.knots, boundary.knots, degree, lagVolSeq, surgeonIdFactorVn="mdnum1.r")
  
  ia.curves.byYear=lappend.yz(ia.curves.byYear, ia.curves.y)
  
}

for(y in 1:length(yearVec)){
  
  md.curves.y=learnCurves.withinStateYear(cuts, betaVec, md.docs,'MD', subset(anaDf.raw.1, year==yearVec[y]), inner.knots, boundary.knots, degree, lagVolSeq, surgeonIdFactorVn="mdnum1.r")
  
  md.curves.byYear=lappend.yz(md.curves.byYear, ia.curves.y)
  
}

for(y in 1:length(yearVec)){
  
  ny.curves.y=learnCurves.withinStateYear(cuts, betaVec, ny.docs,'NY', subset(anaDf.raw.1, year==yearVec[y]), inner.knots, boundary.knots, degree, lagVolSeq, surgeonIdFactorVn="mdnum1.r")
  
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
  print(sum(Mvec))
  cat('sum(Mvec)=',sum(Mvec),'\n')
  cat('N=',N,'\n')
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

solveOptAllocation=function(n.surgery, G.list,  random.seeds, pickBestDoctors=T, picked.n.md=10
                            , m.ran=30
                            #a solution, we randomly assign the solution to providers m.ran times
                            #this is use to get non-learning knowledege outcome through regionalization
                            
                            ){
  #you cannot solve the problem by feeding all the MD
  #however, you can randomly pick MDs assume you have no prior information about MD quality
  avgVol=optNumMd=minLos=rep(NA,length(random.seeds))
  learnAbilityUnknownLos.mat=matrix(NA,nrow=length(random.seeds),ncol=m.ran)
  rownames(learnAbilityUnknownLos.mat)=paste('randSeed.',seq(length(random.seeds)))
  colnames(learnAbilityUnknownLos.mat)=paste('m.ran.',seq(m.ran))
  optSolution.list=list()
  
  
  #the error is based on a bootstrapping....., we can bootstrap the population and find the best.
  #need to change the code....
  for(i in 1:length(random.seeds)){
    cat('processing random seed ', i, '\n')
    set.seed(random.seeds[i])
    bt.G.list=G.list[sample(seq(length(G.list)), length(G.list), replace=TRUE)]
    
    if(pickBestDoctors){
      #use the second element of G to fine the best doctor, the first element is for no volume
      G.volume1.vec=unlist(lapply(bt.G.list,function(x){x[2]}))
      pickedMdIndx=whichLargestN.yz(-G.volume1.vec, picked.n.md)
      #best doctor has smallest length of stay, so there is a negative sign before G.loc2.vec 
    } else{ pickedMdIndx = sample(seq(length(bt.G.list)), picked.n.md, replace=FALSE) }
    
    
    G.list.picked=bt.G.list[pickedMdIndx]
    
   opt.obj=scilpPatAlloc.yz(n.surgery
                             , G.list.picked #G is a list of payoff, with 0 patient, 1, 2......a maximum number of patients
                             , allTreated=TRUE)
    
    
    #get outcome gain when learning information is unknown to policy maker
    
    for(iii in 1:m.ran){
      sum.G=0
      randomPicked.gLoc=sample(1:length(bt.G.list),length(opt.obj$opt.n))
      randomPicked.G=bt.G.list[randomPicked.gLoc]
      for (w in 1:length(opt.obj$opt.n)){
        sum.G=sum.G+randomPicked.G[[w]][opt.obj$opt.n[w]+1]
      } 
      learnAbilityUnknownLos.mat[i,iii]=sum.G
    }
    
    minLos[i]=opt.obj$objval
    optNumMd[i]=length(which(opt.obj$opt.n>0))
    avgVol[i]=sum(opt.obj$opt.n)/optNumMd[i]
    optSolution.list=lappend.yz(optSolution.list,opt.obj)
  }
  summaryDf=cbind(random.seeds, minLos, optNumMd, avgVol)
  names(summaryDf)=c('random.seeds', 'minLos', 'optNumMd', 'avgVol')
  return(list(optSolution.summary=summaryDf, optSolution.list=optSolution.list, learnAbilityUnknownLos.mat= learnAbilityUnknownLos.mat))
}


n.random.seeds=20


solveByQuarter.dec302013 = function(quarterLap, n.random.seeds, pickBestDoctors, picked.n.md, m.ran){
  n.md.opt=vol.mean.opt=vol.std.opt=los.mean.opt.unobsLearn=los.std.opt.unobsLearn=los.mean.opt=los.std.opt=rep(NA,nrow(quarterLap)) 
  avgVolPerformingMdList=nMdOptList=losVecOptList=list()
  
  for (i in 1:nrow(quarterLap)){
    cat('processing i=',i, 'out of', nrow(quarterLap), 'rows', '\n')
    state=quarterLap[i,'hospst']
    year=quarterLap[i,'year']
    right.G.list=get(paste('G.list', tolower(state),year,sep='.'))
    
    tmp=solveOptAllocation(quarterLap[i,'n.lap'],  right.G.list, seq(n.random.seeds),  pickBestDoctors, picked.n.md=picked.n.md, m.ran=m.ran)  
    
    learnAbilityUnknownLos.mat=tmp$learnAbilityUnknownLos.mat
    
    
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
    los.mean.opt[i]=mean(tmp$optSolution.summary[,'minLos'])
    los.std.opt[i]=sd(tmp$optSolution.summary[,'minLos'])
    
    los.mean.opt.unobsLearn[i]=mean(as.vector(learnAbilityUnknownLos.mat))
    los.std.opt.unobsLearn[i]=sd(as.vector(learnAbilityUnknownLos.mat))
  }
  detailed.losOpt=as.data.frame(do.call(rbind,losVecOptList))
  detailed.avgVolOpt=as.data.frame(do.call(rbind, avgVolPerformingMdList))
  detailed.numMdOpt=as.data.frame(do.call(rbind,nMdOptList))
  names(detailed.losOpt)=paste('optLos.','seed.',seq(n.random.seeds),sep='')
  names(detailed.numMdOpt)=paste('optNumMd.','seed.',seq(n.random.seeds),sep='')
  names(detailed.avgVolOpt)=paste('optAvgVol.','seed.',seq(n.random.seeds),sep='')
  
  summaryDf=cbind(quarterLap,n.md.opt=n.md.opt, vol.mean.opt=vol.mean.opt, vol.std.opt=vol.std.opt, los.mean.opt=los.mean.opt, los.std.opt=los.std.opt,los.mean.opt.unobsLearn=los.mean.opt.unobsLearn,los.std.opt.unobsLearn=los.std.opt.unobsLearn,detailed.losOpt, detailed.numMdOpt,detailed.avgVolOpt)
  return(summaryDf)  
}

#vol.mean.obs is the number of LAP in the quarter for surgeons who have at least one surgery in the quarter
quarterLap=ddply(anaDf.raw.1,c('hospst','dqtr','year'),function(x){c(n.lap=nrow(x),los.obs=sum(as.numeric(x[,'los'])), n.md.obs=length(unique(x[,'mdnum1.r'])), vol.mean.obs=nrow(x)/length(unique(x[,'mdnum1.r'])))})

opt.by.st.quarter=solveByQuarter.dec302013(quarterLap, 50, T, 15, 30)
#reduction
(sum(opt.by.st.quarter$los.obs)-sum(opt.by.st.quarter$los.mean.opt))/sum(opt.by.st.quarter$los.obs)


sum(opt.by.st.quarter$los.mean.opt)
sum(opt.by.st.quarter$los.obs)/sum()

#0.40800789


(sum(opt.by.st.quarter$los.obs)-sum(opt.by.st.quarter$los.mean.opt.unobsLearn))/sum(opt.by.st.quarter$los.obs)
# -0.2784



namesVec=paste('opt.by.st.quarter.random.doc','[',sQuote(paste('optLos.seed.',seq(50),sep='')),']',sep='')
lapply(namesVec,get)
opt.by.st.quarter.random.doc['optLos.seed.1']
str(opt.by.st.quarter.random.doc)








#79 is three states, and different quaters, see object quarterLap

str(opt.by.st.quarter.random.doc)

#I like to lump them into year



names(opt.by.st.quarter)
head(opt.by.st.quarter)

#30 is number of random seeds
nMdPlotDf=function(opt.by.st.quarter){
  
  output=ddply(opt.by.st.quarter,'year', function(x){
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
 
  return(output)
  
} 

nMdPlotDf.random=nMdPlotDf(opt.by.st.quarter.random.doc)
nMdPlotDf.best=nMdPlotDf(opt.by.st.quarter.best.doc)


str(opt.by.st.quarter.random.doc)
names(nMdPlotDf.random)

yy.nmd.random=t(as.matrix(nMdPlotDf.random[,c(2,3)]))
lower.nmd.random=rbind(rep(0.0001,7),nMdPlotDf.random[,3]-nMdPlotDf.random[,4])
upper.nmd.random=rbind(rep(0.0001,7),nMdPlotDf.random[,5]-nMdPlotDf.random[,3])

yy.nmd.best=t(as.matrix(nMdPlotDf.best[,c(2,3)]))
lower.nmd.best=rbind(rep(0.0001,7),nMdPlotDf.best[,3]-nMdPlotDf.best[,4])
upper.nmd.best=rbind(rep(0.0001,7),nMdPlotDf.best[,5]-nMdPlotDf.best[,3])


yy.nmd=rbind(yy.nmd.best,yy.nmd.random[2,])
lower.nmd=rbind(lower.nmd.best,lower.nmd.random[2,])
upper.nmd=rbind(upper.nmd.best,upper.nmd.random[2,])

rownames(yy.nmd)=rownames(lower.nmd)=rownames(upper.nmd)=c('obs','random','best')


# error.bar <- function(x, y, upper, lower=upper, length=0.05,...){
#   if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
#     stop("vectors must be same length")
#   arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
#   legend('topleft',legend=c('observed','regionalization (unknown learning ability)','regionalization (known learning ability)'), fill=c('black',"grey","white"), inset=c(0.01,0.01), cex=0.7)
#   box()
# } 



error.bar <- function(x, y, upper, lower=upper, length=0.05,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
  legend('topleft',legend=c('observed','regionalization'), fill=c("grey","white"), inset=c(0.01,0.01), cex=0.8)
  box()
} 



#I do not want to get into random (not knowning learning ability cass, because I found this would lead the finding that there is already a lot of selection going on, I do not want to get into this issue)
pdf('C:/Dropbox/paper/surgVol/optimalReferral/figure/numMdPlot_dec232013.pdf')
# barx.nmd <- barplot(yy.nmd, beside=TRUE,col=c("black","grey","white"), ylim=c(0,20), names.arg=seq(2004,2010), axis.lty=1, xlab="Year", ylab="Average number of MIRP-performing surgeons per quarter")
#error.bar(barx.nmd, yy.nmd, lower.nmd, upper.nmd)
barx.nmd <- barplot(yy.nmd[c(1,3),], beside=TRUE ,col=c("grey","white"), ylim=c(0,20), names.arg=seq(2004,2010), axis.lty=1, xlab="Year", ylab="Average number of MIRP-performing surgeons per quarter")
error.bar(barx.nmd, yy.nmd[c(1,3),], lower.nmd[c(1,3),], upper.nmd[c(1,3),])
dev.off()

yy.nmd[1,]/yy.nmd[3,]

#next get another plot (stop here)
avgvolperfmdPlotDf=function(opt.by.st.quarter) {
  
  out=ddply(opt.by.st.quarter,'year', function(x){
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
  return(out)
  
}


avgvol.random=avgvolperfmdPlotDf(opt.by.st.quarter.random.doc)
yy.volpermd.random=t(as.matrix(avgvol.random[,c(2,3)]))
lower.volpermd.random=rbind(rep(0.0001,7),avgvol.random[,3]-avgvol.random[,4])
upper.volpermd.random=rbind(rep(0.0001,7),avgvol.random[,5]-avgvol.random[,3])

avgvol.best=avgvolperfmdPlotDf(opt.by.st.quarter.best.doc)
yy.volpermd.best=t(as.matrix(avgvol.best[,c(2,3)]))
lower.volpermd.best=rbind(rep(0.0001,7),avgvol.best[,3]-avgvol.best[,4])
upper.volpermd.best=rbind(rep(0.0001,7),avgvol.best[,5]-avgvol.best[,3])


yy.volpermd=rbind(yy.volpermd.best,yy.volpermd.random[2,])
lower.volpermd=rbind(lower.volpermd.best,lower.volpermd.random[2,])
upper.volpermd=rbind(upper.volpermd.best,upper.volpermd.random[2,])


pdf('C:/Dropbox/paper/surgVol/optimalReferral/figure/avgvolperfMdPlot.pdf')
# barx.volpermd <- barplot(yy.volpermd, beside=TRUE,col=c("black","grey","white"), ylim=c(0,40), names.arg=seq(2004,2010), axis.lty=1, xlab="Year", ylab="Average number of MIRPs per performing surgeon per quarter")
# error.bar(barx.volpermd,yy.volpermd,lower.volpermd,upper.volpermd)
barx.volpermd <- barplot(yy.volpermd[c(1,3),], beside=TRUE,col=c("grey","white"), ylim=c(0,40), names.arg=seq(2004,2010), axis.lty=1, xlab="Year", ylab="Average number of MIRPs per performing surgeon per quarter")
error.bar(barx.volpermd,yy.volpermd[c(1,3),],lower.volpermd[c(1,3),],upper.volpermd[c(1,3),])
dev.off()

tiff('C:/Dropbox/paper/surgVol/optimalReferral/figure/avgvolperfMdPlot.tiff')
# barx.volpermd <- barplot(yy.volpermd, beside=TRUE,col=c("black","grey","white"), ylim=c(0,40), names.arg=seq(2004,2010), axis.lty=1, xlab="Year", ylab="Average number of MIRPs per performing surgeon per quarter")
# error.bar(barx.volpermd,yy.volpermd,lower.volpermd,upper.volpermd)
barx.volpermd <- barplot(yy.volpermd[c(1,3),], beside=TRUE,col=c("grey","white"), ylim=c(0,40), names.arg=seq(2004,2010), axis.lty=1, xlab="Year", ylab="Average number of MIRPs per performing surgeon per quarter", main='Figure 1: Observed average volume vs. volume under regionalization',cex.main=0.9)
error.bar(barx.volpermd,yy.volpermd[c(1,3),],lower.volpermd[c(1,3),],upper.volpermd[c(1,3),])
dev.off()

yy.volpermd[c(3),]/yy.volpermd[c(1),]

# [,1]       [,2]       [,3]      [,4]      [,5]       [,6]       [,7]
# mean.obs.avgVol.perq  9.3251634  6.6422619  7.0951659  5.926135  4.527448  2.7814353  2.5557244
# 23.6412037 25.1227315 26.7980952 27.794411 24.889878 21.7287374 23.2409402
# > yy.volpermd[c(3),]/yy.volpermd[c(1),]
# [1] 2.5352053 3.7822555 3.7769512 4.6901414 5.4975514 7.8120592 9.0936802


losSumVec.random=unlist(lapply(opt.by.st.quarter.random.doc[paste('optLos.seed.',seq(50),sep='')],sum))
losSumVec.best=unlist(lapply(opt.by.st.quarter.best.doc[paste('optLos.seed.',seq(50),sep='')],sum))

losSum.sd.random=sd(losSumVec.random)
losSum.sd.best=sd(losSumVec.best)
losSum.mean.random=mean(losSumVec.random)
losSum.mean.best=mean(losSumVec.best)

los.obs=sum(opt.by.st.quarter.random.doc$los.obs)

mean.los=matrix(c(los.obs, losSum.mean.random, losSum.mean.best),nrow=3)

lower.meanlos.best=quantile(losSumVec.best, probs=c(0.025))
upper.meanlos.best=quantile(losSumVec.best, probs=c(0.975))

lower.meanlos.random=quantile(losSumVec.random, probs=c(0.025))
upper.meanlos.random=quantile(losSumVec.random, probs=c(0.975))

lower.meanLos=matrix(c(los.obs,lower.meanlos.random,lower.meanlos.best),nrow=3)
upper.meanLos=matrix(c(los.obs,upper.meanlos.random,upper.meanlos.best),nrow=3)

lower.meanLosForPlot=lower.meanLos
lower.meanLosForPlot[3]=lower.meanLos[3]-40 #do this just to make the error bar looks nice


totLap=sum(quarterLap[,'n.lap']) #6172 surgery


pdf('C:/Dropbox/paper/surgVol/optimalReferral/figure/los_dec312013_1.pdf')
# barx.nmd <- barplot(mean.los/totLap, beside=TRUE, col=c("black","grey","white"), ylim=c(1,2), axis.lty=1, xlab="Year", ylab="Average number of MIRP-performing surgeons per quarter")
# error.bar(barx.nmd, mean.los/totLap, lower.meanLos/totLap, upper.meanLos/totLap)
barx.nmd <- barplot(mean.los[c(1,3)]/totLap, beside=TRUE, col=c("grey","white"), ylim=c(1,2), axis.lty=1, xlab="Year", ylab="Average number of MIRP-performing surgeons per quarter")
error.bar(barx.nmd, mean.los[c(1,3)]/totLap, lower.meanLos[c(1,3)]/totLap, upper.meanLos[c(1,3)]/totLap)
dev.off()

#mean.los/totLap 
# [1,] 1.6916721
# [2,] 1.1213770
# [3,] 1.0014565
# 
# (1.6916721- 1.1213770)/1.6916721 = 33.71%
# (1.6916721- 1.0014565)/1.6916721= 40.80%

# barPlotWithErrorBar(  mean.los/totLap #can be vector, number of column or length is number of blocks #each block can have one or nrow stacked bars
#                                , (mean.los-lower.meanLosForPlot)/totLap #can be vector positive numbers, deviation from yvalue, you can put a very small number to make it no error bar
#                                , (upper.meanLos-mean.los)/totLap #can be vector positive numbers, deviation from yvalue, you can put a very small number to make it no error bar
#                                , c(0,2)
#                                , c('','','') #i.e., values on the x axis
#                                , c('black','grey',"white") #a vector with length of nrow ymat, is ymat is a vector, then only one bar in each block, then this is a scalor there are ncol block, each block has nrow stached bars
#                                , ''
#                                , 'LOS'
#                                ,  0.1 #use this to adjust for wdieth of error bar
#                                , 'topright'
#                                , c('observed','regionalization (unknown surgeon ability)','regionalization (known surgeon ability)') #length should be nrow of ymat
#                                , legendInset=c(0.01,0.01)
#                                , 'C:/Dropbox/paper/surgVol/optimalReferral/figure/losByPolicy_dec302013.pdf'
#                                , axis.lty=1
#                                , beside=TRUE
# )


barPlotWithErrorBar(  mean.los[c(1,3)]/totLap #can be vector, number of column or length is number of blocks #each block can have one or nrow stacked bars
                      , (mean.los-lower.meanLosForPlot)[c(1,3)]/totLap #can be vector positive numbers, deviation from yvalue, you can put a very small number to make it no error bar
                      , (upper.meanLos-mean.los)[c(1,3)]/totLap #can be vector positive numbers, deviation from yvalue, you can put a very small number to make it no error bar
                      , c(0,2)
                      , c('','') #i.e., values on the x axis
                      , c('grey',"white") #a vector with length of nrow ymat, is ymat is a vector, then only one bar in each block, then this is a scalor there are ncol block, each block has nrow stached bars
                      , ''
                      , 'LOS'
                      ,  0.1 #use this to adjust for wdieth of error bar
                      , 'topright'
                      , c('observed','regionalization') #length should be nrow of ymat
                      , legendInset=c(0.01,0.01)
                      , 'C:/Dropbox/paper/surgVol/optimalReferral/figure/losByPolicy_jan042014.pdf'
                      , axis.lty=1
                      , beside=TRUE
)



names(quarterLap)


str(opt.by.st.quarter.random.doc)

# pdf('C:/Dropbox/paper/surgVol/optimalReferral/figure/numMdPlot_dec232013.pdf')
# barx.nmd <- barplot(yy.nmd, beside=TRUE,col=c("black","grey","white"), ylim=c(0,20), names.arg=seq(2004,2010), axis.lty=1, xlab="Year", ylab="Average number of MIRP-performing surgeons per quarter")
# error.bar(barx.nmd,yy.nmd, lower.nmd, upper.nmd)
# dev.off()



tabout = table.yz(c('age4cat', 'race5cat','comorbcat','pay1','year'),'los',anaDf.raw.1)
sampleSizeByLos= ddply(anaDf.raw.1, 'los', function(x){nrow(x)})
count.yz(anaDf.raw.1,'age4cat',getPercent=T)

count.yz(anaDf.raw.1,'los',getPercent=T)
count.yz(anaDf.raw.1,'race5cat',getPercent=T)
count.yz(anaDf.raw.1,'comorbcat',getPercent=T)
count.yz(anaDf.raw.1,'pay1',getPercent=T)
count.yz(anaDf.raw.1,'year',getPercent=T)



c(mean(anaDf.raw.1[,'n.lag1.lap']),sd(anaDf.raw.1[,'n.lag1.lap']))
  
 names(anaDf.raw.1)

save(anaDf.raw.1 ,tabout, sampleSizeByLos
     ,testModel.lag1, testModel.lag2,testModel.lag3,testModel.lag4,testModel.lag5,testModel.lag6
     , fitList.logLike, fitList, meanLosbyvolTile
     , tileList,inner.knots.list, boundary.knots.list, lagvolvec, bsDegree,  nbt, avgLearnCurveByYear, plotAvgLearnDf, cex.pch
     ,avgLearn.allyear, avgLearn.2004, avgLearn.2005, avgLearn.2006, avgLearn.2007, avgLearn.2008, avgLearn.2009, avgLearn.2010, avgLearn.20062010, plotAvgLearnDf,lagvol.bs
     , nCuts, meanLosMat, ubLbLearnCurveTiles, lbub, lb, ub, avgLearnCurve    
     , ia.docs, ny.docs, md.docs
     , ia.curves.byYear
     , md.curves.byYear
     , ny.curves.byYear   
     , quarterLap
     ,G.list.ia.2004
     ,G.list.md.2004
     ,G.list.ny.2004
     ,G.list.ia.2005
     ,G.list.md.2005
     ,G.list.ny.2005
     ,G.list.ia.2006
     ,G.list.md.2006
     ,G.list.ny.2006
     ,G.list.ia.2007
     ,G.list.md.2007
     ,G.list.ny.2007  
     ,G.list.ia.2008
     ,G.list.md.2008
     ,G.list.ny.2008
     ,G.list.ia.2009
     ,G.list.md.2009
     ,G.list.ny.2009
     ,G.list.ia.2010
     ,G.list.md.2010
     ,G.list.ny.2010
     , opt.by.st.quarter.best.doc #solution for knowing best doctor
     , opt.by.st.quarter.random.doc #solutgion for not knowning best doctors
     , yy.nmd
     , lower.nmd
     , upper.nmd
     , yy.volpermd
     , lower.volpermd
     , upper.volpermd
     , file="Z:/j_scrdata/lapLearn/bsplineSensitivity_feb052014.RData")
 


(load(file="Z:/j_scrdata/lapLearn/bsplineSensitivity_feb052014.RData"))



# #get table 1
# 
# anaDf.raw.1
# names(anaDf.raw.1)
# tabout = table.yz(c('age4cat', 'race5cat','comorbcat','pay1','year'),'los',anaDf.raw.1)

# xtable(do.call('rbind',tabout$cellsize.list))
# 
# xtable(tabout$tabout.mat)
# 
# 
# ddply(anaDf.raw.1, 'los', function(x){c(mean=mean(x[,'n.lag1.lap']),sd=sd(x[,'n.lag1.lap']))})
# ddply(anaDf.raw.1, 'los', function(x){nrow(x)})
# anova(aov(n.lag1.lap ~ los, data=anaDf.raw.1))
# 
# 
# 
# table(anaDf.raw.1[,'comorbcat'])
# table(anaDf.raw.1[,'year'])
# 
# scilpPatAlloc.yz(50,G.list.ia.2004[10:20])
# scilpPatAlloc.yz(200,G.list.ia.2008[1:10])
# 
# 
# names(anaDf.raw.1)
# 
# 
# fitted(fit)
# 
# ?VGAM::predict
# 
# 
# 
# names(anaDf.raw.1)
# 
# xb.obj=xb.matched.yz(fit$beta,
#                      model.matrix(passVarToFormula.yz('los',c(paste('n.lag1.lap.log.bs',seq(3),sep=''),'age4cat', 'race5cat','comorbcat','pay1','year','mdnum1.r'))
#                                   , anaDf.raw.1)
# )
# xb=xb.obj$xb.matched
# predProbMat=xbCuts2probmat.orderedprobit.yz(xb,fit$alpha)
# 
# 
# cbind(anaDf.raw.1[,'los'],predProbMat, predict(fit, newdata=anaDf.raw.1)$fit)
# cbind(anaDf.raw.1$los, predict(fit, newdata=anaDf.raw.1)$fit)
# 
# 
# 
# mean(predProbMat %*% matrix(seq(ncol(predProbMat)), ncol=1)) # 1.6917993
# mean(as.numeric(anaDf.raw.1[,'los'])) #1.692208
# 
# 
# #the predictions are very close, so it is good sign.
# 
# cbind(anaDf.raw.1[,'los'], predProbMat)
# 
# ddply(data.frame(los=anaDf.raw.1[,'los'], predProbMat),'los', function(x){mean(as.matrix(x[,2:8])%*%matrix(seq(ncol(predProbMat)), ncol=1))})
# 
# # los        V1
# # 1   1 1.4139851
# # 2   2 1.9248169
# # 3   3 2.0889311
# # 4   4 2.1296789
# # 5   5 2.2418668
# # 6   6 2.2674645
# # 7   7 2.8553684
# 
# 
# #generate average learning cruve
# 
# testDf=anaDf.raw.1
# volumeVec=range(anaDf.raw.1[,'n.lag1.lap'])
# 
# i=1
# bsDegree=2
# boundary.knots[[15]]
# 
# for(i in 1:length(volumeVec)){
#   
#   
#   lagvol.bs=bsWrapper.yz(log1p(volumeVec[i]) #this is the x in bs function
#                          , log.lag1.innerKnots #inner knots, if it contains boundary knots, function will stop and issue erro message
#                          , 'n.lag1.lap.log.bs' #output data's columen name stem
#                          , degree=bsDegree
#                          , boundary.knots=boundary.knots
#                          , intercept=FALSE
#                          ,  dataType='data.frame'
#                          #or ='data.frame'
#                          , closenessCutoffPct=0.02 #closeness of inner knots to bdnots, lower than this number means it is too close.. and not good, 0.02 means two percent of range
#   )$bsdata
#   
#   bsPart=lagvol.bs[rep(1,nrow(anaDf.raw.1)),]
#   
#   cbind(deldfcols.yz(anaDf.raw.1,c("n.lag1.lap.log.bs1", "n.lag1.lap.log.bs2", "n.lag1.lap.log.bs3")),)
#   
#   
# }
# names(anaDf.raw.1)
# 
# 
# xb.obj=xb.matched.yz(fit$beta,
#                      model.matrix(passVarToFormula.yz('los',c(paste('n.lag1.lap.log.bs',seq(3),sep=''),'age4cat', 'race5cat','comorbcat','pay1','year','mdnum1.r'))
#                                   , anaDf.raw.1)
# )
# xb=xb.obj$xb.matched
# predProbMat=xbCuts2probmat.orderedprobit.yz(xb,fit$alpha)
# 
# 
# 
# 
# 
# pred=predict(fit, newdata=anaDf.raw.1)
# str(pred)
# str(fit)
# predicted(fit)
# 
# #I chose k=15, tileList[[15]], inner knot=0.8 i.e., 80 quantile which is also the best fit
# # plot(seq(length(tileList)),unlist(lapply(fitList,logLik)),type='b')
# #how I decide the final model basis spline selection.
# #note from tileList[[1]] and tileList[[2]] has 5 and 9 mid points, so it is overfitting.
# #the valid comparison is among tileList[[3]] on, from three mid points to even single mid point.
# #at last tileList[[15]] which has only one single point has the best fit, so we need to use it.
# pdf('C:/Dropbox/paper/surgVol/optimalReferral/figure/splineSelectionLogLikelihood.pdf')
# plot(seq(length(tileList[3:16])),unlist(lapply(fitList[3:16],logLik)),type='b', xlab='Different choice of inner knots', ylab='log-likelihood', main='Inner knots selection based on likelihoold')
# dev.off()
# 
# #the 15th model is the best I would pick, so what I need is then based on the model to predict run optimization problem.
# head(summary(fitList[[15]])$coefficients,12)
# 
# #----------------next is to solve the optimization problem--------------------
# 
# 
# #the idea we have is to solve for the optimal solution
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# fit.list=lappend.yz(fit.list,testModel.lag1bs)
# 
# bsMat = as.matrix(gen.bs(log1p(seq(0,50)),knots.list$lag1,bdknots.list$lag1,2,'lag1.nlap.bs')[,c(1,2,3)])
# vec=bsMat %*% matrix(testModel.lag1bs$beta[1:3],ncol=1)
# minLos.vol[i]=which.min(vec)
# losbyvol[,i]=vec
# }
# 
# head(summary(fit.list[[7]])$coefficients[7:9,])
# plot(log1p(seq(0,50)),losbyvol[,8],type='b')
# #76 per year, this meakes sense, also based on significant level of the terms
# unlist(lapply(fit.list,logLik)) #8th is the best, 7th is is the second bead, but think of terms significance
# #I think we need to choose 7th one.
# #now we can use the final model.
# 
# 
# #=----------fine search for the percentile
# 
# fine.fit.list=list()
# fine.tileVec=seq(0.6,0.8,0.025)
# fine.minLos.vol=rep(NA,length(fine.tileVec))
# fine.losbyvol=matrix(NA,nrow=51,ncol=length(fine.tileVec))
# 
# for(i in 1:length(fine.tileVec)){
#   print(i)
#   cleaned.obj=finalCleaning(anaDf.raw, fine.tileVec[i],2)
#   cleaned=cleaned.obj$df
#   knots.list=cleaned.obj$knots
#   bdknots.list=cleaned.obj$bdknots
#   chkMissVars.lag6=c('n.lag6.lap','n.lag6.nonLap','n.lag6.rap','age3cat','race5cat','year','comorbcat','pay1','mdnum1.r')
#   anaDf.raw.1 = droplevels(cleaned[complete.cases(cleaned[,chkMissVars.lag6]),])
#   
#   testModel.lag1bs <- clm(los ~ lag1.nlap.bs1+lag1.nlap.bs2+lag1.nlap.bs3+age4cat+race5cat+comorbcat+pay1+year+mdnum1.r, data=anaDf.raw.1, link=c('probit'))
#   
#   fine.fit.list=lappend.yz(fine.fit.list,testModel.lag1bs)
#   
#   bsMat = as.matrix(gen.bs(log1p(seq(0,50)),knots.list$lag1,bdknots.list$lag1,2,'lag1.nlap.bs')[,c(1,2,3)])
#   vec=bsMat %*% matrix(testModel.lag1bs$beta[1:3],ncol=1)
#   fine.minLos.vol[i]=which.min(vec)
#   fine.losbyvol[,i]=vec
# }
# 
# plot(fine.tileVec,fine.minLos.vol)
# plot(unlist(lapply(fine.fit.list,logLik)))
# cbind(fine.losbyvol)
# 
# #Final choice
# #lag=1
# #use second order B-spline 
# #use percentile=0.75 for lag1 as knot
# 
# save(anaDf.raw,cleaned.obj,fine.tileVec,fine.minLos.vol,fine.fit.list,fine.losbyvol,tileVec,minLos.vol,fit.list,losbyvol
#      , file='Z:/j_scrdata/lapLearn/lagKnotPctSearch_may162017.RData'
# )
# 
# load(file='Z:/j_scrdata/lapLearn/lagKnotPctSearch_may162017.RData')
# 
# plot(fine.losbyvol[,6])
# #test two knots
# 
# 
# twoKnots.fit.list=list()
# #0.3 seems to good
# firstKnot.tileVec=seq(0.1,0.5,0.05)
# twoknots.minLos.vol=rep(NA,length(firstKnot.tileVec))
# twoknots.losbyvol=matrix(NA,nrow=51,ncol=length(firstKnot.tileVec))
# i=2
# for(i in 1:length(firstKnot.tileVec)){
#   print(i)
#   cleaned.obj=finalCleaning(anaDf.raw, c(firstKnot.tileVec[i],0.75),2)
#   cleaned=cleaned.obj$df
#   knots.list=cleaned.obj$knots
#   bdknots.list=cleaned.obj$bdknots
#   
#   chkMissVars.lag6=c('n.lag6.lap','n.lag6.nonLap','n.lag6.rap','age3cat','race5cat','year','comorbcat','pay1','mdnum1.r')
#   
#   anaDf.raw.1 = droplevels(cleaned[complete.cases(cleaned[,chkMissVars.lag6]),])
#   
#   testModel.lag1bs <- clm(los ~ lag1.nlap.bs1+lag1.nlap.bs2+lag1.nlap.bs3+lag1.nlap.bs4+age4cat+race5cat+comorbcat+pay1+year+mdnum1.r, data=anaDf.raw.1, link=c('probit'))
#   
#   
#   plot(summary(testModel.lag1bs)$coefficients[7:48,1])
#   
#   
#   testModel.lag1bs <- clm(los ~ factor(n.lag1.lap)+lag1.nlap.bs4+age4cat+race5cat+comorbcat+pay1+year+mdnum1.r, data=anaDf.raw.1, link=c('probit'))
#   summary(testModel.lag1bs)
#   
#   
#   
#   twoKnots.fit.list=lappend.yz(twoKnots.fit.list,testModel.lag1bs)
#   
#   bsMat = as.matrix(gen.bs(log1p(seq(0,50)),knots.list$lag1,bdknots.list$lag1,2,'lag1.nlap.bs')[,c(1,2,3,4)])
#   
#   vec=bsMat[,c(1,2,3)] %*% matrix(testModel.lag1bs$beta[1:3],ncol=1)
#   twoknots.minLos.vol[i]=which.min(vec)
#   twoknots.losbyvol[,i]=vec
# }
# 
# 
# plot(fine.losbyvol[,6])
# 
# head(summary(twoKnots.fit.list[[1]])$coefficients[7:10,])
# plot(log1p(seq(0,50)),losbyvol[,8],type='b')
# #76 per year, this meakes sense, also based on significant level of the terms
# unlist(lapply(fit.list,logLik)) #8th is the best, 7th is is the second bead, but think of terms significance
# #I think we need to choose 7th one.
# #now we can use the final model.
# 
