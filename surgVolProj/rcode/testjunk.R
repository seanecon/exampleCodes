# 
# 
# nobsformu=passVarToFormula.yz('', c('age4cat', 'race5cat','comorbcat','pay1','year','mdnum1.r'))
# nobsformu.nomd=passVarToFormula.yz('', c('age4cat', 'race5cat','comorbcat','pay1','year'))
# 
# 
# k=15
# fit=fitList[[k]]
# 
# cuts=fit$alpha 
# beta=fit$beta
# 
# bsDegree=2
# whatYears=as.character(seq(2004,2010))
# lagvolvec=seq(0,50,1)
# indf=nomiss.lag6
# boundary.knots=boundary.knots.list[[k]]
# inner.knots=inner.knots.list[[k]]
# bsVnStem='n.lag1.lap.log.bs'
# indf=nomiss.lag6
# yearVn='year'
# 
# predLos=getAvgLearnCurve(   beta #beta coefficient vector
#                               ,cuts #cuts coefficient vector
#                               ,nobsformu #non Bspline part formula nobsformu=passVarToFormula.yz('', c('age4cat', 'race5cat','comorbcat','pay1','year','mdnum1.r'))
#                               ,nomiss.lag6 #input data, the data used to fit model
#                               ,whatYears #the years of interest
#                               ,lagvolvec# LAP volume of inerest
#                               ,bsDegree #bspline degreee
#                               ,inner.knots #innner knots of bspline
#                               ,boundary.knots #bspline boundary
#                               ,yearVn='year' #appear in input data
#                               , bsVnStem='n.lag1.lap.log.bs')
# 
# 
# 
# predLos.2004=getAvgLearnCurve(   beta
#                                ,cuts
#                                ,nobsformu
#                                ,nomiss.lag6
#                                ,'2004'
#                                ,lagvolvec
#                                ,bsDegree
#                                ,inner.knots
#                                ,boundary.knots
#                                ,yearVn='year'
#                                , bsVnStem='n.lag1.lap.log.bs')
# 
# predLos.2010=getAvgLearnCurve(   beta
#                                     ,cuts
#                                     ,nobsformu
#                                     ,nomiss.lag6
#                                     ,'2010'
#                                     ,lagvolvec
#                                     ,bsDegree
#                                     ,inner.knots
#                                     ,boundary.knots
#                                     ,yearVn='year'
#                                     , bsVnStem='n.lag1.lap.log.bs')
# 
# 
# 
# 


# predLos.nomd=getAvgLearnCurve(beta #beta coefficient vector
#                                  ,cuts #cuts coefficient vector
#                                  ,nobsformu #non Bspline part formula nobsformu=passVarToFormula.yz('', c('age4cat', 'race5cat','comorbcat','pay1','year','mdnum1.r'))
#                                  ,nomiss.lag6 #input data, the data used to fit model
#                                  ,whatYears #the years of interest
#                                  ,lagvolvec# LAP volume of inerest
#                                  ,bsDegree #bspline degreee
#                                  ,inner.knots #innner knots of bspline
#                                  ,boundary.knots #bspline boundary
#                                  ,yearVn='year' #appear in input data
#                                  , bsVnStem='n.lag1.lap.log.bs')
# 
# plot(predLos,ylim=c(1,2.3))
# lines(predLos.2004)
# lines(predLos.2010)
# which.min(predLos.2004)


getAvgLearnCurve = function(beta #beta coefficient vector
                              ,cuts #cuts coefficient vector
                              ,nobsformu #non Bspline part formula nobsformu=passVarToFormula.yz('', c('age4cat', 'race5cat','comorbcat','pay1','year','mdnum1.r'))
                              ,nomiss.lag6 #input data, the data used to fit model
                              ,whatYears #the years of interest
                              ,lagvolvec# LAP volume of inerest
                              ,bsDegree #bspline degreee
                              ,inner.knots #innner knots of bspline
                              ,boundary.knots #bspline boundary
                              ,yearVn='year' #appear in input data
                              , bsVnStem='n.lag1.lap.log.bs'){
  
  
  
  #generate basis spline terms
  lagvol.bs=bsWrapper.yz(log1p(lagvolvec) #this is the x in bs function
                         , inner.knots #inner knots, if it contains boundary knots, function will stop and issue erro message
                         , bsVnStem #output data's columen name stem
                         , degree=bsDegree
                         , boundary.knots=boundary.knots
                         , intercept=FALSE
                         , dataType='data.frame'
                         #or ='data.frame'
                         , closenessCutoffPct=0.02 #closeness of inner knots to bdnots, lower than this number means it is too close.. and not good, 0.02 means two percent of range
  )$bsdata
  n.basis=ncol(lagvol.bs)
  bsformu=passVarToFormula.yz('', paste(bsVnStem,seq(n.basis),sep=''))
  
  
  #yearsDf= subset(indf,yearvn %in% whatYears) 
  yearsDf= indf[which(indf[,yearVn] %in% whatYears),]
  
  nrow(yearsDf)
  x.noBs=model.matrix(nobsformu,yearsDf)
  
  xbout.noBs=xb.matched.yz(beta,x.noBs)
  
#   xbout.noBs$unmatched
  xb.noBs=xbout.noBs$xb.matched
  xb.noBsPart=xbout.noBs$xb.matched
  meanLosVec=rep(NA,length(lagvolvec))
  for(vi in 1:length(lagvolvec)){
    print(vi)
    bsDf.vi = as.data.frame(lagvol.bs[vi,,drop=F][rep(1,nrow(yearsDf)),])
    x.bs=model.matrix(bsformu, data = bsDf.vi)
    xbout.bs=xb.matched.yz(beta,x.bs)
    xb.bs=xbout.bs$xb.matched
    #xbout.bs$matched
    xb=xb.bs+xb.noBs
    probMat=xbCuts2probmat.orderedprobit.yz(xb,cuts)
    meanLosVec[vi]=mean(probMat %*% matrix(seq(1:(length(cuts)+1)), ncol=1)) 
  }
  
  return(meanLosVec)
}
  