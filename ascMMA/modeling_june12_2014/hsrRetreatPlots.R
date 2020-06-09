# 

#save.image(file = "Z:/j_scrdata/ascMMA/rImages/ascMMA_june172014.RData")

# save(anaDfNoBs,file='Z:/j_scrdata/ascMMA/result/anaDfNoBs.RData')

(load(file='Z:/j_scrdata/ascMMA/result/anaDfNoBs.RData'))



addBs=function(knots.tiles, anaDfNoBs){
  
  
  bslist=bsWrapper1.yz(anaDfNoBs[,'.qtr.since2006'] #this is the x in bs function
                       , quantile(anaDfNoBs[,'.qtr.since2006'],probs=knots.tiles) #inner knots, if it contains boundary knots, function will stop and issue erro message
                       , paste('.qtr.since2006','.bs',sep='') #output data's columen name stem
                       , degree=2
                       , dataType='data.frame'
                       , Boundary.knots=quantile(anaDfNoBs[,'.qtr.since2006'],prob=c(0,1))
  )
  n.basis=bslist$n.basis
  anaDfWithBs=cbind(anaDfNoBs,bslist$bsdata)
  
  names(anaDfWithBs)=tolower(names(anaDfWithBs))
  
  return(anaDfWithBs)
}


#first add bs
anaDfWithBs=addBs(c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), anaDfNoBs)

#anaDfWithBs=addBs(c(0.2,0.4,0.6,0.8), anaDfNoBs)
names(anaDfWithBs)

#new compile
#first fit a model for total expenditure


#figure 1 outpatient rate with and without FASC


#per capital outpatient vol
fit.percap.outpVol.fasc=glm(
  totaloutpvol ~
    mma2008.impact 
  +firstqtraftermma
  +fascsurgeongrpannual
+ qtr 
  + mean.age + .avg.comorbsum +mean.age
  +race.0+race.1+race.2+race.3+race.4+race.5
  +sex.1
  +.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4+.qtr.since2006.bs5+.qtr.since2006.bs6+.qtr.since2006.bs7+.qtr.since2006.bs8+.qtr.since2006.bs8+.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11,
  , offset=log(n.bene)
  , family = poisson
  , data=anaDfWithBs)

#names(anaDfWithBs)
summary(fit.percap.outpVol.fasc)

#goodness of fit

#this is observed with MMA
predVal=exp(btXbWithShock.yz( coef(fit.percap.outpVol.fasc) 
                              , vcov(fit.percap.outpVol.fasc)  
                              , changedBetas=NULL
                              , 0 #for poission model, the random shock follows a poission 
                              , model.matrix(formula(fit.percap.outpVol.fasc),fit.percap.outpVol.fasc$data) 
                              , 100                        
                              , changedBetasHaveVariation=TRUE
                              , seed=1  
                              , sameShockVec=FALSE   
                              
))


locList=valLoc.yz(anaDfWithBs$'.qtr.since2006')
meanMat=do.call(rbind,lapply(locList,function(x){apply(predVal[x,],2,mean)}))
percap.outpvol.mma.hat=apply(meanMat,1,mean)
percap.outpvol.mma.ci=apply(meanMat,1,function(x){quantile(x,probs=c(0.025,0.975))})
#percap.outpvol.true=unlist(lapply(locList,function(x){mean(anaDfWithBs$percap.outpvol[x])}))

#this is counterfactural without MMA
changedBetas=c(mma2008.impact=0, firstqtraftermma=0)
predVal=exp(btXbWithShock.yz( coef(fit.percap.outpVol.fasc) 
                              , vcov(fit.percap.outpVol.fasc)  
                              , changedBetas=changedBetas
                              , 0 #for poission model, the random shock follows a poission 
                              , model.matrix(formula(fit.percap.outpVol.fasc),fit.percap.outpVol.fasc$data) 
                              , 100                        
                              , changedBetasHaveVariation=TRUE
                              , seed=1  
                              , sameShockVec=FALSE   
                              
))


locList=valLoc.yz(anaDfWithBs$'.qtr.since2006')
meanMat=do.call(rbind,lapply(locList,function(x){apply(predVal[x,],2,mean)}))
percap.outpvol.noMma.hat=apply(meanMat,1,mean)
percap.outpvol.noMma.ci=apply(meanMat,1,function(x){quantile(x,probs=c(0.025,0.975))})

percap.outpvol.mma.nomma=list(
  data.frame(x=seq(24),percap.outpvol.mma.hat,percap.outpvol.mma.ci[1,],percap.outpvol.mma.ci[2,])
  , data.frame(x=seq(24),percap.outpvol.noMma.hat,percap.outpvol.noMma.ci[1,],percap.outpvol.noMma.ci[2,])
   )

plotLineCi.yz(percap.outpvol.mma.nomma
              #each element of the list is a data.frame has four columns, the first column x, the second columen is mean.y, third columen is lowerbound.y, the four column is upperbound.y
              ,xlim=c(0,24)
              ,ylim=c(0.2,0.42)
              ,pchVec=c(1,2) #this should be the same lenght as plotdata.list
              ,ciBar=TRUE #if FALSE then there is no CI bar, then it beomes nonerror bar line point
              ,jit.size=0 #to avoid overlapping
              ,ciBarLwd=1 #width of error bar
              ,type='b' #can line 'l' or both 'b'
              ,gap=0
              ,sfrac=0.01
              ,maintitle=c('per capita outpatient volume: MMA vs. no MMA') #title of plot
              ,x.label=c('quarter since 2006')
              ,y.label=c('per capita outpatient volume (encounter day)')
              ,legendLoc='bottomright'
              ,legend.txt.vec=c('MMA','no MMA') #the legend text characteri vector
              ,cex.legend.text=1
              ,#pdf.path=NULL
              pdf.path=pdf('Z:/j_scrdata/ascMMA/result/fig/percapOutPatientVolMmaNoMma.pdf') #with e.g., C;/abc.pdf
)


#figure 2

# With fasc effect outpatient per capita volume modling 
#__________________________



#per capital outpatient  vol
# fit.percap.outpVol.fasc=glm(
#   totaloutpvol ~
#     mma2008.impact +fascsurgeongrpannual+fascsurgeongrpannual*mma2008.impact
#  # + qtr 
#   + mean.age + .avg.comorbsum +mean.age
#   +race.0+race.1+race.2+race.3+race.4+race.5
#   +sex.1
#   +.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4+.qtr.since2006.bs5+.qtr.since2006.bs6+.qtr.since2006.bs7+.qtr.since2006.bs8+.qtr.since2006.bs8+.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11,
#   , offset=log(n.bene)
#   , family = poisson
#   , data=anaDfWithBs)

#names(anaDfWithBs)
#summary(fit.percap.outpVol.nofasc)
summary(fit.percap.outpVol.fasc)
#this is observed with MMA 


predVal=exp(btXbWithShock.yz( coef(fit.percap.outpVol.fasc) 
                              , vcov(fit.percap.outpVol.fasc)  
                              , changedBetas=NULL
                              , 0 #for poission model, the random shock follows a poission 
                              , model.matrix(formula(fit.percap.outpVol.fasc),fit.percap.outpVol.fasc$data) 
                              , 100                        
                              , changedBetasHaveVariation=TRUE
                              , seed=1  
                              , sameShockVec=FALSE   
                              
))

locList=valLoc.yz(anaDfWithBs$'.qtr.since2006')
meanMat=do.call(rbind,lapply(locList,function(x){apply(predVal[x,],2,mean)}))
percap.outpvol.mma.fasc.hat=apply(meanMat,1,mean)
percap.outpvol.mma.fasc.ci=apply(meanMat,1,function(x){quantile(x,probs=c(0.025,0.975))})
#percap.outpvol.true=unlist(lapply(locList,function(x){mean(anaDfWithBs$percap.outpvol[x])}))


#FASC group 1 MMA
testdf=fit.percap.outpVol.fasc$data
chgLoc=which(testdf[,'.qtr.since2006']>8)
testdf[chgLoc,'fascsurgeongrpannual']=factor(levels(testdf[,'fascsurgeongrpannual'])[1],levels=levels(testdf[,'fascsurgeongrpannual']))

#this is counterfactural without MMA
#this is observed
predVal=exp(btXbWithShock.yz( coef(fit.percap.outpVol.fasc) 
                              , vcov(fit.percap.outpVol.fasc)  
                              , changedBetas=NULL
                              , 0 #for poission model, the random shock follows a poission 
                              , model.matrix(formula(fit.percap.outpVol.fasc),testdf) 
                              , 100                        
                              , changedBetasHaveVariation=TRUE
                              , seed=1  
                              , sameShockVec=FALSE   
                              
))

locList=valLoc.yz(anaDfWithBs$'.qtr.since2006')
meanMat=do.call(rbind,lapply(locList,function(x){apply(predVal[x,],2,mean)}))
percap.outpvol.mma.fasc1.hat=apply(meanMat,1,mean)
percap.outpvol.mma.fasc1.ci=apply(meanMat,1,function(x){quantile(x,probs=c(0.025,0.975))})



#FASC group 2 MMA
testdf=fit.percap.outpVol.fasc$data
chgLoc=which(testdf[,'.qtr.since2006']>8)
testdf[chgLoc,'fascsurgeongrpannual']=factor(levels(testdf[,'fascsurgeongrpannual'])[2],levels=levels(testdf[,'fascsurgeongrpannual']))

#this is counterfactural without MMA
#this is observed
predVal=exp(btXbWithShock.yz( coef(fit.percap.outpVol.fasc) 
                              , vcov(fit.percap.outpVol.fasc)  
                              , changedBetas=NULL
                              , 0 #for poission model, the random shock follows a poission 
                              , model.matrix(formula(fit.percap.outpVol.fasc),testdf) 
                              , 100                        
                              , changedBetasHaveVariation=TRUE
                              , seed=1  
                              , sameShockVec=FALSE   
                              
))

locList=valLoc.yz(anaDfWithBs$'.qtr.since2006')
meanMat=do.call(rbind,lapply(locList,function(x){apply(predVal[x,],2,mean)}))
percap.outpvol.mma.fasc2.hat=apply(meanMat,1,mean)
percap.outpvol.mma.fasc2.ci=apply(meanMat,1,function(x){quantile(x,probs=c(0.025,0.975))})



#FASC group 3 MMA
testdf=fit.percap.outpVol.fasc$data
chgLoc=which(testdf[,'.qtr.since2006']>8)
testdf[chgLoc,'fascsurgeongrpannual']=factor(levels(testdf[,'fascsurgeongrpannual'])[3],levels=levels(testdf[,'fascsurgeongrpannual']))

#this is counterfactural without MMA
#this is observed
predVal=exp(btXbWithShock.yz( coef(fit.percap.outpVol.fasc) 
                              , vcov(fit.percap.outpVol.fasc)  
                              , changedBetas=NULL
                              , 0 #for poission model, the random shock follows a poission 
                              , model.matrix(formula(fit.percap.outpVol.fasc),testdf) 
                              , 100                        
                              , changedBetasHaveVariation=TRUE
                              , seed=1  
                              , sameShockVec=FALSE   
                              
))

locList=valLoc.yz(anaDfWithBs$'.qtr.since2006')
meanMat=do.call(rbind,lapply(locList,function(x){apply(predVal[x,],2,mean)}))
percap.outpvol.mma.fasc3.hat=apply(meanMat,1,mean)
percap.outpvol.mma.fasc3.ci=apply(meanMat,1,function(x){quantile(x,probs=c(0.025,0.975))})


percap.outpvol.mma.byFasc=list(
  data.frame(x=seq(24),percap.outpvol.mma.fasc1.hat,percap.outpvol.mma.fasc1.ci[1,],percap.outpvol.mma.fasc1.ci[2,])
  ,  data.frame(x=seq(24),percap.outpvol.mma.fasc2.hat,percap.outpvol.mma.fasc2.ci[1,],percap.outpvol.mma.fasc2.ci[2,])
  ,  data.frame(x=seq(24),percap.outpvol.mma.fasc3.hat,percap.outpvol.mma.fasc3.ci[1,],percap.outpvol.mma.fasc3.ci[2,])
)


plotLineCi.yz(percap.outpvol.mma.byFasc
              #each element of the list is a data.frame has four columns, the first column x, the second columen is mean.y, third columen is lowerbound.y, the four column is upperbound.y
              ,xlim=c(0,24)
              ,ylim=c(0.32,0.41)
              ,pchVec=c(1,2,4) #this should be the same lenght as plotdata.list
              ,ciBar=TRUE #if FALSE then there is no CI bar, then it beomes nonerror bar line point
              ,jit.size=0 #to avoid overlapping
              ,ciBarLwd=1 #width of error bar
              ,type='b' #can line 'l' or both 'b'
              ,gap=0
              ,sfrac=0.01
              ,maintitle=c('per capita outpatient volume: with MMA by FASC penetration') #title of plot
              ,x.label=c('quarter since 2006')
              ,y.label=c('per capita outpatient volume (encounter day)')
              ,legendLoc='bottomright'
              ,legend.txt.vec=c('low FASC','medium FASC', 'high FASC') #the legend text characteri vector
              ,cex.legend.text=1
              ,
              #pdf.path=NULL
              pdf.path=pdf('Z:/j_scrdata/ascMMA/result/fig/percapOutPatientVolMmaByFasc.pdf') #with e.g., C;/abc.pdf
)
dev.off()


#------------------------------

#figure 3

fit.totopexp.fasc=glm(log(totopexp) ~ mma2008.impact+fascsurgeongrpannual
                        + qtr + mean.age + .avg.comorbsum  +race.0+race.1+race.2+race.3+race.4+race.5
                        +.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4
                        +.qtr.since2006.bs5
                        +.qtr.since2006.bs6
                        +.qtr.since2006.bs7+.qtr.since2006.bs8
                        +.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11
                        , data=anaDfWithBs)

#names(anaDfWithBs)

#summary(fit.percap.outpVol.fasc)

summary(fit.totopexp.fasc)

#no MMA
changedBetas.noMmaEffect=c(mma2008.impact=0)
predVal=btXbWithShock.yz(coef(fit.totopexp.fasc) #a named  usually coef(fit)[colnames(vcov(fit))]
                         , vcov(fit.totopexp.fasc)  #at least colnamed matrix usually vcov(fit)
                         , changedBetas.noMmaEffect
                         , sd(fit.totopexp.fasc$residuals)  # sd(fit$residuals) this is from the model  stardard error of shock
                         , model.matrix(formula(fit.totopexp.fasc),fit.totopexp.fasc$data) #usually it is  model.matrix(formula(fit),data)
                         , 100                   
                         
                         , changedBetasHaveVariation=T
                         
                         #using this will be more accurate when there is nonlinear operation
                         , seed=211 #this seed will be used to draw coef and shock      
                         , sameShockVec=FALSE    #you always set it to FALSE         
                         #if False, then use different shock in each simulation
                         #if you use same shock, it usually is bad. because the same shock would cause consistent underestimation or overestimation
)


endof2007.fasc=subset(fit.totopexp.fasc$data, .qtr.since2006==8)[,c('hrrnum','fascsurgeongrpannual')]
beforeMMA=subset(fit.totopexp.fasc$data, .qtr.since2006<=8)
afterMMA=subset(fit.totopexp.fasc$data, .qtr.since2006>8)
afterMMA[,'fascsurgeongrpannual'] <- NULL
afterMMA.changedFasc=join(afterMMA, endof2007.fasc, by=c('hrrnum'))
dataChangedFascGrp=rbind(beforeMMA, afterMMA.changedFasc)
locList=valLoc.yz(dataChangedFascGrp$'.qtr.since2006')



sumMat=do.call(rbind,lapply(locList,function(x){apply(exp(predVal[x,]),2,sum)}))

mean.totopexp.hat.noMma=5*apply(sumMat,1,mean)/1e9
ci.totopexp.hat.noMma=5*apply(sumMat,1,function(x){quantile(x,probs=c(0.025,0.975))})/1e9


#MMA
changedBetas.noMmaEffect=NULL
predVal=btXbWithShock.yz(coef(fit.totopexp.fasc) #a named  usually coef(fit)[colnames(vcov(fit))]
                         , vcov(fit.totopexp.fasc)  #at least colnamed matrix usually vcov(fit)
                         , changedBetas.noMmaEffect
                         , sd(fit.totopexp.fasc$residuals)  # sd(fit$residuals) this is from the model  stardard error of shock
                         , model.matrix(formula(fit.totopexp.fasc),fit.totopexp.fasc$data) #usually it is  model.matrix(formula(fit),data)
                         , 100                   
                         
                         , changedBetasHaveVariation=T
                         
                         #using this will be more accurate when there is nonlinear operation
                         , seed=211 #this seed will be used to draw coef and shock      
                         , sameShockVec=FALSE    #you always set it to FALSE         
                         #if False, then use different shock in each simulation
                         #if you use same shock, it usually is bad. because the same shock would cause consistent underestimation or overestimation
)


sumMat=do.call(rbind,lapply(locList,function(x){apply(exp(predVal[x,]),2,sum)}))

mean.totopexp.hat.mma=5*apply(sumMat,1,mean)/1e9
ci.totopexp.hat.mma=5*apply(sumMat,1,function(x){quantile(x,probs=c(0.025,0.975))})/1e9


totopexp.mma.noMma=list(data.frame(seq(24),mean.totopexp.hat.mma, ci.totopexp.hat.mma[1,],ci.totopexp.hat.mma[2,])
                        ,data.frame(seq(24),mean.totopexp.hat.noMma, ci.totopexp.hat.noMma[1,],ci.totopexp.hat.noMma[2,])
)

plotLineCi.yz(totopexp.mma.noMma
              #each element of the list is a data.frame has four columns, the first column x, the second columen is mean.y, third columen is lowerbound.y, the four column is upperbound.y
              ,xlim=c(0,24)
              ,ylim=c(0,6)
              ,pchVec=c(1,2) #this should be the same lenght as plotdata.list
              ,ciBar=TRUE #if FALSE then there is no CI bar, then it beomes nonerror bar line point
              ,jit.size=0 #to avoid overlapping
              ,ciBarLwd=1 #width of error bar
              ,type='b' #can line 'l' or both 'b'
              ,maintitle=c('total outpatient spending: MMA vs. no MMA') #title of plot
              ,x.label=c('quarter since 2006')
              ,y.label=c('total outpatient spending on surgery (billion dollars)')
              ,legendLoc='bottomright'
              ,legend.txt.vec=c('MMA','no MMA') #the legend text characteri vector
              ,cex.legend.text=1
              ,sfrac=0.002
              ,pdf.path=pdf('Z:/j_scrdata/ascMMA/result/fig/totalOutPatientSpendingMmaNoMma_july22.pdf') #with e.g., C;/abc.pdf
)


#figure 4 outpatient per capita spending
#---------------------------------------------------


#this is the preferred model (do not add qtr)
fit.percap.outopexp.fasc.noqtr=glm(
  log(percap.totopexp) ~
    mma2008.impact
  +firstqtraftermma
  +fascsurgeongrpannual
# + qtr #do not add quarterly effect....
   + mean.age + .avg.comorbsum +mean.age
  +race.0+race.1+race.2+race.3+race.4+race.5
   + sex.1
  #+.qtr.since2006
  +.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3
  +.qtr.since2006.bs4+.qtr.since2006.bs5+.qtr.since2006.bs6
  +.qtr.since2006.bs7+.qtr.since2006.bs8+.qtr.since2006.bs8+.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11
  , data=anaDfWithBs)
summary(fit.percap.outopexp.fasc.noqtr) #no effect on per capita spending

# this model does NOT provide upcoding story
# fit.percap.outopexp.fasc.qtr=glm(
#   log(percap.totopexp) ~
#     mma2008.impact
#   +firstqtraftermma
#   +fascsurgeongrpannual
#   + qtr #do not add quarterly effect....
#   + mean.age + .avg.comorbsum +mean.age
#   +race.0+race.1+race.2+race.3+race.4+race.5
#   + sex.1
#   #+.qtr.since2006
#   +.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3
#   +.qtr.since2006.bs4+.qtr.since2006.bs5+.qtr.since2006.bs6
#   +.qtr.since2006.bs7+.qtr.since2006.bs8+.qtr.since2006.bs8+.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11
#   , data=anaDfWithBs)
# summary(fit.percap.outopexp.fasc.qtr) #no effect on per capita spending

#names(anaDfWithBs)
summary(fit.percap.outpVol.nofasc) #signficantly reduce per capital volume
summary(fit.percap.outpVol.fasc) #drops and it is signficant
summary(fit.totopexp.fasc) #increase spedning with a pvalue is 0.08
summary(fit.percap.outopexp.fasc.noqtr) #no qtr will provide poistive and signficaint estimate on mam impact, I bleieve this model more
#summary(fit.percap.outopexp.fasc.qtr)# addiubg qtr will change the sign and make it insignificant


#no MMA
changedBetas=c(mma2008.impact=0,firstqtraftermma=0)
predVal=btXbWithShock.yz( coef(fit.percap.outopexp.fasc.noqtr) 
                          , vcov(fit.percap.outopexp.fasc.noqtr)  
                          , changedBetas=changedBetas
                          , sd(fit.percap.outopexp.fasc.noqtr$residuals)
                          , model.matrix(formula(fit.percap.outopexp.fasc.noqtr),fit.percap.outopexp.fasc.noqtr$data) 
                          , 100                        
                          , changedBetasHaveVariation=TRUE
                          , seed=1  
                          , sameShockVec=FALSE   
                          
)

locList=valLoc.yz(anaDfWithBs$'.qtr.since2006')
meanMat=do.call(rbind,lapply(locList,function(x){apply(exp(predVal[x,]),2,mean)}))

percap.totoutpexp.nomma.hat=apply(meanMat,1,mean)
percap.totoutpexp.nomma.ci=apply(meanMat,1,function(x){quantile(x,probs=c(0.025,0.975))})

#MMA
changedBetas=NULL
predVal=btXbWithShock.yz( coef(fit.percap.outopexp.fasc.noqtr) 
                          , vcov(fit.percap.outopexp.fasc.noqtr)  
                          , changedBetas=changedBetas
                          , sd(fit.percap.outopexp.fasc.noqtr$residuals)
                          , model.matrix(formula(fit.percap.outopexp.fasc.noqtr),fit.percap.outopexp.fasc.noqtr$data) 
                          , 100                        
                          , changedBetasHaveVariation=TRUE
                          , seed=1  
                          , sameShockVec=FALSE   
                          
)

locList=valLoc.yz(anaDfWithBs$'.qtr.since2006')
meanMat=do.call(rbind,lapply(locList,function(x){apply(exp(predVal[x,]),2,mean)}))

percap.totoutpexp.mma.hat=apply(meanMat,1,mean)
percap.totoutpexp.mma.ci=apply(meanMat,1,function(x){quantile(x,probs=c(0.025,0.975))})


percap.totoutpexp.mma.noMma=list(data.frame(seq(24),percap.totoutpexp.mma.hat, percap.totoutpexp.mma.ci[1,],percap.totoutpexp.mma.ci[2,])
                        ,data.frame(seq(24),percap.totoutpexp.nomma.hat, percap.totoutpexp.nomma.ci[1,],percap.totoutpexp.nomma.ci[2,])
)

plotLineCi.yz(percap.totoutpexp.mma.noMma
              #each element of the list is a data.frame has four columns, the first column x, the second columen is mean.y, third columen is lowerbound.y, the four column is upperbound.y
              ,xlim=c(0,24)
              ,ylim=c(0,250)
              ,pchVec=c(1,2) #this should be the same lenght as plotdata.list
              ,ciBar=TRUE #if FALSE then there is no CI bar, then it beomes nonerror bar line point
              ,jit.size=0 #to avoid overlapping
              ,ciBarLwd=1 #width of error bar
              ,type='b' #can line 'l' or both 'b'
              ,maintitle=c('per capita outpatient spending: MMA vs. no MMA') #title of plot
              ,x.label=c('quarter since 2006')
              ,y.label=c('per capita outpatient spending on surgery (dollar)')
              ,legendLoc='bottomright'
              ,legend.txt.vec=c('MMA','no MMA') #the legend text characteri vector
              ,cex.legend.text=1
              ,sfrac=0.002
              ,pdf.path=pdf('Z:/j_scrdata/ascMMA/result/fig/percapOutPatientSpendingMmaNoMma.pdf') #with e.g., C;/abc.pdf
)

#---------------------------------------------------
#Figure 5
anaDfWithBs[,'per.outp.entday.exp']=anaDfWithBs[,'totopexp']/anaDfWithBs[,'totaloutpvol']


#this is the preferred model (do not add qtr)
fit.peroutpentdayexp.fasc.noqtr=glm(
  log(per.outp.entday.exp) ~
    mma2008.impact
  +firstqtraftermma
  +fascsurgeongrpannual
   #+ qtr #do not add quarterly effect....
  + mean.age + .avg.comorbsum +mean.age
  +race.0+race.1+race.2+race.3+race.4+race.5
  + sex.1
  
  +.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3
  +.qtr.since2006.bs4+.qtr.since2006.bs5+.qtr.since2006.bs6
  +.qtr.since2006.bs7+.qtr.since2006.bs8+.qtr.since2006.bs8+.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11
 
  , data=anaDfWithBs)
summary(fit.peroutpentdayexp.fasc.noqtr) #no effect on per capita spending



#no MMA
changedBetas=c(mma2008.impact=0,firstqtraftermma=0)
predVal=btXbWithShock.yz( coef(fit.peroutpentdayexp.fasc.noqtr) 
                          , vcov(fit.peroutpentdayexp.fasc.noqtr)  
                          , changedBetas=changedBetas
                          , sd(fit.peroutpentdayexp.fasc.noqtr$residuals)
                          , model.matrix(formula(fit.peroutpentdayexp.fasc.noqtr),fit.peroutpentdayexp.fasc.noqtr$data) 
                          , 100                        
                          , changedBetasHaveVariation=TRUE
                          , seed=1  
                          , sameShockVec=FALSE   
                          
)

locList=valLoc.yz(anaDfWithBs$'.qtr.since2006')
meanMat=do.call(rbind,lapply(locList,function(x){apply(exp(predVal[x,]),2,mean)}))

perentday.exp.nomma.hat=apply(meanMat,1,mean)
perentday.exp.nomma.ci=apply(meanMat,1,function(x){quantile(x,probs=c(0.025,0.975))})

#MMA
changedBetas=NULL
predVal=btXbWithShock.yz( coef(fit.peroutpentdayexp.fasc.noqtr) 
                          , vcov(fit.peroutpentdayexp.fasc.noqtr)  
                          , changedBetas=changedBetas
                          , sd(fit.peroutpentdayexp.fasc.noqtr$residuals)
                          , model.matrix(formula(fit.peroutpentdayexp.fasc.noqtr),fit.peroutpentdayexp.fasc.noqtr$data) 
                          , 100                        
                          , changedBetasHaveVariation=TRUE
                          , seed=1  
                          , sameShockVec=FALSE   
                          
)

locList=valLoc.yz(anaDfWithBs$'.qtr.since2006')
meanMat=do.call(rbind,lapply(locList,function(x){apply(exp(predVal[x,]),2,mean)}))

perentday.exp.mma.hat=apply(meanMat,1,mean)
perentday.exp.mma.ci=apply(meanMat,1,function(x){quantile(x,probs=c(0.025,0.975))})


percap.totoutpexp.mma.noMma=list(data.frame(seq(24),perentday.exp.mma.hat, perentday.exp.mma.ci[1,],perentday.exp.mma.ci[2,])
                                 ,data.frame(seq(24),perentday.exp.nomma.hat, perentday.exp.nomma.ci[1,],perentday.exp.nomma.ci[2,])
)

plotLineCi.yz(percap.totoutpexp.mma.noMma
              #each element of the list is a data.frame has four columns, the first column x, the second columen is mean.y, third columen is lowerbound.y, the four column is upperbound.y
              ,xlim=c(0,24)
              ,ylim=c(300,600)
              ,pchVec=c(1,2) #this should be the same lenght as plotdata.list
              ,ciBar=TRUE #if FALSE then there is no CI bar, then it beomes nonerror bar line point
              ,jit.size=0 #to avoid overlapping
              ,ciBarLwd=1 #width of error bar
              ,type='b' #can line 'l' or both 'b'
              ,maintitle=c('per outpatient encounter day spending: MMA vs. no MMA') #title of plot
              ,x.label=c('quarter since 2006')
              ,y.label=c('per outpatient encounter day spending on surgery (dollar)')
              ,legendLoc='bottomright'
              ,legend.txt.vec=c('MMA','no MMA') #the legend text characteri vector
              ,cex.legend.text=1
              ,sfrac=0.002
              ,pdf.path=pdf('Z:/j_scrdata/ascMMA/result/fig/peroutpentdayexpMmaNoMma.pdf') #with e.g., C;/abc.pdf
)


