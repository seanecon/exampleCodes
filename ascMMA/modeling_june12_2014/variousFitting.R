# 


save.image(file = "Z:/j_scrdata/ascMMA/rImages/ascMMA_june132014.RData")

# save(anaDfNoBs,file='Z:/j_scrdata/ascMMA/result/anaDfNoBs.RData')

(load(file='Z:/j_scrdata/ascMMA/result/anaDfNoBs.RData'))

xtabs(~MMA2008.impact+year,data=anaDfNoBs)

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



#new compile


#first fit a model for total expenditure

fit.totexp=glm(log(totexp) ~ mma2008.impact 
               + fascsurgeongrpannual
               + mma2008.impact*fascsurgeongrpannual
               + qtr + mean.age + .avg.comorbsum 
               #+ firstqtraftermma
               #          +.qtr.since2006+.qtr.since2006.sqrt
               +.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4
               +.qtr.since2006.bs5
               +.qtr.since2006.bs6
               +.qtr.since2006.bs7+.qtr.since2006.bs8
               +.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11
               , data=anaDfWithBs)

summary(fit.totexp)

fit.totexp.nofasc=glm(log(totexp) ~ mma2008.impact 
               
               + qtr + mean.age + .avg.comorbsum 
               #+ firstqtraftermma
               #          +.qtr.since2006+.qtr.since2006.sqrt
               +.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4
               +.qtr.since2006.bs5
               +.qtr.since2006.bs6
               +.qtr.since2006.bs7+.qtr.since2006.bs8
               +.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11
               , data=anaDfWithBs)

summary(fit.totexp.nofasc)


coefDf.withFasc=rowname2vn.yz(as.data.frame(summary(fit.totexp)$coefficients[,c(1,4)]), 'coef')
coefDf.withFasc=rename.vars(coefDf.withFasc,c('Estimate','Pr(>|t|)'),c('coefEst.withFasc','pvalue.withFasc'))


coefDf.noFasc=rowname2vn.yz(as.data.frame(summary(fit.totexp.nofasc)$coefficients[,c(1,4)]), 'coef')
coefDf.noFasc=rename.vars(coefDf.noFasc,c('Estimate','Pr(>|t|)'),c('coefEst.noFasc','pvalue.noFasc'))

coeff.totexp.withfasc.without=join(coefDf.withFasc,coefDf.noFasc)

coeff.totexp.withfasc.without=ordcol.yz(coeff.totexp.withfasc.without,c('coef'))
xtable(coeff.totexp.withfasc.without)
options(digits=3)

names(anaDfWithBs)
anaDfWithBs[,'annual.nfascdocper10kbene']
pdf('Z:/j_scrdata/ascMMA/result/fig/moreFascAfter2008.pdf')
hist(subset(anaDfWithBs,mma2008.impact==0)$annual.nfascdocper10kbene,col='red',freq=F,ylim=c(0,0.06),main='distribution of FASC before and after MMA',xlab='number of fasc doc per 10k')
hist(subset(anaDfWithBs,mma2008.impact==1)$annual.nfascdocper10kbene,add=T,col=rgb(0, 1, 0, 0.5),freq=F)
dev.off()

ddply(anaDfWithBs,'mma2008.impact',function(x){mean(x[,'annual.nfascdocper10kbene'])})


#presence of MMA tends to increase expenditure. at least it did not slow down spending
#in places with more fasc it increases more

#this may be drven by change in number of beneficiaries
#so we fit a model on per capital expenditrue, we still see that it failed to stop
#in places with more fasc, it increases more

fit.percap.totexp=glm(log(percap.totexp) ~ 
                       mma2008.impact 
                      + fascsurgeongrpannual
                      + mma2008.impact*fascsurgeongrpannual
                      + qtr + mean.age + .avg.comorbsum 
                      #+ firstqtraftermma
                      #          +.qtr.since2006+.qtr.since2006.sqrt
                      +.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4
                      +.qtr.since2006.bs5
                      +.qtr.since2006.bs6
                      +.qtr.since2006.bs7+.qtr.since2006.bs8
                      +.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11
                      , data=anaDfWithBs)

coeff.totexp.percap.totexp=cbind(summary(fit.totexp)$coefficients[,c(1,4)],summary(fit.percap.totexp)$coefficients[,c(1,4)])
colnames(coeff.totexp.percap.totexp)=c('totexp.beta','totexp.p','perca.totexp.beta','perca.totexp.p')

xtable(coeff.totexp.percap.totexp)

pdf('Z:/j_scrdata/ascMMA/result/fig/negativeCorrelationBetweenPopulatonSizeAndFascPenetration.pdf')
plot(anaDfWithBs[,'fascsurgeongrpannual'],anaDfWithBs[,'n.bene'])
dev.off()

predVal=btXbWithShock.yz( coef(fit.percap.totexp) 
                            , vcov(fit.percap.totexp)  
                            , changedBetas=NULL
                            , sd(fit.percap.totexp$residuals)
                            , model.matrix(formula(fit.percap.totexp),fit.percap.totexp$data) 
                            , 100                        
                            , changedBetasHaveVariation=TRUE
                            , seed=1  
                            , sameShockVec=FALSE   
                            
)

locList=valLoc.yz(anaDfWithBs$'.qtr.since2006')
meanMat=do.call(rbind,lapply(locList,function(x){apply(exp(predVal[x,]),2,mean)}))
percap.totexp.mma.hat=apply(meanMat,1,mean)
percap.totexp.mma.ci=apply(meanMat,1,function(x){quantile(x,probs=c(0.025,0.975))})
percap.totexp.true=unlist(lapply(locList,function(x){mean(anaDfWithBs$percap.totexp[x])}))


#no MMA

changedBetas.noMMAEffect=c(0,0,0,0)
names(changedBetas.noMMAEffect)=c('mma2008.impact','mma2008.impact:fascsurgeongrpannual[13.2,18.2)','mma2008.impact:fascsurgeongrpannual[18.2,25.3)','mma2008.impact:fascsurgeongrpannual[25.3, Inf]')
predVal=btXbWithShock.yz( coef(fit.percap.totexp) 
                          , vcov(fit.percap.totexp)  
                          , changedBetas=changedBetas.noMMAEffect
                          , sd(fit.percap.totexp$residuals)
                          , model.matrix(formula(fit.percap.totexp),fit.percap.totexp$data) 
                          , 100                        
                          , changedBetasHaveVariation=TRUE
                          , seed=1  
                          , sameShockVec=FALSE   
                          
)

locList=valLoc.yz(anaDfWithBs$'.qtr.since2006')
meanMat=do.call(rbind,lapply(locList,function(x){apply(exp(predVal[x,]),2,mean)}))

percap.totexp.nomma.hat=apply(meanMat,1,mean)
percap.totexp.nomma.ci=apply(meanMat,1,function(x){quantile(x,probs=c(0.025,0.975))})


pdf('Z:/j_scrdata/ascMMA/result/fig/percapTotExpGof.pdf')
plot(percap.totexp.mma.hat,type='b',pch=1)
lines(percap.totexp.true,pch=2,type='b')
legend('bottomright',pch=c(1,2),legend=c('prediction with MMA','observation'),cex=0.9)
abline(v=8,lty=3)
dev.off()

pdf('Z:/j_scrdata/ascMMA/result/fig/mmaEffectOnPercapTotExp.pdf')
plot(percap.totexp.mma.hat,type='b',pch=1)
#lines(percap.totexp.true,pch=2,type='b')
lines(percap.totexp.nomma.hat,type='b',pch=3)
legend('bottomright',pch=c(1,3),legend=c('with MMA','no MMA'),cex=0.9)
abline(v=8,lty=3)
dev.off()

percap.totexp.nomma.ci
percap.totexp.mma.ci
summary(fit.percap.totexp)

percap.totexp.ci
percap.totexp.namma.ci

#now if you really want to check the impact of MMA on total expenditure

#MMA
summary(fit.totexp)
changedBetas.noMMAEffect=NULL
predVal=btXbWithShock.yz( coef(fit.totexp) 
                          , vcov(fit.totexp)  
                          , changedBetas=changedBetas.noMMAEffect
                          , sd(fit.totexp$residuals)
                          , model.matrix(formula(fit.totexp),fit.totexp$data) 
                          , 100                        
                          , changedBetasHaveVariation=TRUE
                          , seed=1  
                          , sameShockVec=FALSE   
                          
)

locList=valLoc.yz(anaDfWithBs$'.qtr.since2006')
sumMat=do.call(rbind,lapply(locList,function(x){apply(exp(predVal[x,]),2,sum)}))

totexp.mma.hat=apply(sumMat,1,mean)/1e9
totexp.mma.ci=apply(sumMat,1,function(x){quantile(x,probs=c(0.025,0.975))})/1e9

totexp.true=unlist(lapply(locList,function(x){sum(anaDfWithBs$totexp[x])}))//1e9


#no MMA with change in FASC
summary(fit.totexp)
changedBetas.noMMAEffect=c(0,0,0,0)
names(changedBetas.noMMAEffect)=c('mma2008.impact','mma2008.impact:fascsurgeongrpannual[13.2,18.2)','mma2008.impact:fascsurgeongrpannual[18.2,25.3)','mma2008.impact:fascsurgeongrpannual[25.3, Inf]')
predVal=btXbWithShock.yz( coef(fit.totexp) 
                          , vcov(fit.totexp)  
                          , changedBetas=changedBetas.noMMAEffect
                          , sd(fit.totexp$residuals)
                          , model.matrix(formula(fit.totexp),fit.totexp$data) 
                          , 100                        
                          , changedBetasHaveVariation=TRUE
                          , seed=1  
                          , sameShockVec=FALSE   
                          
)

locList=valLoc.yz(anaDfWithBs$'.qtr.since2006')
sumMat=do.call(rbind,lapply(locList,function(x){apply(exp(predVal[x,]),2,sum)}))

totexp.nomma.hat=apply(sumMat,1,mean)/1e9
totexp.nomma.ci=apply(sumMat,1,function(x){quantile(x,probs=c(0.025,0.975))})/1e9


#no MMA effect no FASC change
endof2007.fasc=subset(fit.totexp$data, .qtr.since2006==8)[,c('hrrnum','fascsurgeongrpannual')]
beforeMMA=subset(fit.totexp$data, .qtr.since2006<=8)
afterMMA=subset(fit.totexp$data, .qtr.since2006>8)
afterMMA[,'fascsurgeongrpannual'] <- NULL
afterMMA.changedFasc=join(afterMMA, endof2007.fasc, by=c('hrrnum'))
dataChangedFascGrp=rbind(beforeMMA, afterMMA.changedFasc)
locList=valLoc.yz(dataChangedFascGrp$'.qtr.since2006')

changedBetas.noMMAEffect=c(0,0,0,0)
names(changedBetas.noMMAEffect)=c('mma2008.impact','mma2008.impact:fascsurgeongrpannual[13.2,18.2)','mma2008.impact:fascsurgeongrpannual[18.2,25.3)','mma2008.impact:fascsurgeongrpannual[25.3, Inf]')
predVal=btXbWithShock.yz(coef(fit.totexp) #a named  usually coef(fit)[colnames(vcov(fit))]
                         , vcov(fit.totexp)  #at least colnamed matrix usually vcov(fit)
                         , changedBetas.noMMAEffect
                         , sd(fit.totexp$residuals)  # sd(fit$residuals) this is from the model  stardard error of shock
                         , model.matrix(formula(fit.totexp),dataChangedFascGrp) #usually it is  model.matrix(formula(fit),data)
                         , 100                   
                         
                         , changedBetasHaveVariation=T
                         
                         #using this will be more accurate when there is nonlinear operation
                         , seed=211 #this seed will be used to draw coef and shock      
                         , sameShockVec=FALSE    #you always set it to FALSE         
                         #if False, then use different shock in each simulation
                         #if you use same shock, it usually is bad. because the same shock would cause consistent underestimation or overestimation
)


sumMat=do.call(rbind,lapply(locList,function(x){apply(exp(predVal[x,]),2,sum)}))

meanVec.noMMANoFascChange=apply(sumMat,1,mean)/1e9
ciMat.noMMANoFascChange=apply(sumMat,1,function(x){quantile(x,probs=c(0.025,0.975))})/1e9

#with MMA no FASC chagne

endof2007.fasc=subset(fit.totexp$data, .qtr.since2006==8)[,c('hrrnum','fascsurgeongrpannual')]
beforeMMA=subset(fit.totexp$data, .qtr.since2006<=8)
afterMMA=subset(fit.totexp$data, .qtr.since2006>8)
afterMMA[,'fascsurgeongrpannual'] <- NULL
afterMMA.changedFasc=join(afterMMA, endof2007.fasc, by=c('hrrnum'))
dataChangedFascGrp=rbind(beforeMMA, afterMMA.changedFasc)
locList=valLoc.yz(dataChangedFascGrp$'.qtr.since2006')

changedBetas=NULL

predVal=btXbWithShock.yz(coef(fit.totexp) #a named  usually coef(fit)[colnames(vcov(fit))]
                         , vcov(fit.totexp)  #at least colnamed matrix usually vcov(fit)
                         , changedBetas
                         , sd(fit.totexp$residuals)  # sd(fit$residuals) this is from the model  stardard error of shock
                         , model.matrix(formula(fit.totexp),dataChangedFascGrp) #usually it is  model.matrix(formula(fit),data)
                         , 100                   
                         
                         , changedBetasHaveVariation=T
                         
                         #using this will be more accurate when there is nonlinear operation
                         , seed=211 #this seed will be used to draw coef and shock      
                         , sameShockVec=FALSE    #you always set it to FALSE         
                         #if False, then use different shock in each simulation
                         #if you use same shock, it usually is bad. because the same shock would cause consistent underestimation or overestimation
)


sumMat=do.call(rbind,lapply(locList,function(x){apply(exp(predVal[x,]),2,sum)}))

meanVec.MMANoFascChange=apply(sumMat,1,mean)/1e9
ciMat.MMANoFascChange=apply(sumMat,1,function(x){quantile(x,probs=c(0.025,0.975))})/1e9


pdf('Z:/j_scrdata/ascMMA/result/fig/totExpGof.pdf')
plot(5*totexp.mma.hat,type='b',pch=1,ylim=5*c(1,3.5))
lines(5*totexp.true,pch=2,type='b')
legend('bottomright',pch=c(1,2),legend=c('prediction with MMA','observation'),cex=0.9)
abline(v=8,lty=3)
dev.off()

pdf('Z:/j_scrdata/ascMMA/result/fig/mmaFascEffectOnTotExp.pdf')
plot(5*totexp.mma.hat,type='b',pch=1,ylim=c(8,18),ylab='total expenditure (billion dollars)',lwd=2)
lines(5*meanVec.MMANoFascChange,type='b',pch=2)
lines(5*totexp.nomma.hat,type='b',pch=3)
lines(5*meanVec.noMMANoFascChange,type='b',pch=4)
legend('bottomright',pch=c(1,2,3,4),legend=c('with MMA with FASC change','with MMA no FASC change','no MMA with FASC change','no MMA no FASC change'),cex=0.9)
abline(v=8,lty=3)
dev.off()

#the other way to present FASC effect
# MMA
#no MMA
#no MMA FASC grp1
testdf=fit.totexp$data
chgLoc=which(testdf[,'.qtr.since2006']>8)
testdf[chgLoc,'fascsurgeongrpannual']=factor(levels(testdf[,'fascsurgeongrpannual'])[1],levels=levels(testdf[,'fascsurgeongrpannual']))

changedBetas.noMMAEffect=c(0,0,0,0)
names(changedBetas.noMMAEffect)=c('mma2008.impact','mma2008.impact:fascsurgeongrpannual[13.2,18.2)','mma2008.impact:fascsurgeongrpannual[18.2,25.3)','mma2008.impact:fascsurgeongrpannual[25.3, Inf]')
predVal=btXbWithShock.yz(coef(fit.totexp) #a named  usually coef(fit)[colnames(vcov(fit))]
                         , vcov(fit.totexp)  #at least colnamed matrix usually vcov(fit)
                         , changedBetas.noMMAEffect
                         , sd(fit.totexp$residuals)  # sd(fit$residuals) this is from the model  stardard error of shock
                         , model.matrix(formula(fit.totexp),testdf) #usually it is  model.matrix(formula(fit),data)
                         , 100                   
                         
                         , changedBetasHaveVariation=T
                         
                         #using this will be more accurate when there is nonlinear operation
                         , seed=211 #this seed will be used to draw coef and shock      
                         , sameShockVec=FALSE    #you always set it to FALSE         
                         #if False, then use different shock in each simulation
                         #if you use same shock, it usually is bad. because the same shock would cause consistent underestimation or overestimation
)


sumMat=do.call(rbind,lapply(locList,function(x){apply(exp(predVal[x,]),2,sum)}))

meanVec.totexp.hat.noMMA.fasc.1=apply(sumMat,1,mean)/1e9
ciMat.totexp.hat.noMMA.fasc.1=apply(sumMat,1,function(x){quantile(x,probs=c(0.025,0.975))})/1e9


#no MMA FASC grp2
testdf=fit.percap.outpVol$data
chgLoc=which(testdf[,'.qtr.since2006']>8)
testdf[chgLoc,'fascsurgeongrpannual']=factor(levels(testdf[,'fascsurgeongrpannual'])[2],levels=levels(testdf[,'fascsurgeongrpannual']))

changedBetas.noMMAEffect=c(0,0,0,0)
names(changedBetas.noMMAEffect)=c('mma2008.impact','mma2008.impact:fascsurgeongrpannual[13.2,18.2)','mma2008.impact:fascsurgeongrpannual[18.2,25.3)','mma2008.impact:fascsurgeongrpannual[25.3, Inf]')
predVal=btXbWithShock.yz(coef(fit.totexp) #a named  usually coef(fit)[colnames(vcov(fit))]
                         , vcov(fit.totexp)  #at least colnamed matrix usually vcov(fit)
                         , changedBetas.noMMAEffect
                         , sd(fit.totexp$residuals)  # sd(fit$residuals) this is from the model  stardard error of shock
                         , model.matrix(formula(fit.totexp),testdf) #usually it is  model.matrix(formula(fit),data)
                         , 100                   
                         
                         , changedBetasHaveVariation=T
                         
                         #using this will be more accurate when there is nonlinear operation
                         , seed=211 #this seed will be used to draw coef and shock      
                         , sameShockVec=FALSE    #you always set it to FALSE         
                         #if False, then use different shock in each simulation
                         #if you use same shock, it usually is bad. because the same shock would cause consistent underestimation or overestimation
)


sumMat=do.call(rbind,lapply(locList,function(x){apply(exp(predVal[x,]),2,sum)}))

meanVec.totexp.hat.noMMA.fasc.2=apply(sumMat,1,mean)/1e9
ciMat.totexp.hat.noMMA.fasc.2=apply(sumMat,1,function(x){quantile(x,probs=c(0.025,0.975))})/1e9


#no MMA FASC grp3
testdf=fit.percap.outpVol$data
chgLoc=which(testdf[,'.qtr.since2006']>8)
testdf[chgLoc,'fascsurgeongrpannual']=factor(levels(testdf[,'fascsurgeongrpannual'])[3],levels=levels(testdf[,'fascsurgeongrpannual']))


changedBetas.noMMAEffect=c(0,0,0,0)
names(changedBetas.noMMAEffect)=c('mma2008.impact','mma2008.impact:fascsurgeongrpannual[13.2,18.2)','mma2008.impact:fascsurgeongrpannual[18.2,25.3)','mma2008.impact:fascsurgeongrpannual[25.3, Inf]')
predVal=btXbWithShock.yz(coef(fit.totexp) #a named  usually coef(fit)[colnames(vcov(fit))]
                         , vcov(fit.totexp)  #at least colnamed matrix usually vcov(fit)
                         , changedBetas.noMMAEffect
                         , sd(fit.totexp$residuals)  # sd(fit$residuals) this is from the model  stardard error of shock
                         , model.matrix(formula(fit.totexp),testdf) #usually it is  model.matrix(formula(fit),data)
                         , 100                   
                         
                         , changedBetasHaveVariation=T
                         
                         #using this will be more accurate when there is nonlinear operation
                         , seed=211 #this seed will be used to draw coef and shock      
                         , sameShockVec=FALSE    #you always set it to FALSE         
                         #if False, then use different shock in each simulation
                         #if you use same shock, it usually is bad. because the same shock would cause consistent underestimation or overestimation
)


sumMat=do.call(rbind,lapply(locList,function(x){apply(exp(predVal[x,]),2,sum)}))

meanVec.totexp.hat.noMMA.fasc.3=apply(sumMat,1,mean)/1e9
ciMat.totexp.hat.noMMA.fasc.3=apply(sumMat,1,function(x){quantile(x,probs=c(0.025,0.975))})/1e9

#no MMA FASC grp4
testdf=fit.percap.outpVol$data
chgLoc=which(testdf[,'.qtr.since2006']>8)
testdf[chgLoc,'fascsurgeongrpannual']=factor(levels(testdf[,'fascsurgeongrpannual'])[4],levels=levels(testdf[,'fascsurgeongrpannual']))


changedBetas.noMMAEffect=c(0,0,0,0)
names(changedBetas.noMMAEffect)=c('mma2008.impact','mma2008.impact:fascsurgeongrpannual[13.2,18.2)','mma2008.impact:fascsurgeongrpannual[18.2,25.3)','mma2008.impact:fascsurgeongrpannual[25.3, Inf]')
predVal=btXbWithShock.yz(coef(fit.totexp) #a named  usually coef(fit)[colnames(vcov(fit))]
                         , vcov(fit.totexp)  #at least colnamed matrix usually vcov(fit)
                         , changedBetas.noMMAEffect
                         , sd(fit.totexp$residuals)  # sd(fit$residuals) this is from the model  stardard error of shock
                         , model.matrix(formula(fit.totexp),testdf) #usually it is  model.matrix(formula(fit),data)
                         , 100                   
                         
                         , changedBetasHaveVariation=T
                         
                         #using this will be more accurate when there is nonlinear operation
                         , seed=211 #this seed will be used to draw coef and shock      
                         , sameShockVec=FALSE    #you always set it to FALSE         
                         #if False, then use different shock in each simulation
                         #if you use same shock, it usually is bad. because the same shock would cause consistent underestimation or overestimation
)


sumMat=do.call(rbind,lapply(locList,function(x){apply(exp(predVal[x,]),2,sum)}))

meanVec.totexp.hat.noMMA.fasc.4=apply(sumMat,1,mean)/1e9
ciMat.totexp.hat.noMMA.fasc.4=apply(sumMat,1,function(x){quantile(x,probs=c(0.025,0.975))})/1e9





pdf('Z:/j_scrdata/ascMMA/result/fig/mmaFascEffectOnTotExpByFascType.pdf')
plot(5*totexp.mma.hat,type='b',pch=1,ylim=c(6,20),ylab='total expenditure (billion dollars)',lwd=2)
lines(5*totexp.nomma.hat,type='b',pch=2)
lines(5*meanVec.totexp.hat.noMMA.fasc.1,type='b',pch=3)
lines(5*meanVec.totexp.hat.noMMA.fasc.2,type='b',pch=4)
lines(5*meanVec.totexp.hat.noMMA.fasc.3,type='b',pch=5)
lines(5*meanVec.totexp.hat.noMMA.fasc.4,type='b',pch=6)
legend('bottomleft',pch=c(1,2,3,4,5,6),legend=c('MMA','no MMA', 'no MMA fasc grp1','no MMA fasc grp2','no MMA fasc grp3','no MMA fasc grp4'),cex=0.9)
abline(v=8,lty=3)
dev.off()


# 
# fit.percap.totopexp=glm(log(percap.totopexp) ~ qtr + mean.age + .avg.comorbsum 
#                       + mma2008.impact 
#                       + fascsurgeongrpannual
#                       + mma2008.impact*fascsurgeongrpannual
#                       #+ firstqtraftermma
#                       #          +.qtr.since2006+.qtr.since2006.sqrt
#                       +.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4
#                       +.qtr.since2006.bs5
#                       +.qtr.since2006.bs6
#                       +.qtr.since2006.bs7+.qtr.since2006.bs8
#                       +.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11
#                       , data=anaDfWithBs)
# 
# summary(fit.percap.totopexp)





#per capital outpatient  vol
fit.percap.outpVol=glm(
    totaloutpvol ~
    mma2008.impact 
  + fascsurgeongrpannual
  + mma2008.impact*fascsurgeongrpannual+ qtr + mean.age + .avg.comorbsum +mean.age+.avg.comorbsum
    +race.0+race.1+race.2+race.3+race.4+race.5
    +sex.1
    +.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4+.qtr.since2006.bs5+.qtr.since2006.bs6+.qtr.since2006.bs7+.qtr.since2006.bs8+.qtr.since2006.bs8+.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11,
  , offset=log(n.bene)
  , family = poisson
  , data=anaDfWithBs)

#names(anaDfWithBs)

summary(fit.percap.outpVol)

#goodness of fit


predVal=exp(btXbWithShock.yz( coef(fit.percap.outpVol) 
                          , vcov(fit.percap.outpVol)  
                          , changedBetas=NULL
                          , 0 #for poission model, the random shock follows a poission
                          , model.matrix(formula(fit.percap.outpVol),fit.percap.outpVol$data) 
                          , 100                        
                          , changedBetasHaveVariation=TRUE
                          , seed=1  
                          , sameShockVec=FALSE   
                          
))

summary(fit.percap.outpVol)
xtable(fit.percap.outpVol)


locList=valLoc.yz(anaDfWithBs$'.qtr.since2006')
meanMat=do.call(rbind,lapply(locList,function(x){apply(predVal[x,],2,mean)}))
percap.outpvol.mma.hat=apply(meanMat,1,mean)
percap.outpvol.mma.ci=apply(meanMat,1,function(x){quantile(x,probs=c(0.025,0.975))})
percap.outpvol.true=unlist(lapply(locList,function(x){mean(anaDfWithBs$percap.outpvol[x])}))

pdf('Z:/j_scrdata/ascMMA/result/fig/outpVolGof.pdf')
plot(percap.outpvol.true,type='b',pch=1,ylim=c(0,0.6))
lines(percap.outpvol.mma.hat,type='b',pch=2)
legend('bottomleft',legend=c('observed','predicted'),pch=c(1,2))
dev.off()

#no MMA
changedBetas.noMMAEffect=c(0,0,0,0)
names(changedBetas.noMMAEffect)=c('mma2008.impact','mma2008.impact:fascsurgeongrpannual[13.2,18.2)','mma2008.impact:fascsurgeongrpannual[18.2,25.3)','mma2008.impact:fascsurgeongrpannual[25.3, Inf]')
predVal=exp(btXbWithShock.yz( coef(fit.percap.outpVol) 
                          , vcov(fit.percap.outpVol)  
                          , changedBetas=changedBetas.noMMAEffect
                          , 0
                          , model.matrix(formula(fit.percap.outpVol),fit.percap.outpVol$data) 
                          , 100                        
                          , changedBetasHaveVariation=TRUE
                          , seed=1  
                          , sameShockVec=FALSE   
                          
))


locList=valLoc.yz(anaDfWithBs$'.qtr.since2006')
meanMat=do.call(rbind,lapply(locList,function(x){apply(predVal[x,],2,mean)}))
percap.outpvol.nomma.hat=apply(meanMat,1,mean)
percap.outpvol.nomma.ci=apply(meanMat,1,function(x){quantile(x,probs=c(0.025,0.975))})




#no MMA and different type of FASC
#fasc group1
testdf=fit.percap.outpVol$data
chgLoc=which(testdf[,'.qtr.since2006']>8)
testdf[chgLoc,'fascsurgeongrpannual']=factor(levels(testdf[,'fascsurgeongrpannual'])[1],levels=levels(testdf[,'fascsurgeongrpannual']))

changedBetas.noMMAEffect=c(0,0,0,0)
names(changedBetas.noMMAEffect)=c('mma2008.impact','mma2008.impact:fascsurgeongrpannual[13.2,18.2)','mma2008.impact:fascsurgeongrpannual[18.2,25.3)','mma2008.impact:fascsurgeongrpannual[25.3, Inf]')
predVal=exp(btXbWithShock.yz( coef(fit.percap.outpVol) 
                              , vcov(fit.percap.outpVol)  
                              , changedBetas=changedBetas.noMMAEffect
                              , 0
                              , model.matrix(formula(fit.percap.outpVol),testdf) 
                              , 100                        
                              , changedBetasHaveVariation=TRUE
                              , seed=1  
                              , sameShockVec=FALSE   
                              
))


locList=valLoc.yz(anaDfWithBs$'.qtr.since2006')
meanMat=do.call(rbind,lapply(locList,function(x){apply(predVal[x,],2,mean)}))
percap.outpvol.nomma.fasc.1.hat=apply(meanMat,1,mean)
percap.outpvol.nomma.fasc.1.ci=apply(meanMat,1,function(x){quantile(x,probs=c(0.025,0.975))})


#gruop 2 fasc
testdf=fit.percap.outpVol$data
chgLoc=which(testdf[,'.qtr.since2006']>8)
testdf[chgLoc,'fascsurgeongrpannual']=factor(levels(testdf[,'fascsurgeongrpannual'])[2],levels=levels(testdf[,'fascsurgeongrpannual']))

changedBetas.noMMAEffect=c(0,0,0,0)
names(changedBetas.noMMAEffect)=c('mma2008.impact','mma2008.impact:fascsurgeongrpannual[13.2,18.2)','mma2008.impact:fascsurgeongrpannual[18.2,25.3)','mma2008.impact:fascsurgeongrpannual[25.3, Inf]')
predVal=exp(btXbWithShock.yz( coef(fit.percap.outpVol) 
                              , vcov(fit.percap.outpVol)  
                              , changedBetas=changedBetas.noMMAEffect
                              , 0
                              , model.matrix(formula(fit.percap.outpVol),testdf) 
                              , 100                        
                              , changedBetasHaveVariation=TRUE
                              , seed=1  
                              , sameShockVec=FALSE   
                              
))


locList=valLoc.yz(anaDfWithBs$'.qtr.since2006')
meanMat=do.call(rbind,lapply(locList,function(x){apply(predVal[x,],2,mean)}))
percap.outpvol.nomma.fasc.2.hat=apply(meanMat,1,mean)
percap.outpvol.nomma.fasc.2.ci=apply(meanMat,1,function(x){quantile(x,probs=c(0.025,0.975))})



#gruop 3 fasc
testdf=fit.percap.outpVol$data
chgLoc=which(testdf[,'.qtr.since2006']>8)
testdf[chgLoc,'fascsurgeongrpannual']=factor(levels(testdf[,'fascsurgeongrpannual'])[3],levels=levels(testdf[,'fascsurgeongrpannual']))

changedBetas.noMMAEffect=c(0,0,0,0)
names(changedBetas.noMMAEffect)=c('mma2008.impact','mma2008.impact:fascsurgeongrpannual[13.2,18.2)','mma2008.impact:fascsurgeongrpannual[18.2,25.3)','mma2008.impact:fascsurgeongrpannual[25.3, Inf]')
predVal=exp(btXbWithShock.yz( coef(fit.percap.outpVol) 
                              , vcov(fit.percap.outpVol)  
                              , changedBetas=changedBetas.noMMAEffect
                              , 0
                              , model.matrix(formula(fit.percap.outpVol),testdf) 
                              , 100                        
                              , changedBetasHaveVariation=TRUE
                              , seed=1  
                              , sameShockVec=FALSE   
                              
))


locList=valLoc.yz(anaDfWithBs$'.qtr.since2006')
meanMat=do.call(rbind,lapply(locList,function(x){apply(predVal[x,],2,mean)}))
percap.outpvol.nomma.fasc.3.hat=apply(meanMat,1,mean)
percap.outpvol.nomma.fasc.3.ci=apply(meanMat,1,function(x){quantile(x,probs=c(0.025,0.975))})



#gruop 4 fasc
testdf=fit.percap.outpVol$data
chgLoc=which(testdf[,'.qtr.since2006']>8)
testdf[chgLoc,'fascsurgeongrpannual']=factor(levels(testdf[,'fascsurgeongrpannual'])[4],levels=levels(testdf[,'fascsurgeongrpannual']))

changedBetas.noMMAEffect=c(0,0,0,0)
names(changedBetas.noMMAEffect)=c('mma2008.impact','mma2008.impact:fascsurgeongrpannual[13.2,18.2)','mma2008.impact:fascsurgeongrpannual[18.2,25.3)','mma2008.impact:fascsurgeongrpannual[25.3, Inf]')
predVal=exp(btXbWithShock.yz( coef(fit.percap.outpVol) 
                              , vcov(fit.percap.outpVol)  
                              , changedBetas=changedBetas.noMMAEffect
                              , 0
                              , model.matrix(formula(fit.percap.outpVol),testdf) 
                              , 100                        
                              , changedBetasHaveVariation=TRUE
                              , seed=1  
                              , sameShockVec=FALSE   
                              
))


locList=valLoc.yz(anaDfWithBs$'.qtr.since2006')
meanMat=do.call(rbind,lapply(locList,function(x){apply(predVal[x,],2,mean)}))
percap.outpvol.nomma.fasc.4.hat=apply(meanMat,1,mean)
percap.outpvol.nomma.fasc.4.ci=apply(meanMat,1,function(x){quantile(x,probs=c(0.025,0.975))})




pdf('Z:/j_scrdata/ascMMA/result/fig/outpVolMMAEffect.pdf')
plot(percap.outpvol.mma.hat,type='b',pch=1,ylim=c(0.3,0.42),ylab='per capita outpatient volume', main=c('outpatient volume with and without MMA (CI coverage will be nonoverlapping)'))
lines(percap.outpvol.nomma.hat,type='b',pch=2)
lines(percap.outpvol.nomma.fasc.1.hat,type='b',pch=3)
lines(percap.outpvol.nomma.fasc.2.hat,type='b',pch=4)
lines(percap.outpvol.nomma.fasc.3.hat,type='b',pch=5)
lines(percap.outpvol.nomma.fasc.4.hat,type='b',pch=6)
legend('bottomright',legend=c('MMA','no MMA', 'no MMA fasc grp1','no MMA fasc grp2','no MMA fasc grp3','no MMA fasc grp4'),pch=c(1,2,3,4,5,6),cex=1)
abline(v=8,lty=3)
dev.off()



plot(anaDfWithBs[,'.qtr.since2006'],anaDfWithBs[,'percap.totexp'],ylab='number of eligiable beneficiaries', xlab='FASC penetration group')


plot(ddply(anaDfWithBs,'.qtr.since2006', function(x){mean(x[,'percap.totexp'])}),type='b')

pdf('Z:/j_scrdata/ascMMA/result/fig/quarterlyCycleTotalExp.pdf')
plot(ddply(anaDfWithBs,'.qtr.since2006', function(x){mean(x[,'totexp'])}),type='b',ylab='total expenditure')
dev.off()

pdf('Z:/j_scrdata/ascMMA/result/fig/quarterlyCyclePercapExp.pdf')
plot(ddply(anaDfWithBs,'.qtr.since2006', function(x){mean(x[,'percap.totexp'])}),type='b',ylab='total percap expenditure')
dev.off()

# negive colreations between beneficianry population size and FASC penetration.
pdf('Z:/j_scrdata/ascMMA/result/fig/negativeCorrelationBetweenPopulatonSizeAndFascPenetration.pdf')
plot(anaDfWithBs[,'fascsurgeongrpannual'],anaDfWithBs[,'n.bene'],ylab='number of eligiable beneficiaries', xlab='FASC penetration group')
dev.off()

plot(anaDfWithBs[,'n.bene'],anaDfWithBs[,'annual.nfascdocper10k'])
plot(anaDfWithBs[,'n.bene'],anaDfWithBs[,'nfascdocper10kbene'])

#per capita increase and in high penetration places, the increase is faster
#this model already adjusted for fasc, so...it may mean that upcoding is more significant in high pentration region

subset(anaDfWithBs[,c('mma2008.impact','.qtr.since2006')],mma2008.impact==0)

#how about just ouppatient care
#we MMA has little impact (sign is positive though)
#but for high FASC places outpatient cost dropped signficantly
fit.totopexp=glm(log(totopexp) ~ qtr + mean.age + .avg.comorbsum 
                 + mma2008.impact 
                 + fascsurgeongrpannual
                 + mma2008.impact*fascsurgeongrpannual
                 #+ firstqtraftermma
                 #          +.qtr.since2006+.qtr.since2006.sqrt
                 +.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4
                 +.qtr.since2006.bs5
                 +.qtr.since2006.bs6
                 +.qtr.since2006.bs7+.qtr.since2006.bs8
                 +.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11
                 , data=anaDfWithBs)

summary(fit.totopexp)

fit.percap.totopexp=glm(log(percap.totopexp) ~ qtr + mean.age + .avg.comorbsum 
                 + mma2008.impact 
                 + fascsurgeongrpannual
                 + mma2008.impact*fascsurgeongrpannual
                 #+ firstqtraftermma
                 #          +.qtr.since2006+.qtr.since2006.sqrt
                 +.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4
                 +.qtr.since2006.bs5
                 +.qtr.since2006.bs6
                 +.qtr.since2006.bs7+.qtr.since2006.bs8
                 +.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11
                 , data=anaDfWithBs)

summary(fit.percap.totopexp)

summary(fit.percap.to)



fit.percap.totipexp=glm(log(percap.totipexp) ~ qtr + mean.age + .avg.comorbsum 
                 + mma2008.impact 
                 + fascsurgeongrpannual
                 + mma2008.impact*fascsurgeongrpannual
                 #+ firstqtraftermma
                 #          +.qtr.since2006+.qtr.since2006.sqrt
                 +.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4
                 +.qtr.since2006.bs5
                 +.qtr.since2006.bs6
                 +.qtr.since2006.bs7+.qtr.since2006.bs8
                 +.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11
                 , data=anaDfWithBs)

summary(fit.percap.totipexp)

fit.totipexp=glm(log(totipexp) ~ qtr + mean.age + .avg.comorbsum 
                 + mma2008.impact 
                 + fascsurgeongrpannual
                 #+ firstqtraftermma
                 #          +.qtr.since2006+.qtr.since2006.sqrt
                 +.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4
                 +.qtr.since2006.bs5
                 +.qtr.since2006.bs6
                 +.qtr.since2006.bs7+.qtr.since2006.bs8
                 +.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11
                 , data=anaDfWithBs)

summary(fit.totipexp)


#what about per cap total expenditure on outpatinet care
fit.percap.totopexp=glm(log(percap.totopexp) ~ qtr + mean.age + .avg.comorbsum 
                        + mma2008.impact 
                        + fascsurgeongrpannual
                        #+ firstqtraftermma
                        #          +.qtr.since2006+.qtr.since2006.sqrt
                        +.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4
                        +.qtr.since2006.bs5
                        +.qtr.since2006.bs6
                        +.qtr.since2006.bs7+.qtr.since2006.bs8
                        +.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11
                        , data=anaDfWithBs)

summary(fit.percap.totopexp)

names(anaDfWithBs)

#per capita outpatient volume
fit.percap.outpvol=glm(
  totaloutpvol ~  mma2008.impact+fascsurgeongrp+mean.age+.avg.comorbsum+race.0+race.1+race.2+race.3+race.4+race.5+sex.1+.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4+.qtr.since2006.bs5+.qtr.since2006.bs6,
  , offset=log(n.bene)
  , family = poisson
  , data=anaDfWithBs)
summary(fit.percap.outpvol)



fit.percap.totipexp=glm(log(percap.totipexp) ~ 
                          + mma2008.impact 
                        + fascsurgeongrpannual
                        + mma2008.impact*fascsurgeongrpannual
                        #+ firstqtraftermma
                        #          +.qtr.since2006+.qtr.since2006.sqrt
                        + qtr + mean.age + .avg.comorbsum 
                        +.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4
                        +.qtr.since2006.bs5
                        +.qtr.since2006.bs6
                        +.qtr.since2006.bs7+.qtr.since2006.bs8
                        +.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11
                        , data=anaDfWithBs)

summary(fit.percap.totipexp)
