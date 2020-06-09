
#we need to have different trend lines for four buckets of surgery vaolume (0 is one separate bucket)

names(anaDfwithBs)

anaDfwithBs[,'totexp']

fit=modelFit.allvol
mmaEffectSimu=function( fit
                       , link='NA' #link='exp' then it is exp()
                       , dataForFitting=anaDfwithBs
                       , knots.tiles=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)  
                       , n.sim=30
                       
                       ){
  
  #with MMA (MMA effect is nonZero), we do policy experiemtn by altering quarter
  #with MMA (MMA effect is absent), we do policy experiment by altering quarter
outList=list()

#change quarter in data
#change indicator
qtrSince2006Vec=sort(unique(as.integer(fit$data[,'.qtr.since2006'])))
outmat.noMma=outmat.mma.dataFascPene=outmat.mma.grp1=outmat.mma.grp2=outmat.mma.grp3=outmat.mma.grp4=matrix(NA, nrow=length(qtrSince2006Vec), ncol=n.sim)
bslist=bsWrapper1.yz(  qtrSince2006Vec #this is the x in bs function
                       , quantile(fit$data[,'.qtr.since2006'],probs=knots.tiles) #inner knots, if it contains boundary knots, function will stop and issue erro message
                       , paste('.qtr.since2006','.bs',sep='') #output data's columen name stem
                       , degree=2
                       , dataType='data.frame'
                       , Boundary.knots=quantile(fit$data[,'.qtr.since2006'],prob=c(0,1))
                       #or ='data.frame
)
n.basis=bslist$n.basis
bsVns=paste(paste('.qtr.since2006', '.bs', sep=''), seq(n.basis), sep='')

#no mma
for(i in 1:length(qtrSince2006Vec)){
  print(i)
  set.seed(1)
  betaMat=mvrnorm(n=n.sim, mu=coef(fit), Sigma=vcov(fit))
  #set MMA effect to zero (no MMA)
  betaMat[,c("mma2008.impactTRUE","mma2008.impactTRUE:fascsurgeongrp[12.9,18.3)","mma2008.impactTRUE:fascsurgeongrp[18.3,25.5)" ,"mma2008.impactTRUE:fascsurgeongrp[25.5, Inf]")]=0

  policyData=dataForFitting
  policyData[,'mma2008.impact']=(qtrSince2006Vec[i]>=9)
  #change spline
  policyData[,bsVns] = matrix(bslist$bsdata[i,bsVns],nrow=1)[rep(1,nrow(policyData)),]
  policy.dataMatrix=model.matrix(formula(fit),policyData)
  xbOut=xb.matched.yz(betaMat, policy.dataMatrix)
  if (link=='exp'){ predOutcome=exp(xbOut$xb.matched)}
  if (link=='NA') { predOutcome=xbOut$xb.matched}
 
  outmat.noMma[i,]=apply(predOutcome,2,mean)
}



#has MMA, same penetration as in data
for(i in 1:length(qtrSince2006Vec)){
  print(i)
  set.seed(1)
  betaMat=mvrnorm(n=n.sim, mu=coef(fit), Sigma=vcov(fit))
  policyData=dataForFitting
 
  policyData[,'mma2008.impact']=(qtrSince2006Vec[i]>=9)

  #change spline
  policyData[,bsVns] = matrix(bslist$bsdata[i,bsVns],nrow=1)[rep(1,nrow(policyData)),]
  policy.dataMatrix=model.matrix(formula(fit),policyData)
  colnames(policy.dataMatrix)
  xbOut=xb.matched.yz(betaMat, policy.dataMatrix)
  if (link=='exp'){ predOutcome=exp(xbOut$xb.matched)}
  if (link=='NA') { predOutcome=xbOut$xb.matched}
  outmat.mma.dataFascPene[i,]=apply(predOutcome,2,mean)
}

#has mma change FASC penetration to bucket 1
for(i in 1:length(qtrSince2006Vec)){
  #set MMA effect to zero (no MMA)
  print(i)
  set.seed(1)
  betaMat=mvrnorm(n=n.sim, mu=coef(fit), Sigma=vcov(fit))
  
  policyData=dataForFitting
  policyData[,'fascsurgeongrp']=factor(x=levels(policyData[,'fascsurgeongrp'])[1],levels=levels(policyData[,'fascsurgeongrp']))
  policyData[,'mma2008.impact']=(qtrSince2006Vec[i]>=9)
  #change spline
  policyData[,bsVns] = matrix(bslist$bsdata[i,bsVns],nrow=1)[rep(1,nrow(policyData)),]
  policy.dataMatrix=model.matrix(formula(fit),policyData)
  xbOut=xb.matched.yz(betaMat, policy.dataMatrix)
  if (link=='exp'){ predOutcome=exp(xbOut$xb.matched)}
  if (link=='NA') { predOutcome=xbOut$xb.matched}
  outmat.mma.grp1[i,]=apply(predOutcome,2,mean)
}

#has mma change penetration to bucket 2
for(i in 1:length(qtrSince2006Vec)){
  print(i)
  set.seed(1)
  betaMat=mvrnorm(n=n.sim, mu=coef(fit), Sigma=vcov(fit))
  policyData=dataForFitting
  policyData[,'fascsurgeongrp']=factor(x=levels(policyData[,'fascsurgeongrp'])[2],levels=levels(policyData[,'fascsurgeongrp']))
  policyData[,'mma2008.impact']=(qtrSince2006Vec[i]>=9)
  #change spline
  policyData[,bsVns] = matrix(bslist$bsdata[i,bsVns],nrow=1)[rep(1,nrow(policyData)),]
  policy.dataMatrix=model.matrix(formula(fit),policyData)
  xbOut=xb.matched.yz(betaMat, policy.dataMatrix)
  if (link=='exp'){ predOutcome=exp(xbOut$xb.matched)}
  if (link=='NA') { predOutcome=xbOut$xb.matched}
  outmat.mma.grp2[i,]=apply(predOutcome,2,mean)
}
#has mma change to bueckt 3
for(i in 1:length(qtrSince2006Vec)){
  print(i)
  set.seed(1)
  betaMat=mvrnorm(n=n.sim, mu=coef(fit), Sigma=vcov(fit))
  policyData=dataForFitting
  policyData[,'fascsurgeongrp']=factor(x=levels(policyData[,'fascsurgeongrp'])[3],levels=levels(policyData[,'fascsurgeongrp']))
  policyData[,'mma2008.impact']=(qtrSince2006Vec[i]>=9)
  #change spline
  policyData[,bsVns] = matrix(bslist$bsdata[i,bsVns],nrow=1)[rep(1,nrow(policyData)),]
  policy.dataMatrix=model.matrix(formula(fit),policyData)
  xbOut=xb.matched.yz(betaMat, policy.dataMatrix)
  if (link=='exp'){ predOutcome=exp(xbOut$xb.matched)}
  if (link=='NA') { predOutcome=xbOut$xb.matched}
  outmat.mma.grp3[i,]=apply(predOutcome,2,mean)
}
#has mma change to bucket 4

for(i in 1:length(qtrSince2006Vec)){
  print(i)
  set.seed(1)
  betaMat=mvrnorm(n=n.sim, mu=coef(fit), Sigma=vcov(fit))
  policyData=dataForFitting
  policyData[,'fascsurgeongrp']=factor(x=levels(policyData[,'fascsurgeongrp'])[4],levels=levels(policyData[,'fascsurgeongrp']))
  policyData[,'mma2008.impact']=(qtrSince2006Vec[i]>=9)
  #change spline
  policyData[,bsVns] = matrix(bslist$bsdata[i,bsVns],nrow=1)[rep(1,nrow(policyData)),]
  policy.dataMatrix=model.matrix(formula(fit),policyData)
  xbOut=xb.matched.yz(betaMat, policy.dataMatrix)
  if (link=='exp'){ predOutcome=exp(xbOut$xb.matched)}
  if (link=='NA') { predOutcome=xbOut$xb.matched}
  outmat.mma.grp4[i,]=apply(predOutcome,2,mean)
}


outList=list(qtr=qtrSince2006Vec, mma.dataFascPene=outmat.mma.dataFascPene
             , mma.fascGrp1=outmat.mma.grp1
             , mma.fascGrp2=outmat.mma.grp2
             , mma.fascGrp3=outmat.mma.grp3
             , mma.fascGrp4=outmat.mma.grp4
             , noMma.dataFascPene=outmat.noMma)
return(outList)
}

plotDf.list.percavol=mmaEffectSimu(modelFit.allvol,link='exp')
plotDf.list.exp=mmaEffectSimu(modelFit.allexp,link='NA')

summary(modelFit.allexp)


plotDf.list=plotDf.list.exp

mma.dataFascPene.mean=apply(plotDf.list$mma.dataFascPene,1,mean)
mma.dataFascPene.bd=apply(plotDf.list$mma.dataFascPene,1,function(x){quantile(x,prob=c(0.025,0.975))})

mma.fascGrp1.mean=apply(plotDf.list$mma.fascGrp1,1,mean)
mma.fascGrp1.mean[1:8]=mma.dataFascPene.mean[1:8]
mma.fascGrp1.bd=apply(plotDf.list$mma.fascGrp1,1,function(x){quantile(x,prob=c(0.025,0.975))})

mma.fascGrp2.mean=apply(plotDf.list$mma.fascGrp2,1,mean)
mma.fascGrp2.mean[1:8]=mma.dataFascPene.mean[1:8]
mma.fascGrp2.bd=apply(plotDf.list$mma.fascGrp2,1,function(x){quantile(x,prob=c(0.025,0.975))})

mma.fascGrp3.mean=apply(plotDf.list$mma.fascGrp3,1,mean)
mma.fascGrp3.mean[1:8]=mma.dataFascPene.mean[1:8]
mma.fascGrp3.bd=apply(plotDf.list$mma.fascGrp3,1,function(x){quantile(x,prob=c(0.025,0.975))})

mma.fascGrp4.mean=apply(plotDf.list$mma.fascGrp4,1,mean)
mma.fascGrp4.mean[1:8]=mma.dataFascPene.mean[1:8]
mma.fascGrp4.bd=apply(plotDf.list$mma.fascGrp4,1,function(x){quantile(x,prob=c(0.025,0.975))})

noMma.dataFascPene.mean=apply(plotDf.list$noMma.dataFascPene,1,mean)
noMma.dataFascPene.bd=apply(plotDf.list$noMma.dataFascPene,1,function(x){quantile(x,prob=c(0.025,0.975))})


pdf('Z:/j_scrdata/ascMMA/result/fig/mmaImpactTotalOutpatientVol.pdf')
plot(noMma.dataFascPene.mean,type='l',ylim=c(0.32,0.42),main='MMA impact on total outpatinet volumn', xlab='quarter since 2006',col='black',lwd=4)
lines(mma.dataFascPene.mean,col='red',lwd=4)
lines(mma.fascGrp1.mean,col='yellow')
lines(mma.fascGrp2.mean,col='green')
lines(mma.fascGrp3.mean,col='grey')
lines(mma.fascGrp4.mean,col='blue')
legend('topleft',col=c('black','red','yellow','green','grey','blue'),legend=c('no MMA','MMA','MMA.fasc.grp1','MMA.fasc.grp2','MMA.fasc.grp3','MMA.fasc.grp4'),lwd=c(2,2,1,1,1,1),cex=0.8)
abline(v=8,lty=3)
dev.off()




#save(modelFit.allvol, mma.mean,nomma.mean,mma.bd, nomma.bd,file='Z:/j_scrdata/ascMMA/result/policySimulation05172014.RData')

load('Z:/j_scrdata/ascMMA/result/policySimulation05172014.RData')