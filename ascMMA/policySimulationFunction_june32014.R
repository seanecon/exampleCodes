

names(anaDfwithBs)

# anaDfwithBs[,'.qtr.since2006.sq']=anaDfwithBs[,'.qtr.since2006']*anaDfwithBs[,'.qtr.since2006']
# anaDfwithBs[,'.qtr.since2006.mma']=anaDfwithBs[,'mma2008.impact']*anaDfwithBs[,'.qtr.since2006']


#anaDfwithBs[,'.qtr.since2006.sq']=anaDfwithBs[,'.qtr.since2006']*anaDfwithBs[,'.qtr.since2006']

anaDfwithBs.less[,'qtr.mma.1']=anaDfwithBs.less[,'mma2008.impact']*anaDfwithBs.less[,'.qtr.since2006.bs1']
anaDfwithBs.less[,'qtr.mma.2']=anaDfwithBs.less[,'mma2008.impact']*anaDfwithBs.less[,'.qtr.since2006.bs2']
anaDfwithBs.less[,'qtr.mma.3']=anaDfwithBs.less[,'mma2008.impact']*anaDfwithBs.less[,'.qtr.since2006.bs3']
anaDfwithBs.less[,'qtr.mma.4']=anaDfwithBs.less[,'mma2008.impact']*anaDfwithBs.less[,'.qtr.since2006.bs4']
anaDfwithBs.less[,'qtr.mma.5']=anaDfwithBs.less[,'mma2008.impact']*anaDfwithBs.less[,'.qtr.since2006.bs5']
anaDfwithBs.less[,'qtr.mma.6']=anaDfwithBs.less[,'mma2008.impact']*anaDfwithBs.less[,'.qtr.since2006.bs6']
# anaDfwithBs[,'qtr.mma.7']=anaDfwithBs[,'mma2008.impact']*anaDfwithBs[,'.qtr.since2006.bs7']
# anaDfwithBs[,'qtr.mma.8']=anaDfwithBs[,'mma2008.impact']*anaDfwithBs[,'.qtr.since2006.bs8']
# anaDfwithBs[,'qtr.mma.9']=anaDfwithBs[,'mma2008.impact']*anaDfwithBs[,'.qtr.since2006.bs9']
# anaDfwithBs[,'qtr.mma.10']=anaDfwithBs[,'mma2008.impact']*anaDfwithBs[,'.qtr.since2006.bs10']
# anaDfwithBs[,'qtr.mma.11']=anaDfwithBs[,'mma2008.impact']*anaDfwithBs[,'.qtr.since2006.bs11']

names(anaDfwithBs.less)


mmaEffectSimu.qtrMMa=function( fit
                        , outcome='rate' #link='rate' then it is exp() if link=='totexp' then it is sum
                        , mmaFascGrpInter=T
                        , dataForFitting=anaDfwithBs
                        , knots.tiles=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)  
                        , n.sim=30
                        , logOperation=F
                        
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
    betaMat=mvrnorm(n=n.sim, mu=coef(fit)[colnames(vcov(fit))], Sigma=vcov(fit))
    #set MMA effect to zero (no MMA)
    if (mmaFascGrpInter){
      betaMat[,c("mma2008.impactTRUE","mma2008.impactTRUE:fascsurgeongrp[12.9,18.3)","mma2008.impactTRUE:fascsurgeongrp[18.3,25.5)" ,"mma2008.impactTRUE:fascsurgeongrp[25.5, Inf]")]=0
    }else {
      betaMat[,c("mma2008.impactTRUE")]=0
    }
    
    
    
    colnames(betaMat)
    colnames(policy.dataMatrix)
    policyData=dataForFitting
    policyData[,'mma2008.impact']=(qtrSince2006Vec[i]>=9)
    #change spline (change qtr since 2006)
    policyData[,bsVns] = matrix(bslist$bsdata[i,bsVns],nrow=1)[rep(1,nrow(policyData)),] 
    policyData[,'qtr.mma.1']=  policyData[,'mma2008.impact']*policyData[,'.qtr.since2006.bs1']
    policyData[,'qtr.mma.2']=  policyData[,'mma2008.impact']*policyData[,'.qtr.since2006.bs2']
    policyData[,'qtr.mma.3']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs3']
    policyData[,'qtr.mma.4']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs4']
    policyData[,'qtr.mma.5']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs5']
    policyData[,'qtr.mma.6']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs6']
#     policyData[,'qtr.mma.7']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs7']
#     
#     policyData[,'qtr.mma.8']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs8']
#     policyData[,'qtr.mma.9']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs9']
#     policyData[,'qtr.mma.10']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs10']
#     policyData[,'qtr.mma.11']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs11']
#     
    #change qtr (1,2,3,4)
    policyData[,'qtr']=factor(get4quarters(qtrSince2006Vec[i]),levels=levels(anaDfwithBs[,'qtr']))
    policy.dataMatrix=model.matrix(formula(fit),policyData)
    xbOut=xb.matched.yz(betaMat, policy.dataMatrix)
   
    if (outcome=='rate'){ predOutcome=exp(xbOut$xb.matched)
                          outmat.noMma[i,]=apply(predOutcome,2,mean)
    }
    if (outcome=='totexp') { 
      if (logOperation){predOutcome=exp(xbOut$xb.matched)} else{predOutcome=xbOut$xb.matched}
      outmat.noMma[i,]=apply(predOutcome,2,sum)/24 
    } 
  }
  
  
  
  #has MMA, same penetration as in data
  for(i in 1:length(qtrSince2006Vec)){
    print(i)
    set.seed(1)
    betaMat=mvrnorm(n=n.sim, mu=coef(fit)[colnames(vcov(fit))], Sigma=vcov(fit))
    policyData=dataForFitting
    
    policyData[,'mma2008.impact']=(qtrSince2006Vec[i]>=9)
    
    #change spline
    policyData[,bsVns] = matrix(bslist$bsdata[i,bsVns],nrow=1)[rep(1,nrow(policyData)),]
    policyData[,'qtr.mma.1']=  policyData[,'mma2008.impact']*policyData[,'.qtr.since2006.bs1']
    policyData[,'qtr.mma.2']=  policyData[,'mma2008.impact']*policyData[,'.qtr.since2006.bs2']
    policyData[,'qtr.mma.3']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs3']
    policyData[,'qtr.mma.4']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs4']
    policyData[,'qtr.mma.5']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs5']
    policyData[,'qtr.mma.6']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs6']
    #change qtr (1,2,3,4)
    policyData[,'qtr']=factor(get4quarters(qtrSince2006Vec[i]),levels=levels(anaDfwithBs[,'qtr']))
    policy.dataMatrix=model.matrix(formula(fit),policyData)
    colnames(policy.dataMatrix)
    xbOut=xb.matched.yz(betaMat, policy.dataMatrix)
    
    #   if (link=='exp'){ predOutcome=exp(xbOut$xb.matched)}
    #   if (link=='NA') { predOutcome=xbOut$xb.matched}
    #   outmat.mma.dataFascPene[i,]=apply(predOutcome,2,mean)
    
    if (outcome=='rate'){ predOutcome=exp(xbOut$xb.matched)
                          outmat.mma.dataFascPene[i,]=apply(predOutcome,2,mean)
    }
    if (outcome=='totexp') { 
      if (logOperation){predOutcome=exp(xbOut$xb.matched)} else{predOutcome=xbOut$xb.matched}
      outmat.mma.dataFascPene[i,]=apply(predOutcome,2,sum)/24 
    } 
    
    
  }
  
  #has mma change FASC penetration to bucket 1
  for(i in 1:length(qtrSince2006Vec)){
    #set MMA effect to zero (no MMA)
    print(i)
    set.seed(1)
    betaMat=mvrnorm(n=n.sim, mu=coef(fit)[colnames(vcov(fit))], Sigma=vcov(fit))
    
    policyData=dataForFitting
    policyData[,'fascsurgeongrp']=factor(x=levels(policyData[,'fascsurgeongrp'])[1],levels=levels(policyData[,'fascsurgeongrp']))
    policyData[,'mma2008.impact']=(qtrSince2006Vec[i]>=9)
    #change spline
    policyData[,bsVns] = matrix(bslist$bsdata[i,bsVns],nrow=1)[rep(1,nrow(policyData)),]
    policyData[,'qtr.mma.1']=  policyData[,'mma2008.impact']*policyData[,'.qtr.since2006.bs1']
    policyData[,'qtr.mma.2']=  policyData[,'mma2008.impact']*policyData[,'.qtr.since2006.bs2']
    policyData[,'qtr.mma.3']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs3']
    policyData[,'qtr.mma.4']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs4']
    policyData[,'qtr.mma.5']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs5']
    policyData[,'qtr.mma.6']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs6']
    #change qtr (1,2,3,4)
    policyData[,'qtr']=factor(get4quarters(qtrSince2006Vec[i]),levels=levels(anaDfwithBs[,'qtr']))
    policy.dataMatrix=model.matrix(formula(fit),policyData)
    xbOut=xb.matched.yz(betaMat, policy.dataMatrix)
    #   if (link=='exp'){ predOutcome=exp(xbOut$xb.matched)}
    #   if (link=='NA') { predOutcome=xbOut$xb.matched}
    #   outmat.mma.grp1[i,]=apply(predOutcome,2,mean)
    
    if (outcome=='rate'){ predOutcome=exp(xbOut$xb.matched)
                          outmat.mma.grp1[i,]=apply(predOutcome,2,mean)
    }
    if (outcome=='totexp') { 
      if (logOperation){predOutcome=exp(xbOut$xb.matched)} else{predOutcome=xbOut$xb.matched}
      outmat.mma.grp1[i,]=apply(predOutcome,2,sum)/24 
    } 
  }
  
  #has mma change penetration to bucket 2
  for(i in 1:length(qtrSince2006Vec)){
    print(i)
    set.seed(1)
    betaMat=mvrnorm(n=n.sim, mu=coef(fit)[colnames(vcov(fit))], Sigma=vcov(fit))
    policyData=dataForFitting
    policyData[,'fascsurgeongrp']=factor(x=levels(policyData[,'fascsurgeongrp'])[2],levels=levels(policyData[,'fascsurgeongrp']))
    policyData[,'mma2008.impact']=(qtrSince2006Vec[i]>=9)
    #change spline
    policyData[,bsVns] = matrix(bslist$bsdata[i,bsVns],nrow=1)[rep(1,nrow(policyData)),]
    policyData[,'qtr.mma.1']=  policyData[,'mma2008.impact']*policyData[,'.qtr.since2006.bs1']
    policyData[,'qtr.mma.2']=  policyData[,'mma2008.impact']*policyData[,'.qtr.since2006.bs2']
    policyData[,'qtr.mma.3']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs3']
    policyData[,'qtr.mma.4']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs4']
    policyData[,'qtr.mma.5']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs5']
    policyData[,'qtr.mma.6']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs6']
    #change qtr (1,2,3,4)
    policyData[,'qtr']=factor(get4quarters(qtrSince2006Vec[i]),levels=levels(anaDfwithBs[,'qtr']))
    policy.dataMatrix=model.matrix(formula(fit),policyData)
    xbOut=xb.matched.yz(betaMat, policy.dataMatrix)
    #   if (link=='exp'){ predOutcome=exp(xbOut$xb.matched)}
    #   if (link=='NA') { predOutcome=xbOut$xb.matched}
    #   outmat.mma.grp2[i,]=apply(predOutcome,2,mean)
    
    if (outcome=='rate'){ predOutcome=exp(xbOut$xb.matched)
                          outmat.mma.grp2[i,]=apply(predOutcome,2,mean)
    }
    if (outcome=='totexp') { 
      if (logOperation){predOutcome=exp(xbOut$xb.matched)} else{predOutcome=xbOut$xb.matched}
      outmat.mma.grp2[i,]=apply(predOutcome,2,sum)/24 
    } 
    
  }
  #has mma change to bueckt 3
  for(i in 1:length(qtrSince2006Vec)){
    print(i)
    set.seed(1)
    betaMat=mvrnorm(n=n.sim, mu=coef(fit)[colnames(vcov(fit))], Sigma=vcov(fit))
    policyData=dataForFitting
    policyData[,'fascsurgeongrp']=factor(x=levels(policyData[,'fascsurgeongrp'])[3],levels=levels(policyData[,'fascsurgeongrp']))
    policyData[,'mma2008.impact']=(qtrSince2006Vec[i]>=9)
    #change spline
    policyData[,bsVns] = matrix(bslist$bsdata[i,bsVns],nrow=1)[rep(1,nrow(policyData)),]
    policyData[,'qtr.mma.1']=  policyData[,'mma2008.impact']*policyData[,'.qtr.since2006.bs1']
    policyData[,'qtr.mma.2']=  policyData[,'mma2008.impact']*policyData[,'.qtr.since2006.bs2']
    policyData[,'qtr.mma.3']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs3']
    policyData[,'qtr.mma.4']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs4']
    policyData[,'qtr.mma.5']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs5']
    policyData[,'qtr.mma.6']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs6']
    #change qtr (1,2,3,4)
    policyData[,'qtr']=factor(get4quarters(qtrSince2006Vec[i]),levels=levels(anaDfwithBs[,'qtr']))
    policy.dataMatrix=model.matrix(formula(fit),policyData)
    xbOut=xb.matched.yz(betaMat, policy.dataMatrix)
    #   if (link=='exp'){ predOutcome=exp(xbOut$xb.matched)}
    #   if (link=='NA') { predOutcome=xbOut$xb.matched}
    #   outmat.mma.grp3[i,]=apply(predOutcome,2,mean)
    
    if (outcome=='rate'){ predOutcome=exp(xbOut$xb.matched)
                          outmat.mma.grp3[i,]=apply(predOutcome,2,mean)
    }
    if (outcome=='totexp') { 
      if (logOperation){predOutcome=exp(xbOut$xb.matched)} else{predOutcome=xbOut$xb.matched}
      outmat.mma.grp3[i,]=apply(predOutcome,2,sum)/24 
    } 
    
  }
  #has mma change to bucket 4
  
  for(i in 1:length(qtrSince2006Vec)){
    print(i)
    set.seed(1)
    betaMat=mvrnorm(n=n.sim, mu=coef(fit)[colnames(vcov(fit))], Sigma=vcov(fit))
    policyData=dataForFitting
    policyData[,'fascsurgeongrp']=factor(x=levels(policyData[,'fascsurgeongrp'])[4],levels=levels(policyData[,'fascsurgeongrp']))
    policyData[,'mma2008.impact']=(qtrSince2006Vec[i]>=9)
    #change spline
    policyData[,bsVns] = matrix(bslist$bsdata[i,bsVns],nrow=1)[rep(1,nrow(policyData)),]
    policyData[,'qtr.mma.1']=  policyData[,'mma2008.impact']*policyData[,'.qtr.since2006.bs1']
    policyData[,'qtr.mma.2']=  policyData[,'mma2008.impact']*policyData[,'.qtr.since2006.bs2']
    policyData[,'qtr.mma.3']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs3']
    policyData[,'qtr.mma.4']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs4']
    policyData[,'qtr.mma.5']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs5']
    policyData[,'qtr.mma.6']=  policyData[,'mma2008.impact']*  policyData[,'.qtr.since2006.bs6']
    #change qtr (1,2,3,4)
    policyData[,'qtr']=factor(get4quarters(qtrSince2006Vec[i]),levels=levels(anaDfwithBs[,'qtr']))
    policy.dataMatrix=model.matrix(formula(fit),policyData)
    xbOut=xb.matched.yz(betaMat, policy.dataMatrix)
    #   if (link=='exp'){ predOutcome=exp(xbOut$xb.matched)}
    #   if (link=='NA') { predOutcome=xbOut$xb.matched}
    #   outmat.mma.grp4[i,]=apply(predOutcome,2,mean)
    
    if (outcome=='rate'){ predOutcome=exp(xbOut$xb.matched)
                          outmat.mma.grp4[i,]=apply(predOutcome,2,mean)
    }
    if (outcome=='totexp') { 
      if (logOperation){predOutcome=exp(xbOut$xb.matched)} else{predOutcome=xbOut$xb.matched}
      outmat.mma.grp4[i,]=apply(predOutcome,2,sum)/24 
    } 
  }
  
  
  outList=list(qtr=qtrSince2006Vec, mma.dataFascPene=outmat.mma.dataFascPene
               , mma.fascGrp1=outmat.mma.grp1
               , mma.fascGrp2=outmat.mma.grp2
               , mma.fascGrp3=outmat.mma.grp3
               , mma.fascGrp4=outmat.mma.grp4
               , noMma.dataFascPene=outmat.noMma)
  return(outList)
}



fit=glm(
  totexp ~ fascsurgeongrp+mean.age+.avg.comorbsum+race.0+race.1+race.2+race.3+race.4+race.5+sex.1
  +.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4+.qtr.since2006.bs5+.qtr.since2006.bs6
  +qtr.mma.1+qtr.mma.2+qtr.mma.3
  , data=anaDfwithBs.less)



summary(fit)



fit=glm(
  log(totexp) ~fascsurgeongrp+posqtrdev+negqtrdev+lastyear+mean.age+.avg.comorbsum+race.0+race.1+race.2+race.3+race.4+race.5+sex.1+.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4+.qtr.since2006.bs5+.qtr.since2006.bs6
  , data=anaDfwithBs.less)

names(anaDfwithBs.less)

fit=glm(
  log(totexp) ~fascsurgeongrp+posqtrdev+mean.age+.avg.comorbsum+race.0+race.1+race.2+race.3+race.4+race.5+sex.1+.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4+.qtr.since2006.bs5+
    .qtr.since2006.bs6+.qtr.since2006.bs7+.qtr.since2006.bs8+.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11
  , data=anaDfwithBs.less)


fit=glm(
  log(totexp) ~fascsurgeongrp+posqtrdev+mean.age+.avg.comorbsum+race.0+race.1+race.2+race.3+race.4+race.5+sex.1+.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3
  ++.qtr.since2006.bs4
  , data=anaDfwithBs.less)

fit=glm(
  log(totexp) ~fascsurgeongrp+posqtrdev+mean.age+.avg.comorbsum+race.0+race.1+race.2+race.3+race.4+race.5+sex.1+.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3
  +.qtr.since2006.bs4+.qtr.since2006.bs5+.qtr.since2006.bs6
  , data=anaDfwithBs.less)

summary(fit)

fit=glm(
  log(totexp) ~fascsurgeongrp+qtr+mean.age+.avg.comorbsum+race.0+race.1+race.2+race.3+race.4+race.5+sex.1
  , data=anaDfwithBs.less)

fit=glm(
  log(totexp) ~qtr+mean.age+.avg.comorbsum+fascsurgeongrp+posqtrdev+posqtrdevsq+negqtrdev
  , data=anaDfwithBs.less)

#anaDfwithBs.less[,c('qtr','.qtr.since2006')]

summary(fit)

#noeffect of negative deviation that means before 2008 it was flat

#so we use the following
fit.sq=glm(log(totexp) ~qtr+.qtr.since2006+.qtr.since2006.sqrt+mean.age+.avg.comorbsum+fascsurgeongrp+posqtrdevpoint4
  # +.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4
  #+.qtr.since2006.bs5
#            +.qtr.since2006.bs6
#            +.qtr.since2006.bs7+.qtr.since2006.bs8
#            +.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11
  , data=anaDfwithBs.less)
summary(fit.sq)
names(anaDfwithBs.less)
logLik(fit.sq)


#we need to model to check wheathere there is a constant effect and time varying effect

fit.simple=glm(log(totexp) ~ qtr + mean.age + .avg.comorbsum 
               + mma2008.impact 
               + fascsurgeongrp
               #+ firstqtraftermma
   #          +.qtr.since2006+.qtr.since2006.sqrt
            +.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4
            +.qtr.since2006.bs5
                        +.qtr.since2006.bs6
                        +.qtr.since2006.bs7+.qtr.since2006.bs8
                        +.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11
           , data=anaDfwithBs.less)


summary(fit.simple)

count(subset(anaDfwithBs.less, .qtr.since2006<8),'fascsurgeongrp')
count(subset(anaDfwithBs.less, .qtr.since2006>8),'fascsurgeongrp')

names(anaDfwithBs.less)
logLik(fit.sq)

xbwithshock.mma.nomma=btXbWithShock.yz(    fit.simple
                                           , n.sim=500
                                           , list(mma2008.impact=0
                                                  #, firstqtraftermma=0
                                                  ) #this named list specify the desired changes of beta, for example, we can block beta to zeros 
                                       #list(posqtrdev=0,posqtrdevsqrt=0)
                                           , sameShockVec=F
                                           #if False, then use different shock in each simulation
                                           , seed=123
)




    locList=valLoc.yz(fit.sq$data$'.qtr.since2006')
    xbwithshock.mma=xbwithshock.mma.nomma[[1]]
    xbwithshock.nomma=xbwithshock.mma.nomma[[2]]

    sumMat.mma=do.call(rbind,lapply(locList,function(x){apply(exp(xbwithshock.mma[x,]),2,sum)}))
    sumMat.nomma=do.call(rbind,lapply(locList,function(x){apply(exp(xbwithshock.nomma[x,]),2,sum)}))
    
    sumCostVec.true=unlist(lapply(locList,function(x){sum(fit.sq$data$totexp[x])}))  
    plot(sumCostVec.true,type='b',col='black',ylim=c(0E9,3.1E9))
    lines(apply(sumMat.mma,1,mean),col='blue')
    lines(apply(sumMat.nomma,1,mean),col='red')


    



#push fasc penetration to end of 2007 level

fit$data[,c('fascsurgeongrp','hrrnum','.qtr.since2006')]
  
  
  

  endof2007.fasc=subset(fit$data, .qtr.since2006==8)[,c('hrrnum','fascsurgeongrp')]
  beforeMMA=subset(fit$data, .qtr.since2006<=8)
  afterMMA=subset(fit$data, .qtr.since2006>8)
  afterMMA[,'fascsurgeongrp'] <- NULL
  afterMMA.changedFasc=join(afterMMA, endof2007.fasc, by=c('hrrnum'))
  dataChangedFascGrp=rbind(beforeMMA, afterMMA.changedFasc)








plot(log(sumCostVec.true))
plot(sumCostVec.true)
# 
# fit.sqrt=glm(
#   log(totexp) ~qtr+mean.age+.avg.comorbsum+fascsurgeongrp+posqtrdev+posqtrdevsqrt
#   , data=anaDfwithBs.less)

xbwithshock.mma.nomma = btXbWithShock.yz(fit.sqrt
                                         , n.sim=500
                                         , list(posqtrdevsqrt=0,posqtrdev=0) #this named list specify the desired changes of beta, for example, we can block beta to zeros 
                                         , sameShockVec=F
                                         #if False, then use different shock in each simulation
                                         ,seed=123
)
    names(fit$data)
    
    model.matrix
    str(fit)
  #no MMA case
    betaMat=btBeta.yz(fit,n.sim,1)
    
    betaMat.noMMA=betaMat
    betaMat.noMMA[,which(colnames(betaMat)==mmaEffVn)]=0
    xbOut.noMMA=xb.matched.yz(betaMat.noMMA, model.matrix(formula(fit),fit$data))
    xb.noMMA=xbOut.noMMA$xb.matched+repmat.yz(matrix(rnorm(nrow(xbOut$xb.matched),0,sd(fit$residuals)),ncol=1),1,n.sim)
    locList=valLoc.yz(fit$data$'.qtr.since2006')
    
    sumMat.noMMA=do.call(rbind,lapply(locList,function(x){apply(exp(xb.noMMA[x,]),2,sum)}))
    
    sumCostVec.true=unlist(lapply(locList,function(x){sum(fit$data$totexp[x])}))
    
    plot(sumCostVec.true,type='b',col='red',ylim=c(0E9,3.1E9))
    lines(apply(sumMat.noMMA,1,mean),col='black')
    lines(apply(sumMat,1,mean),col='blue')
  
  
  return(outlist)
}


summary(fit)


#you do not need the negQtrDev because it is modeled by trend spline





fit=glm(
  totexp ~ fascsurgeongrp+mean.age+.avg.comorbsum+race.0+race.1+race.2+race.3+race.4+race.5+sex.1
  +.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4+.qtr.since2006.bs5+.qtr.since2006.bs6
  +qtr.mma.1+qtr.mma.2+qtr.mma.3
  , data=anaDfwithBs.less)

plotDf.list.exp.nolog=mmaEffectSimu.qtrMMa(fit, outcome='totexp',mmaFascGrpInter=F,knots.tiles=c(0.2,0.4,0.6,0.8))




summary(fit)


fit=glm(
  totexp ~fascsurgeongrp +mean.age+.avg.comorbsum+race.0+race.1+race.2+race.3+race.4+race.5+sex.1+qtr.mma.5+qtr.mma.6+qtr.mma.7+qtr.mma.10+qtr.mma.11
  , data=anaDfwithBs)

summary(fit)

plotDf.list.exp.nolog=mmaEffectSimu(fit, outcome='totexp',mmaFascGrpInter=F,knots.tiles=seq(0.1,0.9,by=0.1))


names(anaDfwithBs)


fit=glm(
  totexp ~ mma2008.impact+.qtr.since2006.sq+.qtr.since2006+qtr+fascsurgeongrp +mean.age+.avg.comorbsum+race.0+race.1+race.2+race.3+race.4+race.5+sex.1+.qtr.since2006.mma
  , data=anaDfwithBs)


summary(fit)

modelFit.allvol=glm(
  totaloutpvol ~ mma2008.impact+qtr+fascsurgeongrp + mma2008.impact*fascsurgeongrp+mean.age+.avg.comorbsum+race.0+race.1+race.2+race.3+race.4+race.5+sex.1+.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4+.qtr.since2006.bs5+.qtr.since2006.bs6+.qtr.since2006.bs7+.qtr.since2006.bs8+.qtr.since2006.bs8+.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11,
  , offset=log(n.bene)
  , family = poisson
  , data=anaDfwithBs)

ddply(anaDfwithBs,c('.qtr.since2006'),function(x){sum(x[,'totaloutpvol'])/sum(x[,'n.bene'])})

summary(modelFit.allvol)
names(anaDfwithBs)

names(anaDfwithBs)

# modelFit.allexp=glm(
#   totexp ~ mma2008.impact+qtr+fascsurgeongrp + mma2008.impact*fascsurgeongrp+mean.age+.avg.comorbsum+race.0+race.1+race.2+race.3+race.4+race.5+sex.1+.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4+.qtr.since2006.bs5+.qtr.since2006.bs6+.qtr.since2006.bs7+.qtr.since2006.bs8+.qtr.since2006.bs8+.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11,
#   , data=anaDfwithBs)


#drop interation due to nonsignificance, no qtr too
modelFit.allexp.nointer.log=glm(
  log(totexp) ~ mma2008.impact+qtr+fascsurgeongrp +mean.age+.avg.comorbsum+race.0+race.1+race.2+race.3+race.4+race.5+sex.1+.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4+.qtr.since2006.bs5+.qtr.since2006.bs6
  , data=anaDfwithBs.less)


anaDfwithBs[,c('qtr','.qtr.since2006')]
names(anaD)

#need to add random shock before doing exponential transformation

modelFit.allexp.nointer.nolog=glm(
  totexp ~ mma2008.impact+fascsurgeongrp +mean.age+.avg.comorbsum+race.0+race.1+race.2+race.3+race.4+race.5+sex.1
  +.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4+.qtr.since2006.bs5+.qtr.since2006.bs6
#   +mma2008.impact*.qtr.since2006.bs1+mma2008.impact*.qtr.since2006.bs2
#   +mma2008.impact*.qtr.since2006.bs3+mma2008.impact*.qtr.since2006.bs4 +mma2008.impact*.qtr.since2006.bs5+mma2008.impact*.qtr.since2006.bs6
  , data=anaDfwithBs.less)
summary(modelFit.allexp.nointer.nolog)


modelFit.allexp.nointer.nolog=glm(
  totexp ~ fascsurgeongrp +mean.age+.avg.comorbsum+race.0+race.1+race.2+race.3+race.4+race.5+sex.1
  +.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4+.qtr.since2006.bs5+.qtr.since2006.bs6
  +mma2008.impact*.qtr.since2006.bs1+mma2008.impact*.qtr.since2006.bs2
#   +mma2008.impact*.qtr.since2006.bs3+mma2008.impact*.qtr.since2006.bs4+mma2008.impact*.qtr.since2006.bs5+mma2008.impact*.qtr.since2006.bs6
  , data=anaDfwithBs.less)
summary(modelFit.allexp.nointer.nolog)


modelFit.allexp.nointer.nolog=glm(
  totexp ~ mma2008.impact+fascsurgeongrp +mean.age+.avg.comorbsum+race.0+race.1+race.2+race.3+race.4+race.5+sex.1+.qtr.since2006
  , data=anaDfwithBs.less)


fit=modelFit.allexp.nointer.nolog
modelFit.allexp.nointer.nolog$data[,'.qtr.since2006']

names(anaDfwithBs.less)
#we need to have different trend lines for four buckets of surgery vaolume (0 is one separate bucket)
get4quarters=function(vec){
  remainderVec=vec%%4
  zeroLoc=which(remainderVec==0)
  remainderVec[zeroLoc]=4
  return(remainderVec)
}
names(anaDfwithBs)

anaDfwithBs[,'totexp']

fit=modelFit.allvol
fit=modelFit.allexp.nointer.noqtr

outcome='totexp'
mmaFascGrpInter=F
knots.tiles=c(0.2,0.4,0.6,0.8)  
dataForFitting=anaDfwithBs.less


fit=modelFit.allexp.nointer.nolog


mmaEffectSimu=function( fit
                       , outcome='rate' #link='rate' then it is exp() if link=='totexp' then it is sum
                       , mmaFascGrpInter=T
                       , dataForFitting=anaDfwithBs
                       , knots.tiles=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)  
                       , n.sim=30
                       , logOperation=F
                       
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
  betaMat=mvrnorm(n=n.sim, mu=coef(fit)[colnames(vcov(fit))], Sigma=vcov(fit))
  #set MMA effect to zero (no MMA)
  if (mmaFascGrpInter){
    betaMat[,c("mma2008.impactTRUE","mma2008.impactTRUE:fascsurgeongrp[12.9,18.3)","mma2008.impactTRUE:fascsurgeongrp[18.3,25.5)" ,"mma2008.impactTRUE:fascsurgeongrp[25.5, Inf]")]=0
  }else {
    betaMat[,c("mma2008.impactTRUE")]=0
  }

  colnames(betaMat)
  colnames(policy.dataMatrix)
  policyData=dataForFitting
  policyData[,'mma2008.impact']=(qtrSince2006Vec[i]>=9)
  #change spline (change qtr since 2006)
  policyData[,bsVns] = matrix(bslist$bsdata[i,bsVns],nrow=1)[rep(1,nrow(policyData)),]
  #change qtr (1,2,3,4)
  policyData[,'qtr']=factor(get4quarters(qtrSince2006Vec[i]),levels=levels(anaDfwithBs[,'qtr']))
  policy.dataMatrix=model.matrix(formula(fit),policyData)
  xbOut=xb.matched.yz(betaMat, policy.dataMatrix)
  xbOut$unmatched.b
  str(xbOut)
  if (outcome=='rate'){ predOutcome=exp(xbOut$xb.matched)
                        outmat.noMma[i,]=apply(predOutcome,2,mean)
  }
  if (outcome=='totexp') { 
    if (logOperation){predOutcome=exp(xbOut$xb.matched)} else{predOutcome=xbOut$xb.matched}
    outmat.noMma[i,]=apply(predOutcome,2,sum)/24 
  } 
}



#has MMA, same penetration as in data
for(i in 1:length(qtrSince2006Vec)){
  print(i)
  set.seed(1)
  betaMat=mvrnorm(n=n.sim, mu=coef(fit)[colnames(vcov(fit))], Sigma=vcov(fit))
  policyData=dataForFitting
 
  policyData[,'mma2008.impact']=(qtrSince2006Vec[i]>=9)

  #change spline
  policyData[,bsVns] = matrix(bslist$bsdata[i,bsVns],nrow=1)[rep(1,nrow(policyData)),]
  #change qtr (1,2,3,4)
  policyData[,'qtr']=factor(get4quarters(qtrSince2006Vec[i]),levels=levels(anaDfwithBs[,'qtr']))
  policy.dataMatrix=model.matrix(formula(fit),policyData)
  colnames(policy.dataMatrix)
  xbOut=xb.matched.yz(betaMat, policy.dataMatrix)
  
#   if (link=='exp'){ predOutcome=exp(xbOut$xb.matched)}
#   if (link=='NA') { predOutcome=xbOut$xb.matched}
#   outmat.mma.dataFascPene[i,]=apply(predOutcome,2,mean)
  
  if (outcome=='rate'){ predOutcome=exp(xbOut$xb.matched)
                        outmat.mma.dataFascPene[i,]=apply(predOutcome,2,mean)
  }
  if (outcome=='totexp') { 
    if (logOperation){predOutcome=exp(xbOut$xb.matched)} else{predOutcome=xbOut$xb.matched}
    outmat.mma.dataFascPene[i,]=apply(predOutcome,2,sum)/24 
  } 
  
  
}

#has mma change FASC penetration to bucket 1
for(i in 1:length(qtrSince2006Vec)){
  #set MMA effect to zero (no MMA)
  print(i)
  set.seed(1)
  betaMat=mvrnorm(n=n.sim, mu=coef(fit)[colnames(vcov(fit))], Sigma=vcov(fit))
  
  policyData=dataForFitting
  policyData[,'fascsurgeongrp']=factor(x=levels(policyData[,'fascsurgeongrp'])[1],levels=levels(policyData[,'fascsurgeongrp']))
  policyData[,'mma2008.impact']=(qtrSince2006Vec[i]>=9)
  #change spline
  policyData[,bsVns] = matrix(bslist$bsdata[i,bsVns],nrow=1)[rep(1,nrow(policyData)),]
  #change qtr (1,2,3,4)
  policyData[,'qtr']=factor(get4quarters(qtrSince2006Vec[i]),levels=levels(anaDfwithBs[,'qtr']))
  policy.dataMatrix=model.matrix(formula(fit),policyData)
  xbOut=xb.matched.yz(betaMat, policy.dataMatrix)
#   if (link=='exp'){ predOutcome=exp(xbOut$xb.matched)}
#   if (link=='NA') { predOutcome=xbOut$xb.matched}
#   outmat.mma.grp1[i,]=apply(predOutcome,2,mean)
  
  if (outcome=='rate'){ predOutcome=exp(xbOut$xb.matched)
                        outmat.mma.grp1[i,]=apply(predOutcome,2,mean)
  }
  if (outcome=='totexp') { 
    if (logOperation){predOutcome=exp(xbOut$xb.matched)} else{predOutcome=xbOut$xb.matched}
    outmat.mma.grp1[i,]=apply(predOutcome,2,sum)/24 
  } 
}

#has mma change penetration to bucket 2
for(i in 1:length(qtrSince2006Vec)){
  print(i)
  set.seed(1)
  betaMat=mvrnorm(n=n.sim, mu=coef(fit)[colnames(vcov(fit))], Sigma=vcov(fit))
  policyData=dataForFitting
  policyData[,'fascsurgeongrp']=factor(x=levels(policyData[,'fascsurgeongrp'])[2],levels=levels(policyData[,'fascsurgeongrp']))
  policyData[,'mma2008.impact']=(qtrSince2006Vec[i]>=9)
  #change spline
  policyData[,bsVns] = matrix(bslist$bsdata[i,bsVns],nrow=1)[rep(1,nrow(policyData)),]
  #change qtr (1,2,3,4)
  policyData[,'qtr']=factor(get4quarters(qtrSince2006Vec[i]),levels=levels(anaDfwithBs[,'qtr']))
  policy.dataMatrix=model.matrix(formula(fit),policyData)
  xbOut=xb.matched.yz(betaMat, policy.dataMatrix)
#   if (link=='exp'){ predOutcome=exp(xbOut$xb.matched)}
#   if (link=='NA') { predOutcome=xbOut$xb.matched}
#   outmat.mma.grp2[i,]=apply(predOutcome,2,mean)
  
  if (outcome=='rate'){ predOutcome=exp(xbOut$xb.matched)
                        outmat.mma.grp2[i,]=apply(predOutcome,2,mean)
  }
  if (outcome=='totexp') { 
    if (logOperation){predOutcome=exp(xbOut$xb.matched)} else{predOutcome=xbOut$xb.matched}
    outmat.mma.grp2[i,]=apply(predOutcome,2,sum)/24 
  } 
  
}
#has mma change to bueckt 3
for(i in 1:length(qtrSince2006Vec)){
  print(i)
  set.seed(1)
  betaMat=mvrnorm(n=n.sim, mu=coef(fit)[colnames(vcov(fit))], Sigma=vcov(fit))
  policyData=dataForFitting
  policyData[,'fascsurgeongrp']=factor(x=levels(policyData[,'fascsurgeongrp'])[3],levels=levels(policyData[,'fascsurgeongrp']))
  policyData[,'mma2008.impact']=(qtrSince2006Vec[i]>=9)
  #change spline
  policyData[,bsVns] = matrix(bslist$bsdata[i,bsVns],nrow=1)[rep(1,nrow(policyData)),]
  #change qtr (1,2,3,4)
  policyData[,'qtr']=factor(get4quarters(qtrSince2006Vec[i]),levels=levels(anaDfwithBs[,'qtr']))
  policy.dataMatrix=model.matrix(formula(fit),policyData)
  xbOut=xb.matched.yz(betaMat, policy.dataMatrix)
#   if (link=='exp'){ predOutcome=exp(xbOut$xb.matched)}
#   if (link=='NA') { predOutcome=xbOut$xb.matched}
#   outmat.mma.grp3[i,]=apply(predOutcome,2,mean)
  
  if (outcome=='rate'){ predOutcome=exp(xbOut$xb.matched)
                        outmat.mma.grp3[i,]=apply(predOutcome,2,mean)
  }
  if (outcome=='totexp') { 
    if (logOperation){predOutcome=exp(xbOut$xb.matched)} else{predOutcome=xbOut$xb.matched}
    outmat.mma.grp3[i,]=apply(predOutcome,2,sum)/24 
  } 
  
}
#has mma change to bucket 4

for(i in 1:length(qtrSince2006Vec)){
  print(i)
  set.seed(1)
  betaMat=mvrnorm(n=n.sim, mu=coef(fit)[colnames(vcov(fit))], Sigma=vcov(fit))
  policyData=dataForFitting
  policyData[,'fascsurgeongrp']=factor(x=levels(policyData[,'fascsurgeongrp'])[4],levels=levels(policyData[,'fascsurgeongrp']))
  policyData[,'mma2008.impact']=(qtrSince2006Vec[i]>=9)
  #change spline
  policyData[,bsVns] = matrix(bslist$bsdata[i,bsVns],nrow=1)[rep(1,nrow(policyData)),]
  #change qtr (1,2,3,4)
  policyData[,'qtr']=factor(get4quarters(qtrSince2006Vec[i]),levels=levels(anaDfwithBs[,'qtr']))
  policy.dataMatrix=model.matrix(formula(fit),policyData)
  xbOut=xb.matched.yz(betaMat, policy.dataMatrix)
#   if (link=='exp'){ predOutcome=exp(xbOut$xb.matched)}
#   if (link=='NA') { predOutcome=xbOut$xb.matched}
#   outmat.mma.grp4[i,]=apply(predOutcome,2,mean)
  
  if (outcome=='rate'){ predOutcome=exp(xbOut$xb.matched)
                        outmat.mma.grp4[i,]=apply(predOutcome,2,mean)
  }
  if (outcome=='totexp') { 
    if (logOperation){predOutcome=exp(xbOut$xb.matched)} else{predOutcome=xbOut$xb.matched}
    outmat.mma.grp4[i,]=apply(predOutcome,2,sum)/24 
  } 
}


outList=list(qtr=qtrSince2006Vec, mma.dataFascPene=outmat.mma.dataFascPene
             , mma.fascGrp1=outmat.mma.grp1
             , mma.fascGrp2=outmat.mma.grp2
             , mma.fascGrp3=outmat.mma.grp3
             , mma.fascGrp4=outmat.mma.grp4
             , noMma.dataFascPene=outmat.noMma)
return(outList)
}
#plotDf.list.percavol=mmaEffectSimu(modelFit.allvol,outcome='rate',mmaFascGrpInter=T)
#plotDf.list.exp.log=mmaEffectSimu(modelFit.allexp.nointer.log, outcome='totexp',mmaFascGrpInter=F,knots.tiles=c(0.2,0.4,0.6,0.8),logOperation=T)


plotDf.list.exp.nolog=mmaEffectSimu(modelFit.allexp.nointer.nolog, outcome='totexp',mmaFascGrpInter=F,knots.tiles=c(0.2,0.4,0.6,0.8))

summary(modelFit.allexp.nointer.nolog)

#plotDf.list=plotDf.list.percavol
#plotDf.list=plotDf.list.exp.log

plotDf.list=plotDf.list.exp.nolog


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

plot(ddply(anaDfwithBs,'.qtr.since2006',function(x){sum(x[,'totexp'])/1e9})[,2]*5,ylim=c(10,20),type='b')
lines()
#pdf('Z:/j_scrdata/ascMMA/result/fig/mmaImpactTotalExp.pdf')
plot(5*noMma.dataFascPene.mean/1e9,type='l',ylim=c(0,20),main='MMA impact on Medicare outpatient expenditure', xlab='quarter since 2006',ylab='billion dollars',col='black',lwd=4)
lines(5*mma.dataFascPene.mean/1e9,col='red',lwd=4)
lines(5*mma.fascGrp1.mean/1e9,col='yellow')
lines(5*mma.fascGrp2.mean/1e9,col='green')
lines(5*mma.fascGrp3.mean/1e9,col='grey')
lines(5*mma.fascGrp4.mean/1e9,col='blue')
lines(ddply(anaDfwithBs,'.qtr.since2006',function(x){sum(x[,'totexp'])/1e9})[,2]*5,col='pink', lwd=3)
legend('bottomleft',col=c('black','red','yellow','green','grey','blue'),legend=c('no MMA','MMA','MMA.fasc.grp1','MMA.fasc.grp2','MMA.fasc.grp3','MMA.fasc.grp4'),lwd=c(2,2,1,1,1,1),cex=0.8)
abline(v=8,lty=3)
#dev.off()





pdf('Z:/j_scrdata/ascMMA/result/fig/mmaImpactTotalOutpatientVol.pdf')
plot(noMma.dataFascPene.mean,type='l',ylim=c(0.32,0.45),main='MMA impact on total outpatinet volumn', xlab='quarter since 2006',col='black',lwd=4)
lines(mma.dataFascPene.mean,col='red',lwd=4)
lines(mma.fascGrp1.mean,col='yellow')
lines(mma.fascGrp2.mean,col='green')
lines(mma.fascGrp3.mean,col='grey')
lines(mma.fascGrp4.mean,col='blue')
lines(ddply(anaDfwithBs,c('.qtr.since2006'),function(x){sum(x[,'totaloutpvol'])/sum(x[,'n.bene'])})[,2],col='pink',lwd=3)
legend('topleft',col=c('black','red','yellow','green','grey','blue'),legend=c('no MMA','MMA','MMA.fasc.grp1','MMA.fasc.grp2','MMA.fasc.grp3','MMA.fasc.grp4'),lwd=c(2,2,1,1,1,1),cex=0.8)
abline(v=8,lty=3)
dev.off()


plot(ddply(anaDfwithBs,'.qtr.since2006',function(x){sum(x[,'totexp'])})[,2],type='b')





#save(modelFit.allvol, mma.mean,nomma.mean,mma.bd, nomma.bd,file='Z:/j_scrdata/ascMMA/result/policySimulation05172014.RData')