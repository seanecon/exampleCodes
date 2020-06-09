#we decided to use per capita spending to predict total spending. so this file is useless now
ddply(anaDfDev,'fascsurgeongrpannual',function(x){mean(x[,'n.bene'])})

addTimeTrendVar=function(df,knots.tiles.2004=
                           #c(0.2,0.4,0.6,0.8)
                           0.5
                         , knots.tiles.2008=c(0.5)){
  names(df)=tolower(names(df))
  
  df[,'.qtr.since2004'] = df[,'.qtr.since2006'] + 8
  df[,'.qtr.since2004.sq'] = df[,'.qtr.since2004']^2
  df[,'.qtr.since2004.sqrt'] = df[,'.qtr.since2004']^0.5
  
  df[,'.qtr.since2008'] = df[,'.qtr.since2006'] - 8
  
  df[,'.qtr.since2008.nonpos']= (df[,'.qtr.since2008']<=0)
  df[,'.qtr.since2008.pos']= (df[,'.qtr.since2008']>0)*df[,'.qtr.since2008']
  df[,'.qtr.since2008.pos.sq']= df[,'.qtr.since2008.pos']^2
  df[,'.qtr.since2008.pos.cu']= df[,'.qtr.since2008.pos']^3
  df[,'.qtr.since2008.pos.sqrt']= df[,'.qtr.since2008.pos']^0.5
  #df[,'.qtr.since2008.pos.sqrt']= (df[,'.qtr.since2008']>0)*sqrt(df[,'.qtr.since2008'])
  
  df[,'firstQtrAfterMMA']=0
  df[which(df[,'.qtr.since2006']==9),'firstQtrAfterMMA']=1
  
  df[,'firstYrAfterMMA']=0
  df[which(df[,'.qtr.since2006']==9),'firstYrAfterMMA']=1
  
  df[,'firstYr.qtr.since2008.pos']=df[,'firstYrAfterMMA']*df[,'.qtr.since2008.pos']
  df[,'firstYr.qtr.since2008.pos']=df[,'firstYrAfterMMA']*df[,'.qtr.since2008.pos.sq']
  df[,'firstQtr.qtr.since2008.pos']=df[,'firstQtrAfterMMA']*df[,'.qtr.since2008.pos']
  df[,'firstQtr.qtr.since2008.pos']=df[,'firstQtrAfterMMA']*df[,'.qtr.since2008.pos.sq']
  
  #how about use bs spline to capture nonlinear trend
  
  #since 2004 bs
  bslist.04=bsWrapper1.yz(df[,'.qtr.since2004'] #this is the x in bs function
                          , quantile(df[,'.qtr.since2004'],probs=knots.tiles.2004) #inner knots, if it contains boundary knots, function will stop and issue erro message
                          , paste('.qtr.since2004','.bs',sep='') #output data's columen name stem
                          , degree=2
                          , dataType='data.frame'
                          , Boundary.knots=quantile(df[,'.qtr.since2004'],prob=c(0,1))
  )
  n.basis.04=bslist.04$n.basis
  df=cbind(df,bslist.04$bsdata)
  
  
  #since 2008 bs
  bslist.08=bsWrapper1.yz(df[,'.qtr.since2008.pos'] #this is the x in bs function
                          , quantile(df[which(df[,'.qtr.since2008.pos']>0),'.qtr.since2008.pos'],probs=knots.tiles.2008) #inner knots, if it contains boundary knots, function will stop and issue erro message
                          , paste('.qtr.since2008.pos','.bs',sep='') #output data's columen name stem
                          , degree=2
                          , dataType='data.frame'
                          , Boundary.knots=quantile(df[,'.qtr.since2008.pos'],prob=c(0,1))
  )
  n.basis.08=bslist.08$n.basis
  df=cbind(df,bslist.08$bsdata)
  
  names(df)=tolower(names(df))
  
  return(df)
  
}

(load(file='Z:/j_scrdata/ascMMA/result/anaDfNoBs.Aug082014.RData'))

#the following is an ad-hoc correction
anaDfNoBs[,'per.outp.entday.exp']=anaDfNoBs[,'percap.totOpExp']/anaDfNoBs[,'percap.outpVol']

anaDfDev=addTimeTrendVar(anaDfNoBs)
names(anaDfDev)

unique(subset(anaDfWithBs, year %in% c('2004','2005','2006','2007'))[,'year'])
names(anaDfDev)

anaDfDev=cbind(anaDfDev,dummyEffectCoding.yz(anaDfDev,'fascsurgeongrpannual',levelList=list(levels(anaDfDev[,'fascsurgeongrpannual'])),typeVec='dummy',returnList=TRUE,codedVnList=list(c('mid','high'))))
names(anaDfDev)


names(anaDfDev)
anaDfDev.till2011=subset(anaDfDev, year %in% seq(2004,2011))
count(anaDfDev.till2011,'year')

fit.poly = glm(log(totopexp)~ 
                 qtr + mean.age + .avg.comorbsum 
              #+race.0 #0 is unknown
               + race.1 #1 is white, good using race 1 is enough and adding others is not good
               #+race.2 +race.3+race.4
               #+ race.5
              +sex.1
               #+race.6
               #+sex.2
               #+ fascsurgeongrpannual
               +fascsurgeongrpannual.mid
               +fascsurgeongrpannual.high
               +  .qtr.since2004
               +.qtr.since2004.sqrt  
               #not good
               #+.qtr.since2004.sq #you really needs this term. without this sqrt term it will not look good
               #once this terms is marginal signficant (actully it is 0.05), you can justify the linear term utilization
               #+.qtr.since2004.bs1+.qtr.since2004.bs2+ .qtr.since2004.bs3 #not good
               #+.qtr.since2004.bs4+.qtr.since2004.bs5+.qtr.since2004.bs6 
               
               # +.qtr.since2004.sq #this will cause really back noMMAprediction for year after 2008
               #+firstqtraftermma
               #+firstyraftermma
               #+firstyr.qtr.since2008.pos
               +.qtr.since2008.nonpos
               +.qtr.since2008.pos
               #+.qtr.since2008.pos.sqrt
               +.qtr.since2008.pos.sq
               #                +.qtr.since2008.pos.cu
               #    +.qtr.since2008.pos.bs1+.qtr.since2008.pos.bs2+.qtr.since2008.pos.bs3
               
               
               , data=
                 anaDfDev.till2011
               #subset(anaDfWithBs, year %in% c('2004','2005','2006','2007'))
)
summary(fit.poly)

names(anaDfDev.till2011)

#no MMA
#before removing 2012 I used .qtr.since2008.pos and .qtr.since2008.pos.sq and .qtr.since2004 and .qtr.since2004.sqrt

changedBetas=c(.qtr.since2008.pos=0
               ,.qtr.since2008.pos.sq=0
               #,.qtr.since2008.pos.cu=0
)
# changedBetas=c(.qtr.since2008.pos=0
#                ,.qtr.since2008.pos.sqrt=0
#                #,.qtr.since2008.pos.cu=0
# )


locList=valLoc.yz(anaDfDev.till2011$'.qtr.since2008')
predVal.nomma=btXbWithShock.yz(coef(fit.poly) #a named  usually coef(fit)[colnames(vcov(fit))]
                               , vcov(fit.poly)  #at least colnamed matrix usually vcov(fit)
                               , changedBetas
                               , sd(fit.poly$residuals)  # sd(fit$residuals) this is from the model  stardard error of shock
                               , model.matrix(formula(fit.poly),anaDfDev.till2011) #usually it is  model.matrix(formula(fit),data)
                               , 100                   
                               , changedBetasHaveVariation=T
                               #using this will be more accurate when there is nonlinear operation
                               , seed=211 #this seed will be used to draw coef and shock      
                               , sameShockVec=FALSE    #you always set it to FALSE         
                               #if False, then use different shock in each simulation
                               #if you use same shock, it usually is bad. because the same shock would cause consistent underestimation or overestimation
)

sumMat.nomma=do.call(rbind,lapply(locList,function(x){apply(exp(predVal.nomma[x,]),2,sum)}))
mean.totopexp.hat.noMma=5*apply(sumMat.nomma,1,mean)/1e9
ci.totopexp.hat.noMma=5*apply(sumMat.nomma,1,function(x){quantile(x,probs=c(0.025,0.975))})/1e9


#with MMA
changedBetas=NULL
predVal.mma=btXbWithShock.yz(coef(fit.poly) #a named  usually coef(fit)[colnames(vcov(fit))]
                             , vcov(fit.poly)  #at least colnamed matrix usually vcov(fit)
                             , changedBetas
                             , sd(fit.poly$residuals)  # sd(fit$residuals) this is from the model  stardard error of shock
                             , model.matrix(formula(fit.poly),anaDfDev.till2011) #usually it is  model.matrix(formula(fit),data)
                             , 100                   
                             , changedBetasHaveVariation=T
                             #using this will be more accurate when there is nonlinear operation
                             , seed=211 #this seed will be used to draw coef and shock      
                             , sameShockVec=FALSE    #you always set it to FALSE         
                             #if False, then use different shock in each simulation
                             #if you use same shock, it usually is bad. because the same shock would cause consistent underestimation or overestimation
)


sumMat.mma=do.call(rbind,lapply(locList,function(x){apply(exp(predVal.mma[x,]),2,sum)}))
mean.totopexp.hat.mma=5*apply(sumMat.mma,1,mean)/1e9
ci.totopexp.hat.mma=5*apply(sumMat.mma,1,function(x){quantile(x,probs=c(0.025,0.975))})/1e9



#with MMA low penetration
anaDfDev.lowfasc.mat=model.matrix(formula(fit.poly),anaDfDev.till2011)
anaDfDev.lowfasc.mat[,'fascsurgeongrpannual.mid']=anaDfDev.lowfasc.mat[,'fascsurgeongrpannual.high']=0
changedBetas=NULL
predVal.mma.lowfasc=btXbWithShock.yz(coef(fit.poly) #a named  usually coef(fit)[colnames(vcov(fit))]
                             , vcov(fit.poly)  #at least colnamed matrix usually vcov(fit)
                             , changedBetas
                             , sd(fit.poly$residuals)  # sd(fit$residuals) this is from the model  stardard error of shock
                             , anaDfDev.lowfasc.mat #usually it is  model.matrix(formula(fit),data)
                             , 100                   
                             , changedBetasHaveVariation=T
                             #using this will be more accurate when there is nonlinear operation
                             , seed=211 #this seed will be used to draw coef and shock      
                             , sameShockVec=F   #you always set it to FALSE         
                             #if False, then use different shock in each simulation
                             #if you use same shock, it usually is bad. because the same shock would cause consistent underestimation or overestimation
)



sumMat.mma.lowfasc=do.call(rbind,lapply(locList,function(x){apply(exp(predVal.mma.lowfasc[x,]),2,sum)}))
mean.totopexp.hat.mma.lowfasc=5*apply(sumMat.mma.lowfasc,1,mean)/1e9
ci.totopexp.hat.mma.lowfasc=5*apply(sumMat.mma.lowfasc,1,function(x){quantile(x,probs=c(0.025,0.975))})/1e9




#with MMA median penetration
anaDfDev.midfasc.mat=model.matrix(formula(fit.poly),anaDfDev.till2011)
anaDfDev.midfasc.mat[,'fascsurgeongrpannual.mid']=1; anaDfDev.midfasc.mat[,'fascsurgeongrpannual.high']=0
changedBetas=NULL
predVal.mma.midfasc=btXbWithShock.yz(coef(fit.poly) #a named  usually coef(fit)[colnames(vcov(fit))]
                                     , vcov(fit.poly)  #at least colnamed matrix usually vcov(fit)
                                     , changedBetas
                                     , sd(fit.poly$residuals)  # sd(fit$residuals) this is from the model  stardard error of shock
                                     , anaDfDev.midfasc.mat #usually it is  model.matrix(formula(fit),data)
                                     , 100                   
                                     , changedBetasHaveVariation=F
                                     #using this will be more accurate when there is nonlinear operation
                                     , seed=211 #this seed will be used to draw coef and shock      
                                     , sameShockVec=FALSE    #you always set it to FALSE         
                                     #if False, then use different shock in each simulation
                                     #if you use same shock, it usually is bad. because the same shock would cause consistent underestimation or overestimation
)

sumMat.mma.midfasc=do.call(rbind,lapply(locList,function(x){apply(exp(predVal.mma.midfasc[x,]),2,sum)}))
mean.totopexp.hat.mma.midfasc=5*apply(sumMat.mma.midfasc,1,mean)/1e9
ci.totopexp.hat.mma.midfasc=5*apply(sumMat.mma.midfasc,1,function(x){quantile(x,probs=c(0.025,0.975))})/1e9


#with MMA high penetration
anaDfDev.highfasc.mat=model.matrix(formula(fit.poly),anaDfDev.till2011)
anaDfDev.highfasc.mat[,'fascsurgeongrpannual.mid']=0; anaDfDev.highfasc.mat[,'fascsurgeongrpannual.high']=1
changedBetas=NULL
predVal.mma.highfasc=btXbWithShock.yz(coef(fit.poly) #a named  usually coef(fit)[colnames(vcov(fit))]
                                     , vcov(fit.poly)  #at least colnamed matrix usually vcov(fit)
                                     , changedBetas
                                     , sd(fit.poly$residuals)  # sd(fit$residuals) this is from the model  stardard error of shock
                                     , anaDfDev.highfasc.mat #usually it is  model.matrix(formula(fit),data)
                                     , 100                   
                                     , changedBetasHaveVariation=F
                                     #using this will be more accurate when there is nonlinear operation
                                     , seed=211 #this seed will be used to draw coef and shock      
                                     , sameShockVec=FALSE    #you always set it to FALSE         
                                     #if False, then use different shock in each simulation
                                     #if you use same shock, it usually is bad. because the same shock would cause consistent underestimation or overestimation
)



sumMat.mma.highfasc=do.call(rbind,lapply(locList,function(x){apply(exp(predVal.mma.highfasc[x,]),2,sum)}))
mean.totopexp.hat.mma.highfasc=5*apply(sumMat.mma.highfasc,1,mean)/1e9
ci.totopexp.hat.mma.highfasc=5*apply(sumMat.mma.highfasc,1,function(x){quantile(x,probs=c(0.025,0.975))})/1e9



totopexp.mma.noMma=list(
  data.frame(as.numeric(names(locList)),mean.totopexp.hat.mma, ci.totopexp.hat.mma[1,],ci.totopexp.hat.mma[2,])
 ,data.frame(as.numeric(names(locList)),mean.totopexp.hat.noMma, ci.totopexp.hat.noMma[1,],ci.totopexp.hat.noMma[2,])
                                  
)


totopexp.mma.byfasc=list(
   data.frame(as.numeric(names(locList)),mean.totopexp.hat.mma.lowfasc, ci.totopexp.hat.mma.lowfasc[1,],ci.totopexp.hat.mma.lowfasc[2,] )         
  ,data.frame(as.numeric(names(locList)),mean.totopexp.hat.mma.midfasc, ci.totopexp.hat.mma.midfasc[1,],ci.totopexp.hat.mma.midfasc[2,] )
  ,data.frame(as.numeric(names(locList)),mean.totopexp.hat.mma.highfasc, ci.totopexp.hat.mma.highfasc[1,],ci.totopexp.hat.mma.highfasc[2,])
)


plotLineCi.yz(totopexp.mma.noMma
              #each element of the list is a data.frame has four columns, the first column x, the second columen is mean.y, third columen is lowerbound.y, the four column is upperbound.y
              ,xlim=c(-15,16)
              ,ylim=c(2,6)
              ,pchVec=c(1,2) #this should be the same lenght as plotdata.list
              ,ciBar=TRUE #if FALSE then there is no CI bar, then it beomes nonerror bar line point
              ,jit.size=0 #to avoid overlapping
              ,ciBarLwd=1 #width of error bar
              ,type='b' #can line 'l' or both 'b'
              ,maintitle=c('total outpatient spending: MMA vs. no MMA') #title of plot
              ,x.label=c('quarter since 2008')
              ,y.label=c('total outpatient spending on surgery (billion dollars)')
              ,legendLoc='bottomright'
              ,legend.txt.vec=c('MMA','no MMA') #the legend text characteri vector
              ,cex.legend.text=1
              ,sfrac=0.002
              ,pdf.path=pdf('Z:/j_scrdata/ascMMA/result/fig/totalOutPatientSpendingMmaNoMma_sep302014_mmaVsNoMma.2011.pdf') #with e.g., C;/abc.pdf
)


plotLineCi.yz(totopexp.mma.byfasc
              #each element of the list is a data.frame has four columns, the first column x, the second columen is mean.y, third columen is lowerbound.y, the four column is upperbound.y
              ,xlim=c(-15,16)
              ,ylim=c(1,9)
              ,pchVec=c(1,2,3) #this should be the same lenght as plotdata.list
              ,ciBar=TRUE #if FALSE then there is no CI bar, then it beomes nonerror bar line point
              ,jit.size=0 #to avoid overlapping
              ,ciBarLwd=1 #width of error bar
              ,type='b' #can line 'l' or both 'b'
              ,maintitle=c('total outpatient spending: MMA by FASC') #title of plot
              ,x.label=c('quarter since 2008')
              ,y.label=c('total outpatient spending on surgery (billion dollars)')
              ,legendLoc='bottomright'
              ,legend.txt.vec=c('MMA low fasc','MMA mid fasc','MMA high fasc') #the legend text characteri vector
              ,cex.legend.text=1
              ,sfrac=0.002
              ,pdf.path=pdf('Z:/j_scrdata/ascMMA/result/fig/totalOutPatientSpendingMmaNoMma_sep302014_mmaByFasc.pdf') #with e.g., C;/abc.pdf
)









                                                                     