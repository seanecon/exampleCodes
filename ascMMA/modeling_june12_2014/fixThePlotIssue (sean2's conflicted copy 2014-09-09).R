
addBs=function(knots.tiles, anaDfNoBs,Boundary.knots){
  
  
  bslist=bsWrapper1.yz(anaDfNoBs[,'.qtr.since2006'] #this is the x in bs function
                       , quantile(anaDfNoBs[,'.qtr.since2006'],probs=knots.tiles) #inner knots, if it contains boundary knots, function will stop and issue erro message
                       , paste('.qtr.since2006','.bs',sep='') #output data's columen name stem
                       , degree=2
                       , dataType='data.frame'
                       , Boundary.knots=Boundary.knots
                         #quantile(anaDfNoBs[,'.qtr.since2006'],prob=c(0,1))
  )
  n.basis=bslist$n.basis
  anaDfWithBs=cbind(anaDfNoBs,bslist$bsdata)
  
  names(anaDfWithBs)=tolower(names(anaDfWithBs))
  
  return(anaDfWithBs)
}


addTimeTrendVar=function(df,knots.tiles.2004=c(0.2,0.4,0.6,0.8), knots.tiles.2008=c(0.5)){
  names(df)=tolower(names(df))
  
  df[,'.qtr.since2004'] = df[,'.qtr.since2006'] + 8
  df[,'.qtr.since2004.sq'] = df[,'.qtr.since2004']^2
  
  df[,'.qtr.since2008'] = df[,'.qtr.since2006'] - 8
  
  df[,'.qtr.since2008.nonpos']= (df[,'.qtr.since2008']<=0)
  df[,'.qtr.since2008.pos']= (df[,'.qtr.since2008']>0)*df[,'.qtr.since2008']
  df[,'.qtr.since2008.pos.sq']= df[,'.qtr.since2008.pos']^2
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

anaDfDev=addTimeTrendVar(anaDfNoBs)
names(anaDfDev)



unique(subset(anaDfWithBs, year %in% c('2004','2005','2006','2007'))[,'year'])


fit.poly = glm(log(totopexp)~ .qtr.since2004
               #+.qtr.since2004.sq
               #+firstqtraftermma
               +firstyraftermma
               +firstyr.qtr.since2008.pos
               +.qtr.since2008.nonpos
               +.qtr.since2008.pos
               +.qtr.since2008.pos.sq    
               #+.qtr.since2008.pos.bs1+.qtr.since2008.pos.bs2+.qtr.since2008.pos.bs3
               
                        + qtr + mean.age + .avg.comorbsum 
                        + fascsurgeongrpannual
                      
                        , data=
                          anaDfDev
                        #subset(anaDfWithBs, year %in% c('2004','2005','2006','2007'))
)
names(anaDfDev)

names(anaDfDev)

df[,'firstYr.qtr.since2008.pos']=df[,'firstYrAfterMMA']*df[,'.qtr.since2008.pos']
df[,'firstYr.qtr.since2008.pos']=df[,'firstYrAfterMMA']*df[,'.qtr.since2008.pos.sq']
df[,'firstQtr.qtr.since2008.pos']=df[,'firstQtrAfterMMA']*df[,'.qtr.since2008.pos']
df[,'firstQtr.qtr.since2008.pos']
summary(fit.poly)
names(anaDfDev)



plot(ddply(anaDfDev,c('.qtr.since2004'),function(x){log(sum(x[,'totopexp']))}))

#0.02066303/(2*0.00136929)=7.54
summary(fit.poly)


#with MMA


changedBetas=NULL
predVal.mma=btXbWithShock.yz(coef(fit.poly) #a named  usually coef(fit)[colnames(vcov(fit))]
                               , vcov(fit.poly)  #at least colnamed matrix usually vcov(fit)
                               , changedBetas
                               , sd(fit.poly$residuals)  # sd(fit$residuals) this is from the model  stardard error of shock
                               , model.matrix(formula(fit.poly),anaDfDev) #usually it is  model.matrix(formula(fit),data)
                               , 100                   
                               , changedBetasHaveVariation=T
                               #using this will be more accurate when there is nonlinear operation
                               , seed=211 #this seed will be used to draw coef and shock      
                               , sameShockVec=FALSE    #you always set it to FALSE         
                               #if False, then use different shock in each simulation
                               #if you use same shock, it usually is bad. because the same shock would cause consistent underestimation or overestimation
)


predObsData.mma=cbind(anaDfDev[,c('hrrnum','year','.qtr.since2008')],totopexp=anaDfDev[,'totopexp']/1e6,pred.totopexp=apply(exp(predVal.mma),1,mean)/1e6)
predvsobs.mma=ddply(predObsData.mma,c('year','.qtr.since2008'),function(x){sum.obs=sum(x[,'totopexp']); sum.pred=sum(x[,'pred.totopexp']);outvec=c(sum.pred=sum.pred,sum.obs=sum.obs);return(outvec)})

plot(x=predvsobs.mma[,2],y=predvsobs.mma[,'sum.pred'],ylim=c(600,1200),type='b',col='red')
points(x=predvsobs.mma[,2],y=predvsobs.mma[,'sum.obs'],col='green')



#no MMA
changedBetas=c(.qtr.since2008.pos=0, .qtr.since2008.pos.sq=0)
predVal.nomma=btXbWithShock.yz(coef(fit.poly) #a named  usually coef(fit)[colnames(vcov(fit))]
                         , vcov(fit.poly)  #at least colnamed matrix usually vcov(fit)
                         , changedBetas
                         , sd(fit.poly$residuals)  # sd(fit$residuals) this is from the model  stardard error of shock
                         , model.matrix(formula(fit.poly),anaDfDev) #usually it is  model.matrix(formula(fit),data)
                         , 100                   
                         , changedBetasHaveVariation=T
                         #using this will be more accurate when there is nonlinear operation
                         , seed=211 #this seed will be used to draw coef and shock      
                         , sameShockVec=FALSE    #you always set it to FALSE         
                         #if False, then use different shock in each simulation
                         #if you use same shock, it usually is bad. because the same shock would cause consistent underestimation or overestimation
)


predObsData.nomma=cbind(anaDfDev[,c('hrrnum','year','.qtr.since2008')],totopexp=anaDfDev[,'totopexp']/1e6,pred.totopexp=apply(exp(predVal.nomma),1,mean)/1e6)
predvsobs.nomma=ddply(predObsData.nomma,c('year','.qtr.since2008'),function(x){sum.obs=sum(x[,'totopexp']); sum.pred=sum(x[,'pred.totopexp']);outvec=c(sum.pred=sum.pred,sum.obs=sum.obs);return(outvec)})

plot(x=predvsobs.nomma[,2],y=predvsobs.nomma[,'sum.pred'],ylim=c(600,1200),type='b',col='red')
points(x=predvsobs.nomma[,2],y=predvsobs.nomma[,'sum.obs'],col='green')


plot(x=predvsobs.allyears[,2],y=predvsobs.allyears[,4])
#I think there is a different slope
#before 2008 there is a slope after 2008 there is another slope



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
