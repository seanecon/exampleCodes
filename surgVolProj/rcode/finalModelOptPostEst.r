#om dataPrepAndExploreLagBspline.r: I have found the choice of lag=1, knot percentile=75%
#also I found that other surgical volumes and rap volumes have no impact. So I decided to remove them from model

#In this file, my job is to do the final model and then do post-est learning curve extraction
#then do optimization and generate LOS gain through optimization.



 source('C:/Dropbox/K/lapLearningCurve/rcode/rFun/fun.r')


sourceFileOneByOne.yz('C:/Dropbox/K/lapLearningCurve/rcode/rFun/indivFunFolder')

#first generate the data, functions are stored in dataPrepAndExploreLagBSpline.r
anaDf.raw=genAnaDf.lapLearn(100)

# chkMissVars.lag1=c('n.lag1.lap','n.lag1.nonLap','n.lag1.rap','age3cat','race5cat','year','comorbcat','pay1','mdnum1.r')
# chkMissVars.lag2=c('n.lag2.lap','n.lag2.nonLap','n.lag2.rap','age3cat','race5cat','year','comorbcat','pay1','mdnum1.r')
# chkMissVars.lag3=c('n.lag3.lap','n.lag3.nonLap','n.lag3.rap','age3cat','race5cat','year','comorbcat','pay1','mdnum1.r')
# chkMissVars.lag4=c('n.lag4.lap','n.lag4.nonLap','n.lag4.rap','age3cat','race5cat','year','comorbcat','pay1','mdnum1.r')
# chkMissVars.lag5=c('n.lag5.lap','n.lag5.nonLap','n.lag5.rap','age3cat','race5cat','year','comorbcat','pay1','mdnum1.r')
chkMissVars.lag6 = c('n.lag6.lap','n.lag6.nonLap','n.lag6.rap','age3cat','race5cat','year','comorbcat','pay1','mdnum1.r')
nomiss.lag6 = droplevels(cleaned[complete.cases(cleaned[,chkMissVars.lag6]),])

cleaned.obj=finalCleaning(anaDf.raw, 0.75,2)

cleaned=cleaned.obj$df
knots.list=cleaned.obj$knots

bdknots.list=cleaned.obj$bdknots

nomiss.lag6 = droplevels(cleaned[complete.cases(cleaned[,chkMissVars.lag6]),])

#I check there is no need for RAP
modelFit <- clm(los ~  lag1.nlap.bs1+lag1.nlap.bs2+lag1.nlap.bs3+age4cat+race5cat+comorbcat+pay1+year+mdnum1.r, data=nomiss.lag6, link=c('probit'))


test=clm(los ~  1+lag1.nlap.bs1+lag1.nlap.bs2+lag1.nlap.bs3+age4cat+race5cat+comorbcat+pay1+year+mdnum1.r, data=nomiss.lag6, link=c('probit'))

#the following model nonLap has NO impact.
modelFit.nonLap <- clm(los ~  lag1.nlap.bs1+lag1.nlap.bs2+lag1.nlap.bs3+n.lag1.nonLap.5cat+age4cat+race5cat+comorbcat+pay1+year+mdnum1.r, data=nomiss.lag6, link=c('probit'))

#base::save(anaDf.raw, cleaned.obj, nomiss.lag6, modelFit.nonLap, modelFit, file='Z:/j_scrdata/lapLearn/finalModel_may162017.RData')

(load(file='Z:/j_scrdata/lapLearn/finalModel_may162017.RData'))
(extract(modelFit))

texreg(modelFit, single.row=T)
#extract (average) learning curve

newData=nomiss.lag6        
#opRiFit=riop.lag2
#this is the chosen model
#300 is large enought to get confidence interval
n.sims=100
lagvol.vec=seq(min(nomiss.lag6[,'n.lag1.lap']),max(nomiss.lag6[,'n.lag1.lap']),1)
knots.list=cleaned.obj$knots
bdknots.list=cleaned.obj$bdknots
knots=knots.list$lag1
bdknots=bdknots.list$lag1

avgLearningCurveDf = genAverageLearningCurve(
  modelFit
, nomiss.lag6
, lagvol.vec
, n.sims
,'n.lag1.lap' #'n.lag1.lap' should be in newData
,knots
,bdknots
,vnStem='lag1.nlap.bs'
,degree=2 #bspline degree
,ran.seed=1
)


names(nomiss.lag6[, "n.lag1.lap.log"])
#plot(avgLearningCurveDf)

avgLearningCurveMat=avgLearningCurveDf[,2:ncol(avgLearningCurveDf)]
lbUbMat=apply(avgLearningCurveMat,1,function(x){quantile(x,prob=c(0.10,0.9))}) 
avgLearnCurve=cbind(avgLearningCurveDf[,1],apply(avgLearningCurveMat,1,mean),t(lbUbMat)) 
colnames(avgLearnCurve)=c('n.lag.lap','mean.los','lb.los','ub.los')
#plot(nomiss.lag6[,'n.lag2.lap'],nomiss.lag6[,'los'])
library(ggplot2)
# chk=ddply(nomiss.lag6,'n.lag2.lap',function(x){mean(as.numeric(x[,'los']),na.rm=T)})
#save(avgLearningCurveMat , avgLearnCurve,file='Z:/j_scrdata/lapLearn/avgLearnCurve_oct172013.RData')
#save(avgLearningCurveMat , avgLearnCurve,file='Z:/j_scrdata/lapLearn/avgLearnCurve_may102013.RData')
(load(file='Z:/j_scrdata/lapLearn/avgLearnCurve_may102013.RData'))
(load(file='Z:/j_scrdata/lapLearn/avgLearnCurve_oct172013.RData'))


qplot(n.lag.lap, mean.los, data=as.data.frame(avgLearnCurve), xlab='Number of MIRP in past 6 months', ylab='Expected length of stay for a MIRP')+geom_smooth(aes(ymin=lb.los, ymax=ub.los), data=as.data.frame(avgLearnCurve), stat="identity")+opts(axis.line = theme_segment(colour = "black"),panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(),panel.border = theme_blank(),panel.background = theme_blank()) 
      
#18 per quarter, i.e, 18/12 per week, Montie says 1 per week is needed to be good. so close to expert opinion

lag1.df.raw=ddply(nomiss.lag6,'n.lag1.lap', function(x){c(meanlos=mean(as.numeric(x[,'los']),na.rm=T), freq=nrow(x))})
lag1.df=cbind(lag1.df.raw,logLap=log1p(lag1.df.raw[,'n.lag1.lap']))
sortDf.yz(lag1.df,'n.lag1.lap')
fit=loess(meanlos~logLap, data=lag1.df,weights=freq, span=0.9)
plot(fit$x, fit$fitted,type='b',xlab='log volume', ylab='average length of stay', ylim=c(1,2.5))
points(fit$x, fit$y)

plot(avgLearnCurve[,'n.lag.lap'], avgLearnCurve[,'mean.los'],ylim=c(1.2,2.5),type='l')
points(lag1.df[,'n.lag1.lap'],lag1.df[,'meanlos'])



opRiFit=modelFit


inNomissDf=nomiss.lag6
state='IA'
whichLag='lag4'
whichLag='lag1'
lagVol.vec=seq(0,175,1)
whichLag='lag1'


g.list.ia=gen.g.list(nomiss.lag6, 'IA', opRiFit ,seq(0,175,1),'lag4')
g.list.ny=gen.g.list(nomiss.lag6, 'NY', opRiFit ,seq(0,175,1),'lag4')
g.list.md=gen.g.list(nomiss.lag6, 'MD', opRiFit ,seq(0,175,1),'lag4')
plot(g.list.ia[[1]])
lines(g.list.ia[[2]])

g.list.ia
                                        
plot(cumsum(g.list.md[[1]]))
                              
#I could have g.list with MdId, but I forget, so I extract them seaprately
#the first elemetn of mdVec.ia corresponds to the first element of g.list.ia
mdVec.ia=unique(subset(nomiss.lag6, hospst=='IA')[,'mdnum1.r'])
mdVec.md=unique(subset(nomiss.lag6, hospst=='MD')[,'mdnum1.r'])
mdVec.ny=unique(subset(nomiss.lag6, hospst=='NY')[,'mdnum1.r'])


#save(g.list.ia, g.list.md, g.list.ny, file='Z:/j_scrdata/lapLearn/glists.RData') 
                    
#May 14 I used log1p and lag4 to fun model and obtained g.list
#save(mdVec.ia, mdVec.md, mdVec.ny, g.list.ia, g.list.md, g.list.ny, file='Z:/j_scrdata/lapLearn/glists_May142013.RData')

load(file='Z:/j_scrdata/lapLearn/glists_May142013.RData') 
        
G.list.ia=genSteadyState.Glist(g.list.ia)
G.list.md=genSteadyState.Glist(g.list.md)
G.list.ny=genSteadyState.Glist(g.list.ny)


#mdRange is [1:25] and the like
mdRange=seq(1,25) #use these doctor to solve for optimal
sol.1to25.ny=scilpPatAlloc.yz(1121, G.list.ny[mdRange])
#you can change the sequence of selected patients

#now from optimal solution to predicted LOS

#extract mdDf information
mdDf.ia=getMdData(mdVec.ia)
mdDf.md=getMdData(mdVec.md)
mdDf.ny=getMdData(mdVec.ny)

optSOl.to.los(sol, mdRange, interestYear, opRiFit, stateYearData, mdDf)
#mdRange is the range of MD picked up solution. This is to get around speed problem

interestState='MD'
interestYear='2004'
mdDf=mdDf.md
G.list=G.list.md
mdRange=seq(1,25)
data=nomiss.lag6
G.list[[21]][44]
plot(g.list.md[[21]])


pointEsLosDrop('NY','2004',G.list.ny, mdDf.ny,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('NY','2005',G.list.ny, mdDf.ny,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('NY','2006',G.list.ny, mdDf.ny,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('NY','2007',G.list.ny, mdDf.ny,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('NY','2008',G.list.ny, mdDf.ny,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('NY','2009',G.list.ny, mdDf.ny,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('NY','2010',G.list.ny, mdDf.ny,seq(1,25),nomiss.lag6, opRiFit)


pointEsLosDrop('MD','2004',G.list.md, mdDf.md,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('MD','2005',G.list.md, mdDf.md,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('MD','2006',G.list.md, mdDf.md,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('MD','2007',G.list.md, mdDf.md,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('MD','2008',G.list.md, mdDf.md,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('MD','2009',G.list.md, mdDf.md,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('MD','2010',G.list.md, mdDf.md,seq(1,25),nomiss.lag6, opRiFit)

pointEsLosDrop('IA','2004',G.list.ia, mdDf.ia,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('IA','2005',G.list.ia, mdDf.ia,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('IA','2006',G.list.ia, mdDf.ia,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('IA','2007',G.list.ia, mdDf.ia,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('IA','2008',G.list.ia, mdDf.ia,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('IA','2009',G.list.ia, mdDf.ia,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('IA','2010',G.list.ia, mdDf.ia,seq(1,25),nomiss.lag6, opRiFit)



plot(g.list.md.chgzero[[1]])
ndoc.list=list()
for(n.patients in seq(30,500,1)){
opt.n=scilpPatAlloc.yz(n.patients, G.list.ny[1:10], allTreated=TRUE)$opt.n
ndoc=length(which(opt.n>0))
ndoc.list=lappend.yz(ndoc.list,ndoc)
}

                                         
plot(cbind(seq(30,500,1),unlist(ndoc.list)),type='s',ylim=c(0,12), ylab="number of MIRP surgeons", xlab='number of MIRP in two quarters')
abline(h=10) #averagely 10 surgical taking care pateints per 2 quarters
abline(v=100)                                  
optSolution2Los=function(optSolution, fit){
  return(expLos)
}


                                
                                        
                                        
                                        
#next I want to get learning curve and get a confidence interval for each states
                                        lagVol.vec=seq(0,5,1) #max is 95


density.yz(nomiss.2[,'n.lag2.lap'])
#ia learning curve
ia.data=subset(nomiss.2.le7, hospst=="IA")
ia.docs=unique(ia.data[,'mdnum1.r'])
#IA
ia.g.list=list()
for(i in 1:length(ia.docs)){
  
  ia.g.list= lappend.yz(ia.g.list, genIndivLearnCurve(opRiFit, ia.docs[i], ia.data, lagVol.vec))
  
}
ia.curve=cbind(lagVol.vec,apply(do.call(rbind,ia.g.list),2,mean))


#md learning curve
md.data=subset(nomiss.2.le7, hospst=="MD")
md.docs=unique(md.data[,'mdnum1.r'])
#IA
md.g.list=list()
for(i in 1:length(md.docs)){
  
  md.g.list= lappend.yz(md.g.list, genIndivLearnCurve(opRiFit, md.docs[i], md.data, lagVol.vec))
  
}
md.curve=cbind(lagVol.vec, apply(do.call(rbind,md.g.list),2,mean))

#NY
ny.data=subset(nomiss.2.le7, hospst=="NY")
ny.docs=unique(ny.data[,'mdnum1.r'])
#IA
ny.g.list=list()
for(i in 1:length(ny.docs)){
  
  ny.g.list= lappend.yz(ny.g.list, genIndivLearnCurve(opRiFit, ny.docs[i], ny.data, lagVol.vec))
  
}
ny.curve=cbind(lagVol.vec, apply(do.call(rbind,ny.g.list),2,mean))



plot(md.curve,ylim=c(1.7,2.2))
lines(ia.curve)
lines(ny.curve)

which.min(ny.curve[,2])
which.min(ia.curve[,2])
which.min(md.curve[,2])
lagVol.vec[39]
#still 39, so there is no way, I can get a different ....

#I think I need to add state fixed effect later, it optimal volume would be different across states and it is easier to write it.



#next  I can then run the optimizaton problem to find the solution.






