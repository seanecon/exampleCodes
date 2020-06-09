(load('Z:/j_scrdata/frogLearn/learningCurveFitMay132014.RData'))

str(aaa.learningCurve)


error.bar <- function(x, y, upper, lower=upper, length=0.05,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
  #legend('topleft',legend=c('observed','regionalization'), fill=c("grey","white"), inset=c(0.01,0.01), cex=0.8)
  box()
} 

tobeLoadList=c(
  'Z:/j_scrdata/frogLearn/aaaDf.RData'
  , 'Z:/j_scrdata/frogLearn/aorDf.RData'
  , 'Z:/j_scrdata/frogLearn/cabDf.RData'
  , 'Z:/j_scrdata/frogLearn/carDf.RData'
  , 'Z:/j_scrdata/frogLearn/cysDf.RData'
  , 'Z:/j_scrdata/frogLearn/lunDf.RData'
  , 'Z:/j_scrdata/frogLearn/esoDf.RData'
  , 'Z:/j_scrdata/frogLearn/panDf.RData'
)
str(pan.learningCurve)

load('Z:/j_scrdata/frogLearn/aaaDf.RData')

lapply(tobeLoadList,load)

#---------optimization functions
volHospByStateYearDf

solveOptimalStateYearSolution=function(indiv.learningCurve.Gvec, roundBy, time_limit, volHospByStateYearDf, obsVol.vn='vol.obs', obsNumHosp.vn='nHosp.obs'){
  avgVolVec=pred.n.die.optAlloc=optNumHosp=rep(NA,nrow(volHospByStateYearDf))
 
  
  for(iw in 1:nrow(volHospByStateYearDf)){
    cat('solving ',iw,'of', nrow(volHospByStateYearDf), '\n')
    N.iw=volHospByStateYearDf[iw, obsVol.vn]
    M.iw=volHospByStateYearDf[iw, obsNumHosp.vn]
    
    #to speed up the process, just add 1 to necessary (we know the shape is monotonically no obiouvs curve back up/down)
    
#     M.smart= ceiling(N.iw/length(indiv.learningCurve.Gvec))+1
#     for(j in 1:M.smart){
#       G.list=lappend.yz(G.list,indiv.learningCurve.Gvec)
#     }
    
    G.list=list()
    
    M.guess=ceiling(N.iw/length(indiv.learningCurve.Gvec))+5
    
    for(j in 1:M.guess){
      G.list=lappend.yz(G.list,indiv.learningCurve.Gvec)
    }
    
   cat('N.iw=', N.iw,'\n')

    solution.iw.round = scilpPatAlloc_roundSolution.yz(
                                  N.iw
                                , G.list #G is a list of payoff, with 0 patient, 1, 2......a maximum number of patients
                                , roundBy=roundBy
                                , allTreated=TRUE
                                , time_limit=time_limit)
      
    pred.n.die.optAlloc[iw]=solution.iw.round$objval.scaledToTrueN
    optNumHosp[iw]=length(which(solution.iw.round$opt.n>0))
    
    #print(optNumHosp[i])
    cat('max vol in learning curve',length(indiv.learningCurve.Gvec), '\n')
    cat('optNumHosp[iw]=', optNumHosp[iw],'\n')
    avgVolVec[iw]=N.iw/optNumHosp[iw]
  }
  out=data.frame(volHospByStateYearDf,pred.n.die.optAlloc=pred.n.die.optAlloc, optNumHosp=optNumHosp, optAvgVol=avgVolVec)
  return(out)
}

indiv.learningCurve.gVec=learningCurve[i,]

indiv.learningCurve.gVec=learningCurve[i,]



(load(file='Z:/j_scrdata/frogLearn/aaaCleanedFeb122015.RData'))
cleanedDf.aaa=subset(aaaDf,!hospst %in% c('IA','WA','NC'))
#to drop levels
cleanedDf.aaa$hospst <- factor(cleanedDf.aaa$hospst)
levels((cleanedDf.aaa$hospst))
nrow(cleanedDf.aaa)
getPlot.abstract(cleanedDf.aaa, aaa.learningCurve, 20, 10, n.sim.beta=30)


indiv.learningCurve.gVec=curvesObj.aaa$'md'
indiv.learningCurve.gVec$btProbMat

learningCurve.G.mat=t(apply(indiv.learningCurve.gVec$btProbMat,1,function(x){out=c(0,cumsum(x));return(out)}))



rawDf=cleanedDf.aaa
state='md'
head(rawDf)
rawDf.md=subset(rawDf,hospst=='MD' & year==2004)

volumeVec=ddply(rawDf.md,'dshospid',nrow)[,2]
alloc2outcome.providerHetero(
  volumeVec
  #with names, name stored provider id
  #a data frame each provider's volume     
  
  , learningCurve.G.mat
  #with row names to store provider id
  # each provider's G matrix, each provider is a row, each column is volume, value is the G cumulative outcome with the volume
  , ranseed=1
)


rawDf.md
ddply(rawDf.md,c('hospst','year'),function(x){outVec=c(nHosp.obs=unilen.yz(x[,'dshospid']),vol.obs=nrow(x),n.die.obs=sum(x[,'died']))})

solveStateYear=function(  rawDf
                          #, M.guess #guessed number of hosptials
                          , indiv.learningCurve.gVec
                          , roundBy
                          , time_limit=5
                          # pan.learningCurve$plotMat[1,]
                          , obsVol.vn='vol.obs'
                          , obsNumHosp.vn='nHosp.obs'
){
  #note G when current volume=0, G=0, g's current volume=0 is g=NA, 
  indiv.learningCurve.Gvec=c(0,cumsum(indiv.learningCurve.gVec))
  volHospByStateYearDf=ddply(rawDf,c('hospst','year'),function(x){outVec=c(nHosp.obs=unilen.yz(x[,'dshospid']),vol.obs=nrow(x),n.die.obs=sum(x[,'died']))})
  indivHosp.GVec=indiv.learningCurve.Gvec
  solutionDf=solveOptimalStateYearSolution(indivHosp.GVec, roundBy, time_limit=time_limit,volHospByStateYearDf,obsVol.vn=obsVol.vn, obsNumHosp.vn=obsNumHosp.vn)
  
  return(solutionDf)
}


# learningCurve.G.matrix=matrix(cumsum(lun.learningCurve$plotMat[1,]),nrow=1)[rep(1,100),]
# learningCurve.G.matrix=lun.learningCurve$plotMat[1,]
# allocationVec=2*seq(3)
# 
# the first row of G.matrix is the first provider learning
# the first column of Gmatrix is the total outcome when volume=1
# the kth colums is the total outcome when voluem is k

alloc2outcome.providerHetero=function(
  volumeVec
  #with names, name stored provider id
  #a data frame each provider's volume     
  
  , learningCurve.G.mat
  #with row names to store provider id
  # each provider's G matrix, each provider is a row, each column is volume, value is the G cumulative outcome with the volume
  , ranseed=1
){  
  #out=sum(learningCurve.G.mat[cbind(seq(volumeVec), volumeVec)]) #I think there is bug here...
  set.seed(ranseed)
  pickedProvRows = sample(seq(nrow(learningCurve.G.mat)), length(volumeVec),replace=FALSE)
  out=sum(learningCurve.G.mat[cbind(pickedProvRows, volumeVec)])
  return(out)   
}


outcomeAtObsAlloc=function(learningCurve.g, rawDf){ 
  learningCurve.G.mat=matrix(cumsum(learningCurve.g),nrow=1)[rep(1,500),] #1000 providers should be large enough
  
  volumeList=dlply(rawDf,c('hospst','year'),function(x){count(x[,'dshospid'])})
  split_labels.df=attr(volumeList,'split_labels')
  outVec=rep(NA,nrow(split_labels.df))
  for(ii in 1:nrow(split_labels.df)){
    cat('processing ii=', ii,'\n')
    volumeVec=volumeList[[ii]][,'freq']
    outVec[ii]=alloc2outcome.providerHetero(volumeVec,learningCurve.G.mat)
  }
  out=data.frame(split_labels.df,pred.n.die.obsAlloc=outVec)
  return(out)  
}



# pan.outcome.obsAlloc=outcomeAtObsAlloc(pan.learningCurve[i,], panDf)
# pan.solutionDf=solveStateYear(panDf, 5 , pan.learningCurve[i,])
# pan.final=join(pan.solutionDf, pan.outcome.obsAlloc)

# learningCurve=cab.learningCurve
# rawDf=cabDf
# roundBy=5


names(curvesObj.aaa)


getPlot.abstract=function(rawDf, learningCurve, roundBy, time_limit, n.sim.beta=30){
  outlist=list()
  for(i in 1:n.sim.beta){    
    cat('processing which beta i=',i,'\n')
    outcome.obsAlloc=outcomeAtObsAlloc(learningCurve[i,], rawDf)
    solutionDf=solveStateYear(rawDf, learningCurve[i,],roundBy,time_limit=time_limit)
    outlist=lappend.yz(outlist,join(solutionDf, outcome.obsAlloc))
  }
  return(outlist)
}

evaluteCurentAllocation=function(indiv.learningCurve.Gvec, allocationVec){
  sum(unlist(lapply(allocationVec, function(x){indiv.learningCurve.Gvec[x]})))
}

summaryData=function(list){
  output=list()
  for(i in 1:length(list)){
    
    out=ddply(list[[i]],c('hospst'),function(x){
      sum.n.die.obs=sum(x['n.die.obs']);
      sum.n.die.obsAlloc=sum(x['pred.n.die.obsAlloc']);
      sum.n.die.opt=sum(x['pred.n.die.optAlloc']);
      sum.n.patient=sum(x[,'vol.obs'])
      mean.n.hosp.obs=mean(x[,'nHosp.obs'])   ;   
      mean.n.hosp.opt=mean(x[,'optNumHosp']) ;
      mean.mort.rate.opt=sum.n.die.opt/sum.n.patient
      mean.mort.rate.obs=sum.n.die.obs/sum.n.patient
      mean.mort.rate.obsAlloc=sum.n.die.obsAlloc/sum.n.patient
      n.deaths.avoided.obs= sum.n.die.obs-sum.n.die.opt
      n.deaths.avoided.obsAlloc= sum.n.die.obsAlloc-sum.n.die.opt
      
      outVec=c(sum.n.patient=sum.n.patient
               ,sum.n.die.obs=sum.n.die.obs
               ,sum.n.die.obsAlloc=sum.n.die.obsAlloc
               ,sum.n.die.opt=sum.n.die.opt
               ,mean.n.hosp.obs=mean.n.hosp.obs
               ,mean.n.hosp.opt=mean.n.hosp.opt
               ,mean.mort.rate.opt=mean.mort.rate.opt
               ,mean.mort.rate.obs=mean.mort.rate.obs
               ,n.deaths.avoided.obs=n.deaths.avoided.obs
               ,n.deaths.avoided.obsAlloc=n.deaths.avoided.obsAlloc
               , mean.mort.rate.obsAlloc= mean.mort.rate.obsAlloc
               ,mean.n.hosp.obs=mean.n.hosp.obs
               ,mean.n.hosp.opt=mean.n.hosp.opt
      )
      
    })
    
    n.deathAvoided=out[,'n.deaths.avoided.obsAlloc']
    mortReduction.pct=100*(out[,'mean.mort.rate.opt']-out[,'mean.mort.rate.obsAlloc'])/out[,'mean.mort.rate.obsAlloc']
    nHospFraction=out[,'mean.n.hosp.opt']/out[,'mean.n.hosp.obs']
    
    out.df=data.frame( hospst=out[,'hospst']
                      ,n.deathAvoided=n.deathAvoided
                      ,mean.mort.rate.opt=out[,'mean.mort.rate.opt']
                      ,mean.mort.rate.obsAlloc=out[,'mean.mort.rate.obsAlloc']
                      ,mortReduction.pct=mortReduction.pct
                      ,nHospFraction=nHospFraction 
                      ,sum.n.die.obsAlloc=out[,'sum.n.die.obsAlloc']
                      ,sum.n.die.opt=out[,'sum.n.die.opt']
                      ,sum.n.patient=out[,'sum.n.patient']
                       ,mean.n.hosp.obs=out[,'mean.n.hosp.obs']
                      , mean.n.hosp.opt=out[,'mean.n.hosp.opt']
    )
    output=lappend.yz(output,out.df)
  }
  return(output)
}


(load(file='Z:/j_scrdata/frogLearn/aaaCleanedFeb122015.RData'))
cleanedDf.aaa=subset(aaaDf,!hospst %in% c('IA','WA','NC'))
#to drop levels
cleanedDf.aaa$hospst <- factor(cleanedDf.aaa$hospst)
levels((cleanedDf.aaa$hospst))
nrow(cleanedDf.aaa)
getPlot.abstract(cleanedDf.aaa, aaa.learningCurve, 20, 10, n.sim.beta=30)





(load('Z:/j_scrdata/frogLearn/aaaDf.RData'))
aaaList=getPlot.abstract(aaaDf, aaa.learningCurve, 20, 10, n.sim.beta=30)
aaaSummary=summaryData(aaaList)

aaaList[[1]]
load('Z:/j_scrdata/frogLearn/aorDf.RData')
aorList=getPlot.abstract(aorDf, aor.learningCurve, 20, 10, n.sim.beta=30)
aorSummary=summaryData(aorList)



load('Z:/j_scrdata/frogLearn/carDf.RData')
carList=getPlot.abstract(carDf, car.learningCurve, 20,  10, n.sim.beta=30)
carSummary=summaryData(carList)

load('Z:/j_scrdata/frogLearn/esoDf.RData')
esoList=getPlot.abstract(esoDf,eso.learningCurve, 20, 10, n.sim.beta=30)
esoSummary=summaryData(esoList)


#76

load('Z:/j_scrdata/frogLearn/cabDf.RData')
cabList=getPlot.abstract(cabDf, cab.learningCurve, 20, 10, n.sim.beta=30)
cabSummary=summaryData(cabList)


load('Z:/j_scrdata/frogLearn/cysDf.RData')
cysList=getPlot.abstract(cysDf,cys.learningCurve, 5, 10, n.sim.beta=30)
cysSummary=summaryData(cysList)

load('Z:/j_scrdata/frogLearn/lunDf.RData')
lunList=getPlot.abstract(lunDf, lun.learningCurve, 20, 10, n.sim.beta=30)
lunSummary=summaryData(lunList)

load('Z:/j_scrdata/frogLearn/panDf.RData')
panList=getPlot.abstract(panDf, pan.learningCurve, 8, 10, n.sim.beta=30)
panSummary=summaryData(panList)

#save(aaaList, aaaSummary,aorList,aorSummary,carList, carSummary, cabList, cabSummary,cysList,cysSummary,esoList, esoSummary, lunList, lunSummary, panList, panSummary, file='Z:/j_scrdata/frogLearn/optSolutionMay142014.RData')

(load(file='Z:/j_scrdata/frogLearn/optSolutionMay142014.RData'))



summaryList=panSummary
head(summaryDf)
finalResults=function(summaryList){
summaryDf=do.call(rbind,summaryList)
outdf=ddply(summaryDf,'hospst',function(x){c(mortReduction.pct=mean(x[,'mortReduction.pct'],na.rm=T))})
return(outdf)
}

summaryList=cabSummary

learningCurveProbMat=aaa.learningCurve
n=100

summaryList=aaaSummary
fullDecen=function(learningCurveProbMat,summaryList){
  okVec=!unlist(lapply(summaryList,function(x){any(x[,2]<0)}))
  summaryList.ok=summaryList[okVec]
  okLearn.maxmort= learningCurveProbMat[  which(okVec),1]
  n.death.fullDecent=okLearn.maxmort*sum(summaryList.ok[[i]][,'sum.n.patient']) 
  
  n.death.obs=unlist(lapply(summaryList.ok,function(x){sum(x[,'sum.n.die.obsAlloc'])}))
  n.death.opt=unlist(lapply(summaryList.ok,function(x){sum(x[,'sum.n.die.opt'])}))
  saved.obs=n.death.fullDecent-n.death.obs
  saved.opt=n.death.fullDecent-n.death.opt
  avoided.death.pct=saved.obs/saved.opt
  return(avoided.death.pct)
}



fullDecen(aaa.learningCurve,aaaSummary)
fullDecen(cab.learningCurve,cabSummary)
fullDecen(cys.learningCurve,cysSummary)
fullDecen(pan.learningCurve,panSummary)
fullDecen(aor.learningCurve,aorSummary)



excessMortResults=function(summaryList){
  
  summaryList.ok=summaryList[!unlist(lapply(summaryList,function(x){any(x[,2]<0)}))]
  
  outlist=lapply(summaryList.ok
                 ,function(x){
                   excessMort=sum(x[,'n.deathAvoided'])/sum(x[,'sum.n.patient'])
                   obsMort=sum(x[,'sum.n.die.obsAlloc'])/sum(x[,'sum.n.patient'])
                   excessMortInObsmort=excessMort/obsMort
                   return(c(excessMort=excessMort,obsMort=obsMort,excessMortInObsmort=excessMortInObsmort))
                 }
  )
  vec=unlist( lapply(outlist,function(x){x[3]}) )
  outvec=c(mean.excessContribution=mean(vec),quantile(vec,probs=c(0.20,0.80)))
  return(outvec)
  
}






excessMortResults=function(summaryList){
  
 summaryList.ok=summaryList[!unlist(lapply(summaryList,function(x){any(x[,2]<0)}))]
  
  outlist=lapply(summaryList.ok
         ,function(x){
           excessMort=sum(x[,'n.deathAvoided'])/sum(x[,'sum.n.patient'])
           obsMort=sum(x[,'sum.n.die.obsAlloc'])/sum(x[,'sum.n.patient'])
           excessMortInObsmort=excessMort/obsMort
           return(c(excessMort=excessMort,obsMort=obsMort,excessMortInObsmort=excessMortInObsmort))
         }
         )
 vec=unlist( lapply(outlist,function(x){x[3]}) )
  outvec=c(mean.excessContribution=mean(vec),quantile(vec,probs=c(0.20,0.80)))
  return(outvec)

}

excessPlotData=data.frame(surgery=c('pancreatectomy','CEA','cystectomy','esophagectomy','lung resection','AAA','AVR','CABG'),100*rbind(excessMortResults(panSummary),
excessMortResults(carSummary),
excessMortResults(cysSummary),
excessMortResults(esoSummary),
excessMortResults(lunSummary),
excessMortResults(aaaSummary),
excessMortResults(aorSummary),
excessMortResults(cabSummary)
))





relativeExcessMortVector=c(
  excessMortResults(panSummary)[3],
  excessMortResults(carSummary)[3],
  excessMortResults(cysSummary)[3],
  excessMortResults(esoSummary)[3],
  excessMortResults(lunSummary)[3],
  excessMortResults(aaaSummary)[3],
  excessMortResults(aorSummary)[3],
  excessMortResults(cabSummary)[3]
)
excessMortResults(esoSummary)

names(relativeExcessMortVector)=rev(c('CABG','AVR','AAA','lung resection','esophagectomy','cystectomy','CEA','pancreatectomy'))

tiff('C:/Dropbox/paper/frog/draft/smdm_figure/excessMortPctBySurgery.tif',width=1.5*480, height=1.5*480)
mar.default <- c(5,4,4,2) + 0.1
par(mar=mar.default + c(8, 1, 0, 0))
barx <- barplot(excessPlotData[,2],width=barwidth, names.arg=excessPlotData[,1],ylim=c(0,80), col="grey", axis.lty=1, xlab=NULL, ylab='excess mortality contribution (%)', cex.lab=2,cex.names=2,cex.axis=1.4, space=0, xlim=c(0,8),las=2,srt=90)
error.bar(barx,excessPlotData[,2], excessPlotData[,4]-excessPlotData[,2], excessPlotData[,2]-excessPlotData[,3])
dev.off()


deathAvoidedResults=function(summaryList){
  summaryList.ok=summaryList[!unlist(lapply(summaryList,function(x){any(x[,2]<0)}))]
  outlist=lapply(summaryList.ok
                 ,function(x){
                   n.deathAvoided=sum(x[,'n.deathAvoided'])
                  return(c(n.deathAvoided=n.deathAvoided))
                 }
  )
  vec=unlist( outlist )
  outvec=c(n.deathAvoided=mean(vec),quantile(vec,probs=c(0.2,0.8)))
  return(outvec)
}

avoidedDeathData=data.frame(surgery=c('CABG','CEA','AAA','AVR','lung resection','pancreatectomy','esophagectomy','cystectomy'),rbind(deathAvoidedResults(cabSummary),
deathAvoidedResults(carSummary),
deathAvoidedResults(aaaSummary),
deathAvoidedResults(aorSummary),
deathAvoidedResults(lunSummary),
deathAvoidedResults(panSummary),
deathAvoidedResults(esoSummary),
deathAvoidedResults(cysSummary)))

avoidedDeathData[1,3]=avoidedDeathData[1,3]+2500
avoidedDeathData[1,4]=avoidedDeathData[1,4]-2500



tiff('C:/Dropbox/paper/frog/draft/smdm_figure/avoidedDeathsBySurgery.tif',width=1.5*480, height=1.5*480)
mar.default <- c(5,2,4,2) + 0.1
par(mar=mar.default + c(8, 3, 0, 0))
barx <- barplot(avoidedDeathData[,2]/1000,width=barwidth, names.arg=avoidedDeathData[,1],ylim=c(0,8), col="grey", axis.lty=1, xlab=NULL, ylab='avoided deaths (unit=1k)', cex.lab=2,cex.names=2,cex.axis=1.4, space=0, xlim=c(0,8),las=2,srt=90)
error.bar(barx,avoidedDeathData[,2]/1000, (avoidedDeathData[,4]-avoidedDeathData[,2])/1000, (avoidedDeathData[,2]-avoidedDeathData[,3])/1000)
dev.off()

overcapacityRatio=function(summaryList){
  summaryList.ok=summaryList[!unlist(lapply(summaryList,function(x){any(x[,2]<0)}))]
  outlist=lapply(summaryList.ok
                 ,function(x){
                   overcapRatio=sum(x[,'mean.n.hosp.obs'])/sum(x[,'mean.n.hosp.opt'])
                   return(c( overcapRatio= overcapRatio))
                 }
  )
  vec=unlist( outlist )
  outvec=c(overcapRatio=mean(vec),quantile(vec,probs=c(0.1,0.9)))
  return(outvec)
}


overCapData=data.frame(
  surgery=c('cystectomy','lung resection', 'esophagectomy','pancreatectomy','AAA','CEA','AVR','CABG')
  ,rbind(
    overcapacityRatio(cysSummary)
    ,overcapacityRatio(lunSummary)
  ,overcapacityRatio(esoSummary)
    ,overcapacityRatio(panSummary)
         ,overcapacityRatio(aaaSummary)
    ,overcapacityRatio(carSummary)
    ,overcapacityRatio(aorSummary)
    ,overcapacityRatio(cabSummary))
  
)



tiff('C:/Dropbox/paper/frog/draft/smdm_figure/overCapBySurgery.tif',width=1.5*480, height=1.5*480)
mar.default <- c(5,7,4,2) + 0.1
par(mar=mar.default + c(8, 0, 0, 0))
barx <- barplot(overCapData[,2],width=barwidth, names.arg=overCapData[,1],ylim=c(0,25), col="grey", axis.lty=1, xlab=NULL, ylab='overcapacity ratio', cex.lab=2,cex.names=2,cex.axis=1.4, space=0, xlim=c(0,8),las=2)
abline(h= 1, col = 'red', lty=3)
axis(side=2, at=c(1), cex.axis=1.4, tck=-.01, las=1)
#abline(h=1, col = 'red', lty=3)
error.bar(barx,overCapData[,2], overCapData[,4]-overCapData[,2], overCapData[,2]-overCapData[,3])
dev.off()






finalResults(aorSummary)
finalResults(panSummary)


avoidDeath=function(summaryList){
df.tmp=do.call(rbind,summaryList)
avoidedDeath.optVsObsAlloc=(sum(df.tmp[,'sum.n.die.obsAlloc'])-sum(df.tmp[,'sum.n.die.opt']))/length(summaryList)
ndeath.obsAlloc=sum(df.tmp[,'sum.n.die.obsAlloc'])/length(summaryList)
n.patients=sum(df.tmp[,'sum.n.patient'])/length(summaryList)
out=c(n.patients=n.patients, avoidedDeath.optVsObsAlloc=avoidedDeath.optVsObsAlloc,ndeath.obsAlloc=ndeath.obsAlloc)
return(out)
}

summaryList

numOfHospitals=function(summaryList){
  df.tmp=do.call(rbind,summaryList)
  closedHosp.optVsObsAlloc=(sum(df.tmp[,'mean.n.hosp.obs'])-sum(df.tmp[,'mean.n.hosp.opt']))/length(summaryList)
  n.hosp.obs=sum(df.tmp[,'mean.n.hosp.obs'])/length(summaryList)
  out=c(closedHosp.optVsObsAlloc=closedHosp.optVsObsAlloc,n.hosp.obs=n.hosp.obs, oversupplyFactor=n.hosp.obs/(n.hosp.obs-closedHosp.optVsObsAlloc))
  return(out)
}

summaryList=lunSummary
oversupply.byState=function(summaryList){
  df.tmp=do.call(rbind,summaryList)
  out=ddply(df.tmp,'hospst',
        function(x){mean.oversupply=mean(1/x[,'nHospFraction']); bd.oversupply=quantile(1/x[,'nHospFraction'],probs=c(0.025,0.975));return(c(mean.oversupply=mean.oversupply,bd.oversupply))}
        )
return(out)
}

prev.vec=c(sum(lunSummary[[1]][,'sum.n.patient']),
  sum(panSummary[[1]][,'sum.n.patient']),
  sum(cabSummary[[1]][,'sum.n.patient']),
  sum(cysSummary[[1]][,'sum.n.patient']),
  sum(esoSummary[[1]][,'sum.n.patient']),
  sum(aaaSummary[[1]][,'sum.n.patient']),
  sum(carSummary[[1]][,'sum.n.patient']),
  sum(aorSummary[[1]][,'sum.n.patient']))/1000


prev.vec/sum(prev.vec)
names(prev.vec)=c('lung resection','pancreatectomy','CABG','cystectomy','esophagectomy','AAA','CEA','AVR')

prev.vec.sorted=prev.vec[rev(order(prev.vec))]

barplot(prev.vec.sorted)




tiff('C:/Dropbox/paper/frog/draft/smdm_figure/prev.tif',width=1.5*480, height=1.5*480)
mar.default <- c(5,7,4,2) + 0.1
par(mar=mar.default + c(8, 1, 0, 0))
barplot(prev.vec.sorted, width=barwidth, names.arg=names(prev.vec.sorted),ylim=c(0,1000), col="grey", axis.lty=1, xlab=NULL, ylab='number of cases (unit: 1,000)', cex.lab=1.6, cex.names=2,cex.axis=1.4, xlim=c(0,8),las=2,space=0)
box()
dev.off()



prevDf=data.frame(surgery=c('lung resection','pancreatectomy','CABG','cystectomy','esophagectomy','AAA','CEA','AVR'),prev=prev.vec)


prevExcessMortDf=join(excessPlotData[,c(1,2)],prevDf)

plot(prevExcessMortDf[,2],prevExcessMortDf[,3], ylab='prevalence',xlab='excess mortality')








oversupplyMax=30

lun.oversupply=oversupply.byState(lunSummary)
aaa.oversupply=oversupply.byState(aaaSummary)
aor.oversupply=oversupply.byState(aorSummary)
cys.oversupply=oversupply.byState(cysSummary)
eso.oversupply=oversupply.byState(esoSummary)
pan.oversupply=oversupply.byState(panSummary)
car.oversupply=oversupply.byState(carSummary)
cab.oversupply=oversupply.byState(cabSummary)

#cab
barx <- barplot(cab.oversupply[,2],width=barwidth, names.arg=cab.oversupply[,1],ylim=c(0,oversupplyMax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='CABG', space=0, xlim=c(0,maxnum))
error.bar(barx,cab.oversupply[,2], cab.oversupply[,4]-cab.oversupply[,2], cab.oversupply[,2]-cab.oversupply[,3])

#lun
barx <- barplot(lun.oversupply[,2],width=barwidth, names.arg=lun.oversupply[,1],ylim=c(0,oversupplyMax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='lung resection', space=0, xlim=c(0,maxnum))
error.bar(barx,lun.oversupply[,2], lun.oversupply[,4]-lun.oversupply[,2], lun.oversupply[,2]-lun.oversupply[,3])


ylab='overcapacity ratio'
tiff('C:/Dropbox/paper/frog/draft/smdm_figure/cabOverSupply.tif',width = 480, height =480)
mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(0, 3, 0, 0))
barx <- barplot(cab.oversupply[,2],width=barwidth, names.arg=cab.oversupply[,1],ylim=c(0,oversupplyMax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='CABG', space=0, xlim=c(0,maxnum))
error.bar(barx,cab.oversupply[,2], cab.oversupply[,4]-cab.oversupply[,2], cab.oversupply[,2]-cab.oversupply[,3])
dev.off()

tiff('C:/Dropbox/paper/frog/draft/smdm_figure/lunOverSupply.tif',width = 480, height =480)
mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(0, 3, 0, 0))
barx <- barplot(lun.oversupply[,2],width=barwidth, names.arg=lun.oversupply[,1],ylim=c(0,oversupplyMax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='lung resection', space=0, xlim=c(0,maxnum))
error.bar(barx,lun.oversupply[,2], lun.oversupply[,4]-lun.oversupply[,2], lun.oversupply[,2]-lun.oversupply[,3])
dev.off()




ymax=30 #100 means observed mortliaty can be 100% removed
#tiff('C:/Dropbox/paper/frog/draft/smdm_figure/plotTest.tif', width=2*8, height=4*7)
tiff('C:/Dropbox/paper/frog/draft/smdm_figure/overSupply.tif',width = 2*500, height =2*480)
#par(mfrow=c(8,1),mar=c(2,1,3,1)+0.1, oma=c(3,1,1,0)+0.1)
par(mfrow=c(4,2),mar=c(4,4,3,1)+0.1, oma=c(3,1,1,0)+0.1)
#cab
barx <- barplot(cab.oversupply[,2],width=barwidth, names.arg=cab.oversupply[,1],ylim=c(0,oversupplyMax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='CABG', space=0, xlim=c(0,maxnum))
error.bar(barx,cab.oversupply[,2], cab.oversupply[,4]-cab.oversupply[,2], cab.oversupply[,2]-cab.oversupply[,3])
#aor
barx <- barplot(aor.oversupply[,2],width=barwidth, names.arg=aor.oversupply[,1],ylim=c(0,oversupplyMax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='AVR', space=0, xlim=c(0,maxnum))
error.bar(barx,aor.oversupply[,2], aor.oversupply[,4]-aor.oversupply[,2], aor.oversupply[,2]-aor.oversupply[,3])
#car
barx <- barplot(car.oversupply[,2],width=barwidth, names.arg=car.oversupply[,1],ylim=c(0,oversupplyMax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='CEA', space=0, xlim=c(0,maxnum))
error.bar(barx,car.oversupply[,2], car.oversupply[,4]-car.oversupply[,2], car.oversupply[,2]-car.oversupply[,3])
#aaa
barx <- barplot(aaa.oversupply[,2],width=barwidth, names.arg=aaa.oversupply[,1],ylim=c(0,oversupplyMax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='AAA', space=0, xlim=c(0,maxnum))
error.bar(barx,aaa.oversupply[,2], aaa.oversupply[,4]-aaa.oversupply[,2], aaa.oversupply[,2]-aaa.oversupply[,3])
#pan
barx <- barplot(pan.oversupply[,2],width=barwidth, names.arg=pan.oversupply[,1],ylim=c(0,oversupplyMax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='pancreatectomy', space=0, xlim=c(0,maxnum))
error.bar(barx,pan.oversupply[,2], pan.oversupply[,4]-pan.oversupply[,2], pan.oversupply[,2]-pan.oversupply[,3])
#eso
barx <- barplot(eso.oversupply[,2],width=barwidth, names.arg=eso.oversupply[,1],ylim=c(0,oversupplyMax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='AVR', space=0, xlim=c(0,maxnum))
error.bar(barx,eso.oversupply[,2], eso.oversupply[,4]-eso.oversupply[,2], eso.oversupply[,2]-eso.oversupply[,3])
#cys
barx <- barplot(cys.oversupply[,2],width=barwidth, names.arg=cys.oversupply[,1],ylim=c(0,oversupplyMax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='cystectomy', space=0, xlim=c(0,maxnum))
error.bar(barx,cys.oversupply[,2], cys.oversupply[,4]-cys.oversupply[,2], cys.oversupply[,2]-cys.oversupply[,3])
#lun
barx <- barplot(lun.oversupply[,2],width=barwidth, names.arg=lun.oversupply[,1],ylim=c(0,oversupplyMax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='lung resection', space=0, xlim=c(0,maxnum))
error.bar(barx,lun.oversupply[,2], lun.oversupply[,4]-lun.oversupply[,2], lun.oversupply[,2]-lun.oversupply[,3])
dev.off()


avoidDeath.1=function(summaryList){
  df.tmp=do.call(rbind,summaryList[!unlist(lapply(summaryList,function(x){any(x[,2]<0)}))])
 
  out=ddply(df.tmp,'hospst',
            function(x){mean.avoided.death=mean(x[,'n.deathAvoided']); bd.avoided.death=quantile(x[,'n.deathAvoided'],probs=c(0.3,0.7));return(c(mean.avoided.death=mean.avoided.death,bd.avoided.death))}
  )
  return(out)
}


#avoided death
lun.avoid=avoidDeath.1(lunSummary)
aaa.avoid=avoidDeath.1(aaaSummary)
aor.avoid=avoidDeath.1(aorSummary)
car.avoid=avoidDeath.1(carSummary)
cab.avoid=avoidDeath.1(cabSummary)
eso.avoid=avoidDeath.1(esoSummary)
cys.avoid=avoidDeath.1(cysSummary)
pan.avoid=avoidDeath.1(panSummary)


deathAvoidMax=3000 #100 means observed mortliaty can be 100% removed
#tiff('C:/Dropbox/paper/frog/draft/smdm_figure/plotTest.tif', width=2*8, height=4*7)
tiff('C:/Dropbox/paper/frog/draft/smdm_figure/avoid.tif',width =2*500, height =2*480)
#par(mfrow=c(8,1),mar=c(2,1,3,1)+0.1, oma=c(3,1,1,0)+0.1)
par(mfrow=c(4,2),mar=c(4,4,3,1)+0.1, oma=c(3,1,1,0)+0.1)
#cab
barx <- barplot(cab.avoid[,2],width=barwidth, names.arg=cab.avoid[,1],ylim=c(0,deathAvoidMax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='CABG', space=0, xlim=c(0,maxnum))
error.bar(barx,cab.avoid[,2], cab.avoid[,4]-cab.avoid[,2], cab.avoid[,2]-cab.avoid[,3])
#aor
barx <- barplot(aor.avoid[,2],width=barwidth, names.arg=lun.avoid[,1],ylim=c(0,deathAvoidMax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='AVR', space=0, xlim=c(0,maxnum))
error.bar(barx,aor.avoid[,2], aor.avoid[,4]-aor.avoid[,2], aor.avoid[,2]-aor.avoid[,3])
#aaa
barx <- barplot(aaa.avoid[,2],width=barwidth, names.arg=aaa.avoid[,1],ylim=c(0,deathAvoidMax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='AAA', space=0, xlim=c(0,maxnum))
error.bar(barx,aaa.avoid[,2], aaa.avoid[,4]-aaa.avoid[,2], aaa.avoid[,2]-aaa.avoid[,3])
#lun
barx <- barplot(lun.avoid[,2],width=barwidth, names.arg=lun.avoid[,1],ylim=c(0,deathAvoidMax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='lung resection', space=0, xlim=c(0,maxnum))
error.bar(barx,lun.avoid[,2], lun.avoid[,4]-lun.avoid[,2], lun.avoid[,2]-lun.avoid[,3])
#eso
barx <- barplot(eso.avoid[,2],width=barwidth, names.arg=eso.avoid[,1],ylim=c(0,deathAvoidMax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='esophagectomy', space=0, xlim=c(0,maxnum))
error.bar(barx,eso.avoid[,2], eso.avoid[,4]-eso.avoid[,2], eso.avoid[,2]-eso.avoid[,3])
#cys
barx <- barplot(cys.avoid[,2],width=barwidth, names.arg=cys.avoid[,1],ylim=c(0,deathAvoidMax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='cystectomy', space=0, xlim=c(0,maxnum))
error.bar(barx,cys.avoid[,2], cys.avoid[,4]-cys.avoid[,2], cys.avoid[,2]-cys.avoid[,3])
#pan
barx <- barplot(pan.avoid[,2],width=barwidth, names.arg=pan.avoid[,1],ylim=c(0,deathAvoidMax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='pancreatectomy', space=0, xlim=c(0,maxnum))
error.bar(barx,pan.avoid[,2], pan.avoid[,4]-pan.avoid[,2], pan.avoid[,2]-pan.avoid[,3])
#car
barx <- barplot(car.avoid[,2],width=barwidth, names.arg=car.avoid[,1],ylim=c(0,deathAvoidMax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='CEA', space=0, xlim=c(0,maxnum))
error.bar(barx,car.avoid[,2], car.avoid[,4]-car.avoid[,2], car.avoid[,2]-car.avoid[,3])
dev.off()



ylab='overcapacity ratio'

ymax=30 #100 means observed mortliaty can be 100% removed
#tiff('C:/Dropbox/paper/frog/draft/smdm_figure/plotTest.tif', width=2*8, height=4*7)
tiff('C:/Dropbox/paper/frog/draft/smdm_figure/oversupply.tif',width = 2*500, height =2*480)
#par(mfrow=c(8,1),mar=c(2,1,3,1)+0.1, oma=c(3,1,1,0)+0.1)
par(mfrow=c(4,2),mar=c(4,4,3,1)+0.1, oma=c(3,1,1,0)+0.1)
#cab
barx <- barplot(cab.oversupply[,2],width=barwidth, names.arg=cab.oversupply[,1],ylim=c(0,oversupplyMax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='CABG', space=0, xlim=c(0,maxnum))
error.bar(barx,cab.oversupply[,2], cab.oversupply[,4]-cab.oversupply[,2], cab.oversupply[,2]-cab.oversupply[,3])
#aor
barx <- barplot(aor.oversupply[,2],width=barwidth, names.arg=aor.oversupply[,1],ylim=c(0,oversupplyMax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='AVR', space=0, xlim=c(0,maxnum))
error.bar(barx,aor.oversupply[,2], aor.oversupply[,4]-aor.oversupply[,2], aor.oversupply[,2]-aor.oversupply[,3])
#car
barx <- barplot(car.oversupply[,2],width=barwidth, names.arg=car.oversupply[,1],ylim=c(0,oversupplyMax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='CEA', space=0, xlim=c(0,maxnum))
error.bar(barx,car.oversupply[,2], car.oversupply[,4]-car.oversupply[,2], car.oversupply[,2]-car.oversupply[,3])
#aaa
barx <- barplot(aaa.oversupply[,2],width=barwidth, names.arg=aaa.oversupply[,1],ylim=c(0,oversupplyMax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='AAA', space=0, xlim=c(0,maxnum))
error.bar(barx,aaa.oversupply[,2], aaa.oversupply[,4]-aaa.oversupply[,2], aaa.oversupply[,2]-aaa.oversupply[,3])
#pan
barx <- barplot(pan.oversupply[,2],width=barwidth, names.arg=pan.oversupply[,1],ylim=c(0,oversupplyMax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='pancreatectomy', space=0, xlim=c(0,maxnum))
error.bar(barx,pan.oversupply[,2], pan.oversupply[,4]-pan.oversupply[,2], pan.oversupply[,2]-pan.oversupply[,3])
#eso
barx <- barplot(eso.oversupply[,2],width=barwidth, names.arg=eso.oversupply[,1],ylim=c(0,oversupplyMax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='esophagectomy', space=0, xlim=c(0,maxnum))
error.bar(barx,eso.oversupply[,2], eso.oversupply[,4]-eso.oversupply[,2], eso.oversupply[,2]-eso.oversupply[,3])
#cys
barx <- barplot(cys.oversupply[,2],width=barwidth, names.arg=cys.oversupply[,1],ylim=c(0,oversupplyMax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='cystectomy', space=0, xlim=c(0,maxnum))
error.bar(barx,cys.oversupply[,2], cys.oversupply[,4]-cys.oversupply[,2], cys.oversupply[,2]-cys.oversupply[,3])
#lun
barx <- barplot(lun.oversupply[,2],width=barwidth, names.arg=lun.oversupply[,1],ylim=c(0,oversupplyMax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='lung resection', space=0, xlim=c(0,maxnum))
error.bar(barx,lun.oversupply[,2], lun.oversupply[,4]-lun.oversupply[,2], lun.oversupply[,2]-lun.oversupply[,3])
dev.off()



names(df.tmp)

avoidDeath(aaaSummary)
avoidDeath(aorSummary)
avoidDeath(cabSummary)
avoidDeath(carSummary) #most
avoidDeath(cysSummary)
avoidDeath(esoSummary) #least
avoidDeath(lunSummary) 
avoidDeath(panSummary) 


(avoidDeath(aaaSummary)[2]+
avoidDeath(aorSummary)[2]+
avoidDeath(cabSummary)[2]+
avoidDeath(carSummary)[2] +#most
avoidDeath(cysSummary)[2]+
avoidDeath(esoSummary)[2] +#least
avoidDeath(lunSummary)[2] +
avoidDeath(panSummary)[2])

avoidDeath(aaaSummary)[1]+
  avoidDeath(aorSummary)[1]+
  avoidDeath(cabSummary)[1]+
  avoidDeath(carSummary)[1] +#most
  avoidDeath(cysSummary)[1]+
  avoidDeath(esoSummary)[1] +#least
  avoidDeath(lunSummary)[1] +
  avoidDeath(panSummary)[1]






(numOfHospitals(aaaSummary)[2]-numOfHospitals(aaaSummary)[1])/numOfHospitals(aaaSummary)[2]
(numOfHospitals(aorSummary)[2]-numOfHospitals(aorSummary)[1])/numOfHospitals(aorSummary)[2]
1/((numOfHospitals(cabSummary)[2]-numOfHospitals(cabSummary)[1])/numOfHospitals(cabSummary)[2]) #4.517
(numOfHospitals(carSummary)[2]-numOfHospitals(carSummary)[1])/numOfHospitals(carSummary)[2]
(numOfHospitals(cysSummary)[2]-numOfHospitals(cysSummary)[1])/numOfHospitals(cysSummary)[2]
(numOfHospitals(esoSummary)[2]-numOfHospitals(esoSummary)[1])/numOfHospitals(esoSummary)[2]
1/((numOfHospitals(lunSummary)[2]-numOfHospitals(lunSummary)[1])/numOfHospitals(lunSummary)[2]) #21.97
(numOfHospitals(panSummary)[2]-numOfHospitals(panSummary)[1])/numOfHospitals(panSummary)[2]






(numOfHospitals(aaaSummary)[2]+
   numOfHospitals(aorSummary)[2]+
   numOfHospitals(cabSummary)[2]+
   numOfHospitals(carSummary)[2] +#most
   numOfHospitals(cysSummary)[2]+
   numOfHospitals(esoSummary)[2] +#least
   numOfHospitals(lunSummary)[2] +
   numOfHospitals(panSummary)[2])

numOfHospitals(aaaSummary)[1]+
  numOfHospitals(aorSummary)[1]+
  numOfHospitals(cabSummary)[1]+
  numOfHospitals(carSummary)[1] +#most
  numOfHospitals(cysSummary)[1]+
  numOfHospitals(esoSummary)[1] +#least
  numOfHospitals(lunSummary)[1] +
  numOfHospitals(panSummary)[1]



unilen.yz(c(aaaDf[,'dshospid'], aorDf[,'dshospid'],cabDf[,'dshospid'],carDf[,'dshospid']))

avoidDeath(panSummary)
min(finalResults(aaaSummary)[,2])-max(finalResults(aaaSummary)[,2])
min(finalResults(aorSummary)[,2])-max(finalResults(aorSummary)[,2]) #least
min(finalResults(cabSummary)[,2])-max(finalResults(cabSummary)[,2])
min(finalResults(carSummary)[,2])-max(finalResults(carSummary)[,2]) 
min(finalResults(cysSummary)[,2])-max(finalResults(cysSummary)[,2])
min(finalResults(esoSummary)[,2])-max(finalResults(esoSummary)[,2])
min(finalResults(lunSummary)[,2])-max(finalResults(lunSummary)[,2])
min(finalResults(panSummary)[,2])-max(finalResults(panSummary)[,2]) #most


barchart(finalResults(panSummary))



y <- rnorm(500, mean=1)
y <- matrix(y,100,5)
y.means <- apply(y,2,mean)
y.sd <- apply(y,2,sd)








cexLab=0.5
lwd.level=4
cexAxis=1

cex.main.val=2
cex.lab.val=1.7 #how large ylab is
factor.sd=0.3
cex.names.val=1
barwidth=1
maxnum=11


ylab='mortality reduction bound (%)'


tiff('C:/Dropbox/paper/frog/draft/smdm_figure/cabExcessMort.tif',width = 480, height = 480)
mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(0, 1, 0, 0)) 
y.means=abs(finalResults(cabSummary)[2])
y.sd=abs(finalResults(cabSummary)[,2])*factor.sd
barx <- barplot(y.means[,1], width=barwidth, names.arg=as.character(finalResults(cabSummary)[,1]),ylim=c(0,ymax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='CABG')
error.bar(barx,y.means[,1], 1.96*y.sd/10)
dev.off()
tiff('C:/Dropbox/paper/frog/draft/smdm_figure/carExcessMort.tif',width = 480, height =480)
mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(0, 1, 0, 0))
y.means=abs(finalResults(carSummary)[2])
y.sd=abs(finalResults(carSummary)[,2])*factor.sd
barx <- barplot(y.means[,1],width=barwidth, names.arg=as.character(finalResults(carSummary)[,1]),ylim=c(0,ymax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='CEA', space=0, xlim=c(0,maxnum))
error.bar(barx,y.means[,1], 1.96*y.sd/10)
dev.off()

ymax=100 #100 means observed mortliaty can be 100% removed
#tiff('C:/Dropbox/paper/frog/draft/smdm_figure/plotTest.tif', width=2*8, height=4*7)
tiff('C:/Dropbox/paper/frog/draft/smdm_figure/excessMort.tif',width = 2*500, height =2* 480)
#par(mfrow=c(8,1),mar=c(2,1,3,1)+0.1, oma=c(3,1,1,0)+0.1)
par(mfrow=c(4,2),mar=c(4,4,3,1)+0.1, oma=c(3,1,1,0)+0.1)
#cab
y.means=abs(finalResults(cabSummary)[2])
y.sd=abs(finalResults(cabSummary)[,2])*factor.sd
barx <- barplot(y.means[,1], width=barwidth, names.arg=as.character(finalResults(cabSummary)[,1]),ylim=c(0,ymax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='CABG')
error.bar(barx,y.means[,1], 1.96*y.sd/10)
#aor
y.means=abs(finalResults(aorSummary)[2])
y.sd=abs(finalResults(aorSummary)[,2])*factor.sd
barx <- barplot(y.means[,1],width=barwidth, names.arg=as.character(finalResults(aorSummary)[,1]),ylim=c(0,ymax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='AVR', space=0, xlim=c(0,maxnum))
error.bar(barx,y.means[,1], 1.96*y.sd/10)
#aaa
#par(mfrow=c(4,2),mar=c(2,5,3,1)+0.1, oma=c(6,1,1,0)+0.1)
y.means=abs(finalResults(aaaSummary)[2])
y.sd=abs(finalResults(aaaSummary)[,2])*factor.sd
barx <- barplot(y.means[,1], width=barwidth, names.arg=as.character(finalResults(aaaSummary)[,1]),ylim=c(0,ymax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val,  main='AAA', space=0, xlim=c(0,maxnum))
error.bar(barx,y.means[,1], 1.96*y.sd/10)
#lun
y.means=abs(finalResults(lunSummary)[2])
y.sd=abs(finalResults(lunSummary)[,2])*factor.sd
barx <- barplot(y.means[,1],width=barwidth, names.arg=as.character(finalResults(lunSummary)[,1]),ylim=c(0,ymax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val, cex.names=cex.names.val,main='lung resection', space=0, xlim=c(0,maxnum))
error.bar(barx,y.means[,1], 1.96*y.sd/10)
#eso
y.means=abs(finalResults(esoSummary)[2])
y.sd=abs(finalResults(esoSummary)[,2])*factor.sd
barx <- barplot(y.means[,1], width=barwidth,names.arg=as.character(finalResults(esoSummary)[,1]),ylim=c(0,ymax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val, cex.names=cex.names.val,main='esophagectomy', space=0, xlim=c(0,maxnum))
error.bar(barx,y.means[,1], 1.96*y.sd/10)
#cys
y.means=abs(finalResults(cysSummary)[2])
y.sd=abs(finalResults(cysSummary)[,2])*factor.sd
barx <- barplot(y.means[,1], width=barwidth,names.arg=as.character(finalResults(cysSummary)[,1]),ylim=c(0,ymax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='cystectomy', space=0, xlim=c(0,maxnum))
error.bar(barx,y.means[,1], 1.96*y.sd/10)
#pan
y.means=abs(finalResults(panSummary)[2])
y.sd=abs(finalResults(panSummary)[,2])*factor.sd
barx <- barplot(y.means[,1], width=barwidth,names.arg=as.character(finalResults(aaaSummary)[,1]),ylim=c(0,ymax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val, cex.names=cex.names.val,main='pancreatectomy', space=0, xlim=c(0,maxnum))
error.bar(barx,y.means[,1], 1.96*y.sd/10)
#car
y.means=abs(finalResults(carSummary)[2])
y.sd=abs(finalResults(carSummary)[,2])*factor.sd
barx <- barplot(y.means[,1],width=barwidth, names.arg=as.character(finalResults(carSummary)[,1]),ylim=c(0,ymax), col="grey", axis.lty=1, xlab=NULL, ylab=ylab, cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='CEA', space=0, xlim=c(0,maxnum))
error.bar(barx,y.means[,1], 1.96*y.sd/10)
dev.off()


oversupplyVec=c(numOfHospitals(lunSummary)[3],
                numOfHospitals(cysSummary)[3],
                numOfHospitals(esoSummary)[3],#least
                numOfHospitals(panSummary)[3],
                numOfHospitals(aaaSummary)[3],
                numOfHospitals(carSummary)[3], #most
                numOfHospitals(aorSummary)[3],
                numOfHospitals(cabSummary)[3])

procVec=c('lung resection','cystectomy','esophagectomy','pancreatectomy','AAA','CEA','AVR','CABG')

The default is
par(mar= c(5, 4, 4, 2) + 0.1)
par(oma=c(1,5,1,1))
par(pin=c(1.9,1.9)) 

tiff('C:/Dropbox/paper/frog/draft/smdm_figure/oversupply.tif', width=480,height=480)
barplot(oversupplyVec,names.arg=procVec, xlab='overcapacity ratio',ylab='surgery',xlim=c(0,25),las=1,cex.names=1,offset=0,horiz=T,space=1,cex.axis=0.5)
dev.off()


par()$oma

oversupplyDf=data.frame(procVec=procVec,oversupplyVec=oversupplyVec)

means.barplot <- qplot(x=procVec, y=oversupplyVec, 
                       data=oversupplyDf, geom="bar", stat="identity",
                       position="dodge")





par(mar=c(5.1, max(4.1,max(nchar(c('lung resection','cystectomy','esophagectomy','pancreatectomy','repair of AAA','carotid endarterectomy','aortic-valve replacement','CABG')))/1.8) ,4.1 ,2.1))


par()$mfrow
par(mfrow=c(1,1))
par(oma=c(1,10,0,0))


mean(abs(finalResults(cabSummary)[2])[,1])
mean(abs(finalResults(aorSummary)[2])[,1])
mean(abs(finalResults(aaaSummary)[2])[,1])
mean(abs(finalResults(lunSummary)[2])[,1])
mean(abs(finalResults(esoSummary)[2])[,1])
mean(abs(finalResults(cysSummary)[2])[,1])
mean(abs(finalResults(panSummary)[2])[,1])
mean(abs(finalResults(carSummary)[2])[,1])






#par(mfrow=c(8,1),mar=c(2,1,2,1)+0.1, oma=c(3,3,1,0)+0.1)
#aaa
pdf('C:/Dropbox/paper/frog/draft/smdm_figure/plot.pdf', width=2*8, height=4*7)
#par(mfrow=c(8,1),mar=c(2,1,3,1)+0.1, oma=c(3,1,1,0)+0.1)
par(mfrow=c(8,1),mar=c(2,1,2,1)+0.1, oma=c(3,3,1,0)+0.1)
y.means=abs(finalResults(aaaSummary)[2])
y.sd=abs(finalResults(aaaSummary)[,2])*factor.sd
barx <- barplot(y.means[,1], names.arg=as.character(finalResults(aaaSummary)[,1]),ylim=c(0,ymax), col="grey", axis.lty=1, xlab=NULL, ylab="Mortality gap (%)", cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val,  main='AAA')
error.bar(barx,y.means[,1], 1.96*y.sd/10)
#aor
y.means=abs(finalResults(aorSummary)[2])
y.sd=abs(finalResults(aorSummary)[,2])*factor.sd
barx <- barplot(y.means[,1], names.arg=as.character(finalResults(aorSummary)[,1]),ylim=c(0,ymax), col="grey", axis.lty=1, xlab=NULL, ylab="Mortality gap (%)", cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='Aortic-valve replacement')
error.bar(barx,y.means[,1], 1.96*y.sd/10)
#cab
y.means=abs(finalResults(cabSummary)[2])
y.sd=abs(finalResults(cabSummary)[,2])*factor.sd
barx <- barplot(y.means[,1], names.arg=as.character(finalResults(cabSummary)[,1]),ylim=c(0,ymax), col="grey", axis.lty=1, xlab=NULL, ylab="Mortality gap (%)", cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='CABG')
error.bar(barx,y.means[,1], 1.96*y.sd/10)
#car
y.means=abs(finalResults(carSummary)[2])
y.sd=abs(finalResults(carSummary)[,2])*factor.sd
barx <- barplot(y.means[,1], names.arg=as.character(finalResults(carSummary)[,1]),ylim=c(0,ymax), col="grey", axis.lty=1, xlab=NULL, ylab="Mortality gap (%)", cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='Carotid endarterectomy')
error.bar(barx,y.means[,1], 1.96*y.sd/10)
#cys
y.means=abs(finalResults(cysSummary)[2])
y.sd=abs(finalResults(cysSummary)[,2])*factor.sd
barx <- barplot(y.means[,1], names.arg=as.character(finalResults(cysSummary)[,1]),ylim=c(0,ymax), col="grey", axis.lty=1, xlab=NULL, ylab="Mortality gap (%)", cex.lab=cex.lab.val,cex.main=cex.main.val,cex.names=cex.names.val, main='Cystectomy')
error.bar(barx,y.means[,1], 1.96*y.sd/10)
#eso
y.means=abs(finalResults(esoSummary)[2])
y.sd=abs(finalResults(esoSummary)[,2])*factor.sd
barx <- barplot(y.means[,1], names.arg=as.character(finalResults(esoSummary)[,1]),ylim=c(0,ymax), col="grey", axis.lty=1, xlab=NULL, ylab="Mortality gap (%)", cex.lab=cex.lab.val,cex.main=cex.main.val, cex.names=cex.names.val,main='Esophagectomy')
error.bar(barx,y.means[,1], 1.96*y.sd/10)

#lun
y.means=abs(finalResults(lunSummary)[2])
y.sd=abs(finalResults(lunSummary)[,2])*factor.sd
barx <- barplot(y.means[,1], names.arg=as.character(finalResults(lunSummary)[,1]),ylim=c(0,ymax), col="grey", axis.lty=1, xlab=NULL, ylab="Mortality gap (%)", cex.lab=cex.lab.val,cex.main=cex.main.val, cex.names=cex.names.val,main='Lung resection')
error.bar(barx,y.means[,1], 1.96*y.sd/10)
#pan
y.means=abs(finalResults(panSummary)[2])
y.sd=abs(finalResults(panSummary)[,2])*factor.sd
barx <- barplot(y.means[,1], names.arg=as.character(finalResults(aaaSummary)[,1]),ylim=c(0,ymax), col="grey", axis.lty=1, xlab=NULL, ylab="Mortality gap (%)", cex.lab=cex.lab.val,cex.main=cex.main.val, cex.names=cex.names.val,main='Pancreatectomy')
error.bar(barx,y.means[,1], 1.96*y.sd/10)
dev.off()





