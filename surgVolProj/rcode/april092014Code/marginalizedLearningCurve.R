#we have to use patient pool (state-year) to get one single learning curve based on estimate of beta vector (not matrix) to get 
#learning curve for all the physicians in a year and state
#we first identify operating physician in a year.
#then among them, we get marginalized state-year averge learning curve
#then then we solve the best type 4 allocation.
#afterwards, based on best type-4 allocation, we do ramdom slection to generate a collection (say 100)  type-2 allocation.
#for each type 2 allocation, we can then estimate LOS so we obtain 100 LOSs, these will give confidence interval


#for an observed allocation, we treat it as a type 4 then do the same to get many correspnoding type 2 allocations.
#then from there, we obtain confidence interval of observed type-4 allocation.




inputs: (1) model coeffient estimate vector #this can be surgeon specific, can be surgeon-state-year specific
        (2) patient characteristics df (i.e., patient pool, not including....)
        (3) bs volume matrix, nrow is the number of different volumnes, ncol is number of volume bs vns.


output: a marginalized learning curve over patient characteristics



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

marginalizedLearningCurve=function(coefVec,patient.pool.df,volume.matrix){
  
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

  
  
  k=15
  fit=fitList[[k]]
  names(coef(fit))
  
  
  
}









#about missing quarter information data points
# There are 12 observations that do not have the qtr information. this is a very samll number. I think, ideally, we should remove these paitents whose quarter is unknown when we do the model....
# 
# subset(anaDf.raw.1,is.na(dqtr))


#I like to update the paper's effect plot using predicted instead of observed. 
#observed is not right. Using model predicted is better.
#in the middle, we need to a goodness of fit check


#I am going to create a final copy of analysis file this file intends to modularize all the steps
#on April 09, 2014, I found that enforcing surgeon dummy variable to zero is not right. I should have used is effect coding for surgeon fixed effect 


# --------step 1: first load the previous model

(load(file="Z:/j_scrdata/lapLearn/bsplineSensitivity_feb052014.RData"))



k=15
fit=fitList[[k]]


surgeonIdFactorVn='mdnum1.r'
mdId='746'

model.matrix.Formula.rhsPatYear=passVarToFormula.yz('',c('age4cat', 'race5cat','comorbcat','pay1','year'))
yearVec=as.character(seq(2004,2010))


vcov=solve(fit$Hessian) 
paraMat=mnormt::rmnorm(n=50,mean=fit$coefficients, varcov=vcov)
cutsMat=paraMat[,1:6]
betaMat=paraMat[,7:ncol(paraMat)]

docs=ia.docs
stateName='IA'
patientVarsYearDf=indf=subset(anaDf.raw.1, year==yearVec[1])

coef(fit)
inner.knots=inner.knots.list[[k]]

boundary.knots=boundary.knots.list[[k]]
degree=2
lagVolSeq=seq(0,50)



# step2 --------------  get bootstrapped learning curves. 

#the following are two functions used for getting within-state learning curve
indivDocWithinStateLearnCurve = function(
    cutsMat #cuts are extracted from fit model's mean and vcov
  , betaMat #betaVec extracted from fit model's mean and vcov
  , mdId 
  , model.matrix.Formula.rhsPatYear
  , inner.knots #inner knots
  , boundary.knots #anaDf.raw.1[,'n.lap.loag1']
  , degree #degree used to generate bs
  #can be empty, if so then surgeon specific intercept is zero
  #if specified, it is a character vector
  , patientVarsYearDf #should be corresponds to the surgeon's state in a specific year
  , lagVolSeq #the lag volume sequence used for generating leanring curve
  , surgeonIdFactorVn #in fit. it is 'mdnum1.r'
){

  n.bt=nrow(cutsMat)
  meanLosMat=matrix(NA,nrow=length(lagVolSeq),ncol=n.bt)
  
  #basically score each surgeon for all the patients belong to a region (e.g., a state, or a county or something) he resides.  
  J=ncol(cutsMat)+1 #number of ordered choices
  
  x.patYear=model.matrix(model.matrix.Formula.rhsPatYear, data=patientVarsYearDf)
  xbout.patYear=xb.matched.yz(betaMat,x.patYear)
  xbout.patYear$unmatched.b
  xb.noBs=xbout.patYear$xb.matched
  #subset(anaDf.raw.1,n.lag1.lap==9,select=c('n.lag1.lap.log.bs1','n.lag1.lap.log.bs2','n.lag1.lap.log.bs3'))
  for(i in 1:length(lagVolSeq)){
#     cat('processing i=',i,'\n')
    
    x.volSpline=bsWrapper.yz(log1p(lagVolSeq[i]) #this is the x in bs function
                             , inner.knots #inner knots, if it contains boundary knots, function will stop and issue erro message
                             , 'n.lag1.lap.log.bs' #output data's columen name stem
                             , degree=2
                             , boundary.knots=boundary.knots
                             , intercept=FALSE
                             , dataType='matrix'
    )$bsdata[rep(1,nrow(xb.noBs)),]
    
    xbout.volBs=xb.matched.yz(betaMat,x.volSpline)
    xb.volBs=xbout.volBs$xb.matched
    xbout.volBs$unmatched.b
    #     mdId='746'
    #     surgeonIdFactorVn='mdnum1.r'
    mdDummy=paste(surgeonIdFactorVn,mdId,sep='')
    if (mdDummy %in% colnames(betaMat)){
      xb=xb.volBs+xb.noBs+repmat.yz(matrix(betaMat[,mdDummy],nrow=1),nrow(xb.noBs),1)
    } else{
      xb=xb.volBs+xb.noBs
    }
    #2.5321702
    for (j in 1:n.bt){ 
      probMat = xbCuts2probmat.orderedprobit.yz(xb[,j],cutsMat[j,])
      meanLosMat[i,j]= mean(probMat %*% matrix(seq(1,J), ncol=1))
    }
    
  }
colnames(meanLosMat)=paste('sim.',seq(n.bt),sep='')
  return(meanLosMat)
  
}#function


#the following function will generate learning by doctor by state-year
#within each doctor there is a matrix with dimneions of n.bt X max.vol, so each row is a learning curve for the doctor


learnCurves.withinStateYear=function(cutsMat, betaMat, docs, stateName, indf, inner.knots, boundary.knots, degree, lagVolSeq,model.matrix.Formula.rhsPatYear, surgeonIdFactorVn="mdnum1.r"){
  learnList.G=list()
  
  for(i in 1:length(docs)){
    
    cat('processing doc ', docs[i],'\n')

      indiv.learn.g=indivDocWithinStateLearnCurve(
          cutsMat#cuts are extracted from fit model's mean and vcov
        , betaMat
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

      indiv.learn.G=apply(indiv.learn.g,2,function(x){out=cumsum(c(0,x));return(out)})

      learnList.G=lappend.yz(learnList.G, indiv.learn.G)
  }
  names(learnList.G)=docs
  return(learnList.G) 
}

#extract MDs by states
ia.docs=as.character(unique(subset(anaDf.raw.1,hospst=='IA')[,'mdnum1.r']))
md.docs=as.character(unique(subset(anaDf.raw.1,hospst=='MD')[,'mdnum1.r']))
ny.docs=as.character(unique(subset(anaDf.raw.1,hospst=='NY')[,'mdnum1.r']))



ia.curves.byYear=md.curves.byYear=ny.curves.byYear=list()

for(y in 1:length(yearVec)){
  
  ia.curves.y=learnCurves.withinStateYear(cutsMat, betaMat, ia.docs,'IA', subset(anaDf.raw.1, year==yearVec[y]), inner.knots, boundary.knots, degree, lagVolSeq,model.matrix.Formula.rhsPatYear, surgeonIdFactorVn="mdnum1.r")
  
  ia.curves.byYear=lappend.yz(ia.curves.byYear, ia.curves.y)
  
}
names(ia.curves.byYear)=yearVec

for(y in 1:length(yearVec)){
  
  md.curves.y=learnCurves.withinStateYear(cutsMat, betaMat, md.docs,'MD', subset(anaDf.raw.1, year==yearVec[y]), inner.knots, boundary.knots, degree, lagVolSeq,model.matrix.Formula.rhsPatYear, surgeonIdFactorVn="mdnum1.r")
  
  md.curves.byYear=lappend.yz(md.curves.byYear, md.curves.y)
  
}
names(md.curves.byYear)=yearVec


for(y in 1:length(yearVec)){
  
  ny.curves.y=learnCurves.withinStateYear(cutsMat, betaMat, ny.docs,'NY', subset(anaDf.raw.1, year==yearVec[y]), inner.knots, boundary.knots, degree, lagVolSeq,model.matrix.Formula.rhsPatYear, surgeonIdFactorVn="mdnum1.r")
  
  ny.curves.byYear=lappend.yz(ny.curves.byYear, ny.curves.y)
  
}



names(ia.curves.byYear)=names(md.curves.byYear)=names(ny.curves.byYear)=yearVec

dim(ia.curves.byYear$'2004'[[1]])
str(ny.curves.byYear)

plot(ny.curves.byYear[[1]][[1]][,3])

plot(diff(ny.curves.byYear[[1]][[1]][,6]))

(load(file='Z:/j_scrdata/lapLearn/stochasticByYearByDocLearningCurves.RData'))


plot(diff(ny.curves.byYear$'2004'[[1]][,5]))
plot(diff(ny.curves.byYear$'2004'[[2]][,1]))

plot(diff(ia.curves.byYear$'2004'[[1]][,1]))
plot(diff(ia.curves.byYear$'2004'[[2]][,1]))
names(ia.curves.byYear$'2004')



# step 3 --------- solve state-quartery allocation based on surgeron-year speciric leanring curves.


stocastic.learning.curve=ia.curves.byYear
#next, I would then solve for the optimal solution and compare the optimal solution to observed los

#1 get volume by state quarter

quarterLap=ddply(anaDf.raw.1,c('hospst','dqtr','year'),function(x){c(n.lap=nrow(x),los.obs=sum(as.numeric(x[,'los'])), n.md.obs=length(unique(x[,'mdnum1.r'])), vol.mean.obs=nrow(x)/length(unique(x[,'mdnum1.r'])))})

quarterLap.noNa=subset(quarterLap, !is.na(dqtr))     
     

quarterLap.state=subset(quarterLap.noNa, hospst=='IA')
stocastic.learning.curve=ia.curves.byYear

solveByQuarter.useDocYearSpecificLearnCurve = function(quarterLap.state, stocastic.learning.curve, roundBy=1){
  
  #R:/model/optimalAllocation
#   
#   n.md.opt=vol.mean.opt=vol.std.opt=los.mean.opt.unobsLearn=los.std.opt.unobsLearn=los.mean.opt=los.std.opt=rep(NA,nrow(quarterLap.state)) 
#   avgVolPerformingMdList=nMdOptList=losVecOptList=list()
  
  sol.list=list()
  
  for (i in 1:nrow(quarterLap.state)){
    cat('processing i=',i, 'out of', nrow(quarterLap.state), 'rows', '\n')
    year=quarterLap.state[i,'year']
    allDoc.G.list.year=stocastic.learning.curve[[as.character(year)]]
    
    n.sim=ncol(allDoc.G.list.year[[1]])
    
    sol.list.i=list()
    
    for (sim.i in 1:n.sim)
    {
      cat('processing row', i,  ' sim.i=', sim.i,'\n')
      G.list.state.year.sim.i=lapply(allDoc.G.list.year, function(x){x[,sim.i]})
     # tmp=solveOptAllocation(quarterLap[i,'n.lap'],  G.list.iter, seq(n.random.seeds),  pickBestDoctors, picked.n.md=picked.n.md, m.ran=m.ran)  
      
      tmp.sim.i=scilpPatAlloc_roundSolution.yz(
          quarterLap.state[i,'n.lap']
        , G.list.state.year.sim.i #G is a list of payoff, with 0 patient, 1, 2......a maximum number of patients
        , roundBy=roundBy
        , allTreated=TRUE
        , time_limit=10 #use 300 
        , node_limit=500 # use 1000
        , first_feasible=TRUE
        #use false
      )           
      sol.list.i=lappend.yz(sol.list.i, tmp.sim.i)
    }
    names(sol.list.i)=paste('sim',seq(n.sim))
    sol.list=lappend.yz(sol.list, sol.list.i)
  }
  names(sol.list)=paste('quarterLapData Row', seq(nrow(quarterLap.state)), sep='') 
  return(sol.list)
}

    
     
names(subset(quarterLap.noNa, hospst=='IA'))

sol.list.ia=solveByQuarter.useDocYearSpecificLearnCurve(subset(quarterLap.noNa, hospst=='IA'), ia.curves.byYear)
  
sol.list.md=solveByQuarter.useDocYearSpecificLearnCurve(subset(quarterLap.noNa, hospst=='MD'), md.curves.byYear)
  
sol.list.ny=solveByQuarter.useDocYearSpecificLearnCurve(subset(quarterLap.noNa, hospst=='NY'), ny.curves.byYear,5)

#in solution list, the first level is quarterly data row, second level is simulation.

extractSolSummary=function(sol.list, quarterLapData, hospState){
       volPerProvMat=nProvMat=losMat=matrix(NA,nrow=length(sol.list), ncol=length(sol.list[[1]]))
       colnames(volPerProvMat)=colnames(nProvMat)=colnames(losMat)=paste('sim', seq(length(sol.list[[1]])),sep='')
       rownames(volPerProvMat)=rownames(nProvMat)=rownames(losMat)=paste('quarterDataRow', seq(length(sol.list)),sep='')
       
       for(r in 1:length(sol.list)){
         
         for (s in 1:length(sol.list[[1]])){
           losMat[r,s]= sol.list[[r]][[s]]$objval.scaledToTrueN
           nProvMat[r,s]= length(which(sol.list[[r]][[s]]$opt.n>0))
           volPerProvMat[r,s]=sol.list[[r]][[s]]$total.opt.n/nProvMat[r,s] 
         }
         
       }
       mean.tile.yz=function(vec,probs){
         outVec=c(mean(vec),quantile(vec,probs=probs))
         names(outVec)=c('mean',paste('quantile',as.character(probs),sep=''))
         return(outVec)
       }
       
       volPerProvSummary=t(apply(volPerProvMat,1,function(x){mean.tile.yz(x,c(0.025,0.975))}))
       volPerDoc.sol=cbind(subset(quarterLapData, hospst==hospState)[c('year','dqtr','hospst')], volPerProvSummary)
       volPerDoc.sol=rename.vars(volPerDoc.sol,c('mean',"quantile0.025", "quantile0.975"),c('volPerDoc.mean','volPerDoc.lb','volPerDoc.ub'))
       
       
       nProvSummary=t(apply(nProvMat,1,function(x){mean.tile.yz(x,c(0.025,0.975))}))
       nProv.sol=cbind(subset(quarterLapData, hospst==hospState)[c('year','dqtr','hospst')], nProvSummary)
       nProv.sol=rename.vars(nProv.sol,c('mean',"quantile0.025", "quantile0.975"),c('nProv.mean','nProv.lb','nProv.ub'))
       
       losSummary=t(apply(losMat,1,function(x){mean.tile.yz(x,c(0.025,0.975))}))
       los.sol=cbind(subset(quarterLapData, hospst==hospState)[c('year','dqtr','hospst')], losSummary)
       los.sol=rename.vars(los.sol,c('mean',"quantile0.025", "quantile0.975"),c('los.mean','los.lb','los.ub'))
 
       solDf=join(join(los.sol,nProv.sol),volPerDoc.sol)

       return(solDf)
       
     }
     

#the following is summary of optimal solution
sol.ia=extractSolSummary(sol.list.ia,quarterLap.noNa,'IA')
sol.md=extractSolSummary(sol.list.md,quarterLap.noNa,'MD')
sol.ny=extractSolSummary(sol.list.ny,quarterLap.noNa,'NY')

#step 4 -------------now we need to solve predicted LOS based on observed allcation.
obsAlloc.ia = state.qtr.volAlloc=ddply(subset(anaDf.raw.1, !is.na(dqtr) & hospst=="IA"), c('mdnum1.r','dqtr','year'), function(x){c(vol=nrow(x))})
obsAlloc.md = state.qtr.volAlloc=ddply(subset(anaDf.raw.1, !is.na(dqtr) & hospst=="MD"), c('mdnum1.r','dqtr','year'), function(x){c(vol=nrow(x))})
obsAlloc.ny = state.qtr.volAlloc=ddply(subset(anaDf.raw.1, !is.na(dqtr) & hospst=="NY"), c('mdnum1.r','dqtr','year'), function(x){c(vol=nrow(x))})

count(subset(anaDf.raw.1, !is.na(dqtr) & hospst=="MD" & year=='2004'),'dqtr')
count(subset(anaDf.raw.1, !is.na(dqtr) & hospst=="MD" & year=='2009'),'dqtr')


count(subset(anaDf, !is.na(dqtr) & hospst=="MD" & year=='2004'),'dqtr')
count(subset(anaDf, !is.na(dqtr) & hospst=="MD" & year=='2009'),'dqtr')
#from anaDf to anaDf.raw.1, we lost quarter 1 and 2 for 2004...why
#for 2009, even anaDf dose not have quarter 1 and 2


subset(obsAlloc.md, year=='2009')

subset(obsAlloc.md, year=='2009')

names(md.curves.byYear[[1]])
names(ia.curves.byYear[[1]])
obsAlloc=obsAlloc.md
stochastic.learning.curve = md.curves.byYear

obsAlloc=obsAlloc.md
stochastic.learning.curve=md.curves.byYear
predBasedOnObservedAlloc=function(obsAlloc, stochastic.learning.curve){
  
  mu=ci.lb=ci.ub=qtrList=yearList=list()
  yearVec=sort(as.character(unique(obsAlloc[,'year'])))

  for(y in 1:length(yearVec)){ #loop year
    cat('processing year', yearVec[y],'\n')
   
    for(qtr in 1:4) { #loop quarter
    
      yearList=lappend.yz(yearList,yearVec[y])
      qtrList=lappend.yz(qtrList,qtr)
      
      
    cat('processing qtr', qtr,'\n')
   
    qtr.volAlloc=subset(obsAlloc, dqtr==qtr & year==yearVec[y])
      
      
    if (nrow(qtr.volAlloc)>0){
    mdVec=as.character(qtr.volAlloc[,'mdnum1.r'])
                    
    learnCurve.year=stochastic.learning.curve[[match(year,names(stochastic.learning.curve))]]
    doc.loc=match(mdVec, names(learnCurve.year))
    
    n.sim=dim(learnCurve.year[[1]])[2]
    ndoc.obs=length(doc.loc)
    los.obs=matrix(NA,nrow=ndoc.obs,ncol=n.sim)
    
    for (i in 1:ndoc.obs){
      los.obs[i,]=learnCurve.year[[doc.loc[i]]][qtr.volAlloc[i,'vol']+1,]
    }
    
    los.predict.obsAlloc=apply(los.obs,2,sum)
    mu=lappend.yz(mu,mean(los.predict.obsAlloc))
    ci.lb=lappend.yz(ci.lb,quantile(los.predict.obsAlloc, probs=c(0.025)))
    ci.ub=lappend.yz(ci.ub,quantile(los.predict.obsAlloc, probs=c(0.975)))
    } else {
      
      mu=lappend.yz(mu,NA)
      ci.lb=lappend.yz(ci.lb,NA)
      ci.ub=lappend.yz(ci.ub,NA)
    }

  } #loop qtr
                             
  } #loop year
  
outdf=data.frame(year=unlist(yearList),qtr=unlist(qtrList),mu=unlist(mu),ci.lb=unlist(ci.lb),ci.ub=unlist(ci.ub))
return(outdf)
}


#the following are predicted LOS based on observed allocation
los.hat.obsAlloc.ia=predBasedOnObservedAlloc(obsAlloc.ia, ia.curves.byYear)
los.hat.obsAlloc.ia=rename.vars(los.hat.obsAlloc.ia, c('mu','qtr','ci.lb','ci.ub'),c('los.hat.obsAlloc','dqtr','los.hat.obsAlloc.lb','los.hat.obsAlloc.ub'))

los.hat.obsAlloc.md=predBasedOnObservedAlloc(obsAlloc.md, md.curves.byYear)


count(subset(anaDf, year=='2009' & hospst=='MD'),'year')

los.hat.obsAlloc.md=rename.vars(los.hat.obsAlloc.md, c('mu','qtr','ci.lb','ci.ub'),c('los.hat.obsAlloc','dqtr','los.hat.obsAlloc.lb','los.hat.obsAlloc.ub'))

los.hat.obsAlloc.ny=predBasedOnObservedAlloc(obsAlloc.ny, ny.curves.byYear)
los.hat.obsAlloc.ny=rename.vars(los.hat.obsAlloc.ny, c('mu','qtr','ci.lb','ci.ub'),c('los.hat.obsAlloc','dqtr','los.hat.obsAlloc.lb','los.hat.obsAlloc.ub'))

obsLos=ddply(subset(anaDf.raw.1,!is.na(dqtr)), c('hospst','year','dqtr'), function(x){sum.los.obs=sum(as.numeric(as.character(x[,'los'])));
                                                                                 mean.los.obs=mean(as.numeric(as.character(x[,'los'])))
                                                                                 outvec=c(sum.los.obs=sum.los.obs, mean.los.obs=mean.los.obs)
                                                                                 return(outvec)})
obsLos=fac2char.yz(obsLos)

obsAlloc.los.hat=rbind(los.hat.obsAlloc.ia,los.hat.obsAlloc.md,los.hat.obsAlloc.ny)
join(obsAlloc.los.hat, obsLos)
varTypeCompare.yz(obsAlloc.los.hat, obsLos)
obsAlloc.los.hat
obsLos

#step 5: create one single data which include both optimal solution and preicted LOS from observed allocation

vnTypeCompare.yz(data.frame(hospst='IA',los.hat.obsAlloc.ia), sol.ia)
ia.obsOpt=join(data.frame(hospst='IA',los.hat.obsAlloc.ia), fac2char.yz(sol.ia), type='left')
md.obsOpt=join(data.frame(hospst='MD',los.hat.obsAlloc.md), fac2char.yz(sol.md), type='left')
ny.obsOpt=join(data.frame(hospst='NY',los.hat.obsAlloc.ny), fac2char.yz(sol.ny), type='left')

#save(ia.curves.byYear, md.curves.byYear,ny.curves.byYear,file='Z:/j_scrdata/lapLearn/stochasticByYearByDocLearningCurves_jun252014.RData')
(load(file='Z:/j_scrdata/lapLearn/stochasticByYearByDocLearningCurves_jun252014.RData'))

#sol.md  no 2009 quarter 1 and 2 why

#los.hat.obsAlloc.md sol.md

#save(sol.list.ia, sol.list.md, sol.list.ny, file ='Z:/j_scrdata/lapLearn/solutionList_jun252014.RData')\
load(file ='Z:/j_scrdata/lapLearn/solutionList_jun252014.RData')




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
  legend('topleft',legend=c('observed','regionalization'), fill=c("grey","white"), inset=c(0.01,0.01), cex=0.7)
  box()
} 



#I do not want to get into random (not knowning learning ability cass, because I found this would lead the finding that there is already a lot of selection going on, I do not want to get into this issue)
pdf('C:/Dropbox/paper/surgVol/optimalReferral/figure/numMdPlot_dec232013.pdf')
# barx.nmd <- barplot(yy.nmd, beside=TRUE,col=c("black","grey","white"), ylim=c(0,20), names.arg=seq(2004,2010), axis.lty=1, xlab="Year", ylab="Average number of MIRP-performing surgeons per quarter")
#error.bar(barx.nmd, yy.nmd, lower.nmd, upper.nmd)
barx.nmd <- barplot(yy.nmd[c(1,3),], beside=TRUE ,col=c("grey","white"), ylim=c(0,20), names.arg=seq(2004,2010), axis.lty=1, xlab="Year", ylab="Average number of MIRP-performing surgeons per quarter")
error.bar(barx.nmd, yy.nmd[c(1,3),], lower.nmd[c(1,3),], upper.nmd[c(1,3),])
dev.off()



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

pdf('C:/Dropbox/paper/surgVol/optimalReferral/figure/numMdPlot_dec232013.pdf')
barx.nmd <- barplot(yy.nmd, beside=TRUE,col=c("black","grey","white"), ylim=c(0,20), names.arg=seq(2004,2010), axis.lty=1, xlab="Year", ylab="Average number of MIRP-performing surgeons per quarter")
error.bar(barx.nmd,yy.nmd, lower.nmd, upper.nmd)
dev.off()



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

# save(anaDf.raw.1 ,tabout, sampleSizeByLos
#      ,testModel.lag1, testModel.lag2,testModel.lag3,testModel.lag4,testModel.lag5,testModel.lag6
#      , fitList.logLike, fitList, meanLosbyvolTile
#      , tileList,inner.knots.list, boundary.knots.list, lagvolvec, bsDegree,  nbt, avgLearnCurveByYear, plotAvgLearnDf, cex.pch
#      ,avgLearn.allyear, avgLearn.2004, avgLearn.2005, avgLearn.2006, avgLearn.2007, avgLearn.2008, avgLearn.2009, avgLearn.2010, avgLearn.20062010, plotAvgLearnDf,lagvol.bs
#      , nCuts, meanLosMat, ubLbLearnCurveTiles, lbub, lb, ub, avgLearnCurve    
#      , ia.docs, ny.docs, md.docs
#      , ia.curves.byYear
#      , md.curves.byYear
#      , ny.curves.byYear   
#      , quarterLap
#      ,G.list.ia.2004
#      ,G.list.md.2004
#      ,G.list.ny.2004
#      ,G.list.ia.2005
#      ,G.list.md.2005
#      ,G.list.ny.2005
#      ,G.list.ia.2006
#      ,G.list.md.2006
#      ,G.list.ny.2006
#      ,G.list.ia.2007
#      ,G.list.md.2007
#      ,G.list.ny.2007  
#      ,G.list.ia.2008
#      ,G.list.md.2008
#      ,G.list.ny.2008
#      ,G.list.ia.2009
#      ,G.list.md.2009
#      ,G.list.ny.2009
#      ,G.list.ia.2010
#      ,G.list.md.2010
#      ,G.list.ny.2010
#      , opt.by.st.quarter.best.doc #solution for knowing best doctor
#      , opt.by.st.quarter.random.doc #solutgion for not knowning best doctors
#      , yy.nmd
#      , lower.nmd
#      , upper.nmd
#      , yy.volpermd
#      , lower.volpermd
#      , upper.volpermd
#      , file="Z:/j_scrdata/lapLearn/bsplineSensitivity_feb052014.RData")
 


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
