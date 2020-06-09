blup.df.yz <- function(lmer.model)
{
  #library(lme4)
  #extract blups for each group
  ngrp.blup.vec <- lme4::ranef(lmer.model)
  ngrp.blup.data <- data.frame (I(rownames(ngrp.blup.vec[[1]])), ngrp.blup.vec[[1]], row.names=NULL)
  names(ngrp.blup.data) <- c(names(lme4::ranef(lmer.model)),"blup")
  return(ngrp.blup.data)
  
}

dataPath='Z:/j_scrdata/frogLearn/cys_ana.csv'
fitObjPath='Z:/j_scrdata/frogLearn/cys_fitTest_tmp.RData'
hospFixed=T
knots.tiles=c(0.5)
Boundary.knots.tiles=c(0,1)
minToMaxVolume.byWhat=5
logVolVn='logVol'
bsDegree=2
nMax=5000
logVolConstant=1


getdffit=function(  dataPath
                    , fitObjPath
                    , hospFixed
                    , knots.tiles
                    , Boundary.knots.tiles
                    , minToMaxVolume.byWhat
                    , logVolVn='logVol'
                    , bsDegree=2
                    , nMax=5000
                    , logVolConstant=1){
  keptVns=c('died','x_comorbsum','hospid','age','los','race','year')
  #raw=readTableCols.yz('Z:/j_scrdata/frogLearn/eso_ana.csv',vns=keptVns,keep=TRUE)
  raw=readTableCols.yz(dataPath,vns=keptVns,keep=TRUE)
  names(raw)=tolower(names(raw))
  
  raw=wsEmpRows.yz(raw,keptVns)$df.noWsEmp
  raw=raw[complete.cases(raw),]
  raw[,'died']=as.numeric(raw[,'died'])
  raw=subset(raw,died %in% c('0','1'))
  raw[,'race']=as.numeric(raw[,'race']) #there aer '.' so problem
  hospVol=ddply(raw,c('year','hospid'),function(x){c(hosp.vol=nrow(x))})
  
  raw.1=join(raw,hospVol,by=c('year','hospid'))
  # raw.1[,'logVol']=log(raw.1[,'hosp.vol'])
  # raw.1[,'logVol.sq']=log(raw.1[,'hosp.vol'])*log(raw.1[,'hosp.vol'])
  # names(raw.1)
  raw.1[,'age']=as.numeric(raw.1[,'age'])
  outdf=completeCases.yz(raw.1,c('hosp.vol',keptVns))$complete.df
  outdf[,'x_comorbsum']=as.numeric(outdf[,'x_comorbsum'])
  outdf[,logVolVn]=log(outdf[,'hosp.vol']+logVolConstant)
  
  comorbcuts=c(0,1,2,3,4,5,Inf)
  outdf[,'x_comorbsum']=as.numeric(outdf[,'x_comorbsum'])
  outdf=grpnv.supplycuts.yz(outdf, 'x_comorbsum', comorbcuts, 'comorbcat')
  
  age4catcuts=quantile(outdf[,'age'],prob=c(0,0.25,0.5,0.75,1))
  outdf=grpnv.supplycuts.yz(outdf, 'age', age4catcuts, 'age4cat')
  
  outdf[,'race5cat']=replaceValJoin.yz(outdf[,'race'] #this is typically a data column
                                       ,list(1,2,3,4,c(5,6)) 
                                       ,c(1,2,3,4,5)
                                       ,origValVn='race'
                                       ,newValVn='race5cat' #if output is vector, this newValVn is inapplicable
                                       ,outputJoinedDf=TRUE #if F means only output the new vector
                                       #if T, then output df             
  )[,'race5cat']
  # 1 White 
  # 2 Black 
  # 3 Hispanic 
  # 4 Asian or Pacific Islander 
  # 5 Native American 
  # 6 Other 
  outdf[,'race5cat']=as.character(outdf[,'race5cat'])
  outdf[,'year']=as.character(outdf[,'year'])
  
  bslist=bsWrapper1.yz(outdf[,logVolVn] #this is the x in bs function
                       , quantile(outdf[,logVolVn],probs=knots.tiles) #inner knots, if it contains boundary knots, function will stop and issue erro message
                       , 'logVol.bs' #output data's columen name stem
                       , degree=bsDegree
                       , dataType='data.frame'
                       , Boundary.knots=quantile(outdf[,logVolVn],prob=Boundary.knots.tiles)
                       #or ='data.frame
  )
  
  n.basis=bslist$n.basis
  
  outdf=cbind(outdf,bslist$bsdata)
  
  if (nrow(outdf)>nMax){
    set.seed(1)
    anaDf = outdf[sample(seq(nrow(outdf)),nMax,replace=F),]
    #   fitdf=outdf[sample(seq(1:nrow(outdf)),5000,replace=FALSE),]; logitfit = glm(as.numeric(died)~ hosp.vol+age+ x_comorbsum+factor(race)+factor(hospid), data = fitdf)
    
  } else{
    anaDf=outdf
  }
  
  if(hospFixed){formu=passVarToFormula.yz('died',c(paste(logVolVn,'.bs',seq(n.basis),sep=''),'age4cat', 'race5cat','comorbcat','year','hospid'))
                
                #logitfit=glm(as.factor(died) ~logVol.bs1+factor(age)+ factor(x_comorbsum)+factor(race)+factor(hospid), data = anaDf, family = "binomial")
  } else{formu=passVarToFormula.yz('died',c(paste(logVolVn,'.bs',seq(n.basis),sep=''),'age4cat', 'race5cat','comorbcat','year'))
  }
  
  # 
  # anaDf[,'logVol.sq']=anaDf[,'logVol']*anaDf[,'logVol']
  # anaDf[,'logVol.cu']=anaDf[,'logVol']*anaDf[,'logVol']*anaDf[,'logVol']
  # formu=passVarToFormula.yz('died', c('logVol','logVol.sq','logVol.cu','age4cat', 'race5cat','comorbcat','year'))
  
  #many times the model will not even converge when I used fixed effect, so I think we should use random effect\
  
  logitRE <- glmer(died ~ logVol.bs1 + logVol.bs2 + logVol.bs3 + age4cat + race5cat + 
                     comorbcat + year +  
                     (1 | hospid), data = anaDf, family =binomial, nAGQ = 10)
#   
#   
#   lmerFit=lmer(died ~ logVol.bs1 + logVol.bs2 + logVol.bs3 + age4cat + race5cat + 
#          comorbcat + year +  
#          (1 | hospid), data = anaDf, family ='binomial')
    
  hist(blup.df.yz(logitRE)[,2],100)
  predProb=exp(xb+ranInt)/(1+exp(xb+ranInt))
  
  
  
  
    
  #save(anaDf,logitfit,file='Z:/j_scrdata/frogLearn/eso10KFit.R')
  #save(anaDf,logitfit,file='Z:/j_scrdata/frogLearn/car10KFit.R')
  fitsummary=summary(logitfit)
  head(summary(logitfit)$coefficients,10)
  b=coef(logitfit)
  
  # minToMaxVolume.byWhat=10
  troubleCoef=which(is.na(b))
  if (length(troubleCoef)>0){break}
  hospVolVec=seq(min(outdf[,'hosp.vol']),max(outdf[,'hosp.vol']),by=minToMaxVolume.byWhat)
  
  bsForSim=bsWrapper1.yz(log(hospVolVec+logVolConstant) #this is the x in bs function
                         ,  quantile(outdf[,logVolVn],probs=knots.tiles)  #inner knots, if it contains boundary knots, function will stop and issue erro message
                         , 'logVol.bs' #output data's columen name stem
                         , degree=bsDegree
                         , dataType='data.frame'
                         #or ='data.frame
                         , Boundary.knots=quantile(outdf[,logVolVn],prob=Boundary.knots.tiles)
  )
  
  bsPart=bsForSim$bsdata
  
  nsim=nrow(bsForSim$bsdata)
  
  anaDf.nobs=deldfcols.yz(anaDf,c(paste(logVolVn,'.bs',seq(n.basis),sep='')))
  
  bsPartDf=as.data.frame(bsPart[rep(1,nrow(anaDf.nobs)),])
  names(bsPartDf)=c(paste(logVolVn,'.bs',seq(n.basis),sep=''))
  polDf=data.frame(bsPartDf,anaDf.nobs) #policy data
  
  # if(hospFixed){ polmat.orig=model.matrix(passVarToFormula.yz('',c(paste(logVolVn,'.bs',seq(n.basis),sep=''),'age4cat', 'race5cat','comorbcat','year','hospid')), anaDf)} 
  # else{polmat.orig=model.matrix(passVarToFormula.yz('',c(paste(logVolVn,'.bs',seq(n.basis),sep=''),'age4cat', 'race5cat','comorbcat','year')), anaDf)}
  
  #even for hospFixed==T, we should ignore hospid fixed effect
  polmat.orig=model.matrix(passVarToFormula.yz('',c(paste(logVolVn,'.bs',seq(n.basis),sep=''),'age4cat', 'race5cat','comorbcat','year')), anaDf)
  
  
  
  polmat=polmat.orig
  meanProb=rep(NA,nsim)
  
  for(i in 1:nsim){
    polmat[,c(paste(logVolVn,'.bs',seq(n.basis),sep=''))]=as.matrix(bsForSim$bsdata[i,,drop=F][rep(1,nrow(polmat.orig)),])
    xb=xb.matched.yz(b,polmat)$xb.matched  
    outComePr= exp(xb)/(1+exp(xb))
    meanProb[i]= mean(outComePr)
    cat('process volume of ', i, '\n')
  }
  rawDiePr=ddply(anaDf,'hosp.vol',function(x){out=c(rawPr=mean(x[,'died']),cell.n=nrow(x));return(out)})
  predDiePr=data.frame(hosp.vol=hospVolVec, prDieProb=meanProb)
  rawPredPr=join(rawDiePr,predDiePr)
  save(logitfit, file=fitObjPath)
  return(rawPredPr=rawPredPr)
}