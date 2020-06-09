#read in data
# C:\Dropbox\K\lapLearningCurve\sascode\getLapSrgLos_bladder.sas
countLines('Z:/j_scrdata/frogLearn/eso_ana.csv')
countLines('Z:/j_scrdata/frogLearn/aaa_ana.csv')
countLines('Z:/j_scrdata/frogLearn/car_ana.csv')
countLines('Z:/j_scrdata/frogLearn/lun_ana.csv')
countLines('Z:/j_scrdata/frogLearn/pan_ana.csv')
countLines('Z:/j_scrdata/frogLearn/cab_ana.csv')
countLines('Z:/j_scrdata/frogLearn/aor_ana.csv')
countLines('Z:/j_scrdata/frogLearn/cys_ana.csv')
countLines('Z:/j_scrdata/frogLearn/pan_ana.csv')

path='Z:/j_scrdata/frogLearn/eso_ana.csv'
path='Z:/j_scrdata/frogLearn/car_ana.csv'
logVolConstant=1

dataPath='Z:/j_scrdata/frogLearn/eso_ana.csv'
fitObjPath='Z:/j_scrdata/frogLearn/eso_fit.RData'

dataPath='Z:/j_scrdata/frogLearn/cys_ana.csv'
hospFixed=T
knots.tiles=c(0.5)
Boundary.knots.tiles=c(0,1)
minToMaxVolume.byWhat=5
logVolVn='logVol'
bsDegree=2
nMax=5000
logVolConstant=1
#innerTiles=seq(0.2,0.8,by=0.2)
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


logitRE <- glmer(died ~ logVol.bs1 + logVol.bs2 + logVol.bs3 + age4cat + race5cat + 
             comorbcat + year +  
             (1 | hospid), data = anaDf, family = binomial, nAGQ = 10)



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


(load(file='Z:/j_scrdata/frogLearn/cys_fit_point5_hospFixed.RData'))

plotFun(rawPredPr)
rawDf=readTableCols.yz('Z:/j_scrdata/frogLearn/cys_ana.csv',vns=keptVns,keep=TRUE)

head(rawDf)
(load(file='Z:/j_scrdata/frogLearn/cys_fit.RData'))
head(summary(logitfit)$coefficients)

cysPredProbTest=getdffit('Z:/j_scrdata/frogLearn/cys_ana.csv', 'Z:/j_scrdata/frogLearn/cys_fit.RData', F, c(0.5), c(0,1), 5, logVolVn='logVol', bsDegree=2, nMax=5000, logVolConstant=1)
plotFun(cysPredProbTest)


#the problem comes from unstable intercept. so there is little hosptial heterogeneity....so we should assume hospitals are homogenous


load(file='Z:/j_scrdata/frogLearn/cys_fit.RData')
head(summary(logitfit)$coefficients,30)
load(file='Z:/j_scrdata/frogLearn/cys_fit_point5_hospFixed.RData')
head(summary(logitfit)$coefficients,30)
# 
# cysPredProbTest=getdffit('Z:/j_scrdata/frogLearn/cys_ana.csv','Z:/j_scrdata/frogLearn/cys_fitTest.RData',F, seq(0.2,0.8,by=0.2), c(0,1),5,logVolVn='logVol', bsDegree=2, nMax=5000,  logVolConstant=1)
# plotFun(cysPredProbTest)


#ok, I should figure out a way to get an average learning curve....enforce hosp intercept to be zeros. that is it..
#c(0.1) does not work
#c(0.2) shape is not right
#seq(0.2,0.8,by=0.2) shape is apprantly wrong (overfitting)
cysPredProbTest=getdffit('Z:/j_scrdata/frogLearn/cys_ana.csv'
                         ,'Z:/j_scrdata/frogLearn/cys_fit_point5_hospFixed.RData'
                         , T
                         , c(0.5)
                         , c(0,1)
                         , 5
                         , logVolVn='logVol', bsDegree=2, nMax=5000,  logVolConstant=1)

plotFun(cysPredProbTest)

(load(file='Z:/j_scrdata/frogLearn/cys_fit_point2_hospFixed.RData'))
summary(logitfit)

cysPredProb.point5=getdffit('Z:/j_scrdata/frogLearn/cys_ana.csv','Z:/j_scrdata/frogLearn/cys_fit.RData', innerTiles=0.1,minToMaxVolume.byWhat=10)
plotFun(cysPredProb.point5)

plot(rawPredPr[,1],rawPredPr[,2])
lines(rawPredPr[,1],rawPredPr[,4])


pdf('C:/Dropbox/paper/frog/results/fig/esoLearnCurve.pdf')
plot(x=predDiePr[,1],y=predDiePr[,2],type='b',col='red',lwd=3, xlab='annual volume',ylab='mortablity - esophagectomy')
points(x=rawPredPr[ok.row,1],y=rawPredPr[ok.row,2])
dev.off()



plot(hospVolVec,meanProb,type='b')

count(outdf,'year')

esoPredProb=getdffit('Z:/j_scrdata/frogLearn/eso_ana.csv','Z:/j_scrdata/frogLearn/eso_fit.RData')


esoPredProb.point5=getdffit('Z:/j_scrdata/frogLearn/eso_ana.csv','Z:/j_scrdata/frogLearn/eso_fit.RData')
plotFun(esoPredProb.point5)

cabPredProb=getdffit('Z:/j_scrdata/frogLearn/cab_ana.csv','Z:/j_scrdata/frogLearn/cab_fit.RData')
plotFun(cabPredProb)

cysPredProb=getdffit('Z:/j_scrdata/frogLearn/cys_ana.csv','Z:/j_scrdata/frogLearn/cys_fit.RData')
plotFun(cysPredProb)

cysPredProb.point5=getdffit('Z:/j_scrdata/frogLearn/cys_ana.csv','Z:/j_scrdata/frogLearn/cys_fit.RData')
plotFun(cysPredProb.point5)

lunPredProb=getdffit('Z:/j_scrdata/frogLearn/lun_ana.csv','Z:/j_scrdata/frogLearn/lun_fit.RData')
aorPredProb=getdffit('Z:/j_scrdata/frogLearn/aor_ana.csv','Z:/j_scrdata/frogLearn/aor_fit.RData')
carPredProb=getdffit('Z:/j_scrdata/frogLearn/car_ana.csv','Z:/j_scrdata/frogLearn/car_fit.RData')
aaaPredProb=getdffit('Z:/j_scrdata/frogLearn/aaa_ana.csv','Z:/j_scrdata/frogLearn/aaa_fit.RData')
panPredProb=getdffit('Z:/j_scrdata/frogLearn/pan_ana.csv','Z:/j_scrdata/frogLearn/pan_fit.RData')

plotFun(panPredProb)


plotFun=function(indf){
  plot(indf[,1],indf[,2])
  okRows=which(!is.na(indf[,4]))
  lines(indf[okRows,1],indf[okRows,4])
  
}

plot(esoPredProb[,1],esoPredProb[,2])
lines(esoPredProb[,1],esoPredProb[,4])

plot(aorPredProb[,1],aorPredProb[,2])
lines(aorPredProb[,1],aorPredProb[,4])

plot(panPredProb[,1],panPredProb[,4])
plot(aaaPredProb[,1],aaaPredProb[,4])
plot(cabPredProb[,1],cabPredProb[,4])
plot(cysPredProb[,1],cysPredProb[,4])
plot(lunPredProb[,1],lunPredProb[,4])


panRes=getdffit('Z:/j_scrdata/frogLearn/pan_ana.csv')
cabRes=getdffit('Z:/j_scrdata/frogLearn/cab_ana.csv')
cysRes=getdffit('Z:/j_scrdata/frogLearn/cys_ana.csv')
lunRes=getdffit('Z:/j_scrdata/frogLearn/lun_ana.csv')
aorRes=getdffit('Z:/j_scrdata/frogLearn/aor_ana.csv')
carRes=getdffit('Z:/j_scrdata/frogLearn/car_ana.csv')


head(esoRes$fitsummary$coefficients)
head(panRes$fitsummary$coefficients)
head(cabRes$fitsummary$coefficients)
head(cysRes$fitsummary$coefficients)
head(lunRes$fitsummary$coefficients)
head(aorRes$fitsummary$coefficients)
head(carRes$fitsummary$coefficients)
aaaRes=getdffit('Z:/j_scrdata/frogLearn/aaa_ana.csv')
carRes=getdffit('Z:/j_scrdata/frogLearn/car_ana.csv')
cysRes=getdffit('Z:/j_scrdata/frogLearn/cys_ana.csv')
aorRes=getdffit('Z:/j_scrdata/frogLearn/aor_ana.csv')
cabRes=getdffit('Z:/j_scrdata/frogLearn/cab_ana.csv')
lunRes=getdffit('Z:/j_scrdata/frogLearn/lun_ana.csv')
panRes=getdffit('Z:/j_scrdata/frogLearn/pan_ana.csv')


table(ana.eso[,'died'])
names(ana.eso)

glm(as.factor(died)~ hosp.vol+factor(age)+ factor(x_comorbsum)+factor(race)+factor(hospid), data = ana.eso, family = "binomial")



glm(as.factor(died)~ hosp.vol+factor(age), data = ana.eso, family = "binomial")
glm(as.factor(died)~ hosp.vol+factor(age)+x_comorbsum, data = ana.eso, family = "binomial")
glm(as.numeric(died) ~ hosp.vol,data=ana.eso)

plot(outdf.nona[,'annualVol'],outdf.nona[,'dieprob'])
cor(outdf[,'annualVol'],outdf[,'dieprob'])



raw.1=raw.1[!is.na(raw.1[,'race']),]

head(hospVol)

hospid.set=subset(count(hospVol,c('hospid')),freq>3)[,'hospid']
unilen.yz(hospid.set)
comorbcuts=c(0,1,2,3,Inf)
raw.1=grpnv.supplycuts.yz(raw.1, '.comorbsum', comorbcuts, 'comorbcat')

raw.1[,'race5cat']=replaceValJoin.yz(raw.1[,'race'] #this is typically a data column
                                         ,list(1,2,3,4,c(5,6)) 
                                         ,c(1,2,3,4,5)
                                         ,origValVn='race'
                                         ,newValVn='race5cat' #if output is vector, this newValVn is inapplicable
                                         ,outputJoinedDf=TRUE #if F means only output the new vector
                                         #if T, then output df             
)[,'race5cat']

raw.1=raw.1[!is.na(raw.1[,'age']),]
age4catcuts=quantile(raw.1[,'age'],prob=c(0,0.25,0.5,0.75,1))
raw.1=grpnv.supplycuts.yz(raw.1, 'age', age4catcuts, 'age4cat')

names(raw.1)

names(raw.1)
load(file='Z:/j_scrdata/cysLearn/nis/anaFiles.RData')
glm(los ~ logVol+logVol.sq+factor(hospid)+age+factor(year)+factor(.comorbsum)+factor(pay1), data=subset(raw.1,hospid %in% hospid.set))
dieFit=glm(died ~ logVol+logVol.sq+age+factor(year)+factor(.comorbsum)+factor(pay1)+factor(hospid),data=subset(raw.1,hospid %in% hospid.set), family=binomial(link = "logit"))
summary(dieFit)

createBs = function(anaDf.raw
                    , interestVn # interestVn='n.lag1.lap.log'
                    , inner.knots #tell the knots inner.knots=tileList[[i]]
                    , outputVnStem #output data's columen name stem outputVnStem='n.lag1.lap.log.bs'
                    , bsDegree){   
  
  print('i am a')
  bslist=bsWrapper.yz(anaDf.raw[,interestVn] #this is the x in bs function
                      , inner.knots #inner knots, if it contains boundary knots, function will stop and issue erro message
                      , outputVnStem #output data's columen name stem
                      , degree=bsDegree
                      , dataType='data.frame'
                      #or ='data.frame
  )
  
  print('after')
  
  anaDf.raw=cbind(anaDf.raw,bslist$bsdata)
  boundary.knots=bslist$boundary.knots
  n.basis=bslist$n.basis
  
  inner.knots.list=list(inner.knots.lag1=inner.knots)
  boundary.knots.list=list(boundary.knots=boundary.knots)
  
  names(inner.knots.list)=paste('lag',seq(1),sep='')
  names(boundary.knots.list)=paste('lag',seq(1),sep='')
  
  outlist=list(inner.knots=inner.knots.list,boundary.knots=boundary.knots.list,n.basis=n.basis, df=anaDf.raw)
  
  return(outlist)
}
names(raw.1)




bsDegree=2
outputVnStem='logVolBs'
inner.knots.list=boundary.knots.list=fitList=list()

#you cannot just use equal spaced knots. they would be overly curveline

tileList=list(seq(0.1,0.9,0.2), seq(0.1,0.9,0.1), seq(0.1,0.9,0.3),seq(0.1,0.9,0.4), seq(0.1,0.9,0.5), c(0.1,0.7), c(0.2,0.7), 
              0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)

meanLosbyvolTile=matrix(NA,nrow=length(logvolvec),ncol=length(tileList))


raw.1[,'year']=as.factor(raw.1[,'year'])
raw.1[,'pay1']=as.factor(raw.1[,'pay1'])
raw.1[,'hospid']=as.factor(raw.1[,'hospid'])
raw.1[,'race5cat']=as.factor(raw.1[,'race5cat'])


raw.1=raw.1[complete.cases(raw.1[,c('died','age4cat', 'race5cat','comorbcat','pay1','year','hospid','logVol')]),]


hospid.set=subset(count(hospVol,c('hospid')),freq>3)[,'hospid']
raw.1=subset(raw.1, hospid %in% hospid.set)


formu=passVarToFormula.yz('died',c(paste(outputVnStem,seq(n.basis),sep=''),'age4cat','year','hospid'))
bsFit=glm(formu,data=cleaned, family=binomial(link = "logit"))
length(predict(bsFit))

str(bsFit)

i=1
  log.innerKnots=quantile(raw.1[,'logVol'],prob=tileList[[i]],na.rm=T)
  cleaned.obj=createBs(raw.1, "logVol",log.innerKnots,outputVnStem, bsDegree) #degree=2
  cleaned=cleaned.obj$df
  nrow(cleaned)
  #get basis spline fit model

  bsFit$coefficients
  

  #generate basis spline terms
  logvol.bs=bsWrapper.yz(raw.1[,'logVol'] #this is the x in bs function
                         , log.innerKnots #inner knots, if it contains boundary knots, function will stop and issue erro message
                         , outputVnStem #output data's columen name stem
                         , degree=bsDegree
                        
                         , intercept=FALSE
                         , dataType='data.frame'
                         , closenessCutoffPct=0.02 #closeness of inner knots to bdnots, lower than this number means it is too close.. and not good, 0.02 means two percent of range
  )$bsdata
  
  col.type.yz(raw.1)
  
  n.basis=ncol(logvol.bs)
  nobsformu=passVarToFormula.yz('', c('age4cat', 'year','hospid'))
  bsformu=passVarToFormula.yz('', paste(outputVnStem,seq(n.basis),sep=''))
  noLogBsDf=deldfcols.yz(cleaned,c(paste(outputVnStem,seq(n.basis),sep='')))
  
  
  x.noBs=model.matrix(nobsformu,noLogBsDf)
  head(x.noBs)
nrow(x.noBs)


  b=bsFit$coefficients
  names(cleaned)
  xbout.noBs=xb.matched.yz(b,x.noBs)

  xb.noBs=xbout.noBs$xb.matched
  
head(x.noBs)
x.bs[1:2,]

cleaned[1:2,]
  
  #newDf.vi = as.data.frame(logvol.bs[vi,,drop=F][rep(1,nrow(cleaned)),])
  newDf.vi=as.data.frame(logvol.bs)
 
    #newDf.vi = as.data.frame(logvol.bs[vi,,drop=F][rep(1,nrow(cleaned)),])

expXB=exp(xb.matched.yz(b,model.matrix(formu, data=cleaned))$xb.matched)
mean(expXB/(1+expXB))


x.bs[1:2,]

    xbout.bs=xb.matched.yz(b,x.bs)
    xb.bs=xbout.bs$xb.matched
    #xbout.bs$matched
    xb=xb.bs+xb.noBs
#     cuts=bsFit$alpha
#     probMat=xbCuts2probmat.orderedprobit.yz(xb,cuts)
   (probVec=exp(xb)/(1+exp(xb)))
mean(probVec)
mean(predict(bsFit,type='response'))




    [(exp(predict(bsFit))/(1+exp(predict(bsFit))))
    predict(bsFit,data=cleaned, type='response')
     mean(predict(bsFit,data=cleaned, type='response'))

    xb-predict(bsFit)
    length(xb)
    length(predict(bsFit))
     
     mean(cleaned[,'died'])
    
     
    meanLosVec[vi]=mean(probMat %*% matrix(seq(1:(length(cuts)+1)), ncol=1)) 
  }
  meanLosbyvolTile[,i]=meanLosVec 
}







save(raw.1, dieFit, hospid.set, file='Z:/j_scrdata/cysLearn/nis/anaFiles.RData')

#we need to run a loess to figure out the trend.


fit=loess(died~logVol, data=raw.1, span=0.9)
fit=loess(died~hospVol, data=raw.1, span=0.9)

plot(fit$x, fit$fitted,xlab='log volume', ylab='die prob')
points(fit$x, fit$y)
plot(raw.1[,'logVol'],raw.1[,'died'])



