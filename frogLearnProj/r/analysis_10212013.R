raw=xpt2r.yz('Z:/j_scrdata/cysLearn/nis','all_cys_ana')
outcomeVars=c('los','totchg','died')
names(raw)
hospVol=ddply(raw,c('year','hospid'),function(x){c(hosp.vol=nrow(x))})
names(hospVol)
names(hospVol)
head(hospVol)
raw.1=join(raw,hospVol,by=c('year','hospid'))
raw.1[,'logVol']=log(raw.1[,'hosp.vol'])
raw.1[,'logVol.sq']=log(raw.1[,'hosp.vol'])*log(raw.1[,'hosp.vol'])
names(raw.1)

raw.1=raw.1[!is.na(raw.1[,'race']),]

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



