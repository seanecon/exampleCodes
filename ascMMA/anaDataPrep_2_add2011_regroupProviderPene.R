#the analysis is the HRR level.
libVec=c('ascMMA')
pathVec=c('Z:/j_scrdata/ascMMA')
libPathDf=data.frame(lib=libVec, path=pathVec)

#next, get denominator file var

asciiDataVn.yz("Z:/j_scrdata/ascMMA/denVarsHrr_2006.csv")
(load(file= "Z:/j_scrdata/ascMMA/result/denHrr.20062011.RData"))


denAggByHrr=function(denDataPath,year){
indf = readTableCols.yz(
  denDataPath
  #"Z:/j_scrdata/ascMMA/denVarsHrr_2006.csv"
                 , c("AGE", "RACE","SEX","hrrnum")
                 )
# asciiDataVn.yz("Z:/j_scrdata/ascMMA/denVarsHrr_2006.csv")
names(indf)=tolower(names(indf))
indf[,'age']=as.numeric(indf[,'age'])

okRows=rowsWithInterestValues.yz(indf
                                 ,c('sex')
                                 #there is k vns then the interestValuesList has a length of k
                                 , list(c(0))
)$rowsWithoutInterestValues

aggHrrList = aggregateVarsBy.yz( indf[okRows,]
                               ,'hrrnum' #by variable
                               ,c("race", "sex")
                               ,'percent'
                               #either 'freq' or 'percent'
                               ,'age' #these are numerical vars, will generate mean.varname, sd.varname, '.' is the vnSep
                               #meanSdVars can be empty
                               ,
                               #these are numerical vars, will generate median.varname
                               #medianVars can be empty
                               ,tabNA=TRUE
                               ,na.rm.quantile=TRUE
)

denVarsDf = data.frame(join(aggHrrList[[1]],aggHrrList[[2]],by='hrrnum'),year=year)
return(denVarsDf)
}

indf = readTableCols.yz(
  "Z:/j_scrdata/ascMMA/denVarsHrr_2010.csv"
  #"Z:/j_scrdata/ascMMA/denVarsHrr_2006.csv"
  , c("AGE", "RACE","SEX","hrrnum")
)
count(indf,'SEX')



denHrrLevel.2006 = denAggByHrr("Z:/j_scrdata/ascMMA/denVarsHrr_2006.csv",2006)
denHrrLevel.2007 = denAggByHrr("Z:/j_scrdata/ascMMA/denVarsHrr_2007.csv",2007)
denHrrLevel.2008 = denAggByHrr("Z:/j_scrdata/ascMMA/denVarsHrr_2008.csv",2008)
denHrrLevel.2009 = denAggByHrr("Z:/j_scrdata/ascMMA/denVarsHrr_2009.csv",2009)
denHrrLevel.2010 = denAggByHrr("Z:/j_scrdata/ascMMA/denVarsHrr_2010.csv",2010)
denHrrLevel.2011 = denAggByHrr("Z:/j_scrdata/ascMMA/denVarsHrr_2011.csv",2011)

denHrr.20062011=do.call(rbind,list(denHrrLevel.2006,denHrrLevel.2007,denHrrLevel.2008,denHrrLevel.2009,denHrrLevel.2010,denHrrLevel.2011))

#save(denHrr.20062011,file= "Z:/j_scrdata/ascMMA/result/denHrr.20062011.RData")
#(load(file= "Z:/j_scrdata/ascMMA/result/denHrr.20062011.RData"))

tail(denHrr.20062011)

#sapply(list(denHrrLevel.2006,denHrrLevel.2007,denHrrLevel.2008,denHrrLevel.2009,denHrrLevel.2010,denHrrLevel.2011),names)

head(denHrr.20062011)

#comorbidity 
hrrMeanComorb=xpt2r.yz("Z:/j_scrdata/ascMMA/",'ran_meanComorbHrr_2010')[,c('hrrnum','.avg.comorbsum')]

hrrMeanComorb.2006=data.frame(hrrMeanComorb,year=2006)
hrrMeanComorb.2007=data.frame(hrrMeanComorb,year=2007)
hrrMeanComorb.2008=data.frame(hrrMeanComorb,year=2008)
hrrMeanComorb.2009=data.frame(hrrMeanComorb,year=2009)
hrrMeanComorb.2010=data.frame(hrrMeanComorb,year=2010)
hrrMeanComorb.2011=data.frame(hrrMeanComorb,year=2011)


hrrMeanComorb.20062011=do.call(rbind,list(hrrMeanComorb.2006,hrrMeanComorb.2007,hrrMeanComorb.2008,hrrMeanComorb.2009, hrrMeanComorb.2010,hrrMeanComorb.2011))


#indepVarDf.20062011=join(posVarsDf.20062011,join(denHrr.20062011,hrrMeanComorb.20062011))
#no need for pos file anymore

indepVarDf.20062011=join(denHrr.20062011,hrrMeanComorb.20062011)

  dep2006=xpt2r.yz("Z:/j_scrdata/ascMMA/",'fascOfOpVol_2006')
  dep2007=xpt2r.yz("Z:/j_scrdata/ascMMA/",'fascOfOpVol_2007')
  dep2008=xpt2r.yz("Z:/j_scrdata/ascMMA/",'fascOfOpVol_2008')
  dep2009=xpt2r.yz("Z:/j_scrdata/ascMMA/",'fascOfOpVol_2009')
  dep2010=xpt2r.yz("Z:/j_scrdata/ascMMA/",'fascOfOpVol_2010')
  dep2011=xpt2r.yz("Z:/j_scrdata/ascMMA/",'fascOfOpVol_2011')

depVarDf.20062011 = do.call(rbind,list(dep2006,dep2007,dep2008,dep2009,dep2010,dep2011))


list.files('Z:/j_scrdata/ascMMA/result')
nrow(depVarDf)
nrow(indepVarDf)


anaDf.raw=join(depVarDf.20062011, indepVarDf.20062011, by=c('hrrnum','year'))

count(anaDf.raw[,'year'])
anaDf.raw[,'entday.opFascOf']=anaDf.raw[,'entday.op']+anaDf.raw[,'entday.of']+anaDf.raw[,'entday.fasc']

vec=anaDf.raw[,'.qtr.since2006']

get4quarters=function(vec){
  remainderVec=vec%%4
  zeroLoc=which(remainderVec==0)
  remainderVec[zeroLoc]=4
  return(remainderVec)
}

anaDf.raw[,'qtr']=as.factor(get4quarters(as.numeric(anaDf.raw[,'.qtr.since2006'])))
#anaDf.raw[,'MMA2008.impact']=(as.integer(as.character(anaDf.raw[,'year']))>=2008)

anaDf.raw[,'MMA2008.impact']=1
anaDf.raw[which(as.integer(as.character(anaDf.raw[,'year']))<2008),'MMA2008.impact']=0

anaDf.raw[,'year']=as.factor(anaDf.raw[,'year'])

xtabs(~MMA2008.impact+year,data=anaDf.raw)

#knots.tiles=c(0.2,0.4,0.8)

knots.tiles=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
# #knots.tiles=c(0.5)
# knots.tiles=c(0.2,0.4,0.6,0.8)
# knots.tiles=c(0.2)
anaDf.raw[,'.qtr.since2006']=as.integer(anaDf.raw[,'.qtr.since2006'])

bslist=bsWrapper1.yz(anaDf.raw[,'.qtr.since2006'] #this is the x in bs function
                     , quantile(anaDf.raw[,'.qtr.since2006'],probs=knots.tiles) #inner knots, if it contains boundary knots, function will stop and issue erro message
                     , paste('.qtr.since2006','.bs',sep='') #output data's columen name stem
                     , degree=2
                     , dataType='data.frame'
                     , Boundary.knots=quantile(anaDf.raw[,'.qtr.since2006'],prob=c(0,1))
                     #or ='data.frame
)

names(bslist)
n.basis=bslist$n.basis
names(bslist$bsdata)
outdf.with.bs=cbind(anaDf.raw,bslist$bsdata)
names(outdf.with.bs)

# adjustVars=c('MMA2008.impact','mean.age', '.avg.comorbsum','n.hosp','n.oprooms', 'n.fasc','n.hospasc','race.0','race.1','race.2','race.3','race.4','race.5','sex.1'
#              ,'qtr')
# 

# qtr.val=4
# year.val=2011
#save(numFascDocBid.HrrQtrDf,file='Z:/j_scrdata/ascMMA/numFascDocId.hrrByQtr.RData')
(load(file='Z:/j_scrdata/ascMMA/numFascDocId.hrrByQtr.RData'))

anaDf.raw.1=joinLeftMissHandle.yz(anaDf.raw, numFascDocBid.HrrQtrDf[,c('hrrnum','.qtr.since2006','nFascDocPer10kBene')], c('hrrnum','.qtr.since2006'), list(nFascDocPer10kBene=0) )



count(anaDf.raw.1,'qtr')

count(numFascDocBid.HrrQtrDf,'year')

count(anaDf.raw.1,'.inRight')

count(subset(anaDf.raw.1,!.inRight),'year')

#then break the penetration variable int three groups.
names(numFascDocBid.HrrQtrDf)
# rowSetDiff.yz(numFascDocBid.HrrQtrDf, anaDf.raw.1, c('hrrnum','.qtr.since2006'))$in1not2
# rowSetDiff.yz(numFascDocBid.HrrQtrDf, anaDf.raw.1, c('hrrnum','.qtr.since2006'))$inner[,'.qtr.since2006']
# rowSetDiff.yz(numFascDocBid.HrrQtrDf, anaDf.raw.1, c('hrrnum','.qtr.since2006'))$rightJoinResult
# head(rowSetDiff.yz(numFascDocBid.HrrQtrDf, anaDf.raw.1, c('hrrnum','.qtr.since2006'))$leftJoinResult)


n.grp=4
cuts=quantile(anaDf.raw.1[,'nFascDocPer10kBene'],seq(0,1,by=1/n.grp))[2:n.grp]
anaDf.raw.2=grpnv.supplycuts.yz(anaDf.raw.1,'nFascDocPer10kBene',c(-Inf,cuts,Inf),'fascSurgeonGrp')
names(anaDf.raw.2)

# this has quarterly pattern
plot(ddply(anaDf.raw.1,'.qtr.since2006',function(x){mean(x[,'nFascDocPer10kBene'])}),type='b')

annualFascDf=ddply(anaDf.raw.2,c('year','hrrnum'),function(x){c(annual.nfascDocPer10k=mean(x[,'nFascDocPer10kBene']))})
anaDf.raw.3=grpnv.supplycuts.yz(join(anaDf.raw.2, annualFascDf),'annual.nfascDocPer10k',c(-Inf,cuts,Inf),'fascSurgeonGrpAnnual')

(load(file='Z:/j_scrdata/ascMMA/result/allRawExp.RData'))

anaDf.raw.3=join(anaDf.raw.3,rename.vars(allRawExp,'.quartersince2006','.qtr.since2006'))
anaDf.raw.3[,'percap.totExp']=anaDf.raw.3[,'totExp']/anaDf.raw.3[,'n.bene']
anaDf.raw.3[,'percap.totOpExp']=anaDf.raw.3[,'totOpExp']/anaDf.raw.3[,'n.bene']
anaDf.raw.3[,'percap.totIpExp']=anaDf.raw.3[,'totIpExp']/anaDf.raw.3[,'n.bene']

anaDf.raw.3[,'totalOutpVol']=anaDf.raw.3[,'entday.fasc']+anaDf.raw.3[,'entday.of']+anaDf.raw.3[,'entday.op']

knots.tiles=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
# knots.tiles=c(0.2,0.4,0.6,0.8)
# 
# knots.tiles=c(0.3333,0.666)
# 
# knots.tiles=c(0.1,0.2)


bslist=bsWrapper1.yz(anaDf.raw.3[,'.qtr.since2006'] #this is the x in bs function
                     , quantile(anaDf.raw.3[,'.qtr.since2006'],probs=knots.tiles) #inner knots, if it contains boundary knots, function will stop and issue erro message
                     , paste('.qtr.since2006','.bs',sep='') #output data's columen name stem
                     , degree=2
                     , dataType='data.frame'
                     , Boundary.knots=quantile(anaDf.raw.3[,'.qtr.since2006'],prob=c(0,1))
)
n.basis=bslist$n.basis

anaDfwithBs=cbind(anaDf.raw.3,bslist$bsdata)

names(anaDfwithBs)=tolower(names(anaDfwithBs))
anaDfwithBs[,'.qtr.since2006.sqrt']=anaDfwithBs[,'.qtr.since2006']^0.5
anaDfwithBs[,'posQtrDev']=anaDfwithBs[,'.qtr.since2006']-8
anaDfwithBs[which(anaDfwithBs[,'posQtrDev']<0),c('posQtrDev')]=0
anaDfwithBs[,'posQtrDevSq']=anaDfwithBs[,'posQtrDev']^2
anaDfwithBs[,'posQtrDevSqrt']=anaDfwithBs[,'posQtrDev']^0.5
anaDfwithBs[,'posQtrDevpoint4']=anaDfwithBs[,'posQtrDev']^0.25


anaDfwithBs[,'negQtrDev']=8-anaDfwithBs[,'.qtr.since2006']
anaDfwithBs[which(anaDfwithBs[,'negQtrDev']<0),'negQtrDev']=0


anaDfwithBs[,'qtr5to9']=anaDfwithBs[,'.qtr.since2006']<=9 &anaDfwithBs[,'.qtr.since2006']>=5
anaDfwithBs[,'firstQtrAfterMMA']=0
anaDfwithBs[which(anaDfwithBs[,'.qtr.since2006']==9),'firstQtrAfterMMA']=1


anaDfwithBs[,'MMA2008.impact']=1
anaDfwithBs[which(as.integer(as.character(anaDfwithBs[,'year']))<2008),'MMA2008.impact']=0

anaDfwithBs[,'.qtr.since2006.sqrt']=anaDfwithBs[,'.qtr.since2006']^0.5
anaDfwithBs[,'.qtr.since2006.sq']=anaDfwithBs[,'.qtr.since2006']^2



names(anaDfwithBs)

anaDfwithBs=join(anaDfwithBs,annualFascDf,by=c('hrrnum','year'))

names(anaDfwithBs)=tolower(names(anaDfwithBs))





# trendPredFunc=function(modelFit,outcomeVn){ 
#   xMat=model.matrix(formula(modelFit),modelFit$data)
#   xbOut=xb.matched.yz(coef(modelFit),xMat)
#   predExpectedRateVec=exp(xbOut$xb.matched)
#   predVol=modelFit$data$n.bene*predExpectedRateVec
#   outDf=cbind(modelFit$data[,c('n.bene',outcomeVn, '.qtr.since2006','hrrnum')],predVol=predVol)
#   return(outDf)
# }
# getSplineFit=function(
#   outcomeVn
#   ,analysisDf
#   ,whichQtrVn='.qtr.since2006'
#   ,knots.tiles=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
#   ,adjustVars=c('MMA2008.impact','mean.age', '.avg.comorbsum','n.hosp','n.oprooms', 'n.fasc','n.hospasc','race.0','race.1','race.2','race.3','race.4','race.5','sex.1' ,'qtr')
# ){
#   bslist=bsWrapper1.yz(analysisDf[,whichQtrVn] #this is the x in bs function
#                        , quantile(analysisDf[,whichQtrVn],probs=knots.tiles) #inner knots, if it contains boundary knots, function will stop and issue erro message
#                        , paste(whichQtrVn,'.bs',sep='') #output data's columen name stem
#                        , degree=2
#                        , dataType='data.frame'
#                        , Boundary.knots=quantile(analysisDf[,whichQtrVn],prob=c(0,1))
#   )
#   n.basis=bslist$n.basis
#   anaDfwithBs=cbind(analysisDf,bslist$bsdata)
#   
#   modelFit=glm(passVarToFormula.yz(outcomeVn,c(adjustVars,paste(whichQtrVn,'.bs',seq(n.basis),sep='')))
#                , offset=log(n.bene)
#                , family = poisson
#                , data=anaDfwithBs)
#   
#   return(modelFit)
#   
# }
# fit.fasc=getSplineFit(
#   outcomeVn='entday.fasc'
#   ,anaDf.raw
#   ,whichQtrVn='.qtr.since2006'
#   ,knots.tiles=c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
#   ,adjustVars=c('MMA2008.impact','mean.age', '.avg.comorbsum','n.hosp','n.oprooms', 'n.fasc','n.hospasc','race.0','race.1','race.2','race.3','race.4','race.5','sex.1' ,'qtr')
# )

#knots.tiles=c(0.2,0.4,0.6,0.8)

# save(plotDf.opFascOf.rate, plotDf.op.rate, plotDf.fasc.rate, plotDf.of.rate, fit.opFascOf, fit.op, fit.of, fit.fasc, anaDf.raw,anaDf.raw.2,file='Z:/j_scrdata/ascMMA/result/mmaResult03112014.RData')

#load(file='Z:/j_scrdata/ascMMA/result/mmaResult03112014.RData')

# save(plotDf.opFascOf.rate, plotDf.op.rate, plotDf.fasc.rate, plotDf.of.rate, fit.opFascOf, fit.op, fit.of, fit.fasc, anaDf.raw,anaDf.raw.2,anaDf.raw.3,anaDfwithBs,file='Z:/j_scrdata/ascMMA/result/mmaResult03112014.RData')

#load(file='Z:/j_scrdata/ascMMA/result/mmaResult03112014.RData')
