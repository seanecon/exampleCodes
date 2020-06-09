#the analysis is the HRR level.
libVec=c('ascMMA')
pathVec=c('Z:/j_scrdata/ascMMA')
libPathDf=data.frame(lib=libVec, path=pathVec)


#get POS file mean
posVarsHrr=xpt2r.yz("Z:/j_scrdata/ascMMA", "posVars_2006")
posVarsDf2006 = ddply(posVarsHrr,'hrrnum',function(x){c(n.hosp=sum(x[,'hospind']),n.oprooms=sum(x[,'numoprooms']), n.fasc=sum(x[,'fascind']),n.hospasc=sum(x[,'hospascind']))})

posVarsHrr=xpt2r.yz("Z:/j_scrdata/ascMMA", "posVars_2007")
posVarsDf2007 = ddply(posVarsHrr,'hrrnum',function(x){c(n.hosp=sum(x[,'hospind']),n.oprooms=sum(x[,'numoprooms']), n.fasc=sum(x[,'fascind']),n.hospasc=sum(x[,'hospascind']))})

posVarsHrr=xpt2r.yz("Z:/j_scrdata/ascMMA", "posVars_2008")
posVarsDf2008 = ddply(posVarsHrr,'hrrnum',function(x){c(n.hosp=sum(x[,'hospind']),n.oprooms=sum(x[,'numoprooms']), n.fasc=sum(x[,'fascind']),n.hospasc=sum(x[,'hospascind']))})

posVarsHrr=xpt2r.yz("Z:/j_scrdata/ascMMA", "posVars_2009")
posVarsDf2009 = ddply(posVarsHrr,'hrrnum',function(x){c(n.hosp=sum(x[,'hospind']),n.oprooms=sum(x[,'numoprooms']), n.fasc=sum(x[,'fascind']),n.hospasc=sum(x[,'hospascind']))})

posVarsHrr=xpt2r.yz("Z:/j_scrdata/ascMMA", "posVars_2010")
posVarsDf2010 = ddply(posVarsHrr,'hrrnum',function(x){c(n.hosp=sum(x[,'hospind']),n.oprooms=sum(x[,'numoprooms']), n.fasc=sum(x[,'fascind']),n.hospasc=sum(x[,'hospascind']))})

posVarsDf2011=posVarsDf2010
posVarsDf.20062011 = do.call(rbind, list(data.frame(posVarsDf2006,year=2006)
                                         , data.frame(posVarsDf2007,year=2007)
                                         , data.frame(posVarsDf2008,year=2008)
                                         , data.frame(posVarsDf2009,year=2009)
                                         , data.frame(posVarsDf2010,year=2010)
                                         , data.frame(posVarsDf2011,year=2011)))

posVarsDf.mean= ddply(posVarsDf.20062010, 'hrrnum', function(x){c(n.hosp=mean(x[,'n.hosp']),n.oprooms=mean(x[,'n.oprooms']), n.fasc=mean(x[,'n.fasc']),n.hospasc=mean(x[,'n.hospasc']))})



#next we will get denominator file mean
hist(posVarsDf.mean[,'n.hosp'],30, main='number of hospitals')
box()
hist(posVarsDf.mean[,'n.oprooms'],30, main='number of oprooms')
box()
hist(posVarsDf.mean[,'n.fasc'],30, main='number of fasc')
box()
hist(posVarsDf.mean[,'n.hospasc'],30, main='number of hospasc')
box()

#next, get denominator file var

asciiDataVn.yz("Z:/j_scrdata/ascMMA/denVarsHrr_2006.csv")
#den means denominator file

denAggByHrr(denDataPath)


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

indepVarDf.20062011=join(posVarsDf.20062011,join(denHrr.20062011,hrrMeanComorb.20062011))

sapply(list(posVarsDf.20062011,denHrr.20062011,hrrMeanComorb.20062011),nrow)

  dep2006=xpt2r.yz("Z:/j_scrdata/ascMMA/",'fascOfOpVol_2006')
  dep2007=xpt2r.yz("Z:/j_scrdata/ascMMA/",'fascOfOpVol_2007')
  dep2008=xpt2r.yz("Z:/j_scrdata/ascMMA/",'fascOfOpVol_2008')
  dep2009=xpt2r.yz("Z:/j_scrdata/ascMMA/",'fascOfOpVol_2009')
  dep2010=xpt2r.yz("Z:/j_scrdata/ascMMA/",'fascOfOpVol_2010')
  dep2011=xpt2r.yz("Z:/j_scrdata/ascMMA/",'fascOfOpVol_2011')

depVarDf.20062011 = do.call(rbind,list(dep2006,dep2007,dep2008,dep2009,dep2010,dep2011))
nrow(depVarDf)
nrow(indepVarDf)


anaDf.raw=join(depVarDf.20062011, indepVarDf.20062011, by=c('hrrnum','year'))
anaDf.raw[,'entday.opFascOf']=anaDf.raw[,'entday.op']+anaDf.raw[,'entday.of']+anaDf.raw[,'entday.fasc']
head(anaDf.raw)
names(anaDf.raw)

vec=anaDf.raw[,'.qtr.since2006']

get4quarters=function(vec){
  remainderVec=vec%%4
  zeroLoc=which(remainderVec==0)
  remainderVec[zeroLoc]=4
  return(remainderVec)
}

anaDf.raw[,'qtr']=as.factor(get4quarters(as.numeric(anaDf.raw[,'.qtr.since2006'])))
anaDf.raw[,'MMA2008.impact']=(as.integer(as.character(anaDf.raw[,'year']))>=2008)
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

adjustVars=c('MMA2008.impact','mean.age', '.avg.comorbsum','n.hosp','n.oprooms', 'n.fasc','n.hospasc','race.0','race.1','race.2','race.3','race.4','race.5','sex.1'
             ,'qtr')


# qtr.val=4
# year.val=2011

anaDf.raw.1=joinLeftMissHandle.yz(anaDf.raw, numFascDocBid.HrrQtrDf[,c('hrrnum','.qtr.since2006','nFascDocPer10kBene')], c('hrrnum','.qtr.since2006'), list(nFascDocPer10kBene=0) )

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



# save(plotDf.opFascOf.rate, plotDf.op.rate, plotDf.fasc.rate, plotDf.of.rate, fit.opFascOf, fit.op, fit.of, fit.fasc, anaDf.raw,anaDf.raw.2
#      ,file='Z:/j_scrdata/ascMMA/result/mmaResult03112014.RData')

#load(file='Z:/j_scrdata/ascMMA/result/mmaResult03112014.RData')


summary(fit.of)

















