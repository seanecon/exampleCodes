head(encounterDayVarDf.20062011)

chk=ddply(encounterDayVarDf.20062011, c('hrrnum','.qtr.since2006'),function(x){unique(x[,'n.bene'])})
head(chk)
ddply(chk,'.qtr.since2006',function(x){sum(x[,'V1'])})

getAnaDfNoBs=function(){
  
  
  (load(file= "Z:/j_scrdata/ascMMA/result/encounterDayVarDf.20062011.RData"))
  (load(file= "Z:/j_scrdata/ascMMA/result/indepVarDf.20062011.RData"))
  
  anaDf.raw=join(encounterDayVarDf.20062011, indepVarDf.20062011, by=c('hrrnum','year'))
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
  
 
  
  
  
  
  count(numFascDocBid.HrrQtrDf,'year')
  
  count(anaDf.raw.1,'.inRight')
  
  count(subset(anaDf.raw.1,!.inRight),'year')
  
  #then break the penetration variable int three groups.
  names(numFascDocBid.HrrQtrDf)
  # rowSetDiff.yz(numFascDocBid.HrrQtrDf, anaDf.raw.1, c('hrrnum','.qtr.since2006'))$in1not2
  # rowSetDiff.yz(numFascDocBid.HrrQtrDf, anaDf.raw.1, c('hrrnum','.qtr.since2006'))$inner[,'.qtr.since2006']
  # rowSetDiff.yz(numFascDocBid.HrrQtrDf, anaDf.raw.1, c('hrrnum','.qtr.since2006'))$rightJoinResult
  # head(rowSetDiff.yz(numFascDocBid.HrrQtrDf, anaDf.raw.1, c('hrrnum','.qtr.since2006'))$leftJoinResult)
  
  
  n.grp=3
  cuts=quantile(anaDf.raw.1[,'nFascDocPer10kBene'],seq(0,1,by=1/n.grp))[2:n.grp]
  anaDf.raw.2=grpnv.supplycuts.yz(anaDf.raw.1,'nFascDocPer10kBene',c(-Inf,cuts,Inf),'fascSurgeonGrp')
  
  
  # this has quarterly pattern
 # plot(ddply(anaDf.raw.1,'.qtr.since2006',function(x){mean(x[,'nFascDocPer10kBene'])}),type='b')
  print('pass here 1')
  annualFascDf=ddply(anaDf.raw.2,c('year','hrrnum'),function(x){c(annual.nfascDocPer10kBene=mean(x[,'nFascDocPer10kBene']))})
  print('pass here 2')
  anaDf.raw.2=join(anaDf.raw.2, annualFascDf)
  cuts.annual=quantile(anaDf.raw.2[,'annual.nfascDocPer10kBene'],seq(0,1,by=1/n.grp))[2:n.grp]
  anaDf.raw.3=grpnv.supplycuts.yz(anaDf.raw.2,'annual.nfascDocPer10kBene',c(-Inf,cuts.annual,Inf),'fascSurgeonGrpAnnual')

  (load(file='Z:/j_scrdata/ascMMA/result/allRawExp.RData'))
  
  anaDf.raw.3=join(anaDf.raw.3,rename.vars(allRawExp,'.quartersince2006','.qtr.since2006'))
  anaDf.raw.3[,'percap.totExp']=anaDf.raw.3[,'totExp']/anaDf.raw.3[,'n.bene']
  anaDf.raw.3[,'percap.totOpExp']=anaDf.raw.3[,'totOpExp']/anaDf.raw.3[,'n.bene']
  anaDf.raw.3[,'percap.totIpExp']=anaDf.raw.3[,'totIpExp']/anaDf.raw.3[,'n.bene']
  
  anaDf.raw.3[,'totalOutpVol']=anaDf.raw.3[,'entday.fasc']+anaDf.raw.3[,'entday.of']+anaDf.raw.3[,'entday.op']
  
  anaDf.raw.3[,'percap.outpVol']=anaDf.raw.3[,'totalOutpVol']/anaDf.raw.3[,'n.bene']
  
  #anaDf.raw.3[,'per.outp.entday.exp']=anaDf.raw.3[,'percap.totOpExp']/anaDf.raw.3[,'totalOutpVol']
  anaDf.raw.3[,'per.outp.entday.exp']=anaDf.raw.3[,'percap.totOpExp']/anaDf.raw.3[,'percap.outpVol']
  
  anaDf.raw.3[,'.qtr.since2006.sqrt']=anaDf.raw.3[,'.qtr.since2006']^0.5
  anaDf.raw.3[,'posQtrDev']=anaDf.raw.3[,'.qtr.since2006']-8
  anaDf.raw.3[which(anaDf.raw.3[,'posQtrDev']<0),c('posQtrDev')]=0
  anaDf.raw.3[,'posQtrDevSq']=anaDf.raw.3[,'posQtrDev']^2
  anaDf.raw.3[,'posQtrDevSqrt']=anaDf.raw.3[,'posQtrDev']^0.5
  anaDf.raw.3[,'posQtrDevpoint4']=anaDf.raw.3[,'posQtrDev']^0.25
  
  
  anaDf.raw.3[,'negQtrDev']=8-anaDf.raw.3[,'.qtr.since2006']
  anaDf.raw.3[which(anaDf.raw.3[,'negQtrDev']<0),'negQtrDev']=0
  
  
  anaDf.raw.3[,'qtr5to9']=anaDf.raw.3[,'.qtr.since2006']<=9 &anaDf.raw.3[,'.qtr.since2006']>=5
  anaDf.raw.3[,'firstQtrAfterMMA']=0
  anaDf.raw.3[which(anaDf.raw.3[,'.qtr.since2006']==9),'firstQtrAfterMMA']=1
  
  anaDf.raw.3[,'.qtr.since2006.sqrt']=anaDf.raw.3[,'.qtr.since2006']^0.5
  anaDf.raw.3[,'.qtr.since2006.sq']=anaDf.raw.3[,'.qtr.since2006']^2
  
  
  anaDf.raw.3[,'MMA2008.impact']=1
  anaDf.raw.3[which(as.integer(as.character(anaDf.raw.3[,'year']))<2008),'MMA2008.impact']=0

 
  return(anaDf.raw.3)
}


anaDfNoBs=getAnaDfNoBs()
#save(anaDfNoBs,file='Z:/j_scrdata/ascMMA/result/anaDfNoBs.RData')

# names(anaDfWithBs)

#load(file='Z:/j_scrdata/ascMMA/result/mmaResult03112014.RData')
