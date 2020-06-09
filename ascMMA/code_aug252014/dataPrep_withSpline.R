# 
# save(anaDfNoBs,file='Z:/j_scrdata/ascMMA/result/anaDfNoBs.RData')



getAnaDfNoBs=function(n.grp){
  
#   
#   uniqueRows.yz(encounterDayDf.20042012[,c('hrrnum','.qtr.since2006')])
#   uniqueRows.yz(indepVarDf.20042012[,c('year'),drop=F])
#   
#   subset(encounterDayDf.20042012[,c('hrrnum','.qtr.since2006')], hrrnum==1 & .qtr.since2006==25)
#   
#   encounterDayDf.20042012[9795,]
  
  
  (load(file= "Z:/j_scrdata/ascMMA/result/encounterDayDf.20042012.RData"))
  (load(file= "Z:/j_scrdata/ascMMA/result/indepVarDf.20042012.RData"))
  
  anaDf.raw=join(encounterDayDf.20042012, indepVarDf.20042012, by=c('hrrnum','year'))
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
  
 # knots.tiles=c(0.2,0.4,0.8)
  
  
  anaDf.raw[,'.qtr.since2006']=as.integer(anaDf.raw[,'.qtr.since2006'])
  
#   bslist=bsWrapper1.yz(anaDf.raw[,'.qtr.since2006'] #this is the x in bs function
#                        , quantile(anaDf.raw[,'.qtr.since2006'],probs=knots.tiles) #inner knots, if it contains boundary knots, function will stop and issue erro message
#                        , paste('.qtr.since2006','.bs',sep='') #output data's columen name stem
#                        , degree=2
#                        , dataType='data.frame'
#                        , Boundary.knots=quantile(anaDf.raw[,'.qtr.since2006'],prob=c(0,1))
#                        #or ='data.frame
#   )
#   
#   names(bslist)
#   n.basis=bslist$n.basis
#   names(bslist$bsdata)
#   outdf.with.bs=cbind(anaDf.raw,bslist$bsdata)
#   names(outdf.with.bs)
  
#   str(bslist)
  
  # adjustVars=c('MMA2008.impact','mean.age', '.avg.comorbsum','n.hosp','n.oprooms', 'n.fasc','n.hospasc','race.0','race.1','race.2','race.3','race.4','race.5','sex.1'
  #              ,'qtr')
  # 
  
  # qtr.val=4
  # year.val=2011
  #save(numFascDocBid.HrrQtrDf,file='Z:/j_scrdata/ascMMA/numFascDocId.hrrByQtr.RData')
  #(load(file='Z:/j_scrdata/ascMMA/numFascDocId.hrrByQtr.RData'))
  (load(file='Z:/j_scrdata/ascMMA/numFascDocId.hrrByQtraug282014.RData'))
  
  anaDf.raw.1=joinLeftMissHandle.yz(anaDf.raw, numFascDocBid.HrrQtrDf[,c('hrrnum','.qtr.since2006','nFascDocPer10kBene')], c('hrrnum','.qtr.since2006'), list(nFascDocPer10kBene=0) )

  anaDf.raw.1=subset(anaDf.raw.1, .qtr.since2006>-9 & .qtr.since2006<29)
  
  #rowSetDiff.yz(anaDf.raw, numFascDocBid.HrrQtrDf[,c('hrrnum','.qtr.since2006','nFascDocPer10kBene')], c('hrrnum','.qtr.since2006'))$in1not2

#   head(numFascDocBid.HrrQtrDf)
#   
#   count(numFascDocBid.HrrQtrDf,'year')
#   
#   count(anaDf.raw.1,'.inRight')
#   
#   count(subset(anaDf.raw.1,!.inRight),'year')
#   
#   then break the penetration variable int three groups.
#   names(numFascDocBid.HrrQtrDf)
  # rowSetDiff.yz(numFascDocBid.HrrQtrDf, anaDf.raw.1, c('hrrnum','.qtr.since2006'))$in1not2
  # rowSetDiff.yz(numFascDocBid.HrrQtrDf, anaDf.raw.1, c('hrrnum','.qtr.since2006'))$inner[,'.qtr.since2006']
  # rowSetDiff.yz(numFascDocBid.HrrQtrDf, anaDf.raw.1, c('hrrnum','.qtr.since2006'))$rightJoinResult
  # head(rowSetDiff.yz(numFascDocBid.HrrQtrDf, anaDf.raw.1, c('hrrnum','.qtr.since2006'))$leftJoinResult)
  
  
  
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
  
  #(load(file='Z:/j_scrdata/ascMMA/result/allRawExp.RData'))
  (load(file='Z:/j_scrdata/ascMMA/result/allOpRawExp.RData'))
    
  anaDf.raw.3=join(anaDf.raw.3,rename.vars(allOpRawExp,'.quartersince2006','.qtr.since2006'))

  # anaDf.raw.3[,'percap.totExp']=anaDf.raw.3[,'totExp']/anaDf.raw.3[,'n.bene']
  anaDf.raw.3[,'percap.totOpExp']=anaDf.raw.3[,'totOpExp']/anaDf.raw.3[,'n.bene']
  #anaDf.raw.3[,'percap.totIpExp']=anaDf.raw.3[,'totIpExp']/anaDf.raw.3[,'n.bene']
  
  anaDf.raw.3[,'totalOutpVol']=anaDf.raw.3[,'entday.fasc']+anaDf.raw.3[,'entday.of']+anaDf.raw.3[,'entday.op']
  
  anaDf.raw.3[,'per.outp.entday.exp']=anaDf.raw.3[,'percap.totOpExp']/anaDf.raw.3[,'totalOutpVol']

  anaDf.raw.3[,'percap.outpVol']=anaDf.raw.3[,'totalOutpVol']/anaDf.raw.3[,'n.bene']
  
  anaDf.raw.3[,'.qtr.since2006.sqrt']=anaDf.raw.3[,'.qtr.since2006']^0.5
  anaDf.raw.3[,'posQtrDev']=anaDf.raw.3[,'.qtr.since2006']-8
  anaDf.raw.3[which(anaDf.raw.3[,'posQtrDev']<0),c('posQtrDev')]=0
  anaDf.raw.3[,'posQtrDevSq']=anaDf.raw.3[,'posQtrDev']^2
  anaDf.raw.3[,'posQtrDevSqrt']=anaDf.raw.3[,'posQtrDev']^0.5
  anaDf.raw.3[,'posQtrDevpoint4']=anaDf.raw.3[,'posQtrDev']^0.25
  
  
  anaDf.raw.3[,'negQtrDev']=8-anaDf.raw.3[,'.qtr.since2006']
  anaDf.raw.3[which(anaDf.raw.3[,'negQtrDev']<0),'negQtrDev']=0
  
  
  anaDf.raw.3[,'qtr5to9']=anaDf.raw.3[,'.qtr.since2006']<=9 &anaDf.raw.3[,'.qtr.since2006']>=5

  
  anaDf.raw.3[,'.qtr.since2006.sqrt']=anaDf.raw.3[,'.qtr.since2006']^0.5
  anaDf.raw.3[,'.qtr.since2006.sq']=anaDf.raw.3[,'.qtr.since2006']^2
  
  
  anaDf.raw.3[,'MMA2008.impact']=1
  anaDf.raw.3[which(as.integer(as.character(anaDf.raw.3[,'year']))<2008),'MMA2008.impact']=0
  

  #save(arfIndepClean,file= "Z:/j_scrdata/ascMMA/result/arfIndepClean.RData")
  (load("Z:/j_scrdata/ascMMA/result/arfIndepClean.RData"))
  
  anaDf.raw.4=join(anaDf.raw.3, arfIndepClean, by='hrrnum')
  
  return(anaDf.raw.4)
}

anaDfNoBs=getAnaDfNoBs(3)
save(anaDfNoBs, file='Z:/j_scrdata/ascMMA/result/anaDfNoBs.Jan062015.RData')

col.type.yz(anaDfNoBs)
(load(file='Z:/j_scrdata/ascMMA/result/anaDfNoBs.Jan062015.RData'))
#save(anaDfNoBs, file='Z:/j_scrdata/ascMMA/result/anaDfNoBs.Aug082014.RData')
#load(file='Z:/j_scrdata/ascMMA/result/anaDfNoBs.Aug082014.RData')

#(load(file='Z:/j_scrdata/ascMMA/result/anaDfNoBs.RData'))




addBs=function(knots.tiles, anaDfNoBs){
  
  bslist=bsWrapper1.yz(anaDfNoBs[,'.qtr.since2006'] #this is the x in bs function
                       , quantile(anaDfNoBs[,'.qtr.since2006'],probs=knots.tiles) #inner knots, if it contains boundary knots, function will stop and issue erro message
                       , paste('.qtr.since2006','.bs',sep='') #output data's columen name stem
                       , degree=2
                       , dataType='data.frame'
                       , Boundary.knots=quantile(anaDfNoBs[,'.qtr.since2006'],prob=c(0,1))
  )
  n.basis=bslist$n.basis
  anaDfWithBs=cbind(anaDfNoBs,bslist$bsdata)
  
  names(anaDfWithBs)=tolower(names(anaDfWithBs))
  
  return(anaDfWithBs)
}

#first add bs
anaDfWithBs=addBs(c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), anaDfNoBs)

ddply(anaDfWithBs,'year',nrow)
save(anaDfWithBs, file='Z:/j_scrdata/ascMMA/result/anaDfWithBs.Aug082014.RData')


