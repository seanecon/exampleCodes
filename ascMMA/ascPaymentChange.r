#read in the 2008 code
?read.csv

read.table(file, header = TRUE, sep = ",", quote="\"", dec=".",
         fill = TRUE, comment.char="", ...)


rate.2007=xpt2r.yz("Z:/j_scrdata/interstim",'ascRate2007')
rate.2008=xpt2r.yz("Z:/j_scrdata/interstim",'ascRate2008')
names(rate.2008)
names(rate.2007)

in07not08=setdiff.yz(rate.2007[,'hcpcs'],rate.2008[,'hcpcs'],'in1not2')
in08not07=setdiff.yz(rate.2007[,'hcpcs'],rate.2008[,'hcpcs'],'in2not1')
inboth=setdiff.yz(rate.2007[,'hcpcs'],rate.2008[,'hcpcs'],'inboth')

lapply(list(in07not08, in08not07, inboth),length)


rate.2008

rate2008.1=na2valDf.yz(rate.2008,c('pay2007','pay2008.full','pay2008.transition'),list(0,0,0))

rate2008.1[,'hcpcs']

changeType=rep('NA',nrow(rate2008.1))

for(i in 1:length(changeType)){
  
  if (rate2008.1[i,'pay2007']==0 & rate2008.1[i,'pay2008.full']==0){changeType[i]='remain unreimb'}
  if (rate2008.1[i,'pay2007']==0 & rate2008.1[i,'pay2008.full']>0){changeType[i]='from unreimb to reimb'}
  if (rate2008.1[i,'pay2007']>0 & rate2008.1[i,'pay2008.full']>0 & rate2008.1[i,'pay2008.full'] > rate2008.1[i,'pay2007']){changeType[i]='increase reimb'}
  if (rate2008.1[i,'pay2007']>0 & rate2008.1[i,'pay2008.full'] < rate2008.1[i,'pay2007']){changeType[i]='decrease reimb'}
}

table(changeType)

rate2008.1[3481,]

ascPay=xpt2r.yz('Z:/j_scrdata/ascMMA', "ascHcpcsPayment_0410")
hcpcsByMonth=ddply(ascPay,c('year','month','hcpcs.cd'),function(x){c(avgPay=mean(x[,'linepmt']),medianPay=median(x[,'linepmt']))})
(load(file="Z:/j_scrdata/ascMMA/hcpcsByMonth.RData" ))
hcpcsByMonth=subset(hcpcsByMonth,year>2003)
hcpcsByMonth[,'quarter']=(hcpcsByMonth[,'year']-2004)*4+month2quarter.yz(hcpcsByMonth[,'month'])
firstQuarterSince2004 = ddply(subset(hcpcsByMonth,avgPay>1),'hcpcs.cd', function(x){c(firstReimbQuarter=min(x[,'quarter']))})
subset(hcpcsByMonth,hcpcs.cd=='10021')

head(firstQuarterSince2004)

cptYrMon=expandGrid.yz(allcpt,seq(2004,2010),seq(1,12))
cptYrMon=rename.vars(cptYrMon,c('Var1','Var2','Var3'),c('hcpcs.cd','year','month'))

hcpcsByMonth.1=join(cptYrMon,hcpcsByMonth, by=c('hcpcs.cd','year','month'))



tiff('Z:/j_scrdata/ascMMA/result/fig/firstSeenQuarter.tif')
hist(firstQuarterSince2004[,'firstReimbQuarter'],100, xlab='quarter since 2004', ylab='number of procedures')
dev.off()
allcpt=unique(hcpcsByMonth[,'hcpcs.cd'])
head(firstQuarterSince2004)



length(allcpt)

alwaysThere=subset(firstQuarterSince2004,firstReimbQuarter==1)
new2008=subset(firstQuarterSince2004,firstReimbQuarter==17)
alwaysThere[,'hcpcs.cd']
new2008[,'hcpcs.cd']


#always there have two subtypes: increase or decrease in 2008
#for newly added, they were still performed prior to 2008 in freestanding ASC, we can
#then see how it was increased.
#linepmt
yearDf2QuarterAvg=function(yearDf, startYear, person.idVns, payVn){
  yearDf[,'quarter']=(yearDf[,'year']-startYear)*4+month2quarter.yz(yearDf[,'month'])
  outdf=ddply(yearDf, c('hcpcs.cd','quarter'), function(x){facRowsIndex=which(x[,'typsrvcb']=='F');
                                                           outvec=c(n.person=unilen.yz(x[,person.idVns])
                                         , n.srg=nrow(x), n.facRows=length(facRowsIndex), facReimb.mean.perClaim=mean(x[facRowsIndex,payVn]),
                                         facReimb.median.perClaim=median(x[facRowsIndex,payVn]))
                                                          return(outvec)
                                                           
                                                           })
  return(outdf)
  
}


junk=xpt2r.yz('Z:/j_scrdata/ascMMA', "ascHcpcsAllPay_2004")

#a procedue, if you do not find it in freestadning ASC for ANY reimbusement (carrier file placesrv==24)
#then I would argue it is not done in freestanding ASC at all.
test.2004=yearDf2QuarterAvg(xpt2r.yz('Z:/j_scrdata/ascMMA', "ascHcpcsAllPay_2004"),2004, c('hic','claimindex'), 'linepmt')
test.2005=yearDf2QuarterAvg(xpt2r.yz('Z:/j_scrdata/ascMMA', "ascHcpcsAllPay_2005"),2004, c('hic','claimindex'), 'linepmt')
test.2006=yearDf2QuarterAvg(xpt2r.yz('Z:/j_scrdata/ascMMA', "ascHcpcsAllPay_2006"),2004, c('hic','claimindex'), 'linepmt')
test.2007=yearDf2QuarterAvg(xpt2r.yz('Z:/j_scrdata/ascMMA', "ascHcpcsAllPay_2007"),2004, c('hic','claimindex'), 'linepmt')
test.2008=yearDf2QuarterAvg(xpt2r.yz('Z:/j_scrdata/ascMMA', "ascHcpcsAllPay_2008"),2004, c('hic','claimindex'), 'linepmt')
test.2009=yearDf2QuarterAvg(xpt2r.yz('Z:/j_scrdata/ascMMA', "ascHcpcsAllPay_2009"),2004, c('hic','claimindex'), 'linepmt')
test.2010=yearDf2QuarterAvg(xpt2r.yz('Z:/j_scrdata/ascMMA', "ascHcpcsAllPay_2010"),2004, c('hic','claimindex'), 'linepmt')

test.2004.1=data.frame(year=2004,test.2004)
test.2005.1=data.frame(year=2005,test.2005)
test.2006.1=data.frame(year=2006,test.2006)
test.2007.1=data.frame(year=2007,test.2007)
test.2008.1=data.frame(year=2008,test.2008)
test.2009.1=data.frame(year=2009,test.2009)
test.2010.1=data.frame(year=2010,test.2010)

ascReimbVol=do.call(rbind,list(test.2004.1, test.2005.1, test.2006.1, test.2007.1, test.2008.1, test.2009.1, test.2010.1))
subset(ascReimbVol,hcpcs.cd=='11643')
firstFacReimbQ.df=ddply(ascReimbVol,'hcpcs.cd', function(x){
  which.index=which(x[,'facReimb.mean.perClaim']>0.5)
  if (length(which.index)>0){firstFacReimbQ=min(which.index)} else{firstFacReimbQ=NA}
return(firstFacReimbQ)
}
)



hist(firstFacReimbQ.df[,2],100)

subset(ascReimbVol,hcpcs.cd=='67042')

ascReimbVol = sortDf.yz(ascReimbVol,c('hcpcs.cd','year','quarter'))
#save(ascReimbVol,file='Z:/j_scrdata/ascMMA/ascReimbVol.RData')

#load(file='Z:/j_scrdata/ascMMA/ascReimbVol.RData')

count(ascReimbVol[,'quarter'])
#find the heavy volume ones.
medianVol=ddply(ascReimbVol,'hcpcs.cd', function(x){c(medianVol=median(x[,'n.person']))})
medianVol
medianVol=sortDf.yz(medianVol,c('medianVol'),F)

subset(ascReimbVol,hcpcs.cd=='17304') #16 and 17 are not availabe


testFun=function(df){
  
  firstSeenQ=min(df[,'quarter'])
  lastSeenQ=max(df[,'quarter'])
  
  quartersWithFacReimb=df[which(df[,'facReimb.mean.perClaim']>0.5),'quarter']
  if (length(quartersWithFacReimb)==0){firstSeenQ.fReimb=NA; lastSeenQ.fReimb=NA} else {
    firstSeenQ.fReimb=min(quartersWithFacReimb); 
    lastSeenQ.fReimb=max(quartersWithFacReimb)
  }
  
  if (!is.na(firstSeenQ.fReimb)){
    if ( firstSeenQ.fReimb>=17){
      refQ.start=firstSeenQ.fReimb-1
      refQ.end=firstSeenQ.fReimb
      reimbType='addFacPaymentAfter2008'
    } else{
      refQ.start=16
      refQ.end=17
      reimbType='facPayExistBefore2008'
    }
  } else{
    refQ.start=16
    refQ.end=17
    reimbType='neverFacPay'}
  
  row.end=subset(df,quarter==refQ.end)
  row.start=subset(df,quarter==refQ.start)
  
  
  if(nrow(row.start)==1){row.start.exist=T}else{row.start.exist=F}
  if(nrow(row.end)==1){row.end.exist=T}else {row.end.exist=F}
  if (row.start.exist & row.end.exist){
    change.person=unname(unlist(row.end['n.person']-row.start['n.person']))
    orig.person=unlist(row.start['n.person'])
    change.person.pct=100*change.person/orig.person
    
    change.facility.reimb=unname(unlist(row.end['facReimb.mean.perClaim']-row.start['facReimb.mean.perClaim']))
    orig.facReimb=unlist(row.start['facReimb.mean.perClaim'])
    change.facReimb.pct=100*change.facility.reimb/orig.facReimb
    
  } else{
    change.person=NA
    change.facility.reimb=NA
    if (!row.start.exist){orig.person=NA;orig.facReimb=NA;change.person.pct=NA;change.facReimb.pct=NA}
  }
  
  if (reimbType=='facPayExistBefore2008' & !is.na(change.facility.reimb) ){ if (change.facility.reimb>0){reimbType='facPayExistBefore2008.inc'};
                                                                            if (change.facility.reimb==0){reimbType='facPayExistBefore2008.nochg'};
                                                                            if (change.facility.reimb<0){reimbType='facPayExistBefore2008.dec'}
                                                                            
  }
  
  if (reimbType=='facPayExistBefore2008' & is.na(change.facility.reimb) ){reimbType='facPayExistBefore2008.missQ'}
  
  outRow=data.frame(hcpcs=df[1,'hcpcs.cd']
                    ,change.person=change.person
                    , change.facility.reimb=change.facility.reimb
                    , refQ.start=refQ.start
                    ,row.start.exist=row.start.exist
                    , refQ.end=refQ.end
                    , row.end.exist=row.end.exist
                    ,firstSeenQ=firstSeenQ
                    , lastSeenQ=lastSeenQ
                    ,firstSeenQ.fReimb=firstSeenQ.fReimb
                    ,lastSeenQ.fReimb=lastSeenQ.fReimb
                    
                    ,orig.person=orig.person
                    ,orig.facReimb=orig.facReimb
                    ,change.person.pct=change.person.pct
                    ,change.facReimb.pct=change.facReimb.pct
                    
                    ,reimbType=reimbType)
  return(outRow)
}

outList=list()
for(i in 1:nrow(medianVol)){

  #for(i in 1:100){
  print(i)
  df=subset(ascReimbVol,hcpcs.cd==medianVol[i,'hcpcs.cd'])
  outList=lappend.yz(outList, testFun(df))
}

reimbSummary=do.call(rbind,outList)

head(reimbSummary)












pdf('Z:/j_scrdata/ascMMA/result/fig/firstObsQuarter.pdf')
hist(reimbSummary[,'firstSeenQ.fReimb']
     ,xlab='the first quarter at which the procedure was reimbursed for facility'
     ,ylab='number of procedures',100, main='Distribution of the first facility reimbursement quarter')
dev.off()
table(reimbSummary[,'reimbType'])

head(reimbSummary)
head(reimbSummary)
top500=subset(reimbSummary, hcpcs %in% medianVol[1:500,'hcpcs.cd'])

top500[,'hcpcs']


freqTab=count(top500,'reimbType')

median.volchg=ddply(top500, 'reimbType', function(x){c(mean.volChgPct=mean(x[,'change.person.pct'],na.rm=T))})
mean.volchg=ddply(top500, 'reimbType', function(x){c(median.volChgPct=median(x[,'change.person.pct'],na.rm=T))})

write.csv.yz(top500,'Z:/j_scrdata/ascMMA/top500.csv',row.names=T)
xtable(join(join(freqTab, median.volchg), mean.volchg))
#save(reimbSummary,top500,freqTab,median.volchg,mean.volchg, file='Z:/j_scrdata/ascMMA/reimbSummary.RData')
load(file='Z:/j_scrdata/ascMMA/reimbSummary.RData')
#ddply(reimbSummary, 'reimbType', function(x){mean.volChgPct=mean(x[,'change.person.pct'],na.rm=T)})

yearDf=xpt2r.yz('Z:/j_scrdata/ascMMA', '_out2010_top500')

names(top500)

head(top500)
table(top500[,'reimbType'])

head(reimbSummary)
medianVol[1:100,]



startVol=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','n_clm_start')[,c('hcpcs.cd','count','plcsrvc')],c('count'),c('start.nclm'))


endVol=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','n_clm_end')[,c('hcpcs.cd','count','plcsrvc')],c('count'),c('end.nclm'))


table(startVol[,'plcsrvc'])

gridDf=rename.vars(expandGrid.yz(unique(startVol[,'hcpcs.cd']),c('11','21','22','24')),c('Var1','Var2'),c('hcpcs.cd','plcsrvc'))


tmpDf=join(join(gridDf,startVol),endVol)
volDf=rename.vars(tmpDf[complete.cases(tmpDf),],'hcpcs.cd','hcpcs')



ana=join(volDf,top500[,c('hcpcs','reimbType')],  by='hcpcs')




getVolSubsitution=function(inDf){
outList=list()
hcpcs.vec=unique(inDf[,'hcpcs'])
for(i in 1:length(hcpcs.vec)){
  x=subset(inDf,hcpcs==hcpcs.vec[i])
  n.start=sum(x[,'start.nclm'])
  n.end=sum(x[,'end.nclm'])
  if (all(!(x[,'plcsrvc']=='11'))){
    n.start.of=n.end.of=0
  }else{
    n.start.of=x[x[,'plcsrvc']=='11','start.nclm']
    n.end.of=x[x[,'plcsrvc']=='11','end.nclm']
  }
 
  if (all(!(x[,'plcsrvc']=='22'))){
    n.start.op=n.end.op=0
  }else{
  n.start.op=x[x[,'plcsrvc']=='22','start.nclm']
  n.end.op=x[x[,'plcsrvc']=='22','end.nclm']
  }
  
  if (all(!(x[,'plcsrvc']=='24'))){
    n.start.asc=n.end.asc=0
  }
  else{
    n.start.asc=x[x[,'plcsrvc']=='24','start.nclm']
    n.end.asc=x[x[,'plcsrvc']=='24','end.nclm']
  }

  outVec=c(n.start, n.end, n.start.of, n.end.of, n.start.op, n.end.op, n.start.asc, n.end.asc)
  names(outVec)=c('n.start', 'n.end', 'n.start.of', 'n.end.of', 'n.start.op','n.end.op', 'n.start.asc','n.end.asc')
  outList = lappend.yz(outList,outVec)
  
}
outDf=data.frame(hcpcs=hcpcs.vec,do.call(rbind,outList))
return(outDf)
}

table(ana[,'reimbType'])




table(ana[,'reimbType'])
addFacPaymentAfter2008_df=subset(ana, reimbType=='addFacPaymentAfter2008' & !plcsrvc %in% c('21'))
facPayExistBefore2008.dec_df=subset(ana, reimbType=='facPayExistBefore2008.dec' & !plcsrvc %in% c('21'))
facPayExistBefore2008.inc_df=subset(ana, reimbType=='facPayExistBefore2008.inc' & !plcsrvc %in% c('21'))
neverFacPay_df=subset(ana, reimbType=='neverFacPay' & !plcsrvc %in% c('21'))

allDf=rbind(
  data.frame(reimbType='facPayExistBefore2008.dec',getVolSubsitution(facPayExistBefore2008.dec_df))
,data.frame(reimbType='facPayExistBefore2008.inc',getVolSubsitution(facPayExistBefore2008.inc_df))
,data.frame(reimbType='addFacPaymentAfter2008',getVolSubsitution(addFacPaymentAfter2008_df))
,data.frame(reimbType='neverFacPay',getVolSubsitution(neverFacPay_df))
)
allDf[,'chg.of']=(allDf[,'n.end.of']-allDf[,'n.start.of'])
allDf[,'chg.op']=(allDf[,'n.end.op']-allDf[,'n.start.op'])
allDf[,'chg.asc']=(allDf[,'n.end.asc']-allDf[,'n.start.asc'])
allDf[,'chg.tot']=allDf[,'n.end']-allDf[,'n.start']

type='facPayExistBefore2008.dec'

type='facPayExistBefore2008.inc'
type='addFacPaymentAfter2008'

testCorr=function(type){
  
  x=subset(allDf,reimbType==type)
  asc.of=cor(x[,'chg.of'],x[,'chg.asc'])
  asc.op=cor(x[,'chg.op'],x[,'chg.asc'])
  op.of=cor(x[,'chg.op'],x[,'chg.of'])
  
corVec=c(asc.of, asc.op, op.of)
  names(corVec)=c('asc.of','asc.op','op.of')
return(corVec)
  
}

corDf=data.frame(type=c('facPayExistBefore2008.inc','facPayExistBefore2008.dec','addFacPaymentAfter2008','neverFacPay'),
           
           rbind(testCorr('facPayExistBefore2008.inc')
,testCorr('facPayExistBefore2008.dec')
,testCorr('addFacPaymentAfter2008')
,testCorr('neverFacPay')
)
)

xtable(corDf)





  names(allDf)


allD

setdiff.yz(new2008[,'hcpcs.cd'], alwaysThere[,'hcpcs.cd'], 'inboth')

tiff('Z:/j_scrdata/ascMMA/result/fig/cpt68815.tif')
outDf=sortDf.yz(subset(hcpcsByMonth.1, hcpcs.cd=='68815'),c('year','month') )
plot(outDf[,'avgPay'],type='b', xlab='month since 2004', ylab='reimb',main='68815')
dev.off()

outDf=sortDf.yz(subset(hcpcsByMonth.1, hcpcs.cd=='10121'),c('year','month') )
plot(outDf[,'avgPay'],type='b')

outDf=sortDf.yz(subset(hcpcsByMonth.1, hcpcs.cd=='69930'),c('year','month') )
plot(outDf[,'avgPay'],type='b')

outDf=sortDf.yz(subset(hcpcsByMonth.1, hcpcs.cd=='69650'),c('year','month') )
plot(outDf[,'avgPay'],type='b')

outDf=sortDf.yz(subset(hcpcsByMonth.1, hcpcs.cd=='69450'),c('year','month') )
plot(outDf[,'avgPay'],type='b')

outDf=sortDf.yz(subset(hcpcsByMonth.1, hcpcs.cd=='67031'),c('year','month') ) #ok
plot(outDf[,'avgPay'],type='b')

outDf=sortDf.yz(subset(hcpcsByMonth.1, hcpcs.cd=='69601'),c('year','month') ) #ok
plot(outDf[,'avgPay'],type='b')

outDf=sortDf.yz(subset(hcpcsByMonth.1, hcpcs.cd=='69420'),c('year','month') ) #ok
plot(outDf[,'avgPay'],type='b')

outDf=sortDf.yz(subset(hcpcsByMonth.1, hcpcs.cd=='54700'),c('year','month') ) #ok
plot(outDf[,'avgPay'],type='b')

outDf=sortDf.yz(subset(hcpcsByMonth.1, hcpcs.cd=='66625'),c('year','month') ) #ok
plot(outDf[,'avgPay'],type='b')

outDf=sortDf.yz(subset(hcpcsByMonth.1, hcpcs.cd=='27310'),c('year','month') ) #ok
plot(outDf[,'avgPay'],type='b')

outDf=sortDf.yz(subset(hcpcsByMonth.1, hcpcs.cd=='31032'),c('year','month') ) #ok
plot(outDf[,'avgPay'],type='b')


tiff('Z:/j_scrdata/ascMMA/result/fig/cpt69110.tif')
outDf=sortDf.yz(subset(hcpcsByMonth.1, hcpcs.cd=='69110'),c('year','month') ) #ok
plot(outDf[,'avgPay'], xlab='month since 2004', ylab='reimb',type='b', main='69110')
dev.off()


#save(hcpcsByMonth, file="Z:/j_scrdata/ascMMA/hcpcsByMonth.RData" )

outDf=sortDf.yz(subset(hcpcsByMonth.1, hcpcs.cd=='29822'),c('year','month') )
outDf=sortDf.yz(subset(hcpcsByMonth.1, hcpcs.cd=='64859'),c('year','month') )

freq=count.yz(hcpcsByMonth,vars=c('hcpcs.cd'))

freq=sortDf.yz(freq,'.count')
head(freq,600)

plot(outDf[,'avgPay'],type='b')



getOpRevs
#now I need to assess the payment for all the surgical procedures that ever appeared in outpatient department
revs=xpt2r.yz('Z:/j_scrdata/ascMMA', 'srgRevs2004')
#head(revs2004)

opQC=function(revs,year,pid){
  quantityAndCost=ddply(revs,c('hcpcs.cd','month'), function(x){c(n.person=nrow(x)
  , n.srg=length(unique(x[,pid])), cost.mean=mean(sum(x[,'revpmt'])),
   cost.median=median(sum(x[,'revpmt'])))})
  outDf=data.frame(quantityAndCost, year=year)
  return(outDf)
}
opQC.2004=opQC(xpt2r.yz('Z:/j_scrdata/ascMMA', 'srgRevs2004'), 2004,'hic')
opQC.2005=opQC(xpt2r.yz('Z:/j_scrdata/ascMMA', 'srgRevs2005'), 2005,'hic')
opQC.2006=opQC(xpt2r.yz('Z:/j_scrdata/ascMMA', 'srgRevs2006'), 2006,'hic')
opQC.2007=opQC(xpt2r.yz('Z:/j_scrdata/ascMMA', 'srgRevs2007'), 2007,'hic')
opQC.2008=opQC(xpt2r.yz('Z:/j_scrdata/ascMMA', 'srgRevs2008'), 2008,'bene.id')
opQC.2009=opQC(xpt2r.yz('Z:/j_scrdata/ascMMA', 'srgRevs2009'), 2009,'bene.id')

opQC=do.call(rbind, list(opQC.2004,opQC.2005,opQC.2006,opQC.2007,opQC.2008,opQC.2009))
#save(opQC, file='Z:/j_scrdata/ascMMA/opQC20042009.RData')
head(opQC)


opQC[,'quarter']=(opQC[,'year']-2004)*4+month2quarter.yz(opQC[,'month'])

opQC.nomiss=opQC[complete.cases(opQC),]


opQC.byQuarter=ddply(opQC.nomiss, c('hcpcs.cd','quarter'), function(x){c(mean.cost=mean(x[,'cost.mean']), mean.n.person=mean(x[,'n.person']))})


chk.nperson=sortDf.yz(ddply(opQC.nomiss, 'hcpcs.cd', function(x){c(n.person=mean(x[,'n.person']))}), c('n.person'),c(F))

head(chk.nperson)

opQC.byQuarter[,'hcpcs.cd']

op.alwaysThere=subset(opQC.byQuarter, hcpcs.cd %in% alwaysThere[,1])

op.new2008=subset(opQC.byQuarter, hcpcs.cd %in% new2008[,1])
uniqueNewVec=unique(op.new2008[,'hcpcs.cd'])
plotNewHcpcsDf=subset(op.new2008, hcpcs.cd==uniqueNewVec[7])
plot(plotNewHcpcsDf[,'quarter'], plotNewHcpcsDf[,'mean.n.person'],type='b')
plot(plotNewHcpcsDf[,'quarter'], plotNewHcpcsDf[,'mean.cost'],type='b')
outchk=lm(mean.n.person~mean.cost+factor(quarter),data=op.new2008)
summary(outchk)
plot(coef(outchk)[2:length(coef(outchk))],type='b')

head(op.alwaysThere)
head(op.new2008)
plotIt=function(whichRow){
df.price=subset(opQC.nomiss,hcpcs.cd==chk.nperson[whichRow,1])
df.price[,'quarterSince2004']=12*(df.price[,'year']-2004)+df.price[,'month']
plot(df.price[,'quarterSince2004'], df.price[,'cost.median'],type='b')
}
plotIt(16)

# glm(cost.mean~n.person, data=df.price)
# corr(df.price[,c('cost.mean','n.person')])








