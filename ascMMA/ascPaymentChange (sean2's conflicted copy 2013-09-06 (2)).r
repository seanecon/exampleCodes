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
    orig.person=row.start['n.person']
    change.person.pct=100*change.person/orig.person
    
    change.facility.reimb=unname(unlist(row.end['facReimb.mean.perClaim']-row.start['facReimb.mean.perClaim']))
    orig.facReimb=row.start['facReimb.mean.perClaim']
    change.facReimb.pct=100*change.facility.reimb/orig.facReimb
    
  } else{
    change.person=NA
    change.facility.reimb=NA
    
    orig.person=row.start['n.person']
    orig.facReimb=row.start['facReimb.mean.perClaim']
    
    change.person.pct=NA
    change.facReimb.pct=NA
    
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
#save(reimbSummary, file='Z:/j_scrdata/ascMMA/reimbSummary.RData')
load(file='Z:/j_scrdata/ascMMA/reimbSummary.RData')

top500=subset(reimbSummary, hcpcs %in% medianVol[1:500,'hcpcs.cd'])
count(top500[,'reimbType'])

ddply(top500, 'reimbType', function(x){mean(x[,''])})

names(top500)

head(reimbSummary)
medianVol[1:100,]


yearDf=xpt2r.yz('Z:/j_scrdata/ascMMA', "ascHcpcsAllPay_2004")

head(yearDf)



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








