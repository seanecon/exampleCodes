yearVec=c(2006,2007,2008,2009,2010,2011)





  
n.den=c(4897840, 4808497,  4733505,  4712034,  4808342,4848712) #with AB entitlement  NO HMO
n.ip=c(564500, 544330,  530598,  522105,  514796, 497227)
n.fasc=c(526779, 542807, 549444, 555591, 555323 ,557452)
n.of=c(1430670,1425526,1418716, 1446239,1452292,1459801)
# n.hasc=c(61108,55043,51553,47125)
n.op=c(1151922,1121376, 1096938, 1099325, 1101352, 1094761)



plot(n.den,type='b')
n.ip.r=n.ip/n.den
n.fasc.r=n.fasc/n.den
n.of.r=n.of/n.den
n.op.r=n.op/n.den


plot(yearVec,c(4897840, 4808497,  4733505,  4712034,  4808342,4848712)/1e6,type='b',xlab='year',ylab='N (million)',main='part A and B and no HMO')
plot(yearVec, n.ip.r,axes=F, type='b', ylab="volume (million)", ylim=c(0.05,0.35),pch=1, xlab='year')
lines(yearVec, n.fasc.r,type='b',pch=2)
lines(yearVec, n.op.r,type='b',pch=3)
lines(yearVec, n.of.r,type='b',pch=4)
axis(side = 1, at = yearVec)
axis(side = 2, at = seq(0,0.35,by=0.05))
legend('bottomright',c('inpatient','freestanding ASC','outpatient','office-visit'),pch=seq(4))
box()





ofCost2006=data.frame(type=c('eval','other','path','rad'), cost=c(112186087.26,517701683.12,28670431.4,67836699.01), year=rep(2006,4))

ofCost2007=data.frame(type=c('eval','other','path','rad'), cost= c(126758536.23
                                                                   , 578587315.49
                                                                   ,37694156.80
                                                                   ,67512663.02), year=rep(2007,4))

ofCost2008=data.frame(type=c('eval','other','path','rad'), cost= c(126758536.23
                                                                   , 578587315.49
                                                                   ,37694156.80
                                                                   ,67512663.02), year=rep(2008,4))

ofCost2009=data.frame(type=c('eval','other','path','rad'), cost=c(136224184.65,613275840.70, 45328515.84,66597850.11), year=rep(2009,4))




ofCost2010=data.frame(type=c('eval','other','path','rad'), cost= c(141178382.09
                                                                   , 678374219.91
                                                                   ,51805656.23
                                                                   , 62811478.72), year=rep(2010,4))

ofCost2011=data.frame(type=c('eval','other','path','rad'), cost= c(149238177.12, 739983152.98,56783626.20, 64641831.60)
, year=rep(2011,4))


ofD=rbind(ofCost2006, ofCost2007, ofCost2008, ofCost2009,ofCost2010, ofCost2011)






opFacCost2006=data.frame(type=c('eval','other','path','rad'), cost=c(112186087.26,517701683.12,28670431.4,67836699.01), year=rep(2006,4))

opCost2007=data.frame(type=c('eval','other','path','rad'), cost= c(126758536.23
                                                                   , 578587315.49
                                                                   ,37694156.80
                                                                   ,67512663.02), year=rep(2007,4))

opCost2008=data.frame(type=c('eval','other','path','rad'), cost= c(126758536.23
                                                                   , 578587315.49
                                                                   ,37694156.80
                                                                   ,67512663.02), year=rep(2008,4))

opCost2009=data.frame(type=c('eval','other','path','rad'), cost=c(136224184.65,613275840.70, 45328515.84,66597850.11), year=rep(2009,4))




opCost2010=data.frame(type=c('eval','other','path','rad'), cost= c(141178382.09
                                                                   , 678374219.91
                                                                   ,51805656.23
                                                                   , 62811478.72), year=rep(2010,4))

opCost2011=data.frame(type=c('eval','other','path','rad'), cost= c(149238177.12, 739983152.98,56783626.20, 64641831.60)
                      , year=rep(2011,4))




totCost=opCost+ipCost+fascCost+ofCost+hascCost
1e6*totCost/n.den

nOpPct=n.op/n.den
nHascPct=n.hasc/n.den
nIpPct=n.ip/n.den
nFascPct=n.fasc/n.den
nHascPct=n.hasc/n.den
nOfPct=n.of/n.den

nOpPct+nHascPct


op.costPerca=opCost*1e6/n.op
ip.costPerca=ipCost*1e6/n.ip
fasc.costPerca=fascCost*1e6/n.fasc
hasc.costPerca=hascCost*1e6/n.hasc
of.costPerca=ofCost*1e6/n.of

op.costPerbene=opCost*1e6/n.den
ip.costPerbene=ipCost*1e6/n.den
fasc.costPerbene=fascCost*1e6/n.den
hasc.costPerbene=hascCost*1e6/n.den
of.costPerbene=ofCost*1e6/n.den

hascOp.costPerca=op.costPerca+hasc.costPerca


tiff('Z:/j_scrdata/ascMMA/result/fig/costPerca.tiff', width = 480*1.2, height = 480*1.2)
par(mfrow=c(2,3)) 
plot(yearVec,fasc.costPerca,type='b', ylab='dollar per capita', main='fasc cost per capita')
plot(yearVec,hasc.costPerca,type='b', ylab='dollar per capita', main='hasc cost per capita')
plot(yearVec,of.costPerca,type='b', ylab='dollar per capita', main='of cost per capita')
plot(yearVec,op.costPerca,type='b', ylab='dollar per capita', main='outpatient cost per capita')
plot(yearVec,ip.costPerca,type='b', ylab='dollar per capita', main='inpatient cost per capita')
dev.off()

tiff('Z:/j_scrdata/ascMMA/result/fig/costPerbene.tif', width = 480*1.2, height = 480*1.2)
par(mfrow=c(2,3)) 
plot(yearVec,fasc.costPerbene,type='b', ylab='dollar per beneficiary',xlab='year', main='fasc cost per bene')
plot(yearVec,hasc.costPerbene,type='b', ylab='dollar per beneficiary',xlab='year', main='hasc cost per bene')
plot(yearVec,of.costPerbene,type='b', ylab='dollar per beneficiary', xlab='year', main='of cost per bene')
plot(yearVec,op.costPerbene,type='b', ylab='dollar per beneficiary', xlab='year', main='outpatient cost per bene')
plot(yearVec,ip.costPerbene,type='b', ylab='dollar per beneficiary', xlab='year', main='inpatient cost per bene')
plot(yearVec,1e6*totCost/n.den, type='b',ylab='dollar per beneficiary', xlab='year', main='cost per bene')
dev.off()


pdf('Z:/j_scrdata/ascMMA/result/fig/cost.pdf', width=2*7, height=2*7)
par(mfrow=c(5,1),mar=c(4,5,0,2)+0.1, oma=c(5,3,3,0)+0.2)
plot(yearVec,fascCost,type='b', ylab='dollar', main='fasc cost')
plot(yearVec,hascCost,type='b', ylab='dollar', main='hasc cost')
plot(yearVec,ofCost,type='b', ylab='dollar', main='of cost')
plot(yearVec,opCost,type='b', ylab='dollar', main='outpatient cost')
plot(yearVec,ipCost,type='b', ylab='dollar', main='inpatient cost')
dev.off()

tiff('Z:/j_scrdata/ascMMA/result/fig/nBenePct.tif', width = 480*1.2, height = 480*1.2)
par(mfrow=c(2,3))
plot(cbind(yearVec,nFascPct), type='b',xlab='Year',ylab=c('percent among all beneficiaries'),main='free standing ASC')
plot(cbind(yearVec,nHascPct), type='b',xlab='Year',ylab=c('percent among all beneficiaries'),main='hospital based ASC')
plot(cbind(yearVec,nOfPct), type='b',xlab='Year',ylab=c('percent among all beneficiaries'),main='office visit')
plot(cbind(yearVec,nOpPct), type='b',xlab='Year',ylab=c('percent among all beneficiaries'),main='outpatient')
plot(cbind(yearVec,nIpPct), type='b',xlab='Year',ylab=c('percent among all beneficiaries'),main='inpatient')
dev.off()



pdf('Z:/j_scrdata/ascMMA/result/fig/nBene.pdf', width=2*7, height=2*7)
# par(mfrow=c(5,1),mar=c(4,5,0,2)+0.1, oma=c(5,3,3,0)+0.2)
plot(cbind(yearVec,n.fasc.k), type='b',xlab='Year',ylab=c('number of beneficiaries'),main='free standing ASC')
plot(cbind(yearVec,n.hasc.k), type='b',xlab='Year',ylab=c('number of beneficiaries'),main='hospital based ASC')
plot(cbind(yearVec,n.of.k), type='b',xlab='Year',ylab=c('number of beneficiaries'),main='office visit')
plot(cbind(yearVec,n.op.k), type='b',xlab='Year',ylab=c('number of beneficiaries'),main='outpatient')
plot(cbind(yearVec,n.ip.k), type='b',xlab='Year',ylab=c('number of beneficiaries'),main='inpatient')
dev.off()




