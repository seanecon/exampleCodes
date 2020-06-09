yearVec=c(2007,2008,2009,2010)

n.den=c(9301183,9531312,9747683,10007071)
n.ip=c(757353,736593,719443,714695)
n.fasc=c(670676,677792,685579,688147)
n.of=c(3998295,3952026,3948419,3972232)
n.hasc=c(61108,55043,51553,47125)
n.op=c(3084367,3069076,3088707,3149723)


n.den.k=c(9301183,9531312,9747683,10007071)/1e3
n.ip.k=c(757353,736593,719443,714695)/1e3
n.fasc.k=c(670676,677792,685579,688147)/1e3
n.of.k=c(3998295,3952026,3948419,3972232)/1e3
n.hasc.k=c(61108,55043,51553,47125)/1e3
n.op.k=c(3084367,3069076,3088707,3149723)/1e3



plot(yearVec, n.ip.k,axes=F,type='b', ylab="volume (million)", ylim=c(0,4),pch=1)
lines(yearVec, n.fasc.k,type='b',pch=2)
lines(yearVec, n.hasc.k,type='b',pch=3)
lines(yearVec, n.op.k,type='b',pch=4)
lines(yearVec, n.of.k,type='b',pch=5)
axis(side = 1, at = yearVec)
axis(side = 2, at = seq(0,10,by=1))
legend(2008.8,2.5,c('inpatient','freestanding ASC','hospital-based ASC','outpatient','office-visit'),pch=seq(5))
box()


tiff('Z:/j_scrdata/ascMMA/result/fig/totBene.tif',width = 480, height = 480)
plot(yearVec,n.den/1e6,type='b',xlab='year',ylab=c('number of beneficaries'),ylim=c(9,10.5), main='trend of number of beneficiaries', axes=F)
axis(side = 1, at = yearVec)
axis(side = 2, at = seq(9,11,by=0.5))
box()
dev.off()



opCost=c(1377504938+447556112,1703051273+ 434521740,1939305624+455444997,2150946838+495984174)/1e6
ipCost=c(56362805643+721868625,58890391931+699973187,60980365497+714773953,62263474759+749319846)/1e6
fascCost=c(676509875,706447973,744327587,790830663)/1e6
ofCost=c(910354113,930105148,951937562,1039069832)/1e6
hascCost=c(58613604.76+ 18230447.14 ,63482437.67+ 16461039.45,67390221.62+16213026.74,68475991.92+16581258.74)/1e6


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




