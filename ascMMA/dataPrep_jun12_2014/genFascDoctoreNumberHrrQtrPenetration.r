libVec=c('ascMMA')
pathVec=c('Z:/j_scrdata/ascMMA')
libPathDf=data.frame(lib=libVec, path=pathVec)

test.2004=completeCases.yz(xpt2r_sasLibdsn.yz('ascMMA.fascUniSrgIdHrrQtr_2004', libPathDf))$complete.df
names(test.2004)[3]='doc.id'

test.2005=completeCases.yz(xpt2r_sasLibdsn.yz('ascMMA.fascUniSrgIdHrrQtr_2005', libPathDf))$complete.df
names(test.2005)[3]='doc.id'

test.2012=completeCases.yz(xpt2r_sasLibdsn.yz('ascMMA.fascUniSrgIdHrrQtr_2012', libPathDf))$complete.df
names(test.2012)[3]='doc.id'


test.2006=completeCases.yz(xpt2r_sasLibdsn.yz('ascMMA.fascUniSrgIdHrrQtr_2006', libPathDf))$complete.df
names(test.2006)[3]='doc.id'
test.2007=completeCases.yz(xpt2r_sasLibdsn.yz('ascMMA.fascUniSrgIdHrrQtr_2007', libPathDf))$complete.df
names(test.2007)[3]='doc.id'
test.2008=completeCases.yz(xpt2r_sasLibdsn.yz('ascMMA.fascUniSrgIdHrrQtr_2008', libPathDf))$complete.df
names(test.2008)[3]='doc.id'
test.2009=completeCases.yz(xpt2r_sasLibdsn.yz('ascMMA.fascUniSrgIdHrrQtr_2009', libPathDf))$complete.df
names(test.2009)[3]='doc.id'
test.2010=completeCases.yz(xpt2r_sasLibdsn.yz('ascMMA.fascUniSrgIdHrrQtr_2010', libPathDf))$complete.df
names(test.2010)[3]='doc.id'
test.2011=completeCases.yz(xpt2r_sasLibdsn.yz('ascMMA.fascUniSrgIdHrrQtr_2011', libPathDf))$complete.df
names(test.2011)[3]='doc.id'

head(test.2010)

all=do.call(rbind,list(test.2004, test.2005, test.2006,test.2007,test.2008,test.2009,test.2010,test.2011, test.2012))
sapply(list(test.2004, test.2005, test.2006,test.2007,test.2008,test.2009,test.2010,test.2011, test.2012),names)

numFascDocId.hrrByQtr=rename.vars(ddply(all,c('hrrnum','x.x.1'),nrow),'x.x.1','.qtr.since2006')
numFascDocId.hrrByQtr=rename.vars(numFascDocId.hrrByQtr,'V1','numberOfFascDoctors')

# vec=numFascDocId.hrrByQtr[,'.qtr.since2006']%%4
# vec[which(vec==0)]=4
#yearVec=(numFascDocId.hrrByQtr[,'.qtr.since2006']-vec)/4+2006
yearVec=ceiling(numFascDocId.hrrByQtr[,'.qtr.since2006']/4)+2005
numFascDocId.HrrByQtrWithYear=data.frame(numFascDocId.hrrByQtr, year=yearVec)


nbidByHrr.2004 = data.frame(xpt2r.yz('Z:/j_scrdata/ascMMA/', "nBidByHrr_noEntCri_2004")[,c(1,2)], year=2004)
nbidByHrr.2004=rename.vars(nbidByHrr.2004, 'count','n_bid_noEntCri')

nbidByHrr.2005 = data.frame(xpt2r.yz('Z:/j_scrdata/ascMMA/', "nBidByHrr_noEntCri_2005")[,c(1,2)], year=2005)
nbidByHrr.2005=rename.vars(nbidByHrr.2005, 'count','n_bid_noEntCri')


nbidByHrr.2006 = data.frame(xpt2r.yz('Z:/j_scrdata/ascMMA/', "nBidByHrr_noEntCri_2006")[,c(1,2)], year=2006)
nbidByHrr.2006=rename.vars(nbidByHrr.2006, 'count','n_bid_noEntCri')

nbidByHrr.2007 = data.frame(xpt2r.yz('Z:/j_scrdata/ascMMA/', "nBidByHrr_noEntCri_2007")[,c(1,2)], year=2007)
nbidByHrr.2007=rename.vars(nbidByHrr.2007, 'count','n_bid_noEntCri')

nbidByHrr.2008 = data.frame(xpt2r.yz('Z:/j_scrdata/ascMMA/', "nBidByHrr_noEntCri_2008")[,c(1,2)], year=2008)
nbidByHrr.2008=rename.vars(nbidByHrr.2008, 'count','n_bid_noEntCri')

nbidByHrr.2009 = data.frame(xpt2r.yz('Z:/j_scrdata/ascMMA/', "nBidByHrr_noEntCri_2009")[,c(1,2)], year=2009)
nbidByHrr.2009=rename.vars(nbidByHrr.2009, 'count','n_bid_noEntCri')

nbidByHrr.2010 = data.frame(xpt2r.yz('Z:/j_scrdata/ascMMA/', "nBidByHrr_noEntCri_2010")[,c(1,2)], year=2010)
nbidByHrr.2010=rename.vars(nbidByHrr.2010, 'count','n_bid_noEntCri')

nbidByHrr.2011 = data.frame(xpt2r.yz('Z:/j_scrdata/ascMMA/', "nBidByHrr_noEntCri_2011")[,c(1,2)], year=2011)
nbidByHrr.2011 = rename.vars(nbidByHrr.2011, 'count','n_bid_noEntCri')

nbidByHrr.2012 = data.frame(xpt2r.yz('Z:/j_scrdata/ascMMA/', "nBidByHrr_noEntCri_2012")[,c(1,2)], year=2012)
nbidByHrr.2012 = rename.vars(nbidByHrr.2012, 'count','n_bid_noEntCri')

nbidByHrrYear=rbind(nbidByHrr.2004,nbidByHrr.2005, nbidByHrr.2006,nbidByHrr.2007,nbidByHrr.2008,nbidByHrr.2009,nbidByHrr.2010,nbidByHrr.2011,nbidByHrr.2012)
nrow(rowSetDiff.yz(nbidByHrrYear,numFascDocId.HrrByQtrWithYear, c('hrrnum','year'))$in1not2)

#numFascDocBid.HrrQtrDf=join(nbidByHrrYear,numFascDocId.HrrByQtrWithYear)


leftDf =nbidByHrrYear
by=c('hrrnum','year'),
rightDf=numFascDocId.HrrByQtrWithYear
, namedValueListForMissing=list(numberOfFascDoctors=0)

numFascDocBid.HrrQtrDf=joinLeftMissHandle.yz(leftDf =nbidByHrrYear
                           ,rightDf=numFascDocId.HrrByQtrWithYear
                           , by=c('hrrnum','year')
                           , namedValueListForMissing=list(numberOfFascDoctors=0)
)

numFascDocBid.HrrQtrDf[,'nFascDocPer10kBene']=10000*numFascDocBid.HrrQtrDf[,'numberOfFascDoctors']/numFascDocBid.HrrQtrDf[,'n_bid_noEntCri']

#save(numFascDocBid.HrrQtrDf,file='Z:/j_scrdata/ascMMA/numFascDocId.hrrByQtr.RData')
#save(numFascDocBid.HrrQtrDf,file='Z:/j_scrdata/ascMMA/numFascDocId.hrrByQtraug282014.RData')
(load(file='Z:/j_scrdata/ascMMA/numFascDocId.hrrByQtraug282014.RData'))
#(load(file='Z:/j_scrdata/ascMMA/numFascDocId.hrrByQtr.RData'))
#note what we see is treating docs who may reside outside a HRR
pdf('Z:/j_scrdata/ascMMA/result/fig/nFascDocPer10kBeneHist.pdf')
hist(numFascDocBid.HrrQtrDf[,'nFascDocPer10kBene'],100,xlab='FASC penetration: FASC surgeons per 10K beneficiaries (unit HRR-quarter)', main='Distribution of FASC penetration')
dev.off()

table(numFascDocBid.HrrQtrDf[,'.qtr.since2006'])
test=data.frame(nFascDocPer10kBene=numFascDocBid.HrrQtrDf[,'nFascDocPer10kBene'],qtr=get4quarters.yz(numFascDocBid.HrrQtrDf[,'.qtr.since2006']),.qtr.since2006=numFascDocBid.HrrQtrDf[,'.qtr.since2006'])
pdf('Z:/j_scrdata/ascMMA/result/fig/cylicalPatternOfFascPenetration.pdf')
plot(ddply(test,'.qtr.since2006',function(x){mean(x[,'nFascDocPer10kBene'])}),type='b',ylab='unweighted mean of fasc penetration', xlab='quarter since 2006')
dev.off()
