nbidByHrr.2006 = xpt2r.yz('Z:/j_scrdata/ascMMA/', "nBidByHrr_noEntCri_2006")[,c(1,2)]
nbidByHrr.2006=rename.vars(nbidByHrr.2006, 'count','n_bid_noEntCri')

hrrSet=unique(nbidByHrr.2006[,'hrrnum']) #306 HRR

#2006
hrrQtrDf=expandGrid.yz(hrrSet, seq(4))
names(hrrQtrDf)=c('hrrnum','.quartersince2006')

nbidByHrr = xpt2r.yz('Z:/j_scrdata/ascMMA/', "nBidByHrr_noEntCri_2006")[,c(1,2)]
nbidByHrr=rename.vars(nbidByHrr, 'count','n_bid_noEntCri')

nEncVol = xpt2r.yz('Z:/j_scrdata/ascMMA/', "hrrEncVol_2006")

hrrVol.2006=join(join(hrrQtrDf, nEncVol, by=c('hrrnum','.quartersince2006'), type='left'),nbidByHrr, by='hrrnum', type='left')


#2007
hrrQtrDf=expandGrid.yz(hrrSet, seq(5,8))
names(hrrQtrDf)=c('hrrnum','.quartersince2006')

nbidByHrr = xpt2r.yz('Z:/j_scrdata/ascMMA/', "nBidByHrr_noEntCri_2007")[,c(1,2)]
nbidByHrr=rename.vars(nbidByHrr, 'count','n_bid_noEntCri')

nEncVol = xpt2r.yz('Z:/j_scrdata/ascMMA/', "hrrEncVol_2007")

hrrVol.2007=join(join(hrrQtrDf, nEncVol, by=c('hrrnum','.quartersince2006'), type='left'),nbidByHrr, by='hrrnum', type='left')

#2008
hrrQtrDf=expandGrid.yz(hrrSet, seq(9,12))
names(hrrQtrDf)=c('hrrnum','.quartersince2006')

nbidByHrr = xpt2r.yz('Z:/j_scrdata/ascMMA/', "nBidByHrr_noEntCri_2008")[,c(1,2)]
nbidByHrr=rename.vars(nbidByHrr, 'count','n_bid_noEntCri')

nEncVol = xpt2r.yz('Z:/j_scrdata/ascMMA/', "hrrEncVol_2008")

hrrVol.2008=join(join(hrrQtrDf, nEncVol, by=c('hrrnum','.quartersince2006'), type='left'),nbidByHrr, by='hrrnum', type='left')

#2009
hrrQtrDf=expandGrid.yz(hrrSet, seq(13,16))
names(hrrQtrDf)=c('hrrnum','.quartersince2006')

nbidByHrr = xpt2r.yz('Z:/j_scrdata/ascMMA/', "nBidByHrr_noEntCri_2009")[,c(1,2)]
nbidByHrr=rename.vars(nbidByHrr, 'count','n_bid_noEntCri')

nEncVol = xpt2r.yz('Z:/j_scrdata/ascMMA/', "hrrEncVol_2009")

hrrVol.2009=join(join(hrrQtrDf, nEncVol, by=c('hrrnum','.quartersince2006'), type='left'),nbidByHrr, by='hrrnum', type='left')

#2010
hrrQtrDf=expandGrid.yz(hrrSet, seq(17,20))
names(hrrQtrDf)=c('hrrnum','.quartersince2006')

nbidByHrr = xpt2r.yz('Z:/j_scrdata/ascMMA/', "nBidByHrr_noEntCri_2010")[,c(1,2)]
nbidByHrr=rename.vars(nbidByHrr, 'count','n_bid_noEntCri')

nEncVol = xpt2r.yz('Z:/j_scrdata/ascMMA/', "hrrEncVol_2010")

hrrVol.2010=join(join(hrrQtrDf, nEncVol, by=c('hrrnum','.quartersince2006'), type='left'),nbidByHrr, by='hrrnum', type='left')

#2011
hrrQtrDf=expandGrid.yz(hrrSet, seq(21,24))
names(hrrQtrDf)=c('hrrnum','.quartersince2006')

nbidByHrr = xpt2r.yz('Z:/j_scrdata/ascMMA/', "nBidByHrr_noEntCri_2011")[,c(1,2)]
nbidByHrr=rename.vars(nbidByHrr, 'count','n_bid_noEntCri')

nEncVol = xpt2r.yz('Z:/j_scrdata/ascMMA/', "hrrEncVol_2011")



hrrPeneDf=sortDf.yz(do.call(rbind,list(hrrVol.2006,hrrVol.2007,hrrVol.2008,hrrVol.2009,hrrVol.2010,hrrVol.2011)),'hrrnum')

hrrPeneDf=na2valDf.yz(hrrPeneDf,'.encountervol',list(0))
hrrPeneDf[,'hrrPene10kRate']=10000*hrrPeneDf[,'.encountervol']/hrrPeneDf[,'n_bid_noEntCri']
hrrPeneDf=rename.vars(hrrPeneDf,'.quartersince2006',".qtr.since2006")


pdf('Z:/j_scrdata/ascMMA/result/fig/fascPenetrationHrrQuarter.pdf')
hist(hrrPeneDf[,'hrrPene10kRate'],100, xlab='FASC penetration: FASC medical encounters per 10K beneficiaries (unit HRR-quarter)', main='Distribution of FASC penetration')
dev.off()

tiff('Z:/j_scrdata/ascMMA/result/fig/fascPenetrationHrrQuarter.tiff')
hist(hrrPeneDf[,'hrrPene10kRate'],100, xlab='FASC penetration: FASC medical encounters per 10K beneficiaries (unit HRR-quarter)', main='Distribution of FASC penetration')
dev.off()



