
of.2004=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiofQE_2004'),'.qtrhrrsum.linepmt','ofExp')
of.2005=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiofQE_2005'),'.qtrhrrsum.linepmt','ofExp')
of.2012=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiofQE_2012'),'.qtrhrrsum.linepmt','ofExp')



of.2006=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiofQE_2006'),'.qtrhrrsum.linepmt','ofExp')
of.2007=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiofQE_2007'),'.qtrhrrsum.linepmt','ofExp')
of.2008=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiofQE_2008'),'.qtrhrrsum.linepmt','ofExp')
of.2009=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiofQE_2009'),'.qtrhrrsum.linepmt','ofExp')
of.2010=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiofQE_2010'),'.qtrhrrsum.linepmt','ofExp')
of.2011=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiofQE_2011'),'.qtrhrrsum.linepmt','ofExp')

tmp=ddply(rbind(of.2004, of.2005, of.2006,of.2007,of.2008,of.2009,of.2010,of.2011, of.2012)
          ,c('hrrnum','.quartersince2006')
          ,function(x){c(ofExp=sum(x[,'ofExp']))}
)


ofExp=subset(tmp,.quartersince2006>=-7 & .quartersince2006<=28)

fasc.2004=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiFascQE_2004'),'.qtrhrrsum.linepmt','fascExp')
fasc.2005=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiFascQE_2005'),'.qtrhrrsum.linepmt','fascExp')
fasc.2012=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiFascQE_2012'),'.qtrhrrsum.linepmt','fascExp')

fasc.2006=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiFascQE_2006'),'.qtrhrrsum.linepmt','fascExp')
fasc.2007=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiFascQE_2007'),'.qtrhrrsum.linepmt','fascExp')
fasc.2008=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiFascQE_2008'),'.qtrhrrsum.linepmt','fascExp')
fasc.2009=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiFascQE_2009'),'.qtrhrrsum.linepmt','fascExp')
fasc.2010=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiFascQE_2010'),'.qtrhrrsum.linepmt','fascExp')
fasc.2011=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiFascQE_2011'),'.qtrhrrsum.linepmt','fascExp')

tmp=ddply(rbind(fasc.2004, fasc.2005, fasc.2006,fasc.2007,fasc.2008,fasc.2009,fasc.2010,fasc.2011, fasc.2012)
          ,c('hrrnum','.quartersince2006')
          ,function(x){c(fascExp=sum(x[,'fascExp']))}
)


fascExp=subset(tmp,.quartersince2006>=-7 & .quartersince2006<=28)

# ipPro.2006=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiIpProQE_2006'),'.qtrhrrsum.linepmt','ipProExp')
# ipPro.2007=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiIpProQE_2007'),'.qtrhrrsum.linepmt','ipProExp')
# ipPro.2008=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiIpProQE_2008'),'.qtrhrrsum.linepmt','ipProExp')
# ipPro.2009=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiIpProQE_2009'),'.qtrhrrsum.linepmt','ipProExp')
# ipPro.2010=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiIpProQE_2010'),'.qtrhrrsum.linepmt','ipProExp')
# ipPro.2011=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiIpProQE_2011'),'.qtrhrrsum.linepmt','ipProExp')
# 
# tmp=ddply(rbind(ipPro.2006,ipPro.2007,ipPro.2008,ipPro.2009,ipPro.2010,ipPro.2011)
#           ,c('hrrnum','.quartersince2006')
#           ,function(x){c(ipProExp=sum(x[,'ipProExp']))}
# )
# 
# ipProExp=subset(tmp,.quartersince2006>=1 & .quartersince2006<=24)
# 
# ipFac.2006=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiIpFacQE_2006'),'.qtrhrrsum.pmt.amt','ipFacExp')
# ipFac.2007=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiIpFacQE_2007'),'.qtrhrrsum.pmt.amt','ipFacExp')
# ipFac.2008=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiIpFacQE_2008'),'.qtrhrrsum.pmt.amt','ipFacExp')
# ipFac.2009=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiIpFacQE_2009'),'.qtrhrrsum.pmt.amt','ipFacExp')
# ipFac.2010=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiIpFacQE_2010'),'.qtrhrrsum.pmt.amt','ipFacExp')
# ipFac.2011=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiIpFacQE_2011'),'.qtrhrrsum.pmt.amt','ipFacExp')
# 
# 
# tmp=ddply(rbind(ipFac.2006,ipFac.2007,ipFac.2008,ipFac.2009,ipFac.2010,ipFac.2011)
#           ,c('hrrnum','.quartersince2006')
#           ,function(x){c(ipFacExp=sum(x[,'ipFacExp']))}
# )
# 
# ipFacExp=subset(tmp,.quartersince2006>=1 & .quartersince2006<=24)


opPro.2004=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopProQE_2004'),'.qtrhrrsum.linepmt','opProExp')
opPro.2005=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopProQE_2005'),'.qtrhrrsum.linepmt','opProExp')
opPro.2012=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopProQE_2012'),'.qtrhrrsum.linepmt','opProExp')

opPro.2006=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopProQE_2006'),'.qtrhrrsum.linepmt','opProExp')
opPro.2007=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopProQE_2007'),'.qtrhrrsum.linepmt','opProExp')
opPro.2008=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopProQE_2008'),'.qtrhrrsum.linepmt','opProExp')
opPro.2009=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopProQE_2009'),'.qtrhrrsum.linepmt','opProExp')
opPro.2010=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopProQE_2010'),'.qtrhrrsum.linepmt','opProExp')
opPro.2011=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopProQE_2011'),'.qtrhrrsum.linepmt','opProExp')

tmp=ddply(rbind(opPro.2004, opPro.2005, opPro.2006,opPro.2007,opPro.2008,opPro.2009,opPro.2010,opPro.2011,opPro.2012)
          ,c('hrrnum','.quartersince2006')
          ,function(x){c(opProExp=sum(x[,'opProExp']))}
)

opProExp=subset(tmp,.quartersince2006>=-7 & .quartersince2006<=28)


plot(ddply(opProExp,'.quartersince2006',function(x){sum(x[,'opProExp'])}),type='b')

plot(ddply(opFacExp,'.quartersince2006',function(x){sum(x[,'opFacExp'])}),type='b')

#there is a major drop in op FAC payment from last quarter to the first quarter of 2005

opFac.2004=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopFacQE_2004'),'.qtrhrrsum.revpmt','opFacExp')
opFac.2005=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopFacQE_2005'),'.qtrhrrsum.revpmt','opFacExp')
opFac.2012=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopFacQE_2012'),'.qtrhrrsum.revpmt','opFacExp')



nrow(opFac.2004)
nrow(opFac.2005)
ddply(opFac.2004,'.quartersince2006',function(x){sum(x[,'opFacExp'])}/1e9)
ddply(opFac.2005,'.quartersince2006',function(x){sum(x[,'opFacExp'])}/1e9)
ddply(opFac.2006,'.quartersince2006',function(x){sum(x[,'opFacExp'])}/1e9)



names(opFac.2004)
count(opFac.2005,'.quartersince2006')

sum(opFac.2004[,'opFacExp'])/1e6
sum(opFac.2005[,'opFacExp'])/1e6

opFac.2006=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopFacQE_2006'),'.qtrhrrsum.revpmt','opFacExp')
opFac.2007=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopFacQE_2007'),'.qtrhrrsum.revpmt','opFacExp')
opFac.2008=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopFacQE_2008'),'.qtrhrrsum.revpmt','opFacExp')
opFac.2009=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopFacQE_2009'),'.qtrhrrsum.revpmt','opFacExp')
opFac.2010=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopFacQE_2010'),'.qtrhrrsum.revpmt','opFacExp')
opFac.2011=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopFacQE_2011'),'.qtrhrrsum.revpmt','opFacExp')


tmp=ddply(rbind(opFac.2004, opFac.2005, opFac.2006, opFac.2007, opFac.2008, opFac.2009, opFac.2010, opFac.2011,opFac.2012)
          ,c('hrrnum','.quartersince2006')
          ,function(x){c(opFacExp=sum(x[,'opFacExp']))}
)
head(opFac.2012)

ddply(tmp,'.quartersince2006',function(x){sum(x[,'opFacExp'])/1e9})



count(opFacExp,'.quartersince2006')
opFacExp=subset(tmp,.quartersince2006>=-7 & .quartersince2006<=28)

ddply(opFacExp,'.quartersince2006',function(x){sum(x[,'opFacExp'])/1e9})

# allRawExp=join(join(join(join(join(opFacExp,opProExp),ipFacExp),ipProExp),fascExp),ofExp)
# 
# allRawExp[,'totExp']=allRawExp[,'fascExp']+allRawExp[,'ofExp']+allRawExp[,'opProExp']+allRawExp[,'opFacExp']+
#   allRawExp[,'ipProExp']+allRawExp[,'ipFacExp']
# 
# allRawExp[,'totOpExp']=allRawExp[,'fascExp']+allRawExp[,'ofExp']+allRawExp[,'opProExp']+allRawExp[,'opFacExp']
# allRawExp[,'totIpExp']=allRawExp[,'ipProExp']+allRawExp[,'ipFacExp']
#save(allRawExp, file='Z:/j_scrdata/ascMMA/result/allRawExp.RData')


allOpRawExp=join(join(join(opFacExp,opProExp),fascExp),ofExp)
allOpRawExp[,'totOpExp']=allOpRawExp[,'fascExp']+allOpRawExp[,'ofExp']+allOpRawExp[,'opProExp']+allOpRawExp[,'opFacExp']
allOpRawExp[10000,]
#save(allOpRawExp, file='Z:/j_scrdata/ascMMA/result/allOpRawExp.RData')

plot(allOpRawExp[,'.quartersince2006'],allOpRawExp[,'totOpExp'])

ddply(allOpRawExp,'.quartersince2006',function(x){unilen.yz(x,'hrrnum')})

qt.opexp=ddply(allOpRawExp, '.quartersince2006', function(x){sum(x[,'totOpExp'])/1e9})
plot(qt.opexp,type='b')




