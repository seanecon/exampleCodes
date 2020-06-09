
of.2004=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiofQE_2004'),'.qtrhrrsum.linepmt','ofExp')
of.2005=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiofQE_2005'),'.qtrhrrsum.linepmt','ofExp')
of.2012=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiofQE_2012'),'.qtrhrrsum.linepmt','ofExp')

of.2006=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiofQE_2006'),'.qtrhrrsum.linepmt','ofExp')
of.2007=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiofQE_2007'),'.qtrhrrsum.linepmt','ofExp')
of.2008=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiofQE_2008'),'.qtrhrrsum.linepmt','ofExp')
of.2009=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiofQE_2009'),'.qtrhrrsum.linepmt','ofExp')
of.2010=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiofQE_2010'),'.qtrhrrsum.linepmt','ofExp')
of.2011=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiofQE_2011'),'.qtrhrrsum.linepmt','ofExp')

tmp=ddply(rbind(of.2006,of.2007,of.2008,of.2009,of.2010,of.2011)
          ,c('hrrnum','.quartersince2006')
          ,function(x){c(ofExp=sum(x[,'ofExp']))}
)


ofExp=subset(tmp,.quartersince2006>=1 & .quartersince2006<=24)

fasc.2006=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiFascQE_2006'),'.qtrhrrsum.linepmt','fascExp')
fasc.2007=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiFascQE_2007'),'.qtrhrrsum.linepmt','fascExp')
fasc.2008=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiFascQE_2008'),'.qtrhrrsum.linepmt','fascExp')
fasc.2009=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiFascQE_2009'),'.qtrhrrsum.linepmt','fascExp')
fasc.2010=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiFascQE_2010'),'.qtrhrrsum.linepmt','fascExp')
fasc.2011=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiFascQE_2011'),'.qtrhrrsum.linepmt','fascExp')

tmp=ddply(rbind(fasc.2006,fasc.2007,fasc.2008,fasc.2009,fasc.2010,fasc.2011)
          ,c('hrrnum','.quartersince2006')
          ,function(x){c(fascExp=sum(x[,'fascExp']))}
)


fascExp=subset(tmp,.quartersince2006>=1 & .quartersince2006<=24)

ipPro.2006=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiIpProQE_2006'),'.qtrhrrsum.linepmt','ipProExp')
ipPro.2007=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiIpProQE_2007'),'.qtrhrrsum.linepmt','ipProExp')
ipPro.2008=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiIpProQE_2008'),'.qtrhrrsum.linepmt','ipProExp')
ipPro.2009=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiIpProQE_2009'),'.qtrhrrsum.linepmt','ipProExp')
ipPro.2010=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiIpProQE_2010'),'.qtrhrrsum.linepmt','ipProExp')
ipPro.2011=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiIpProQE_2011'),'.qtrhrrsum.linepmt','ipProExp')

tmp=ddply(rbind(ipPro.2006,ipPro.2007,ipPro.2008,ipPro.2009,ipPro.2010,ipPro.2011)
          ,c('hrrnum','.quartersince2006')
          ,function(x){c(ipProExp=sum(x[,'ipProExp']))}
)

ipProExp=subset(tmp,.quartersince2006>=1 & .quartersince2006<=24)

ipFac.2006=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiIpFacQE_2006'),'.qtrhrrsum.pmt.amt','ipFacExp')
ipFac.2007=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiIpFacQE_2007'),'.qtrhrrsum.pmt.amt','ipFacExp')
ipFac.2008=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiIpFacQE_2008'),'.qtrhrrsum.pmt.amt','ipFacExp')
ipFac.2009=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiIpFacQE_2009'),'.qtrhrrsum.pmt.amt','ipFacExp')
ipFac.2010=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiIpFacQE_2010'),'.qtrhrrsum.pmt.amt','ipFacExp')
ipFac.2011=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiIpFacQE_2011'),'.qtrhrrsum.pmt.amt','ipFacExp')


tmp=ddply(rbind(ipFac.2006,ipFac.2007,ipFac.2008,ipFac.2009,ipFac.2010,ipFac.2011)
          ,c('hrrnum','.quartersince2006')
          ,function(x){c(ipFacExp=sum(x[,'ipFacExp']))}
)

ipFacExp=subset(tmp,.quartersince2006>=1 & .quartersince2006<=24)



opPro.2006=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopProQE_2006'),'.qtrhrrsum.linepmt','opProExp')
opPro.2007=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopProQE_2007'),'.qtrhrrsum.linepmt','opProExp')
opPro.2008=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopProQE_2008'),'.qtrhrrsum.linepmt','opProExp')
opPro.2009=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopProQE_2009'),'.qtrhrrsum.linepmt','opProExp')
opPro.2010=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopProQE_2010'),'.qtrhrrsum.linepmt','opProExp')
opPro.2011=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopProQE_2011'),'.qtrhrrsum.linepmt','opProExp')

tmp=ddply(rbind(opPro.2006,opPro.2007,opPro.2008,opPro.2009,opPro.2010,opPro.2011)
          ,c('hrrnum','.quartersince2006')
          ,function(x){c(opProExp=sum(x[,'opProExp']))}
)

opProExp=subset(tmp,.quartersince2006>=1 & .quartersince2006<=24)

opFac.2006=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopFacQE_2006'),'.qtrhrrsum.revpmt','opFacExp')
opFac.2007=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopFacQE_2007'),'.qtrhrrsum.revpmt','opFacExp')
opFac.2008=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopFacQE_2008'),'.qtrhrrsum.revpmt','opFacExp')
opFac.2009=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopFacQE_2009'),'.qtrhrrsum.revpmt','opFacExp')
opFac.2010=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopFacQE_2010'),'.qtrhrrsum.revpmt','opFacExp')
opFac.2011=rename.vars(xpt2r.yz('Z:/j_scrdata/ascMMA','okBid_srgEpiopFacQE_2011'),'.qtrhrrsum.revpmt','opFacExp')


tmp=ddply(rbind(opFac.2006,opFac.2007,opFac.2008,opFac.2009,opFac.2010,opFac.2011)
          ,c('hrrnum','.quartersince2006')
          ,function(x){c(opFacExp=sum(x[,'opFacExp']))}
)

opFacExp=subset(tmp,.quartersince2006>=1 & .quartersince2006<=24)


allRawExp=join(join(join(join(join(opFacExp,opProExp),ipFacExp),ipProExp),fascExp),ofExp)

allRawExp[,'totExp']=allRawExp[,'fascExp']+allRawExp[,'ofExp']+allRawExp[,'opProExp']+allRawExp[,'opFacExp']+
  allRawExp[,'ipProExp']+allRawExp[,'ipFacExp']

allRawExp[,'totOpExp']=allRawExp[,'fascExp']+allRawExp[,'ofExp']+allRawExp[,'opProExp']+allRawExp[,'opFacExp']
allRawExp[,'totIpExp']=allRawExp[,'ipProExp']+allRawExp[,'ipFacExp']


#save(allRawExp, file='Z:/j_scrdata/ascMMA/result/allRawExp.RData')



load('Z:/j_scrdata/ascMMA/result/allRawExp.RData')