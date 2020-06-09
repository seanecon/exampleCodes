

,
,#office
officeTotalCost=c( 7955160494
, 8123806741
,8346517627
,8611210458)

pdf('C:/Dropbox/K/ascMMA/result/fig/oftotalCost.pdf')
plot(seq(2007,2010),officeTotalCost/10e6,type='b',ylab='Office visit annual expenditures ($ million)', ylim=c(750,900))
dev.off()


officeVol=c(6206024, 6126727 , 6109315,6165819)
pdf('C:/Dropbox/K/ascMMA/result/fig/ofvol.pdf')
plot(seq(2007,2010),c(6206024, 6126727 , 6109315,6165819)/10e3,type='b',ylab='Office-visit annual volume (thousand)', ylim=c(600,630))
dev.off()

 
#op total cost
pdf('C:/Dropbox/K/ascMMA/result/fig/optotalCost.pdf')
plot(seq(2007,2010),c(7532251790+1104684061,8060904790+1112299181,8923939633+ 1199611901,9532844088+1284992227)/10e6,type='b',ylab='Outpatient annual expenditures ($ million)', ylim=c(850,1100))
dev.off()
pdf('C:/Dropbox/K/ascMMA/result/fig/asctotalCost.pdf')
plot(seq(2007,2010),c(759951541, 798600416,844862236, 896743894)/10e6,type='b',ylab='ASC annual expenditures ($ million)', ylim=c(70,90))
dev.off()

pdf('C:/Dropbox/K/ascMMA/result/fig/iptotalCost.pdf')
plot(seq(2007,2010),c(27718699334
                      ,28997977167
                      ,29893526913
                      ,30707686939
)/10e6,type='b',ylab='Inpatient annual expenditures ($ million)', ylim=c(2700,3200))
dev.off()


pdf('C:/Dropbox/K/ascMMA/result/fig/opvol.pdf')
plot(seq(2007,2010),c(4824704, 4791059, 4793447,4840331)/10e3,type='b',ylab='Outpatient annual volume (thousand)')
dev.off()

pdf('C:/Dropbox/K/ascMMA/result/fig/ascvol.pdf')
plot(seq(2007,2010),c(717928,726698,733627,736238)/10e3,type='b',ylab='ASC annual volume (thousand)')
dev.off()

pdf('C:/Dropbox/K/ascMMA/result/fig/ipvol.pdf')
plot(seq(2007,2010),c(1520869 , 1705325 ,1725466 ,1768622)/10e3,type='b',ylab='Inpatient annual volume (thousand)')
dev.off()



#ip total cost
#asc totoal cost
#office  total cost

