
# from add_beto.sas %genxpt(ascmma.costByBeto)

beto.cost=xpt2r.yz('Z:/j_scrdata/ascMMA','costByBeto')

#times 5 (20% sample) and change into billion
beto.cost[,'.totalexp.times5']=beto.cost[,'.totalexp']*5/1e9 #billion

plotdf.proc=subset(beto.cost,service.type=='PROCEDURES')[,c('.qtrsince2008','.totalexp.times5')]
plotdf.imag=subset(beto.cost,service.type=='IMAGING')[,c('.qtrsince2008','.totalexp.times5')]
plotdf.othe=subset(beto.cost,service.type=='OTHER')[,c('.qtrsince2008','.totalexp.times5')]
plotdf.eval=subset(beto.cost,service.type=='EvalManage')[,c('.qtrsince2008','.totalexp.times5')]
plotdf.dura=subset(beto.cost,service.type=='DURABLE_MED_EQU')[,c('.qtrsince2008','.totalexp.times5')]
plotdf.test=subset(beto.cost,service.type=='TESTS')[,c('.qtrsince2008','.totalexp.times5')]

tiff('Z:/j_scrdata/ascMMA/result/fig/betoBreakDown.tif')
plot(plotdf.proc, ylim=c(0,5),type='b',pch=1,ylab=c('billion dollars'), xlab='quarter since 2008')
points(plotdf.test,pch=2)
lines(plotdf.test)
points(plotdf.imag,pch=3)
lines(plotdf.imag)
points(plotdf.eval,pch=4)
lines(plotdf.eval)
points(plotdf.dura,pch=5)
lines(plotdf.dura)
abline(v=0,lty=2)
legend('topleft',c('procedure','test','imaging','evalution/management','durable equipment'),pch=c(1,2,3,4,5))
dev.off()
#lines(plotdf.othe)










