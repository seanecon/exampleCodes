


hrrMeanComorb=xpt2r.yz("Z:/j_scrdata/ascMMA/",'ran_meanComorbHrr_2010')

pdf('C:/Dropbox/K/ascMMA/result/fig/02262014/hrrcomorb2010.pdf')
hist(hrrMeanComorb[,'.avg.comorbsum'],100, main='distribution of average comorbidity in 306 hrrs (year 2010)')
dev.off()


fascVol=readTableCols.yz('Z:/j_scrdata/ascMMA/fasc_encDayVol_qtr.csv')
plot(as.numeric(fascVol[,'X_qtr_since2006']),as.numeric(fascVol[,'COUNT'])*5/1e6,type='b',ylim=c(0,1.2),xlab='Quarter since January 2006'
    ,ylab='Numer of encounters (million)')

opVol=readTableCols.yz('Z:/j_scrdata/ascMMA/op_encDayVol_qtr.csv')
plot(as.numeric(opVol[,'X_qtr_since2006']),as.numeric(opVol[,'COUNT'])*5/1e6,type='b',ylim=c(0,3.2),xlab='Quarter since January 2006'
     ,ylab='Numer of encounters (million)')

ofVol=readTableCols.yz('Z:/j_scrdata/ascMMA/of_encDayVol_qtr.csv')

pdf('C:/Dropbox/K/ascMMA/result/fig/02262014/encounterVol.pdf')
plot(as.numeric(ofVol[,'X_qtr_since2006']),as.numeric(ofVol[,'COUNT'])*5/1e6,type='b',ylim=c(0,6),xlab='Quarter since January 2006'
     ,ylab='Numer of encounters (million)',pch=1)
lines(as.numeric(opVol[,'X_qtr_since2006']),as.numeric(opVol[,'COUNT'])*5/1e6,type='b',pch=2)
lines(as.numeric(fascVol[,'X_qtr_since2006']),as.numeric(fascVol[,'COUNT'])*5/1e6,type='b',pch=3)
legend('topleft',c("office", 'outpatient','fasc'), pch=seq(3),cex=0.65)
dev.off()

# 4897840/4*365 enligable person day in a quarter

entVol=rep(c(4897840,4808497,4733505,4712034,4808342,4848712)/4,each=4)










#this function
#plotdatalist is the list contianing all the dataframes
#each list element contains a series (within each serise there are serveral discrete groups)
#groupvn is the variable tells groups
#lowbdvn is the variable containin lower bd
#upbdvn is the variable containing upbd
#meanvn is the variable containing mean vect




plotdatalist <- list()

meanvec <- as.integer(ofVol[,'COUNT'])/1e6
half.ci.dev=rep(NA,20)
for(i in 1:20){
  rate.hat.i=as.numeric(ofVol[i,'COUNT'])/entVol[i]
  sd.i=sqrt(entVol[i]*rate.hat.i*(1-rate.hat.i))
  half.ci.dev[i]=1.96*sd.i/1e6
}
sdvec <- half.ci.dev
lowbdvec <- (meanvec-sdvec)
upbdvec <- (meanvec+sdvec)
ofDf <-  data.frame(mean=meanvec,liw=sdvec, uiw=sdvec)

meanvec <- as.integer(opVol[,'COUNT'])/1e6
half.ci.dev=rep(NA,20)
for(i in 1:20){
  rate.hat.i=as.numeric(opVol[i,'COUNT'])/entVol[i]
  sd.i=sqrt(entVol[i]*rate.hat.i*(1-rate.hat.i))
  half.ci.dev[i]=1.96*sd.i/1e6
}
sdvec <- half.ci.dev
lowbdvec <- (meanvec-sdvec)
upbdvec <- (meanvec+sdvec)
opDf <-  data.frame(mean=meanvec,liw=sdvec, uiw=sdvec)

meanvec <- as.integer(fascVol[,'COUNT'])/1e6
half.ci.dev=rep(NA,20)
for(i in 1:20){
  rate.hat.i=as.numeric(fascVol[i,'COUNT'])/entVol[i]
  sd.i=sqrt(entVol[i]*rate.hat.i*(1-rate.hat.i))
  half.ci.dev[i]=1.96*sd.i/1e6
}
sdvec <- half.ci.dev
lowbdvec <- (meanvec-sdvec)
upbdvec <- (meanvec+sdvec)
fascDf <-  data.frame(mean=meanvec,liw=lowbdvec, uiw=upbdvec)

plotdatalist <- list(ofDf, opDf, fascDf)
names(plotdatalist) <- c('office visist','out patient','freestanding ASC')

meanvn <- "mean"
upbdvn <-  "uiw"
lowbdvn <- "liw"
lowbdylim <-  0
upbdylim <-  6
xlabel <- "quarter"
ylabel <- "volume"
maintitle <-  "test main title"

library (gplots)
series.names <- names(plotdatalist)
n.series <- length(plotdatalist)
i=1
jit.size=0.08
for (i in 1:n.series) {
  jit=jit.size*(i-1)
  use.df <- plotdatalist[[i]]
  yrange <- max(use.df[,lowbdvn])- min(use.df[,lowbdvn])
  if (i==1) {
    #rint(i)
    plot(x=1:20+jit, y=use.df[,meanvn], xlim = c(0.5,nrow(use.df)+0.5), ylim = c(lowbdylim, upbdylim),type="b", main = maintitle, ylab=ylabel, lty = i, xlab=xlabel)
    plotCI(x=1:20+jit, y=use.df[,meanvn], use.df[,upbdvn]+0.01, use.df[,lowbdvn]-0.01,  gap = 0, add=T, lwd=1,uiw=50, liw=50)
    
  }
  else {
    #print(i)
    lines(x=1:4+jit, y=use.df[,meanvn], lty = i)
    plotCI(x=1:4+jit, y=use.df[,meanvn], use.df[,upbdvn], use.df[,lowbdvn]-0.05, xaxt ="n", gap=0,add=T)
  }
}
legend(0.4,0.5,legend=names(plotdatalist),lty=1:n.series,cex=1)


