#
#save(finalSample1,file='Z:/j_scrdata/pcaOverTreatment/finalSample1.RData')
(load(file='Z:/j_scrdata/pcaOverTreatment/finalSample1.RData'))
outdf <- count.yz(finalSample1[,c('indexyear','riskStrata')],c('indexyear','riskStrata'))

count.yz(finalSample1[,c('indexyear','riskStrata')],c('indexyear'))
names(finalSample1)

# 2004          13985       25.62         13985        25.62
# 2005          13178       24.14         27163        49.77
# 2006          13796       25.28         40959        75.04
# 2007          13622       24.96         54581       100.00


firstOnlyDx.n <-  data.frame(firstOnlyPCa.n=c(13985, 13178, 13796,13622), indexyear=seq(2004,2007))
 outdf <-  join(outdf,        firstOnlyDx.n, by='indexyear' )

outdf1 <- cbind(outdf[,c('indexyear','riskStrata')],pct=outdf[,'.count']*100/outdf[,'firstOnlyPCa.n'])

#low risk low 10 year (least likely to benefit, 2004 989 robobt and imrt 1073, for each treatment, the number of patient low risk and )

# imrt number of patnet low risk 

#high risk, high survial both
#either high risk seasei or high high survial, most likely to benefit...
#for numberator...

#all benefiticaian diagnoissed with prostate cancer taht year...
# 2004 14093
# 2005 13337
# 2006 14055
# 2007 14293
# 2008 12831
# 2009 11961

#low low
#prior standare open+ebrt 1507 946 677 530 357 325
##prior standare open+ebrt 1821 2211 2648 2927 2632 2921

#high high
#prior standare open+ebrt 1893 1353 1068 854 611 445
##prior standare imrt+robot 1984 2405 2879 3229 3174 2978 

confProp=function(n, s){
  pbar=s/n
  SE = sqrt(pbar???(1???pbar)/n)
  E = qnorm(.975)???SE
  lb=pbar ???E
  ub=pbar+E
  return(list(lb,ub))
}
options(digits=3)

confProp(c(14093,
          13337,
         14055,
          14293,
        12831,
         11961), c(1507, 946, 677, 530, 357, 325))


prop.test(c(1507, 946, 677, 530, 357, 325), c(14093,
               13337,
               14055,
               14293,
               12831,
               11961))
confProp(c(14093,
           13337,
           14055,
           14293,
           12831,
           11961), c(1821, 2211, 2648, 2927, 2632, 2921))


prop.test(c(1893, 1353, 1068, 854, 611, 445),c(14093,
                                               13337,
                                               14055,
                                               14293,
                                               12831,
                                               11961))

confProp(c(14093,
           13337,
           14055,
           14293,
           12831,
           11961), c(1893, 1353, 1068, 854, 611, 445))

confProp(c(14093,
           13337,
           14055,
           14293,
           12831,
           11961), c(1984, 2405, 2879, 3229, 3174, 2978 ))


test <- subset(outdf1,riskStrata %in% c('low | surv10Yr.low', 'mid | surv10Yr.low','hig | surv10Yr.low'))
barplot(ddply(test,'indexyear',function(x){sum(x[,'pct'])})[,'V1'])

test <- subset(outdf1,riskStrata %in% c('low | surv10Yr.low', 'low | surv10Yr.mid','low | surv10Yr.high'))
barplot(ddply(test,'indexyear',function(x){sum(x[,'pct'])})[,'V1'])


trtTypeByYrRisk <- count.yz(finalSample1,c('trtType','indexyear','riskStrata'))


chkOut <- join(trtTypeByYrRisk,firstOnlyDx.n, by='indexyear')
chkOut[,'pct'] <- 100*chkOut[,'.count']/chkOut[,'firstOnlyPCa.n']

head(chkOut)

c("low | surv10Yr.low")

riskGroups <- c("low | surv10Yr.low")
treatments <- 'hasimrtnosrg' 

stfun <- function(treatments,riskGroups){
 extractdf <- subset(chkOut,trtType %in% treatments & riskStrata %in% riskGroups ,select=c('.count','indexyear','pct'))
 out <- ddply(extractdf,'indexyear',function(x){sumCount <- sum(x[,'.count']); pct=sum(x[,'pct']); outVec =c(sumCount,pct); names(outVec)<-c('sumTrt','percent'); return(outVec)})
 return(out)
  }

stfun('hasimrtnosrg',c("low | surv10Yr.low","low | surv10Yr.mid","low | surv10Yr.high"))
stfun('hasimrtnosrg',c("hig | surv10Yr.low","mid | surv10Yr.low","low | surv10Yr.low"))
stfun('hasimrtnosrg',c("low | surv10Yr.low"))

stfun('hasrobotnoradia',c("low | surv10Yr.low","low | surv10Yr.mid","low | surv10Yr.high"))
stfun('hasrobotnoradia',c("hig | surv10Yr.low","mid | surv10Yr.low","low | surv10Yr.low"))
stfun('hasrobotnoradia',c("low | surv10Yr.low"))

#imrt, need low life expectancy also for imrt and robot and observation.

barplot(count.yz(finalSample1,'indexyear')[,'.count']/c(13985, 13178, 13796,13622)*100)



#hasrobotnoradia     observation 


