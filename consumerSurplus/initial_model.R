
#save(list=ls(),file='C:/Users/sean/Documents/consumerSurplus.sep140212.RData')
#you need to recategorize Grade, grade==9 is just too few people there.....

#you can load back 
#load(file='C:/Users/sean/Documents/consumerSurplus.sep140212.RData')

#now I want to test the multinomial model for three choices and test how the perceicved value differ across years.
#---read in the analysis data, remove 1185 people who has two mroe treatmnts
#to do
#(1) recode grade so it "9" is off
#(2) start calcuate the consumer surplus value....using crude income variable variable. 
count(raw,'trtchoice')
raw <- subset(xpt2r_1.yz('Z:/j_scrdata/consumerSurplus','anaFile_0'), !trtchoice=='twoMoreTreatments')
(tabout <- tabBy.yz(raw,'x.dxyr','trtchoice',tabNA=TRUE)$percent)
tabout <- tabBy.yz(raw,'x.dxyr','trtchoice',tabNA=F)$percent

lwd=2
pdf(file='C:/Dropbox/paper/techDiffPcaCS/figure/trtChoiceTrend.pdf')
plot(tabout[,'roboticSrg'],type='b',ylim=c(0,65)
     ,xlab='year of diagnosis'
     ,ylab='percent (%)'
     ,lty=1
     ,lwd=lwd
     , xaxt = "n"
     , main='Prostate Cancer Treatment Choice Trend')
lines(tabout[,'openSrg'],type='b',lty=2,lwd=lwd)
lines(tabout[,'radiation'],type='b',lty=3,lwd=lwd)
lines(tabout[,'observation'],type='b',lty=4,lwd=lwd)
      axis(1, 1:7,seq(2001,2007))
legend('topleft',c('robotic surgery','open surgery','radiation','observation'),lty=seq(4),lwd=2, seg.len=3.5)
dev.off()
#now we need to chop off the twoMoreTreatment. because they are rare anayway

keptVars <- c('trtchoice','x.dxyr','x.dxmon','x.dxage','x.grade','x.ses','ctpci00','chrlson')
set.seed(1)
test <- raw[sample(seq(nrow(raw)), 5000),keptVars]
test <- raw[,keptVars]
nrow(test)
rm(raw)
table()
matrix(table(raw[,'trtchoice'])/nrow(raw), nrow=1, byrow=T)

#/*Census 2000 Per Capita Income for census tract based on the 2000 Census Bureau survey.*/
  
test <- rename.vars(test
            ,c('trtchoice','x.dxyr','x.dxmon','x.dxage','x.grade','x.ses','ctpci00','chrlson'), 
             c('trt.choice','dx.yr','dx.mon','dx.age','grade','ses','ctpci00','chrlson')
            )

#generate variables
#1 log income
#2 break chrlson into 0, 1 ,2 ,3+
#4, age into 66-70, 70-75, 76-79 80-84, 85+
# we do not need ses. break ses into terciles based on raw data (or just do not need ses score)

regroupData.consumerSurplus <- function(test){
  #for chrlson score if NA, it means 0
  chrlgrp.1 <- grpNumVarCuts.yz(test[,'chrlson'], c(1,2,3))$groupedVec
  chrlson.new <- replaceValJoin.yz(chrlgrp.1,list(c('[-Inf,   1)',NA),'[   1,   2)','[   2,   3)','[   3, Inf]'),c('0','1','2','3more'))[,'newVal']
  #for age 
  dx.age.1 <- grpNumVarCuts.yz(test[,'dx.age'], c(70,75,80))$groupedVec
  dx.age.new <- replaceValJoin.yz(dx.age.1,c('[-Inf,  70)', '[  70,  75)', '[  75,  80)', '[  80, Inf]'),c('66.69','70.74','75.79','80more'))[,'newVal']
  tmp <- data.frame(dx.age=dx.age.new, comorbGrp=chrlson.new)
  outdf <- cbind(deletecol.yz(test,c('dx.age','chrlson')),tmp)
  
#remove ses
  outdf[,'ses'] <- NULL
  outdf[,'log.ctpci00'] <- log(outdf[,'ctpci00'])
  outdf[,'ctpci00'] <- NULL
  return(outdf)
}

getLongData <- function(test, catIndepVars=c('dx.yr','dx.age','grade','comorbGrp')){
test.1 <- regroupData.consumerSurplus(test)
anaDummy <- dummyMat.yz(test.1,catIndepVars,)
anaFile <- cbind(trt.choice=test.1[,'trt.choice'], log.ctpci00=test.1[,'log.ctpci00'], anaDummy)
anaFile.long <- mlogit.data(anaFile,varying=NULL, shape='wide', choice='trt.choice')
return(anaFile.long)
}

testOrig <- test
test <- testOrig
test <- subset(testOrig,dx.yr=='2001')
names(test)
col.type.yz(test)

studyYrs='2002'


studyYrs <- as.character(seq(2003,2007))
adfSubYr <- getLongData(subset(test,dx.yr %in% studyYrs),catIndepVars=c('dx.age','grade','comorbGrp'))
mlogitFit <- mlogit(trt.choice ~ 0 |log.ctpci00+dx.agedummy.70.74+dx.agedummy.75.79+dx.agedummy.80more+gradedummy.2+gradedummy.3+gradedummy.9+comorbGrpdummy.1+comorbGrpdummy.2+comorbGrpdummy.3more|0, data =adfSubYr)
print(xtable(summary(mlogitFit)$CoefTable,caption='2003-2007 post adoption period estiamtes'))

studyYrs <- as.character(seq(2001,2002))
adfSubYr <- getLongData(subset(test,dx.yr %in% studyYrs),catIndepVars=c('dx.age','grade','comorbGrp'))
mlogitFit <- mlogit(trt.choice ~ 0 |log.ctpci00+dx.agedummy.70.74+dx.agedummy.75.79+dx.agedummy.80more+gradedummy.2+gradedummy.3+gradedummy.9+comorbGrpdummy.1+comorbGrpdummy.2+comorbGrpdummy.3more|0, data =adfSubYr)
summary(mlogitFit)


getwd()


# names(anaFile.long)
# head(anaFile.long)



#we have no choice alternative variable at the momment and a|b|c
# a and c are for choice varying variables, a is assuming coeffect do nto change over choice, c are for choiceing specific choice varyuing variables.

mlogitFit <- mlogit(trt.choice ~ 0 |log.ctpci00+dx.yrdummy.2002+dx.yrdummy.2003+dx.yrdummy.2004+dx.yrdummy.2005+dx.yrdummy.2006+dx.yrdummy.2007+dx.agedummy.70.74+dx.agedummy.75.79+dx.agedummy.80.84+dx.agedummy.85more+gradedummy.2+gradedummy.3+gradedummy.9+comorbGrpdummy.1+comorbGrpdummy.2+comorbGrpdummy.3more|0, data =anaFile.long)


summary(mlogitFit)
coefmat <- matrix(coef(mlogitFit), ncol=4, byrow=T)
colnames(coefmat) <- c('openSrg','radiation', 'roboticSrg', 'twoMoreTreatments')
rownames(coefmat) <- c('intercept','log.ctpci00',colnames(anaDummy))
(coefmat)
#income has a strong effect
#now we need to chop off the twoMoreTreatment. because they are rare anayway

#2001 dx
nrow(anaFile.long.2001)
anaFile.long.2001 <- subset(anaFile.long,  dx.yrdummy.2002==0 & dx.yrdummy.2003==0 &  dx.yrdummy.2004==0 &  dx.yrdummy.2005==0 &  dx.yrdummy.2006==0 &  dx.yrdummy.2007==0)
mlogitFit.2001 <- mlogit(trt.choice ~ 0 |log.ctpci00|0, data =anaFile.long.2001)
head(anaFile.long.2001)

