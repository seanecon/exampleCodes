
#rawTmpold <- xpt2r.yz('Z:/j_scrdata/sidFLRobotAdoption','lap_sidFl_20032009')
rawTmp <- xpt2r.yz('Z:/j_scrdata/sidFLRobotAdoption','lapalldx_fl_20032009')

rawTmp <- xpt2r.yz('Z:/j_scrdata/lapLearn','_allLap')
head(rawTmp)

nrow(rawTmpold) #only differ by 8 patients
nrow(rawTmp)
head(rawTmp)
nrow(rawTmp)/unilen.yz(rawTmp,'mdnum2.r')
ddply(rawTmp,'mdnum2.r',)
#add a cummulative column to indicate how many has been done in the past

names(rawTmp)
#the aglgorithm goes like for each record, we search for past record.

genSrgVol <- function(rawTmp){
  
 tmpout <- dlply(rawTmp,c('mdnum2.r')) #operationg physicians
 
 cumList <- list()
 for(i in 1:length(tmpout)){
  df.i <- tmpout[[i]]
  srgVolVec <- rep(NA,nrow(df.i))
  for (i in 1:nrow(df.i)){dqtr.i <- df.i[i,'dqtr'];srgVolVec[i] <- nrow(subset(df.i,dqtr < dqtr.i)) }
  df.i <- cbind(df.i,srgVol=srgVolVec)
  cumList <- lappend.yz(cumList,df.i)
 }
 newdf <- do.call(rbind,cumList) 
 return(newdf)
}

chkout <- genSrgVol(sortDf.yz(rawTmp,c('mdnum2.r','dqtr')))
head(chkout )
head(chkout[,c('mdnum1.r','srgVol')],100)
head(chkout)
estimationDf <- subset(chkout, pr1=='605' & pr2=='5421' & is.na(pr3))
head(estimationDf)
estimationDf[,'los2more'] <- (estimationDf[,'los']>=2 )
names(estimationDf)
corr(estimationDf[c('los','srgVol')])

glm(los2more ~ srgVol+factor(dshospid)+factor(atype)+factor(dqtr),data=estimationDf, family=binomial("logit"))
#only has 128 physician
unilen.yz(rawTmp,'mdnum2.r')

hist(table(rawTmp[,'mdnum2.r']),40)

subset(rawTmp,mdnum2.r==46674,sel=c('whatyear','los'))