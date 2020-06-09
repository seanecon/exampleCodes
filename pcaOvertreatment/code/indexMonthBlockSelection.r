
sourceFileOneByOne.yz('Q:/SEERdatamani')
sourceFileOneByOne.yz('C:/Dropbox/K/pcaOvertreatment/code/rFun')


#note in coding up deathindicator and followup, we used dec 31, 2009 as the last day.
#so any observed med.
demoVars <- c('hicbic','med.dodm','med.dodd','med.dody','sex','race','birthyr','birthd','birthm')
outcomeVars <- c('deathindicator','followup')
hmoVars <- paste('hmo',seq(288),sep='')
entVars <-  paste('ent',seq(288),sep='')
keptVars <-c(outcomeVars,demoVars, hmoVars, entVars)
#288 #dec 2009, 1 is 1986 Jan

valueOfInterest.hmo <- '0'
valueOfInterest.ent <- '3'
deathYrVn <- 'med.dody'
deathMonVn <- 'med.dodm'
deathDayVn <- 'med.dodd'
hmoAryVns <-  paste('hmo',seq(288),sep='')
entAryVns <-  paste('ent',seq(288),sep='')
#clear past warnings
assign("last.warning", NULL, envir = baseenv())

okIndexTimePoints.list <- list()
for (i in 1:1000){
cat('it is processing',i,'batch of 1000 batches','\n')
test <- xpt2r_1.yz('V:/seanyz/temp/sumdenomncfiles',paste('batch_short_',i,sep=''))[,keptVars]
#search for these month blocks in which only PartA+B and seer claims file has a minNumOfMonDeathFollowYr
candidateBlocks <- onlyABMinLenMonthBlock(
    test
  ,minNumOfHistoryMonths=12 #for comorbidity
  ,minNumOfFollowUpMonths=10*12 #for follow up
  ,birthYrVn='birthyr'
  ,birthMonVn='birthm'
  ,deathEventIndicatorVn='deathindicator'
  ,hmoAryVns
  ,valueOfInterest.hmo #i.e., single value in HMO variable indicates no HMO it is character value '0'
  ,entAryVns #i.e., hmo1 hmo2.....,entXXX (xxx is the last monthIndex, right now it is xxx=288 is for dec 2009)
  ,valueOfInterest.ent #'3' is for A and B
  ,deathYrVn #'med.dody'
  ,deathMonVn #'med.dodm'
  ,deathDayVn #'med.dodd'
  , keyVn='hicbic' #ID variable name in the data
  , claims.lastYr=2009
  , claims.lastMon=12
  , claims.firstYr=1986 #288 is for dec 2009, 1 is for 1986 Jan
  , claims.firstMon=1
  )
okIndexTimePoints.list <- lappend.yz(okIndexTimePoints.list,candidateBlocks)
}

indexMonthIntervalDf <- do.call(rbind,okIndexTimePoints.list)
# file.remove(file='Z:/j_rawdata/seer09/nonCancerEntitlement/indexMonthIntervalDf.RData')
save(indexMonthIntervalDf,file='Z:/j_rawdata/seer09/nonCancerEntitlement/indexMonthIntervalDf.RData')
(load(file='Z:/j_rawdata/seer09/nonCancerEntitlement/indexMonthIntervalDf.RData'))
write.dbf(indexMonthIntervalDf, file='Z:/j_scrdata/pcaOverTreatment/indexMonthIntervalDf.dbf')
# from 699309 uniquie observations in sumdenomnc, end up with 120160 unique obsrvaton and 125344 unique ok time blocks.

#if you use batch_short_4, you can run the following check and it makes sense
# candidateBlocks[1,]
# subset(test,hicbic=='004328470A')[,c('ent168','ent169','ent170','ent235','ent236','ent237')]
# subset(test,hicbic=='004328470A')[,c('hmo168','hmo169','hmo170','hmo235','hmo236','hmo237')]
# subset(test,hicbic=='004328470A')[,c('ent132','ent133','ent134','ent235','ent236','ent237')]
# subset(test,hicbic=='004328470A')[,c('hmo132','hmo133','hmo134','hmo235','hmo236','hmo237')]
#rle(subset(test,hicbic=='004328470A')[,paste('ent',seq(133,236),sep='')])
#rle(subset(test,hicbic=='004328470A')[,paste('hmo',seq(133,236),sep='')])

# subset(candidateBlocks,hicbic=='004518095A')
# subset(test,hicbic=='004518095A')[,c('ent89','ent90','ent91','ent196','ent197','ent198')]
# subset(test,hicbic=='004518095A')[,c('hmo89','hmo90','hmo91','hmo196','hmo197','hmo198')]
# rle(subset(test,hicbic=='004518095A')[,paste('ent',seq(90,197),sep='')])
# rle(subset(test,hicbic=='004518095A')[,paste('hmo',seq(90,197),sep='')])

#next only very few people have more than one eligible blocks.

#so just use the sample which is unique


hicbicFreq <- count(indexMonthIntervalDf,'hicbic')
oneblock.hicbic <- subset(hicbicFreq,freq==1)[,'hicbic',drop=F]
oneBlockSample <- join(oneblock.hicbic,indexMonthIntervalDf)


set.seed(1) #index time is randomly selected. so set seed
randomIndexTime.tmp <- adply(oneBlockSample[,c('hicbic','start','end')],1,function(x){sample(seq(x[,'start'],x[,'end'],by=1),1)})
randomIndexTime <- data.frame(randomIndexTime.tmp, seerMonAryIndexToCalYrMon(randomIndexTime.tmp[,'V1'])) #V1 is random sampled seer month location. 1 is Jan 1986....288 is Dec 1009
rm(randomIndexTime.tmp)
randomIndexTime <- rename.vars(randomIndexTime,c('calendar.year','calendar.month'),c('indexyear','indexmonth'))                            
save(randomIndexTime,file='Z:/j_scrdata/pcaOverTreatment/randomIndexTime.RData')
write.dbf(randomIndexTime[,c('hicbic','indexyear','indexmonth')], file='Z:/j_scrdata/pcaOverTreatment/randomIndexTime.dbf')
#then you can go to sas to read the data and start getting their demographic and comorbidity.
#go to sas to get comorbidity. i found the I could not _osym....need to figure out why.                              

                              

