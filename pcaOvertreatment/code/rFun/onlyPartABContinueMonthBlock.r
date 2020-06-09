
onlyPartABContinueMonthBlock <- function(idHmoEntDeathYrMon.df #contains ID, Hmo and Ent variables
                                         ,keyVn
                                         ,cutoff.numOfmons #a block should have AB only for a period with >= cutoff.numOfmons months
                                         ,deathYrVn #'med.dody'
                                         ,deathMonVn #'med.dodm'
                                         ,hmoAryVns #i.e., hmo1 hmo2.....,hmoXXX (xxx is the last monthIndex, right now it is xxx=288 is for dec 2009)
                                         ,valueOfInterest.hmo #i.e., single value in HMO variable indicates no HMO it is character value '0'       
                                         ,entAryVns #i.e., hmo1 hmo2.....,entXXX (xxx is the last monthIndex, right now it is xxx=288 is for dec 2009)
                                         ,valueOfInterest.ent #'3' is for A and B
                                         ,lastCheckMonAryIndex #this vector tells for each row of idHmoEntDeathYrMon.df we need to check up to which month
                                        
                                         ,claims.lastYr
                                         ,claims.lastMon
                                         
                                         ){
deathMonAryIndex <-calYrMonToSeerMonAryIndex(cbind(as.numeric(idHmoEntDeathYrMon.df[,deathYrVn]),as.numeric(idHmoEntDeathYrMon.df[,deathMonVn]))); #NA means death unobserved at the last specified death obs date (usually aligned with last claims date)
lastCheckMonAryIndex <- deathMonAryIndex
#death obs last date and claims obs last date are forced to be the same
aryIndex.lastdeathmon <- calYrMonToSeerMonAryIndex(matrix(c(claims.lastYr,claims.lastMon),nrow=1))
lastCheckMonAryIndex[is.na(lastCheckMonAryIndex)] <- aryIndex.lastdeathmon
#the following block solves the search of continous block  
foundBlocksList <- list()
for(i in 1:nrow(idHmoEntDeathYrMon.df)){
  rleDf.hmo <- rleStartEndLoc.yz(idHmoEntDeathYrMon.df[i,hmoAryVns[1:lastCheckMonAryIndex[i]]])
  okHmoContinuousBlockSE <- subset(rleDf.hmo,value==valueOfInterest.hmo & runLength>=cutoff.numOfmons)[,c('startLoc','endLoc')]
  
  rleDf.ent <- rleStartEndLoc.yz(idHmoEntDeathYrMon.df[i,entAryVns[1:lastCheckMonAryIndex[i]]])
  okEntContinuousBlockSE <- subset(rleDf.ent,value==valueOfInterest.ent & runLength>=cutoff.numOfmons)[,c('startLoc','endLoc')] 
  
  if (nrow(okHmoContinuousBlockSE)==0 | nrow(okEntContinuousBlockSE)==0) {
    longEnoughBlocks <- data.frame(start=NA,end=NA)
  } else{
    #cartesian join
    tmpIntervalDf <- merge(okHmoContinuousBlockSE,okEntContinuousBlockSE,by=NULL)
    
    onlyABBlocks <- intervalOverlapChk.yz(as.matrix(okHmoContinuousBlockSE),as.matrix(okEntContinuousBlockSE))
    
    #now check the onlyAB block is long enough to ensure we may find at least one index month
    longEnoughBlocks <- onlyABBlocks[which((onlyABBlocks[,'end'] - onlyABBlocks[,'start']+1)>cutoff.numOfmons),,drop=F]
    if(nrow(longEnoughBlocks)==0){longEnoughBlocks <- data.frame(start=NA,end=NA)}
  }
  # keyVn <- 'hicbic'
  foundBlocks.i <- data.frame(idHmoEntDeathYrMon.df[i,keyVn], longEnoughBlocks)
  names(foundBlocks.i)[1] <- keyVn
  foundBlocksList <- lappend.yz(foundBlocksList,foundBlocks.i)    
}
foundBlocksDf <- do.call(rbind,foundBlocksList)
return(foundBlocksDf)
}