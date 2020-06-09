#minNumOfHistoryMonths is the minimum partAB only time window in the past (including current index month)
#minFollowUpMonths is the minum partAB only time windown in the future (indlucidng current index month)

#the algorithm does the following.
#step 1. birth date check. we remove these who were born so late that even if their first 66 yearold birthdate cannot be followed by theminNumOfFollowUpMonths for death (note, we mannually aligned death date to last date of claims; note we choose 66 because we need 12 month for comorbidity).

#step 2. we find out only-part-AB month blocks which are at least 12 months long. One person might have multiple such time blocks
#step 3. make sure the remaining time block's end month will be theminNumOfFollowUpMonths away from end of claims window to ensure follow up. a time block does not meet this criteron will be removed.
#step 4. make sure the start month of time blocks will have the second month (we choose second month to ensure the search window is 12 months for index and prior combordity, not 11.xxx) the person turns age 66, 

onlyABMinLenMonthBlock <- function(
   test
  ,minNumOfHistoryMonths=12 #for comorbidity
  ,minNumOfFollowUpMonths=10*12 #for follow up
  ,birthYrVn='birthyr'
  ,birthMonVn='birthm'
  ,deathEventIndicatorVn='deathindicator' #this variable is generated in sas and is generted assuming last day observing death is the same as last day of claims (2009 dec 31), internally, changed med.dodd med.dodm med.dody later than (i.e., died after dec 31, 2009) into dec 31, 2009 inside this function. 
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
  , claims.firstYr=1986 #288 is dec 2009, 1 is 1986 Jan
  , claims.firstMon=1
  ){
  #since we used dec 31 2009, so any observed death date later than that date is regarded as missing 
  #deathindicator is generated in sas and corresponds to claims.lastYr and claims.lastMon
  #we aligned last observed date for death event to the same as claims last date in SAS
  test[which(test[,deathEventIndicatorVn]==0),c(deathMonVn,deathDayVn,deathYrVn)] <- c(NA,NA,NA)
  #reach 65 before lastFollowUp-minFollowUp+1-66
  #66 not 65 because we want one year comobidit
  minFollowUp.ok <- unname(unlist(test[,birthYrVn])) < (claims.lastYr-(minNumOfFollowUpMonths/12)+1-66)
  df.okFollowup <- test[which(minFollowUp.ok),]
  if (nrow(df.okFollowup)==0) {
    idBlocks.final <- NULL #this is the final output contains ID and start and end ary index of month blocks
    cat('no person can be followed in SEER claims for ', minNumOfFollowUpMonths/12,'years', '\n')
  } else{
    cat('search for continous only Part A and B month blocks that are equal or longer than ', minNumOfHistoryMonths, 'months', '\n')
    chosenMonBlocks.all <-  onlyPartABContinueMonthBlock( df.okFollowup #contains ID, Hmo and Ent variables
                                                         ,keyVn
                                                         ,minNumOfHistoryMonths #a block short than this number is definitely ineligle because it is impossble to find even one month as index month
                                                         ,deathYrVn #'med.dody'
                                                         ,deathMonVn #'med.dodm'
                                                         ,hmoAryVns #i.e., hmo1 hmo2.....,hmoXXX (xxx is the last monthIndex, right now it is xxx=288 is for dec 2009)
                                                         ,valueOfInterest.hmo #i.e., single value in HMO variable indicates no HMO it is character value '0'       
                                                         ,entAryVns #i.e., hmo1 hmo2.....,entXXX (xxx is the last monthIndex, right now it is xxx=288 is for dec 2009)
                                                         ,valueOfInterest.ent #'3' is for A and B
                                                         ,lastCheckMonAryIndex #this vector tells for each row of idHmoEntDeathYrMon.df we need to check up to which month
                                                         
                                                         ,claims.lastYr
                                                         ,claims.lastMon
                                                         )
    chosenMonBlocks <- chosenMonBlocks.all[complete.cases(chosenMonBlocks.all),] #remove rows with NA (i.e. not meet criteria)
    
    #Next(step 3) we need to make sure the druation of follow up, the end ary month index 
    #can be followed up 10 years. 
    
    #first 'end' is the final chosen end, so I rename it into 'end.candidate'
    chosenMonBlocks <- rename.vars(chosenMonBlocks,'end','end.candidate')
    chosenMonBlocks[,'latestEnd.followUpReq'] <- calYrMonToSeerMonAryIndex(matrix(c( claims.lastYr, claims.lastMon),nrow=1))-(minNumOfFollowUpMonths-1)
    chosenMonBlocks[,'end'] <- pmin(chosenMonBlocks[,'latestEnd.followUpReq'], chosenMonBlocks[,'end.candidate'])
    
    #Next (step 4) we need to make sure the 12 months retrospective only AB coverage.
    #first rename 'start' to 'start.candidate'
    chosenMonBlocks <- rename.vars(chosenMonBlocks,'start','start.candidate')
    #the earilest start month is the month the second month he turns 66 (add one month, so not the first month, to ensure for sure 12 months search window is met, otherwise, one actually have 11.xxx months instead of fully 12 months)
    #note (66-65)*12=minNumOfHistoryMonths

    chosenMonBlocksWithBirthMon <- plyr::join(chosenMonBlocks, test[,c(keyVn,birthYrVn,birthMonVn)], by=keyVn)
    rm(chosenMonBlocks)
    chosenMonBlocksWithBirthMon[,'earliestStart.historyReq'] <- calYrMonToSeerMonAryIndex(cbind(as.numeric(unname(unlist(chosenMonBlocksWithBirthMon[,birthYrVn]))), as.numeric(unname(unlist(chosenMonBlocksWithBirthMon[,birthMonVn])))))+65*12+minNumOfHistoryMonths #not 65*12+minNumOfHistoryMonths-1, use extra one month
    chosenMonBlocksWithBirthMon[,'start'] <- pmax(chosenMonBlocksWithBirthMon[,'earliestStart.historyReq'], chosenMonBlocksWithBirthMon[,'start.candidate'])
    
# now only kept those with start<=end
    out <- subset(chosenMonBlocksWithBirthMon,start<=end) 
    out <-  out[,c('hicbic','start','end','earliestStart.historyReq','latestEnd.followUpReq','start.candidate','end.candidate')]
    if (nrow(out)==0) {out <- NULL}
  }
   return(out)
}
