
#this function outputs a (after transition)  health state vector for all agents
hsTransitionResult.allAgents <- function(stateDf
                           ,healthTranProbDf
                           ,healthTranProbDeterminants
                           # healthTranProbDeterminants are varaibles in both stateDf and healthTranProbDf
                           #they are current Age, Eod, gender and the like
                           , hsTranProbVnVec #e.g., prob.N prob.L, prob.A, prob.D; These variables appear in healthTranProbDf
                           , hsStateVec # c('N','L','A','D'), Note order should match hsTranProbVnVec
                           , useFreq.rCatMat.yz=T
                           ){
  
  #stop here...why there are NA, 65 dxdur=NA, cannot have be looked up.
  hsTranProbMat <- plyr::join(stateDf[,healthTranProbDeterminants,drop=F],healthTranProbDf, by=healthTranProbDeterminants)[,hsTranProbVnVec]
  
  whichHsStateVec <- rCatMat.yz(hsTranProbMat, useFreq=useFreq.rCatMat.yz)
   #add id and return
  hsResultVec <- hsStateVec[whichHsStateVec]
  return(hsResultVec)
}