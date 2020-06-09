#based on screenPolicy relevant states (e.g. age, diagnosed status) 
#to determine whether need to take presumptive test (i.e., paraticpate in screening at period t)


state2screenAction <- function( stateDf
                               ,state2screenDecisionDf #contains state and screenAction, basically this is df defines screening Policy
                               ,screenDecisionDeterminants    # in both stateDf and state2ScreenActionDf, these states affect screening Action
                               ,screenDecisionVn # in state2ScreenActionDf, it contains value T/F or action probability
                               ,binaryScreenDecision=T #default is True, this means screenActVn contains TRUE (take test) or FALSE (not take test), 
                               #if binaryScreenAction=F, then it is test taking probability
                               ,reCatMat.useFreq=T #this opion only applies when  binaryScreenDecision=FALSE
                                ){
  #stateDf [agentIdVn currentAge firstDxHs hsAtFirstDx dxDurSinceFirstDx currentHs currentDxHs]
  #statePolicyMappingDf [currentAge firstDxHs hsAtFirstDx dxDurSinceFirstDx currentHs currentDxHs policyAction]
  if (binaryScreenDecision){
#     print(names(stateDf))
#     print(names(state2screenDecisionDf))
  screenActionVec <- plyr::join(stateDf,state2screenDecisionDf,by=screenDecisionDeterminants)[,screenDecisionVn] #left join
  if(length(screenActionVec) != nrow(stateDf) ) {stop('state2screenAction: number of screening actions does not equal to number of patients')}
  } else{
#     cat('you are here',names(stateDf),'\n')
#     cat('you are here 2',names(state2screenDecisionDf),'\n')
#     cat('you are here 3',screenDecisionDeterminants,'\n')
#     cat('you are here 3',screenDecisionVn,'\n')
    screenProb <- plyr::join(stateDf,state2screenDecisionDf,by=screenDecisionDeterminants)[,screenDecisionVn]    
    actionColIndx <- rCatMat.yz(cbind(1-screenProb,screenProb), useFreq=reCatMat.useFreq)
    screenActionVec <- (actionColIndx==2) #second column is take test sequence (i.e., screening)
  }
  return(screenActionVec)
}