#this function would use all agents' stateDf and preConTestResult.screenee to update first Dx health state and corresponding
#age for these who were newly diangosed
updateFirstDxHsAgeForNewlyDiagnosed <- function(stateDf
                                   #,iniAgeVec #initial Age variable name, this variable in stateDf
                                   #,t #current period index t=1,2,....Not starting from 0
                                   ,currentAgeVn
                                   ,preConTestResult.screenee
                                   ,idVn
                                   ,conTestResultVn
                                   ,nonDeath.diseaseHsVec #the collection of diseaseStates. e.g., "L" and "A"
                                   ,firstDxHsVn
                                   ,firstDxAgeVn
                              ){
  #update the following two variables: diagnosed disease at initial diagnosis (firstDxHs) and diagnosis Duration Since First Diagnosis
  #firstDxHs and  firstDxAg 
  #identify these (confirmatorily) diagnosied with disease and their disease states
  newDiagRowIndxInScreeneeDf <- which(preConTestResult.screenee[,conTestResultVn] %in% nonDeath.diseaseHsVec)
  #update firstDxHsVn and firstDxAgeVn
  if (length(newDiagRowIndxInScreeneeDf)>0){ #there are newly diagnosed
  newDiagRowIndxInStateDf <- match(preConTestResult.screenee[newDiagRowIndxInScreeneeDf,idVn],stateDf[,idVn])  
  stateDf[newDiagRowIndxInStateDf,c(firstDxHsVn,firstDxAgeVn)] <- data.frame(preConTestResult.screenee[newDiagRowIndxInScreeneeDf,conTestResultVn],stateDf[newDiagRowIndxInStateDf,currentAgeVn])
  }
  return(stateDf)
}