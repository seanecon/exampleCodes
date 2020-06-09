
#conTestResultVn
#preTestResultVn
#this function generate presumptive and confirmatory test results for all screenees
#conDiagStateValueVec <- hsStateVec
preConSeqOutput <- function(
             screeneeStateDf
           , preTestAccuracyDf #[preTestAccuracyDeterminants,preDiagStateProbVnVec]
           , preDiagStateProbVnVec #in testAccuracyDf
           , preDiagStateValueVec #in testAccuracyDf #typically c(F, T) T means positive diagnosis
           , preTestAccuracyDeterminants 
           #based on testAccuracyDeterminants we look up accuracy 
            # in both stateDf and testAccuracyDf
           # at least contains latent health state
           , conTestAccuracyDf #[preTestAccuracyDeterminants,oreDiagStateProbVnVec]
           , conDiagStateProbVnVec #in testAccuracyDf
           , conDiagStateValueVec #in testAccuracyDf
           , conTestAccuracyDeterminants
            , positivePreTestVec # should be subset of preDiagStateValueVec, if preDiagStateValueVec is T or F, then positivePreTestVec is a singlton (i.e., T)
            , preTestResultVn # in output df
            , conTestResultVn #in output df            
          ){
   preTestResult <- state2diag(screeneeStateDf
           , preTestAccuracyDf #[preTestAccuracyDeterminants,preDiagStateProbVnVec]
           , preDiagStateProbVnVec #in testAccuracyDf
           , preDiagStateValueVec #in testAccuracyDf #typically c(F, T) T means positive diagnosis
           , preTestAccuracyDeterminants 
           #based on testAccuracyDeterminants we look up accuracy 
            # in both stateDf and testAccuracyDf
           # at least contains latent health state
            , useFreq.rCatMat.yz=T 
           #the are small numvber diagnosis probability, so always set rCatMat.yz useFreq option to T to speed up random draw
            )

preTestResult.allScreenees <- data.frame(screeneeStateDf[,idVn], preTestResult)
names(preTestResult.allScreenees) <- c(idVn,preTestResultVn)
#positivePreTestVec <- c(T)  # is a subset of preDiagStateValueVec
conTestTakerStateDf <- screeneeStateDf[which(preTestResult %in% positivePreTestVec),,drop=F]

 if (nrow(conTestTakerStateDf)>0){
      #conTestResult among conTestTakers
        conTestVec <- state2diag(conTestTakerStateDf
                                     , conTestAccuracyDf #[preTestAccuracyDeterminants,oreDiagStateProbVnVec]
                                     , conDiagStateProbVnVec #in testAccuracyDf
                                     , conDiagStateValueVec #in testAccuracyDf
                                     , conTestAccuracyDeterminants 
                                     #based on testAccuracyDeterminants we look up accuracy 
                                      # in both stateDf and testAccuracyDf
                                     # at least contains latent health state
                                     , useFreq.rCatMat.yz=T)  
        conTestDf.conTestTakers <- data.frame(conTestTakerStateDf[,idVn],conTestVec) 
        names(conTestDf.conTestTakers) <- c(idVn,conTestResultVn)
        conTestResult.allScreenees <- plyr::join(screeneeStateDf[,idVn,drop=F],conTestDf.conTestTakers, by=idVn)
    } else {
        conTestResult.allScreenees <- data.frame(screeneeStateDf[,idVn],rep(NA,nrow(screeneeStateDf)))
        names(conTestResult.allScreenees) <- c(idVn,conTestResultVn)
            }  
  #interesting, you cannot use as.data.frame, but you can use cbind
   preConSeqOutDf <- data.frame(preTestResult.allScreenees,conTestResult.allScreenees[,conTestResultVn,drop=F])  
  
   return(preConSeqOutDf)
}
