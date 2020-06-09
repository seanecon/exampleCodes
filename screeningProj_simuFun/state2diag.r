#we do not need id in stateDf, because join's output is unsorted based on left data



#   stateDf <- screeneeStateDf
#           testAccuracyDf<-    preTestAccuracyDf #[preTestAccuracyDeterminants,preDiagStateProbVnVec]
#            diagStateProbVnVec <-  preDiagStateProbVnVec #in testAccuracyDf
#            diagStateValueVec <-  preDiagStateValueVec #in testAccuracyDf #typically c(F, T) T means positive diagnosis
#           testAccuracyDeterminants <-   preTestAccuracyDeterminants 
#            #based on testAccuracyDeterminants we look up accuracy 
#             # in both stateDf and testAccuracyDf
#            # at least contains latent health state
#             useFreq.rCatMat.yz=T
# stateDf <- screeneeStateDf
#  testAccuracyDf<- preTestAccuracyDf #[preTestAccuracyDeterminants,preDiagStateProbVnVec]
#  diagStateProbVnVec<-           preDiagStateProbVnVec #in testAccuracyDf
#            diagStateValueVec<- preDiagStateValueVec #in testAccuracyDf #typically c(F, T) T means positive diagnosis
#            testAccuracyDeterminants<- preTestAccuracyDeterminants




# stateDf <- conTestTakerStateDf
#                                      , conTestAccuracyDf #[preTestAccuracyDeterminants,oreDiagStateProbVnVec]
#                                      , conDiagStateProbVnVec #in testAccuracyDf
#                                      , conDiagStateValueVec #in testAccuracyDf
#                                      , conTestAccuracyDeterminants 
#                                      #based on testAccuracyDeterminants we look up accuracy 
#                                       # in both stateDf and testAccuracyDf
#                                      # at least contains latent health state
#                                      , useFreq.rCatMat.yz=T
# 


state2diag <- function(    stateDf
                           , testAccuracyDf #[preTestAccuracyDeterminants,oreDiagStateProbVnVec]
                           , diagStateProbVnVec #in testAccuracyDf
                           , diagStateValueVec #in testAccuracyDf
                           , testAccuracyDeterminants 
                           #based on testAccuracyDeterminants we look up accuracy 
                            # in both stateDf and testAccuracyDf
                           # at least contains latent health state
                           , useFreq.rCatMat.yz=T 
                           #the are small numvber diagnosis probability, so always set rCatMat.yz useFreq option to T to speed up random draw
                           ){ 
   #get accruacy  , join's output unsorted (based on left data id order) 
   diagProbMat <- plyr::join(stateDf, testAccuracyDf, by=testAccuracyDeterminants)[,diagStateProbVnVec] #left join
   #get diagnosis state
   whichDiagStateVec <- rCatMat.yz(diagProbMat, useFreq=useFreq.rCatMat.yz)
   #add id and return
   diagResultVec <- diagStateValueVec[whichDiagStateVec]
   return(diagResultVec)   
     }
# 
# apply(diagProbMat,1,function(x){any(is.na(x))})
# diagProbMat[9291,]