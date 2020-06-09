
#this function stores all input (and random seed) and output.
#

#stateDf contains [idVn firstDxHsVn firstDxAgeVn, dxDurSinceFirstDxVn, currentAgeVn, currentHsVn]
 #1 for if timeinterval is one year
#preTestAccuracyDf is [preTestAccuracyDeterminants preDiagStateProbVnVec]
#conTestAccuracyDf is [conTestAccuracyDeterminants conDiagStateProbVnVec]
#state2screenDecisionDf defines screening policy
# state2screenDecisionDf is [screenPolicyStateVns screenDecisionVn]
#healthTranProbDf  is age-EOD specific Health Transition Probability Df
#healthTranProbDf contains [healthTranProbDeterminants hsTranProbVnVec]
# healthTranProbDeterminants are varaibles in both stateDf and healthTranProbDf
#they are current Age, EOD, gender and the like, used in join to look up hsTransition probability


#to do 
#(1) generate preTestAccuracyDf and conTestAccuracyDf
#(2) generate iniStateDf 
#(3) generate Screening Policy Df
#(4) generate healthTranProbDf
#(5) test run debug
# 
# #test run below
# head(iniState)
# iniStateDf
# tMax <- 2
# periodInterval <- 1
# idVn <- 'id'
# firstDxHsVn <- 'firstDxHs'
# firstDxAgeVn <- 'firstDxAge'
# dxDurSinceFirstDxVn <- 'dxDurSinceFirstDx.t'
# currentAgeVn <- 'age.t'
# currentHsVn <- 'hs.t'
# hsStateVec <- hsVec
# nonDeath.diseaseHsVec <- c('L','A')
# screenDecisionVn <- 'takeTestSeq.action'
# screenDecisionVariableIsBinary <- T
# screenDecisionDeterminants <- c("age.t","hs.t","firstDxHs") 
# 
# #save(healthTranProbDfTestRun, file='C:/Dropbox/K/withPresumptiveTest/proj_1/get_seer_input/seerInputData/healthTranProbDfTestRun.RData')
# 
# load(file='C:/Dropbox/K/withPresumptiveTest/proj_1/get_seer_input/seerInputData/healthTranProbDfTestRun.RData')
# 
# healthTranProbDf <- healthTranProbDfTestRun
# #this is age-EOD specific Health Transition Probability Df, containts  [healthTranProbDeterminants hsTranProbVnVec]
# healthTranProbDeterminants <- c("age.t","dxDurSinceFirstDx.t","hs.t", "firstDxHs")
# #at least age and EOD (i.e., firstDxVn dxDurSinceFirstDx)
# # healthTranProbDeterminants are varaibles in both stateDf and healthTranProbDf
# #they are current Age, EOD, gender and the like, used in join to look up hsTransition probability
# hsTranProbVnVec <- c("tranPr.N","tranPr.L","tranPr.A","tranPr.D")  #e.g., prob.N prob.L, prob.A, prob.D; These variables appear in healthTranProbDf
#   
# #------------input 5: presumptive Test input df and vn-------------------
# preTestAccuracyDf 
# #[preTestAccuracyDeterminants,preDiagStateProbVnVec]
#  preDiagStateProbVnVec <- c('prDiagNeg', 'prDiagPos') #in preTestAccuracyDf e.g prob.F (not diagnosed), prob.T (diagnoised)
# # order consistent with  preDiagStateValueVec 
#  preDiagStateValueVec <- c(F,T) #in preTestAccuracyDf typically c(F, T) T means positive diagnosis
#  positivePreTestVec <- TRUE # should be subset of preDiagStateVec, if preDiagStateVec is T or F, then posPreTestVec is a singlton (i.e., T)
#  preTestAccuracyDeterminants <- c('hs.t') #at least contains current HS, it may contain other exog characteristics such as age and the like 
# #based on testAccuracyDeterminants we look up accuracy 
# # in both stateDf and testAccuracyDf
# # at least contains latent health state
#   
# #---------input 6: confirmatory Test input df and vn ----------------------
#  conTestAccuracyDf  #[preTestAccuracyDeterminants,oreDiagStateProbVnVec]
#  conDiagStateProbVnVec <-  c('prDiagN', 'prDiagL', 'prDiagA', 'prDiagD') #in conTestAccuracyDf, should be consistent with order of hsStateVec
# #, conDiagStateValueVec # for prostaet cancer study, it is c('N','L','A','D'), same as hsStateVec
#  conTestAccuracyDeterminants <- c('hs.t')
#   
# #--------intermediate Variable names -----------------------
#  preTestResultVn=c('preTestResult') #used internally 
#  conTestResultVn=c('conTestResult') #used internally
# 
# 
# 
# 
# head(state2screenDecisionDf)
# #generate health and diagnosis history
genHealthDxHis.screen <- function(
#--------input 1: model time horizone, time interval, and latent health states and disease states-------
  tMax #the maximum period, say 36 (35) from 65 to 100
, periodInterval #for prostate cancer study it is 1, you can change it to shorter interval say 0.5 if study requires
, hsStateVec #for prostaet cancer study, it is c('N','L','A','D')
  #order matters, should be consistwith  conDiagStateProbVnVec and hsTranProbVnVec
, nonDeath.diseaseHsVec 
  #the collection of nondeath diseaseStates. e.g., "L" and "A" order does Not matter c('L','A') is the same as c('A','L')

  #------------input 2: initial state inputs and Vns------------------
, iniStateDf #initial state input, it contains [idVn firstDxHsVn firstDxAgeVn, dxDurSinceFirstDxVn, currentAgeVn, currentHsVn]
, idVn #in iniStateDf
, firstDxHsVn #in iniStateDf
, firstDxAgeVn #in iniStateDf
, dxDurSinceFirstDxVn #in iniStateDf
, currentAgeVn #in iniStateDf
, currentHsVn # in iniStateDf
#------------input 3: Screening Policy Df input and Vns-------------------
, state2screenDecisionDf #this data defines screening policy 
, screenDecisionDeterminants #age firstDxHsVn (if firstDxHs is NA then undiagnosed)
, screenDecisionVn #in state2screenDecisionDf, it takes value T or F
, screenDecisionVariableIsBinary
  
#------------ input4 : EOD specific Health Production Probability Input and Vns -------------------  
, healthTranProbDf #this is age-EOD specific Health Transition Probability Df, containts  [healthTranProbDeterminants hsTranProbVnVec]
, healthTranProbDeterminants #at least age and EOD (i.e., firstDxVn dxDurSinceFirstDx)
# healthTranProbDeterminants are varaibles in both stateDf and healthTranProbDf
#they are current Age, EOD, gender and the like, used in join to look up hsTransition probability
, hsTranProbVnVec #e.g., prob.N prob.L, prob.A, prob.D; These variables appear in healthTranProbDf
  
#------------input 5: presumptive Test input df and vn-------------------
, preTestAccuracyDf #[preTestAccuracyDeterminants,preDiagStateProbVnVec]
, preDiagStateProbVnVec #in preTestAccuracyDf e.g prob.F (not diagnosed), prob.T (diagnoised)
, preDiagStateValueVec #in preTestAccuracyDf typically c(F, T) T means positive diagnosis
, positivePreTestVec # should be subset of preDiagStateVec, if preDiagStateVec is T or F, then posPreTestVec is a singlton (i.e., T)
, preTestAccuracyDeterminants #at least contains current HS, it may contain other exog characteristics such as age and the like 
#based on testAccuracyDeterminants we look up accuracy 
# in both stateDf and testAccuracyDf
# at least contains latent health state
  
#---------input 6: confirmatory Test input df and vn ----------------------
, conTestAccuracyDf #[preTestAccuracyDeterminants,oreDiagStateProbVnVec]
, conDiagStateProbVnVec #in conTestAccuracyDf, should be consistent with order of hsStateVec
#, conDiagStateValueVec # for prostaet cancer study, it is c('N','L','A','D'), same as hsStateVec
, conTestAccuracyDeterminants
  
#--------intermediate Variable names -----------------------
, preTestResultVn=c('preTestResult') #used internally 
, conTestResultVn=c('conTestResult') #used internally
#----------random seed for replication purpose
, randomSeed=888
) {
  if(default.stringsAsFactors()){stop('please set default.stringsAsFactor as FALSE')}
hsHisList  <- preTestUseHisList <- conTestUseHisList <-  list() #conTestResultList <-
iniAgeVec <- iniStateDf[,currentAgeVn]
                        
                        
#set random seed for replication purpose
set.seed(randomSeed)
                        
for (t in 1:tMax){
  
  #inside this loop stateDf is updated
  cat('processing t=',t,'\n')
  
  if (t==1){stateDf <- iniStateDf} 
  if (t>1) {stateDf[,currentAgeVn] <- iniAgeVec +(t-1)*periodInterval }
  #--------step 1: obtain screening decision based on state and screening policy
  screenAction <- state2screenAction(stateDf
                                 ,state2screenDecisionDf #contains state and screenAction, basically this is df defines screening Policy
                                 ,screenDecisionDeterminants  # in both stateDf and state2screenDecisionDf
                                 ,screenDecisionVn # in state2screenDecisionDf, it takes value T or F
                                 ,binaryScreenDecision=screenDecisionVariableIsBinary #default is True, this means screenDecisionVn contains TRUE (take test) or FALSE (not take test), 
                               #if binaryScreenAction=F, then it is test taking probability
                                  ,reCatMat.useFreq=T
                                 )
  preTestUseHisList <- lappend.yz(preTestUseHisList, screenAction)
  
  #preTestResult among Screenees
 
  #-------step2: among screenees, we get their presumptive and confirmatory test results 
   if (length(which(screenAction))>0) { 
  screeneeStateDf <- stateDf[which(screenAction),,drop=F]
  #need to handle nonScreenee Sitution, i.e, 
  preConTestResult.screenee <- preConSeqOutput(
               screeneeStateDf
             , preTestAccuracyDf #[preTestAccuracyDeterminants,preDiagStateProbVnVec]
             , preDiagStateProbVnVec #in testAccuracyDf
             , preDiagStateValueVec #in testAccuracyDf #typically c(F, T) T means positive diagnosis
             , preTestAccuracyDeterminants 
             #based on testAccuracyDeterminants we look up accuracy 
              # in both stateDf and testAccuracyDf
             # at least contains latent health state
              , conTestAccuracyDf #[preTestAccuracyDeterminants,oreDiagStateProbVnVec]
             , conDiagStateProbVnVec #should be consistent with conDiagStateVec
             , hsStateVec #conDiagStateVec is the same as hsStateVec 
             , conTestAccuracyDeterminants
              , positivePreTestVec # should be subset of preDiagStateVec, if preDiagStateVec is T or F, then posPreTestVec is a singlton (i.e., T)
              , preTestResultVn # in output df
              , conTestResultVn #in output df            
            )
  conTestResultVec.allAgents <- plyr::join(iniStateDf[,idVn,drop=F],preConTestResult.screenee[,c(idVn, conTestResultVn)],by=idVn)[,conTestResultVn]
  conTestUseHisList <- lappend.yz(conTestUseHisList, !is.na(conTestResultVec.allAgents))
  #conTestResultList <- lappend.yz(conTestResultList, conTestResultVec.allAgents)
  #above: state to screenAction 2 diagnosis result
  #------step3: update diagnosis history (first diagnosis state and diagnosis duration) for these newly diagnosed (by confirmatory test) based on diagnosis results
  #update firstDxHs and firstDxAge for newly Diagnosed in stateDf
  
  #if no screenee, firstDxAge and firstDxHs are unchanged ()
  stateDf <- updateFirstDxHsAgeForNewlyDiagnosed(stateDf                                     
                                     ,currentAgeVn
                                     ,preConTestResult.screenee
                                     ,idVn
                                     ,conTestResultVn
                                     ,nonDeath.diseaseHsVec #the collection of diseaseStates. e.g., "L" and "A"
                                     ,firstDxHsVn
                                     ,firstDxAgeVn)
   } else{ #noscreens at all then, stateDf is the same
     conTestUseHisList <- lappend.yz(conTestUseHisList, rep(F,nrow(stateDf)))
   }
  
  #update current diagnosis Duration (is there a problem in join when diagnosis duration is NA, should I use -99 for not applicable diagDur)
  stateDf[,dxDurSinceFirstDxVn] <- stateDf[,currentAgeVn] - stateDf[,firstDxAgeVn] #firstDxAge can be NA
  
  #-----step4: based on diagnosis history to update latent health state (at the end of a period)
  stateDf[,currentHsVn] <- hsTransitionResult.allAgents(stateDf 
                            , healthTranProbDf
                            , healthTranProbDeterminants
                             # healthTranProbDeterminants are varaibles in both stateDf and healthTranProbDf
                             #they are currentAge, firstDxHs, dxDurSinceFirstDx, gender and the like
                            , hsTranProbVnVec #e.g., prob.N prob.L, prob.A, prob.D; These variables appear in healthTranProbDf
                            , hsStateVec # c('N','L','A','D'), Note order should match hsTranProbVnVec
                            , useFreq.rCatMat.yz=T)
  #we assume any post diagnosis progression is diagnoised right after the occurrence
  #we assume that there is progression that occurred after diagnosis was NOT seen by doctor.
  #so there is NO false negative progression. For this reason
  # in the Health production Df, there is no need to model health transition for a person
  #who was diagnosed with Localized and progressed to Advanced and his progression was Not observed by clinician.
  hsHisList <- lappend.yz(hsHisList, stateDf[,currentHsVn])

} #for t loop -----repeat step 1 to step 4.


#---------collecting endogenous quantities below

#hsHisDf has [id hs.1,hs.2,.....,hs.tMax]
#firstDxList has [id, firstDxState, firstDxAge]
#preTestUseHisDf has [id preTestUseIndicator.1,preTestUseIndicator.2,.....,preTestUseIndicator.tMax]
#conTestUseHisDf has [id conTestUseIndicator.1,coTestUseIndicator.2,.....,conTestUseIndicator.tMax]
hsHisDf <- data.frame(stateDf[,idVn],do.call(cbind,hsHisList))
names(hsHisDf) <- c(idVn, paste('eof.',seq(tMax),sep='')) #end of period t 
#(we assume state transition occurs at the end of a period, so a person who come int othe model with age 65, will for sure live to the end of age 65
preTestUseHisDf <- data.frame(stateDf[,idVn],do.call(cbind,preTestUseHisList))
names(preTestUseHisDf) <- c(idVn, paste('bof.',seq(tMax),sep='')) #beinning of period t
conTestUseHisDf <- data.frame(stateDf[,idVn],do.call(cbind,conTestUseHisList))
names(conTestUseHisDf) <- c(idVn, paste('bof.',seq(tMax),sep='')) #beginning of 

#stateDf of the last interation contains the final firstDx information
firstDxDf <- stateDf[,c(idVn,firstDxHsVn,firstDxAgeVn)]
                        
                        
                        
  
#-----store all the inputs into a list, such a list is useful for rerun the model
input <- list( #--------input 1: model time horizone, time interval, and latent health states and disease states-------
  tMax=tMax #the maximum period, say 36 (35) from 65 to 100
, periodInterval=periodInterval #for prostate cancer study it is 1, you can change it to shorter interval say 0.5 if study requires
, hsStateVec=hsStateVec #for prostaet cancer study, it is c('N','L','A','D')
  #order matters, should be consistwith  conDiagStateProbVnVec and hsTranProbVnVec
, nonDeath.diseaseHsVec=nonDeath.diseaseHsVec 
  #the collection of nondeath diseaseStates. e.g., "L" and "A" order does Not matter c('L','A') is the same as c('A','L')

  #------------input 2: initial state inputs and Vns------------------
, iniStateDf=iniStateDf #initial state input, it contains [idVn firstDxHsVn firstDxAgeVn, dxDurSinceFirstDxVn, currentAgeVn, currentHsVn]
, idVn=idVn #in iniStateDf
, firstDxHsVn=firstDxHsVn #in iniStateDf
, firstDxAgeVn=firstDxAgeVn #in iniStateDf
, dxDurSinceFirstDxVn=dxDurSinceFirstDxVn #in iniStateDf
, currentAgeVn=currentAgeVn #in iniStateDf
, currentHsVn=currentHsVn # in iniStateDf
#------------input 3: Screening Policy Df input and Vns-------------------
, state2screenDecisionDf=state2screenDecisionDf #this data defines screening policy 
, screenDecisionDeterminants=screenDecisionDeterminants #age firstDxHsVn (if firstDxHs is NA then undiagnosed)
, screenDecisionVn=screenDecisionVn #in state2screenDecisionDf, it takes value T or F
, screenDecisionVariableIsBinary=screenDecisionVariableIsBinary
  
#------------ input4 : EOD specific Health Production Probability Input and Vns -------------------  
, healthTranProbDf=healthTranProbDf #this is age-EOD specific Health Transition Probability Df, containts  [healthTranProbDeterminants hsTranProbVnVec]
, healthTranProbDeterminants=healthTranProbDeterminants #at least age and EOD (i.e., firstDxVn dxDurSinceFirstDx)
# healthTranProbDeterminants are varaibles in both stateDf and healthTranProbDf
#they are current Age, EOD, gender and the like, used in join to look up hsTransition probability
, hsTranProbVnVec=hsTranProbVnVec #e.g., prob.N prob.L, prob.A, prob.D; These variables appear in healthTranProbDf
  
#------------input 5: presumptive Test input df and vn-------------------
, preTestAccuracyDf=preTestAccuracyDf #[preTestAccuracyDeterminants,preDiagStateProbVnVec]
, preDiagStateProbVnVec=preDiagStateProbVnVec #in preTestAccuracyDf e.g prob.F (not diagnosed), prob.T (diagnoised)
, preDiagStateValueVec=preDiagStateValueVec #in preTestAccuracyDf typically c(F, T) T means positive diagnosis
, positivePreTestVec=positivePreTestVec # should be subset of preDiagStateVec, if preDiagStateVec is T or F, then posPreTestVec is a singlton (i.e., T)
, preTestAccuracyDeterminants=preTestAccuracyDeterminants #at least contains current HS, it may contain other exog characteristics such as age and the like 
#based on testAccuracyDeterminants we look up accuracy 
# in both stateDf and testAccuracyDf
# at least contains latent health state
  
#---------input 6: confirmatory Test input df and vn ----------------------
, conTestAccuracyDf=conTestAccuracyDf #[preTestAccuracyDeterminants,oreDiagStateProbVnVec]
, conDiagStateProbVnVec=conDiagStateProbVnVec #in conTestAccuracyDf, should be consistent with order of hsStateVec
#, conDiagStateValueVec # for prostaet cancer study, it is c('N','L','A','D'), same as hsStateVec
, conTestAccuracyDeterminants=conTestAccuracyDeterminants
  
#--------intermediate Variable names -----------------------
, preTestResultVn=preTestResultVn #used internally 
, conTestResultVn=conTestResultVn
#----------random seed for replication purpose
,randomSeed=randomSeed)
output <- list(
          hsHis=hsHisDf #history of latent health state at the end of each period
        , firstDx=firstDxDf #the first diagnosed disease state
        , preTestUseHis=preTestUseHisDf #whether one uses presumptive test at the beginning of each period
        , conTestUseHis=conTestUseHisDf #whether one uses confrimatory test at the beginning of each period
        )                   
simRes <- list(input=input,output=output)
return(simRes)
} #function ends


