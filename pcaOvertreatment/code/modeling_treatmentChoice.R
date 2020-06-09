

#save(finalSample1,file='Z:/j_scrdata/pcaOverTreatment/finalSample1.RData')
(load(file='Z:/j_scrdata/pcaOverTreatment/finalSample1.RData'))
names(finalSample1)

ddply(finalSample1,'riskStrata',function(x){mean(x[,'indexage'])})
ddply(finalSample1,'riskStrata',function(x){mean(x[,'trtType']=='hasimrtnosrg')})
ddply(finalSample1,'riskStrata',function(x){mean(x[,'trtType']=='hasrobotnoradia')})


head(finalSample1)
# race age
#when strategy by 10 year life expetacny, we will add age race, ses, comrob, tumorgrade dimacorisk and year of diagnosis, seer region
#when strategy by damico reisk then we add  10 year life expetacny age race, ses, comrob, tumorgrade  and year of diagnosis, seer region
##when strategy by 10 year life expetacny and damico, we will add age race, ses, comrob, tumorgrade  and year of diagnosis, seer region
#


#call the functions for generating model and post estiamtion
source('C:/Dropbox/K/pcaOvertreatment/code/functionForMlogitModeling.r')


names(finalSample1)

#10 year survival risk

genFitSubData(c('hig | surv10Yr.low','mid | surv10Yr.low','low | surv10Yr.low')
              ,adjustedVars= c('raceCat','.damicorisk','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','registry')
              ,finalSample1 , 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.low10yrSurv.RData')


(load('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.low10yrSurv.RData'))
genPredictProb.ovtrt.ageGrp(subData.mlogitFit, '65-70')
genPredictProb.ovtrt.ageGrp(subData.mlogitFit, '70-75')



genFitSubData(c('hig | surv10Yr.high','mid | surv10Yr.high','low | surv10Yr.high')
              ,adjustedVars= c('raceCat','.damicorisk','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','registry')
              , finalSample1 , 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.high10yrSurv.RData')


(load('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.high10yrSurv.RData'))
genPredictProb.ovtrt.ageGrp(subData.mlogitFit, '65-70')
genPredictProb.ovtrt.ageGrp(subData.mlogitFit, '70-75')

genFitSubData(c('hig | surv10Yr.mid','mid | surv10Yr.mid','low | surv10Yr.mid')
              ,adjustedVars= c('raceCat','.damicorisk','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','registry')
              , finalSample1 , 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.mid10yrSurv.RData')

(load('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.mid10yrSurv.RData'))
genPredictProb.ovtrt.ageGrp(subData.mlogitFit, '65-70')
genPredictProb.ovtrt.ageGrp(subData.mlogitFit, '70-75')

#damico risk

genFitSubData(c('hig | surv10Yr.low','hig | surv10Yr.mid','hig | surv10Yr.high')
              ,adjustedVars= c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat',"surv10YrProb3Cat",'indexyear','registry')
              , finalSample1 , 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.highDamicoRisk.RData')

genFitSubData(c('mid | surv10Yr.low','mid | surv10Yr.mid','mid | surv10Yr.high')
              ,adjustedVars= c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat',"surv10YrProb3Cat",'indexyear','registry')
              , finalSample1 , 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.midDamicoRisk.RData')

genFitSubData(c('low | surv10Yr.low','low | surv10Yr.mid','low | surv10Yr.high')
              ,adjustedVars= c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat',"surv10YrProb3Cat",'indexyear','registry')
              , finalSample1 , 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.lowDamicoRisk.RData')


#damico and 10yr survival


#low survival
genFitSubData(c('hig | surv10Yr.low')
              ,adjustedVars= c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','registry')
              , finalSample1 , 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.highDamicoRisk.low10YrSurv.RData')

(load('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.highDamicoRisk.low10YrSurv.RData'))
genPredictProb.ovtrt.ageGrp(subData.mlogitFit, '65-70')
genPredictProb.ovtrt.ageGrp(subData.mlogitFit, '70-75')

genFitSubData(c('mid | surv10Yr.low')
              ,adjustedVars= c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','registry')
              , finalSample1 , 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.midDamicoRisk.low10YrSurv.RData')

(load( 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.midDamicoRisk.low10YrSurv.RData'))
genPredictProb.ovtrt.ageGrp(subData.mlogitFit, '65-70')
genPredictProb.ovtrt.ageGrp(subData.mlogitFit, '70-75')

genFitSubData(c('low | surv10Yr.low')
              ,adjustedVars= c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','registry')
              , finalSample1 , 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.lowDamicoRisk.low10YrSurv.RData')

(load('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.lowDamicoRisk.low10YrSurv.RData'))
genPredictProb.ovtrt.ageGrp(subData.mlogitFit, '65-70')
genPredictProb.ovtrt.ageGrp(subData.mlogitFit, '70-75')

#mid survival

genFitSubData(c('hig | surv10Yr.mid')
              ,adjustedVars= c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','registry')
              , finalSample1 , 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.highDamicoRisk.mid10YrSurv.RData')

(load('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.highDamicoRisk.mid10YrSurv.RData'))
genPredictProb.ovtrt.ageGrp(subData.mlogitFit, '65-70')
genPredictProb.ovtrt.ageGrp(subData.mlogitFit, '70-75')

genFitSubData(c('mid | surv10Yr.mid')
              ,adjustedVars= c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','registry')
              , finalSample1 , 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.midDamicoRisk.mid10YrSurv.RData')

(load('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.midDamicoRisk.mid10YrSurv.RData'))
genPredictProb.ovtrt.ageGrp(subData.mlogitFit, '65-70')
genPredictProb.ovtrt.ageGrp(subData.mlogitFit, '70-75')

genFitSubData(c('low | surv10Yr.mid')
              ,adjustedVars= c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','registry')
              , finalSample1 , 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.lowDamicoRisk.mid10YrSurv.RData')

(load('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.lowDamicoRisk.mid10YrSurv.RData'))
genPredictProb.ovtrt.ageGrp(subData.mlogitFit, '65-70')
genPredictProb.ovtrt.ageGrp(subData.mlogitFit, '70-75')


#high survival
genFitSubData(c('hig | surv10Yr.high')
              ,adjustedVars= c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','registry')
              , finalSample1 , 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.highDamicoRisk.high10YrSurv.RData')
(load('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.highDamicoRisk.high10YrSurv.RData'))
genPredictProb.ovtrt.ageGrp(subData.mlogitFit, '65-70')
genPredictProb.ovtrt.ageGrp(subData.mlogitFit, '70-75')


genFitSubData(c('mid | surv10Yr.high')
              ,adjustedVars= c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','registry')
              , finalSample1 , 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.midDamicoRisk.high10YrSurv.RData')

(load('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.midDamicoRisk.high10YrSurv.RData'))
genPredictProb.ovtrt.ageGrp(subData.mlogitFit, '65-70')
genPredictProb.ovtrt.ageGrp(subData.mlogitFit, '70-75')


genFitSubData(c('low | surv10Yr.high')
              ,adjustedVars= c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','registry')
              , finalSample1 , 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.lowDamicoRisk.high10YrSurv.RData')

(load('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.lowDamicoRisk.high10YrSurv.RData'))
genPredictProb.ovtrt.ageGrp(subData.mlogitFit, '65-70')
genPredictProb.ovtrt.ageGrp(subData.mlogitFit, '70-75')

#----------------------------------------

#generate outputs

low10yrSurv.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.low10yrSurv.RData')

mid10yrSurv.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.mid10yrSurv.RData')

high10yrSurv.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.high10yrSurv.RData')

rescale.basedOnTwoChoice(low10yrSurv.res$predProb)
rescale.basedOnTwoChoice(mid10yrSurv.res$predProb)
rescale.basedOnTwoChoice(high10yrSurv.res$predProb)


orRowNameVec <- c('hasimrtnosrg:.damicoriskdummy.low'
                  ,'hasrobotnoradia:.damicoriskdummy.low'       
                  ,'hasimrtnosrg:.damicoriskdummy.mid'       
                  ,'hasrobotnoradia:.damicoriskdummy.mid','hasimrtnosrg:grdCatdummy.wellmodDiff','hasrobotnoradia:grdCatdummy.wellmodDiff')

#we want observation vs. imrt and observation vs. robot, so I take a inverse
xtable(1/low10yrSurv.res$or[orRowNameVec,c('OR','95LB.OR','95UB.OR')], caption='low 10 yr survival')
xtable(1/mid10yrSurv.res$or[orRowNameVec,c('OR','95LB.OR','95UB.OR')], caption='mid 10 yr survival')
xtable(1/high10yrSurv.res$or[orRowNameVec,c('OR','95LB.OR','95UB.OR')], caption='high 10 yr survival')

xtable(low10yrSurv.res$predProb, digits=4)
xtable(mid10yrSurv.res$predProb, digits=4)
xtable(high10yrSurv.res$predProb, digits=4)

#----------------------------------------

lowDamicoRisk.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.lowDamicoRisk.RData')
midDamicoRisk.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.midDamicoRisk.RData')
highDamicoRisk.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.highDamicoRisk.RData')

rescale.basedOnTwoChoice(lowDamicoRisk.res$predProb)
rescale.basedOnTwoChoice(midDamicoRisk.res$predProb)
rescale.basedOnTwoChoice(highDamicoRisk.res$predProb)

xtable(lowDamicoRisk.res$predProb, digits=4)
xtable(midDamicoRisk.res$predProb, digits=4)
xtable(highDamicoRisk.res$predProb, digits=4)

#----------------------------------------
lowSurvLowDamico.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.lowDamicoRisk.low10YrSurv.RData')
lowSurvMidDamico.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.midDamicoRisk.low10YrSurv.RData')
lowSurvHighDamico.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.highDamicoRisk.low10YrSurv.RData')

highSurvLowDamico.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.lowDamicoRisk.high10YrSurv.RData')
highSurvMidDamico.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.midDamicoRisk.high10YrSurv.RData')
highSurvHighDamico.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.highDamicoRisk.high10YrSurv.RData')

midSurvLowDamico.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.lowDamicoRisk.mid10YrSurv.RData')
midSurvMidDamico.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.midDamicoRisk.mid10YrSurv.RData')
midSurvHighDamico.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj/fit.highDamicoRisk.mid10YrSurv.RData')


rescale.basedOnTwoChoice(lowSurvLowDamico.res$predProb)
rescale.basedOnTwoChoice(lowSurvMidDamico.res$predProb)
rescale.basedOnTwoChoice(lowSurvHighDamico.res$predProb)

rescale.basedOnTwoChoice(highSurvLowDamico.res$predProb)
rescale.basedOnTwoChoice(highSurvMidDamico.res$predProb)
rescale.basedOnTwoChoice(highSurvHighDamico.res$predProb)

rescale.basedOnTwoChoice(midSurvLowDamico.res$predProb)
rescale.basedOnTwoChoice(midSurvMidDamico.res$predProb)
rescale.basedOnTwoChoice(midSurvHighDamico.res$predProb)







xtable(lowSurvLowDamico.res$predProb,digits=4)
xtable(lowSurvMidDamico.res$predProb,digits=4)
xtable(lowSurvHighDamico.res$predProb,digits=4)

xtable(midSurvLowDamico.res$predProb,digits=4)
xtable(midSurvMidDamico.res$predProb,digits=4)
xtable(midSurvHighDamico.res$predProb,digits=4)

xtable(highSurvLowDamico.res$predProb,digits=4)
xtable(highSurvMidDamico.res$predProb,digits=4)
xtable(highSurvHighDamico.res$predProb,digits=4)


# source('C:/Dropbox/R_new/model/mlogit_MLEcode/optimFitSummary.yz.R')
# source('C:/Dropbox/R_new/model/mlogit_MLEcode/nLL.mlogit.yz.R')

