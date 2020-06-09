

#save(finalSample1,file='Z:/j_scrdata/pcaOverTreatment/finalSample1.RData')
(load(file='Z:/j_scrdata/pcaOverTreatment/finalSample1.RData'))

finalSampleSensitivity <- subset(finalSample1, .damicot2nosindicator==0)
nrow(finalSampleSensitivity)

# names(finalSample1)
# 
# ddply(finalSample1,'riskStrata',function(x){mean(x[,'indexage'])})
# ddply(finalSample1,'riskStrata',function(x){mean(x[,'trtType']=='hasimrtnosrg')})
# ddply(finalSample1,'riskStrata',function(x){mean(x[,'trtType']=='hasrobotnoradia')})
# 
# 
# head(finalSample1)
# race age
#when strategy by 10 year life expetacny, we will add age race, ses, comrob, tumorgrade dimacorisk and year of diagnosis, seer region
#when strategy by damico reisk then we add  10 year life expetacny age race, ses, comrob, tumorgrade  and year of diagnosis, seer region
##when strategy by 10 year life expetacny and damico, we will add age race, ses, comrob, tumorgrade  and year of diagnosis, seer region
#


#call the functions for generating model and post estiamtion
source('C:/Dropbox/K/pcaOvertreatment/code/functionForMlogitModeling.r')


#10 year survival risk

genFitSubData(c('hig | surv10Yr.low','mid | surv10Yr.low','low | surv10Yr.low')
              ,adjustedVars= c('raceCat','.damicorisk','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','registry')
              ,finalSampleSensitivity , 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.low10yrSurv.RData')


genFitSubData(c('hig | surv10Yr.high','mid | surv10Yr.high','low | surv10Yr.high')
              ,adjustedVars= c('raceCat','.damicorisk','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','registry')
              , finalSampleSensitivity , 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.high10yrSurv.RData')


genFitSubData(c('hig | surv10Yr.mid','mid | surv10Yr.mid','low | surv10Yr.mid')
              ,adjustedVars= c('raceCat','.damicorisk','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','registry')
              , finalSampleSensitivity , 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.mid10yrSurv.RData')


#damico risk

genFitSubData(c('hig | surv10Yr.low','hig | surv10Yr.mid','hig | surv10Yr.high')
              ,adjustedVars= c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat',"surv10YrProb3Cat",'indexyear','registry')
              ,finalSampleSensitivity , 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.highDamicoRisk.RData')

genFitSubData(c('mid | surv10Yr.low','mid | surv10Yr.mid','mid | surv10Yr.high')
              ,adjustedVars= c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat',"surv10YrProb3Cat",'indexyear','registry')
              , finalSampleSensitivity , 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.midDamicoRisk.RData')

genFitSubData(c('low | surv10Yr.low','low | surv10Yr.mid','low | surv10Yr.high')
              ,adjustedVars= c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat',"surv10YrProb3Cat",'indexyear','registry')
              , finalSampleSensitivity, 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.lowDamicoRisk.RData')


#damico and 10yr survival


#low survival
genFitSubData(c('hig | surv10Yr.low')
              ,adjustedVars= c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','registry')
              , finalSampleSensitivity , 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.highDamicoRisk.low10YrSurv.RData')

genFitSubData(c('mid | surv10Yr.low')
              ,adjustedVars= c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','registry')
              , finalSampleSensitivity , 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.midDamicoRisk.low10YrSurv.RData')

genFitSubData(c('low | surv10Yr.low')
              ,adjustedVars= c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','registry')
              , finalSampleSensitivity, 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.lowDamicoRisk.low10YrSurv.RData')

#mid survival

genFitSubData(c('hig | surv10Yr.mid')
              ,adjustedVars= c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','registry')
              , finalSampleSensitivity, 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.highDamicoRisk.mid10YrSurv.RData')

genFitSubData(c('mid | surv10Yr.mid')
              ,adjustedVars= c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','registry')
              , finalSampleSensitivity , 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.midDamicoRisk.mid10YrSurv.RData')

genFitSubData(c('low | surv10Yr.mid')
              ,adjustedVars= c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','registry')
              , finalSampleSensitivity , 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.lowDamicoRisk.mid10YrSurv.RData')


#high survival
genFitSubData(c('hig | surv10Yr.high')
              ,adjustedVars= c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','registry')
              , finalSampleSensitivity , 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.highDamicoRisk.high10YrSurv.RData')

genFitSubData(c('mid | surv10Yr.high')
              ,adjustedVars= c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','registry')
              , finalSampleSensitivity , 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.midDamicoRisk.high10YrSurv.RData')

genFitSubData(c('low | surv10Yr.high')
              ,adjustedVars= c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','registry')
              , finalSampleSensitivity , 'Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.lowDamicoRisk.high10YrSurv.RData')

#----------------------------------------

#generate outputs

low10yrSurv.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.low10yrSurv.RData')
mid10yrSurv.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.mid10yrSurv.RData')
high10yrSurv.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.high10yrSurv.RData')

xtable(low10yrSurv.res$predProb, digits=4)
xtable(mid10yrSurv.res$predProb, digits=4)
xtable(high10yrSurv.res$predProb, digits=4)

#----------------------------------------

lowDamicoRisk.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.lowDamicoRisk.RData')
midDamicoRisk.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.midDamicoRisk.RData')
highDamicoRisk.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.highDamicoRisk.RData')

xtable(lowDamicoRisk.res$predProb, digits=4)
xtable(midDamicoRisk.res$predProb, digits=4)
xtable(highDamicoRisk.res$predProb, digits=4)

#----------------------------------------

lowSurvLowDamico.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.lowDamicoRisk.low10YrSurv.RData')
lowSurvMidDamico.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.midDamicoRisk.low10YrSurv.RData')
lowSurvHighDamico.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.highDamicoRisk.low10YrSurv.RData')



highSurvLowDamico.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.lowDamicoRisk.high10YrSurv.RData')
highSurvMidDamico.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.midDamicoRisk.high10YrSurv.RData')
highSurvHighDamico.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.highDamicoRisk.high10YrSurv.RData')


midSurvLowDamico.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.lowDamicoRisk.mid10YrSurv.RData')
midSurvMidDamico.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.midDamicoRisk.mid10YrSurv.RData')
midSurvHighDamico.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatment/mlogitFitObj_sensitivity/fit.highDamicoRisk.mid10YrSurv.RData')

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


