#1 low | surv10Yr.low by year
#2 low risk disease
#3 only survival 10 low




# source('C:/Dropbox/K/pcaOvertreatment/jamaRevision/genAnalysisData.R')
 interestIniTreatments <- c('abdomen_radpromy','robot_radpromy','ebrt','imrt','obs')
 anaData <- genRevisionData(interestIniTreatments)
#save(anaData,file='Z:/j_scrdata/pcaOverTreatmentRevision/anaData.RData')
(load(file='Z:/j_scrdata/pcaOverTreatmentRevision/anaData.RData'))


source('C:/Dropbox/R_new/example/table1_improved.R')
tab1Output <- table.yz(c('ageCat','raceCat','sesCat','chrlsonRecat','grdCat','stgCat','.damicorisk','surv10YrProb3Cat','indexyear','registry'),'iniTrt',anaData,outDf.indepVn='indepVn')

#  
#  remove mets left 80570 
#  addedVar left.N lost.N
#  1            NA  58672      0
#  2     indexyear  58672      0
#  3        ageCat  58672      0
#  4       raceCat  58631    -41
#  5        grdCat  57569  -1062
#  6        stgCat  56257  -1312
#  7   .damicorisk  56257      0
#  8        sesCat  55971   -286
#  9  chrlsonRecat  55971      0
#  10        urban  55947    -24
#  11     registry  55947      0
#  loading nonCaner survival model 

setwd('Z:/j_scrdata/pcaOverTreatmentRevision')
str(tab1Output)
 write.csv.yz1(tab1Output$tabDf, 'table1Csv66plus')
#write.csv.yz1(tab1Output$tabDf, 'table1Csv')
setwd('Z:/j_scrdata/pcaOverTreatmentRevision')
tab1Output$sample.byDep
subset(tab1Output$tabDf,indepVn %in% c('indexyear=2004','indexyear=2005','indexyear=2006','indexyear=2007'))[,1:11]

 
 #last figure
 #the numerator is the number of imrt robot ebrt open radical and observation....with low risk disease intermedaite high risk disease by year.
 #also for low survival by year
 #number of 3*3 risk categories by (low low, high and high) by year by treatment types.
 

 names(anaData)
 ddply(anaData,c('iniTrt','indexyear','riskStrata'),nrow)
 
 
 
#call the functions for generating model and post estiamtion
source('C:/Dropbox/K/pcaOvertreatment/jamaRevision/rFun/functionForMlogitModeling.r')

#10 year survival risk
genFitSubData(c('hig | surv10Yr.low','mid | surv10Yr.low','low | surv10Yr.low')
              ,c('raceCat','.damicorisk','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','registry')
              ,'iniTrt','obs',anaData, 'Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.low10yrSurv.RData')

 #no index year
 genFitSubData(c('hig | surv10Yr.low','mid | surv10Yr.low','low | surv10Yr.low')
               ,c('raceCat','.damicorisk','sesCat','ageCat','chrlsonRecat','grdCat','registry')
               ,'iniTrt','obs',anaData, 'Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.low10yrSurvNoYear.RData')

 
 count(anaData,'chrlsonRecat')
 
 
 (load('Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.low10yrSurvNoYear.RData'))
 #log Lik.' -18620 (df=124)

 (load('Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.low10yrSurv.RData'))

   'log Lik.' -18013 (df=144)

 1-pchisq(400,20)
 summary(subData.mlogitFit)$CoefTable
 
 
 
 #lwo damico risk
genFitSubData(c('low | surv10Yr.low','low | surv10Yr.mid','low | surv10Yr.high')
              ,adjustedVars= c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat',"surv10YrProb3Cat",'indexyear','registry')
              ,'iniTrt','obs', anaData , 'Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.lowDamicoRisk.RData')
 
 
 #no index year
 
 genFitSubData(c('low | surv10Yr.low','low | surv10Yr.mid','low | surv10Yr.high')
               ,adjustedVars= c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat',"surv10YrProb3Cat",'registry')
               ,'iniTrt','obs', anaData , 'Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.lowDamicoRiskNoYear.RData')
 
 
 count(anaData,'ageCat')
 
 
 
 
#damico and 10yr survival
genFitSubData(c('low | surv10Yr.low')
              ,c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat','indexyear','registry')
              ,'iniTrt','obs', anaData, 'Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.lowDamicoRisk.low10YrSurv.RData')

 #no index year
 genFitSubData(c('low | surv10Yr.low')
               ,c('raceCat','sesCat','ageCat','chrlsonRecat','grdCat','registry')
               ,'iniTrt','obs', anaData, 'Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.lowDamicoRisk.low10YrSurvNoYear.RData')
 
 
 
 names(anaData)
 #no index year
 #10 year survival risk
 
 
 #lwo damico risk
 


 rm(subData.mlogitFit)
 (load( 'Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.low10yrSurvNoYear.RData'))
 shortModel = unclass(logLik(subData.mlogitFit))
 shortModelLogLik=shortModel[[1]]
 shortModelMleDf=attr(shortModel,'df')
 
 rm(subData.mlogitFit)
 
 (load( 'Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.low10yrSurv.RData'))
 longModel = unclass(logLik(subData.mlogitFit))
 longModelLogLik=longModel[[1]]
 longModelMleDf=attr(longModel,'df')
 
 
 
 1-pchisq(-2*(shortModelLogLik-longModelLogLik),  longModelMleDf-shortModelMleDf)
 
 
 logLik(subData.mlogitFit)
 rm(subData.mlogitFit)
 colnames(subData.mlogitFit$model)
 
 mlogitFit=subData.mlogitFit

 names(mlogitFit$model[,1,drop=F])
 
 (load( 'Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.low10yrSurvNoYear.RData'))
noYearVec = rownames(as.data.frame(summary(subData.mlogitFit)$CoefTable))
 
 (load( 'Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.low10yrSurv.RData'))
 yearVec = rownames(as.data.frame(summary(subData.mlogitFit)$CoefTable))
 leftVec = yearVec
 rightVec= noYearVec
 isSubset.yz(yearVec,noYearVec,getThreeSets=T)
 
 
 
rocByChoice=function(mlogitFit,studyChoice, personIdVec){
 predProbMat = mlogitFit$probabilities
 predProbDf = data.frame(choiceCleaned=rep(colnames(predProbMat),nrow(predProbMat)),pred.prob=c(predProbMat))
 choiceDf=rowname2vn.yz(mlogitFit$model[,1,drop=F],rownameVn='personId.choice')
 choiceDf = rename.vars(choiceDf,names(mlogitFit$model[,1,drop=F]),'choice')
 choiceDf[,'choiceCleaned'] = unlist(lapply(choiceDf[,'personId.choice'],function(x){out=strsplit(x,'\\.')[[1]][2];return(out)}))
 choiceDf[,'personId'] = unlist(lapply(choiceDf[,'personId.choice'],function(x){out=strsplit(x,'\\.')[[1]][1];return(out)}))
 choiceDf[,'personId.choice'] <- NULL
 
 studyChoiceProb=data.frame(subset(predProbDf,choiceCleaned==studyChoice),subset(choiceDf,choiceCleaned==studyChoice)[,c('choice',"personId")])
 
 if(missing(personIdVec)){rocObj =roc(studyChoiceProb[,"choice"],studyChoiceProb[,"pred.prob"]) 
                          plot(rocObj)
                          return(rocObj)} else{
                            
                            rocObj =roc(studyChoiceProb[personIdVec,"choice"],studyChoiceProb[personIdVec,"pred.prob"]) 
                            plot(rocObj)
                            return(rocObj)  
                          }
 

}
 
 
 
 
 names(subset(choiceDf,choiceCleaned==studyChoice))
head(subData.mlogitFit$model)
 colnames(predProbMat)

 rowLoc <- 1:nrow(obsChoiceMat)
outlist <- list()
for(i in 1:ncol(predProbMat)) {
  
 mean.predProb=mean(predProbMat[rowLoc,i])
 obs.Chance=mean(obsChoiceMat[rowLoc,i])
 size=length(rowLoc)
 vec = c(mean.predProb,obs.Chance,size)
 outlist=lappend.yz(outlist,vec)
  
}
 
 colnames(predProbMat)
 colnames(obsChoiceMat)
 
strsplit(as.character(formula(subData.mlogitFit)),'~')[[2]]
head(subData.mlogitFit$mode)
 
 
str(subData.mlogitFit)
 
length(coef(subData.mlogitFit))

 
figureDataList <- list()
(load('Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.low10yrSurv.RData'))
figureDataList <- lappend.yz(figureDataList,genPredictProb.ovtrt(subData.mlogitFit))

(load(file='Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.lowDamicoRisk.RData'))
figureDataList <- lappend.yz(figureDataList,genPredictProb.ovtrt(subData.mlogitFit))

(load('Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.lowDamicoRisk.low10YrSurv.RData'))
figureDataList <- lappend.yz(figureDataList,genPredictProb.ovtrt(subData.mlogitFit))
names(figureDataList) <- c('lowSur','lowDamicoRisk','lowSurvLowDamicoRisk')

safeSave.yz(figureDataList,file='Z:/j_scrdata/pcaOverTreatmentRevision/figureDataList.RData',overwrite=T)
(load(file='Z:/j_scrdata/pcaOverTreatmentRevision/figureDataList.RData'))

 
 
 figureDataList
 
 dfList = lapply(figureDataList,function(x){out=as.data.frame(x);names(out)=colnames(x);return(out)})
 
 
 
 
 dF <- rowname2vn.yz( dfList2Df.yz(dfList),rownameVn='treatment')
 
 mDf = melt(dF, id.vars=c('elemName','treatment'))
 mDf[,'year'] = as.numeric(substr(mDf[,'variable'],2,5))
 mDf[,'year'] = as.numeric(substr(mDf[,'variable'],2,5))
 
 mDf[,'iniTreatment'] =unlist(lapply(mDf[,'treatment'],function(x){strsplit(x, "\\.")[[1]][2]})) 
 mDf = remove.vars(mDf,'variable')
 mDf=subset(mDf,iniTreatment %in% c('imrt','robot_radpromy'))
 
mx = rename.vars(mDf,c('elemName'),c('risk.type'))

 tiff('Z:/j_scrdata/pcaOverTreatmentRevision/fig/barChartImrtRobt.tif',width = 480*4, height = 480)
 ggplot(mx, aes(x=risk.type, y=value, fill=iniTreatment,width=0.5)) + 
   geom_bar(stat="identity") + opts(axis.text.x = theme_text(angle=90))+
scale_fill_manual(values = c("black", "grey80",'grey50'))+
   facet_grid(~year)+theme_bw()
 dev.off()
 
 
 names(mx)
 
 source('C:/Dropbox/R_new/example/mlogitFit2CoeffOutput.R')
 
 
 mlogitFit2CoeffOutput <- function(fit){
   options(digits=3)
   coefEstTable <- summary(fit)$CoefTable[,c("Estimate",   "Std. Error", "t-value" ,   "Pr(>|t|)")]
   sigVec <- rep('',nrow(coefEstTable))
   for (i in 1:length(sigVec)){
     if (coefEstTable[i,'Pr(>|t|)'] < 0.01 ) {sigVec[i] <- '**'}
     else if (coefEstTable[i,'Pr(>|t|)'] < 0.05 ) {sigVec[i] <- '*'}
     else if (coefEstTable[i,'Pr(>|t|)'] < 0.1 ) {sigVec[i] <- '.'}
   }
   
   coefDf <- data.frame(coefEstTable[,'Estimate'], coefEstTable[,'Std. Error'],coefEstTable[,"Pr(>|t|)"],sigVec)
   names(coefDf) <- c("Estimate",  'Std. Error', "Pr(>|t|)", 'stat. sig')
   return(coefDf)   
 }
 
 
 
 
 load(file='Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.lowDamicoRisk.low10YrSurv.RData')
 print(xtable(mlogitFit2CoeffOutput(subData.mlogitFit), caption="Regression for patients with low-risk disease and a high risk of non-cancer mortality", label="ALongTable",digits=-3)
        , include.rownames=TRUE
        , tabular.environment="longtable"
        , floating=FALSE
        , size='tiny' , caption.placement="top")
 
 load(file='Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.lowDamicoRisk.RData')
 print( xtable(mlogitFit2CoeffOutput(subData.mlogitFit), caption="Regression for patients with low-risk disease", label="ALongTable",digits=-3)
        , include.rownames=TRUE
        , tabular.environment="longtable"
        , floating=FALSE
        , size='tiny' , caption.placement="top")
 
  
 load(file='Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.low10yrSurv.RData')
 print.xtable( xtable(mlogitFit2CoeffOutput(subData.mlogitFit), caption="Regression for patients with a high risk of non-cancer mortality", label="ALongTable",digits=-3)
        , include.rownames=TRUE
        , tabular.environment="longtable"
        , floating=FALSE
        , size='tiny'
        , caption.placement="top")
 
 #----------------------------------------





#generate outputs

low10yrSurv.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.low10yrSurv.RData')

mid10yrSurv.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.mid10yrSurv.RData')

high10yrSurv.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.high10yrSurv.RData')

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

lowDamicoRisk.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.lowDamicoRisk.RData')
midDamicoRisk.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.midDamicoRisk.RData')
highDamicoRisk.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.highDamicoRisk.RData')

rescale.basedOnTwoChoice(lowDamicoRisk.res$predProb)
rescale.basedOnTwoChoice(midDamicoRisk.res$predProb)
rescale.basedOnTwoChoice(highDamicoRisk.res$predProb)

xtable(lowDamicoRisk.res$predProb, digits=4)
xtable(midDamicoRisk.res$predProb, digits=4)
xtable(highDamicoRisk.res$predProb, digits=4)

#----------------------------------------
lowSurvLowDamico.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.lowDamicoRisk.low10YrSurv.RData')
lowSurvMidDamico.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.midDamicoRisk.low10YrSurv.RData')
lowSurvHighDamico.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.highDamicoRisk.low10YrSurv.RData')

highSurvLowDamico.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.lowDamicoRisk.high10YrSurv.RData')
highSurvMidDamico.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.midDamicoRisk.high10YrSurv.RData')
highSurvHighDamico.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.highDamicoRisk.high10YrSurv.RData')

midSurvLowDamico.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.lowDamicoRisk.mid10YrSurv.RData')
midSurvMidDamico.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.midDamicoRisk.mid10YrSurv.RData')
midSurvHighDamico.res <- fitObj2Output('Z:/j_scrdata/pcaOverTreatmentRevision/mlogitFitObj/fit.highDamicoRisk.mid10YrSurv.RData')


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

