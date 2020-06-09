#save(snc.cleaned,file='Z:/j_scrdata/pcaOverTreatment/snc.cleaned.RData') ini sumdenomnc_demo
(load(file='Z:/j_scrdata/pcaOverTreatment/snc.cleaned.RData'))
#(load(file='Z:/j_scrdata/pcaOverTreatment/finalSample.RData'))
(load(file='Z:/j_scrdata/pcaOverTreatmentRevision/finalSample.jamaRevision.RData'))
#n
finalSample.jamaRevision[,'race'] <- replaceValJoin.yz(finalSample.jamaRevision[,'race'] #this is typically a data column
                                          ,list(c('1'),c('2'),c('3'),c('4'),c('5','6'))
                                          ,c('1','2','3','4','5'))[,2]

final.noreg47 <- subset(finalSample.jamaRevision,!registry=='47')
nrow(final.noreg47)
final.noreg47 <- rename.vars(final.noreg47,'x.ses','ses')

count(finalSample.jamaRevision,'race')

compareVars.yz(snc.cleaned, final.noreg47)
table(finalSample.jamaRevision[,'registry'])
table(snc.cleaned[,'registry'])
#race in final sample has 6 category and in non cancer only has 5
#to trick it to work is to change the race=6 (native amican)


# nlevels(finalSample[,'race'])
# table(finalSample[,'race'])
# # 
# # 
# # fit <- rpart(factor(dieIn10Yr) ~ as.factor(race)+as.factor(chrlsonRecat)+indexage+ses+as.factor(urban)+as.factor(registry)+as.factor(state)+as.factor(county),data=snc.cleaned)
# # 
# trainIndex <- sample(seq(nrow(snc.cleaned)),round(nrow(snc.cleaned)*0.75))
# # valIndex <- setdiff(seq(nrow(snc.cleaned)),trainIndex)
# # snc.cleaned[,'dieIn10Yr'] <- as.factor(snc.cleaned[,'dieIn10Yr'])
# 
# fit <- ipred::bagging(as.factor(dieIn10Yr)~ as.factor(race)+ses+as.factor(urban)+as.factor(chrlsonRecat)+as.factor(registry)+indexage,data=snc.cleaned[trainIndex,],nbagg=10,control=rpart.control(minsplit=100, cp=0, xval=10))
# 
# fit <- ipred::bagging(as.factor(dieIn10Yr)~ race+ses+urban+chrlsonRecat+registry+indexage,data=snc.cleaned[trainIndex,],nbagg=10,control=rpart.control(minsplit=100, cp=0, xval=10))
# predProb.valSample <- predict(fit,snc.cleaned[valIndex,],type='prob')
# roc(snc.cleaned[valIndex,'dieIn10Yr'],predProb.valSample[,2]) 
# #val sample c-index 0.75829
# 
# predProb.trainSample <- predict(fit,snc.cleaned[trainIndex,],type='prob')
# roc.obj <- roc(snc.cleaned[trainIndex,'dieIn10Yr'],predProb.trainSample[,2])
# #within sample 0.80265
# #over fit case has high c-index
# overfit <- ipred::bagging(as.factor(dieIn10Yr) ~ race+ses+urban+chrlsonRecat+indexage+registry,data=snc.cleaned[trainIndex,],nbagg=100,control=rpart.control(minsplit=30, cp=0, xval=10))
# 
# # predProb.trainSample <- predict(overfit,snc.cleaned[trainIndex,],type='prob')
# # roc.obj <- roc(snc.cleaned[trainIndex,'dieIn10Yr'],predProb.trainSample[,2])
# # roc.obj$auc
# #0.91078
# 

#just use the entire sample
# allSampleFit <- ipred::bagging(as.factor(dieIn10Yr)~ race+ses+urban+chrlsonRecat+indexage+registry,data=snc.cleaned,nbagg=100,control=rpart.control(minsplit=30, cp=0, xval=10))
#save(allSampleFit,file='Z:/j_scrdata/pcaOverTreatment/allSampleFit.RData')
(load(file='Z:/j_scrdata/pcaOverTreatment/allSampleFit.RData'))
# rocObj <- roc(snc.cleaned[,'dieIn10Yr'],predict(allSampleFit,snc.cleaned,type='prob')[,2])
# #area under curve 0.91405
# chk<- data.frame(dieIn10Yr=snc.cleaned[,'dieIn10Yr'],predict(allSampleFit,snc.cleaned,type='prob'))
# 
# head(chk)
# ddply(chk,'dieIn10Yr',function(x){mean(x[,'TRUE.'])})
# 
# predict(allSampleFit,snc.cleaned,type='prob')
# 
# y.45 <- x.45<-seq(0,1,0.01)
# tiff('C:/Dropbox/K/pcaOvertreatment/code/fig/rocCurve.tiff')
# plot(1-rocObj$specificities, rocObj$sensitivities, type='l', xlab='False positive rate', ylab='True positive rate', main='Receiver operating characteristic (ROC) curve for 10-year life expectancy prediction', cex.main=0.85, lwd=2)
# lines(x.45,y.45)
# dev.off()
# colnames(predict(allSampleFit,snc.cleaned,type='prob'))

#finalSample is cancer sample
predProb <- predict(allSampleFit,final.noreg47,type='prob') #predProb[,2] is 10 year death prob
finalSample1 <- grpnv.yz(data.frame(final.noreg47,surv10YrProb=predProb[,1]),'surv10YrProb',3,'surv10YrProb3Cat')
ddply(finalSample1,'ageCat',function(x){mean(x[,'surv10YrProb'])})
snames.yz(finalSample1)
finalSample1[,'surv10YrProb3Cat'] <- replaceValJoin.yz(finalSample1[,'surv10YrProb3Cat'] #this is typically a data column
                                          ,names(table(finalSample1[,'surv10YrProb3Cat']))
                                          ,c('surv10Yr.low','surv10Yr.mid','surv10Yr.high')
)[,2]

finalSample1 <- grpnv.yz(finalSample1,'surv10YrProb',4,'surv10YrProb4Cat')

finalSample1[,'surv10YrProb4Cat'] <- replaceValJoin.yz(finalSample1[,'surv10YrProb4Cat'] #this is typically a data column
                                                       ,names(table(finalSample1[,'surv10YrProb4Cat']))
                                                       ,c('surv10Yr.probgrp1','surv10Yr.probgrp2','surv10Yr.probgrp3','surv10Yr.probgrp4')
)[,2]


(load(file='Z:/j_scrdata/pcaOverTreatmentRevision/iniTrtDf.RData'))

isSubset.yz(finalSample2[,'regcase'],iniTrtDf[,'regcase']) 

finalSample2 <- join(finalSample1,iniTrtDf,by='regcase')

finalSample3 <- subset(finalSample2,iniTrt %in% c('abdomen_radpromy','imrt','ebrt','robot_radpromy','obs'))

finalSample3 <- rename.vars(finalSample3,'iniTrt','trtType')
within 6months predicted value of utilization by year.

finalSample3[,'riskStrata'] <- catvars2onecatvar.yz(finalSample3, c('.damicorisk','surv10YrProb3Cat'), ' | ')

table(finalSample3[,'riskStrata'])

#imrt ebrt robot abdomen_radpromy obs



table(finalSample3[,'trtType'])


isSubset.yz(c('indexyear','ageCat','raceCat','grdCat','stgCat','.damicorisk','sesCat','chrlsonRecat','registry','surv10YrProb3Cat','surv10YrProb4Cat'),names(finalSample1))

#load table1.yz function
source('C:/Dropbox/R_new/example/table1_improved.R')
tab1Output <- table.yz(c('indexyear','ageCat','raceCat','grdCat','stgCat','.damicorisk','sesCat','chrlsonRecat','registry','surv10YrProb3Cat','surv10YrProb4Cat'),'trtType',finalSample1,outDf.indepVn='indepVn')
str(tab1Output)
write.csv.yz1(tab1Output$tabDf, 'table1Csv')
tab1Output$sample.byDep


#we need to generate 9 plots., the denomintator should be all patients meet 12-bf 12 after and not die within 6months predicted value of utilization by year.

finalSample3[,'riskStrata'] <- catvars2onecatvar.yz(finalSample3, c('.damicorisk','surv10YrProb3Cat'), ' | ')

#save(finalSample3,file='Z:/j_scrdata/pcaOverTreatmentRevision/finalSample3.RData')
(load(file='Z:/j_scrdata/pcaOverTreatmentRevision/finalSample3.RData'))

ddply(finalSample3,'surv10YrProb4Cat',function(x){mean(x[,'surv10YrProb'])})
ddply(finalSample3,'surv10YrProb3Cat',function(x){mean(x[,'surv10YrProb'])})

#---------New data
# surv10YrProb4Cat   V1
# 1 surv10Yr.probgrp1 0.12
# 2 surv10Yr.probgrp2 0.46
# 3 surv10Yr.probgrp3 0.75
# 4 surv10Yr.probgrp4 0.93
# 
# 
# surv10YrProb3Cat   V1
# 1    surv10Yr.high 0.91
# 2     surv10Yr.low 0.18
# 3     surv10Yr.mid 0.61

#------old data
# surv10YrProb4Cat         V1
# 1 surv10Yr.probgrp1 0.12781029
# 2 surv10Yr.probgrp2 0.45845417
# 3 surv10Yr.probgrp3 0.73823191
# 4 surv10Yr.probgrp4 0.92828095

# surv10YrProb3Cat         V1
# 1    surv10Yr.high 0.90240106
# 2     surv10Yr.low 0.19013722
# 3     surv10Yr.mid 0.61837976



predUse.by.year <- function(df,interestTrtType){
#gen binary indicator dependent variable
#interestTrtType='hasrobotnoradia'
df[,'depVar'] <- (df[,'trtType']==interestTrtType)
df[,'indexyear'] <- as.factor(df[,'indexyear'])
logitModel <- glm(depVar ~ indexyear+sesCat+ageCat+chrlsonRecat+stgCat+raceCat+grdCat+registry, family=binomial("logit"), data=droplevels(df)) 
coeff.vec <- coef(logitModel)
data.mat <- model.matrix(depVar ~ indexyear+sesCat+ageCat+chrlsonRecat+stgCat+raceCat+grdCat+registry, droplevels(df))
data.mat[,c(2,3,4)] <- 0
probVec <- rep(NA,4)
names(probVec) <- c('Yr2004','Yr2005','Yr2006','Yr2007')
for (i in 1:4){
data.mat.i <- data.mat  
if (i==1){data.mat.i <- data.mat} else{data.mat.i[,i] <- 1}
exp.mat.i <- exp(data.mat.i%*%coeff.vec)
prob.mat.i <- exp.mat.i/(1+exp.mat.i)
probVec[i] <- mean(prob.mat.i)
}
re=list(probVec=probVec,modelSummary=summary(logitModel))
return(re)
}

df <- subset(finalSample1,riskStrata=='low | surv10Yr.low')

df <- subset(finalSample1,riskStrata=='mid | surv10Yr.low')

df <- subset(finalSample1,riskStrata=='high | surv10Yr.low')
predUse.by.year(df,'hasrobotnoradia')
predUse.by.year(df,'hasimrtnosrg')
predUse.by.year(df,'observation')









tiff('Z:/j_scrdata/pcaOverTreatment/roc_Hollenbeck.tiff', width=480, height=480)
plot(roc.obj,xlab='False positive rate', ylab='True positive rate', main='Discriminability of 10-year surviva model'
)
legend('bottomright',legend=c('concordance index=0.902'))
dev.off()



#save(fit,file='Z:/j_scrdata/pcaOverTreatment/fit.RData')
#load bag rpart fit model
(load(file='Z:/j_scrdata/pcaOverTreatment/fit.RData'))
#registry should be explored....we want to say we adjusted for registry location. #37 geogiea

#load cancer patient data
# (load(file='Z:/j_scrdata/pcaOverTreatment/cancer.cleaned.RData'))
# predProb.cancerSample <- predict(fit, cancer.cleaned, type='prob')

count(cancer.cleaned,'race')
count(finalSample,'race')
(load(file='Z:/j_scrdata/pcaOverTreatment/finalSample.RData'))
predProb.cancerSample <- predict(fit, finalSample, type='prob')


plotDf <- subset(cbind(cancer.cleaned[,c('indexyear','indexmonth','robot','imrt','.damicorisk')],survProb10yr=predProb.cancerSample[,2]), .damicorisk=='low')

plotDf <- grpnv.yz(plotDf, 'survProb10yr', 4, 'surv10Cat')
tmp <- ddply(plotDf,c('indexyear','indexmonth','surv10Cat'),function(.x){c(robotRate=sum(.x[,'robot'])/nrow(.x),imrtRate=sum(.x[,'imrt'])/nrow(.x),n=nrow(.x))})

lowRiskDf <- subset(tmp, surv10Cat==levels(tmp[,'surv10Cat'])[1])

k=12 #binning 6 month
plotDfLowRisk <- cbind(lowRiskDf,halfYr=rep(seq(1,nrow(lowRiskDf)/k,1),each=k))
imrtUseTrend <- ddply(plotDfLowRisk,'halfYr',function(x){c(sum(x[,'imrtRate']*x[,'n'])/sum(x[,'n']))})

pdf('')
plot(x=imrtUseTrend[,1],y=imrtUseTrend[,2]*100,type='b',xlab='Prostate Cancer Diagnosis Period Since 2004 (unit=6 months)', ylab='IMRT utilization rate among radiation patients(%)')
dev.off()

# error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
#   if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
#     stop("vectors must be same length")
#   arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
# } 


tiff('Z:/j_scrdata/pcaOverTreatment/grant_Hollenbeck.tiff', width=480*1.35, height=480*1.35)
barx <- barplot(imrtUseTrend[,2]*100, names.arg=seq(2004,2007),ylim=c(0,25), col="grey", axis.lty=1, xlab="Year of diagnosis", ylab="IMRT use among radiation patients (%)", cex.names=1.2, cex.axis=1.2)
title('IMRT use among radiation patients with low risk disease and low 10-year life expectancy')
box()
dev.off()



tiff('Z:/j_scrdata/pcaOverTreatment/grant_Hollenbeck.tiff', width=480, height=480)
barx <- barplot(imrtUseTrend[,2]*100, names.arg=seq(2004,2007),ylim=c(0,25), col="grey", axis.lty=1, xlab="Year of diagnosis", ylab="IMRT use among radiation patients (%)", cex.names=1, cex.axis=1)
# title('IMRT use among radiation patients with low risk disease and low 10-year life expectancy')
box()
dev.off()
#error.bar(barx,y.means, 1.96*y.sd/10)

