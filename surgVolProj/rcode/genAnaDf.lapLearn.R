#om dataPrepAndExploreLagBspline.r: I have found the choice of lag=1, knot percentile=75%
#also I found that other surgical volumes and rap volumes have no impact. So I decided to remove them from model

#In this file, my job is to do the final model and then do post-est learning curve extraction
#then do optimization and generate LOS gain through optimization.



cleaned.obj=finalCleaning(anaDf.raw, 0.75,2)

cleaned=cleaned.obj$df
knots.list=cleaned.obj$knots
bdknots.list=cleaned.obj$bdknots
chkMissVars.lag6=c('n.lag6.lap','n.lag6.nonLap','n.lag6.rap','age3cat','race5cat','year','comorbcat','pay1','mdnum1.r')
nomiss.lag6 = droplevels(cleaned[complete.cases(cleaned[,chkMissVars.lag6]),])

testModel.lag1bs <- clm(los ~ lag1.nlap.bs2+lag1.nlap.bs3+age4cat+race5cat+comorbcat+pay1+year+mdnum1.r, data=nomiss.lag6, link=c('probit'))
head(summary(testModel.lag1bs)$coefficients,10)
bsMat = as.matrix(gen.bs(log1p(seq(0,50)),knots.list$lag1,bdknots.list$lag1,2,'lag1.nlap.bs')[,c(2,3)])
vec=bsMat %*% matrix(testModel.lag1bs$beta[1:2],ncol=1)
plot(vec)
nomiss.lag6[,'lag1.nlap.bs3']
k=6
fine.minLos.vol[k]
head(summary(fine.fit.list[[k]])$coefficients,10)
#paste('lag4.nlap.bs',seq(5),sep='')

#I can use 75% that will give the highest volume which is about 88 per year.
#It will be easier to justify a larger number, so I would pick 88.


nomiss.lag6.no48487=droplevels(subset(nomiss.lag6, !mdnum1.r=='48487'))
testModel<- clm(los ~ lag4.nlap.bs1+lag4.nlap.bs2+lag4.nlap.bs3+lag4.nlap.bs4+lag4.nlap.bs5+age4cat+race5cat+comorbcat+pay1+year+mdnum1.r, data=nomiss.lag6, link=c('probit'))

summary(testModel.lag1bs.no48487)
summary(testModel.lag1bs)
testModel.lag1bs.no48487 <- clm(los ~ lag1.nlap.bs1+lag1.nlap.bs2+lag1.nlap.bs3+age4cat+race5cat+comorbcat+pay1+year+mdnum1.r, data=nomiss.lag6.no48487, link=c('probit'))


bsMat = as.matrix(gen.bs(log1p(seq(0,50)),knots.list$lag1,bdknots.list$lag1,2,'lag1.nlap.bs')[,c(1,2,3)])
plot(bsMat %*% matrix(testModel.lag1bs.no48487$beta[c(1,2,3)],ncol=1))
which.min(bsMat %*% matrix(testModel.lag1bs.no48487$beta[c(1,2,3)],ncol=1))

nomiss.lag6.no48487

which.min(bsMat %*% matrix(testModel.lag1bs$beta[1:2],ncol=1))
cbind(bsMat %*% matrix(testModel.lag1bs.no48487$beta[1:3],ncol=1),bsMat %*% matrix(testModel.lag1bs$beta[1:3],ncol=1))
which.min(bsMat %*% matrix(testModel.lag1bs$beta[1:3],ncol=1))

plot(bsMat %*% matrix(testModel.lag1bs.no48487$beta[1:3],ncol=1))

testModel.lag2<- clm(los ~ n.lag2.lap.log+n.lag2.lap.logsq+age4cat+race5cat+comorbcat+pay1+mdnum1.r+year, data=nomiss.lag6, link=c('probit'))
tmp.junk(testModel.lag2)
summary(testModel.lag2)
#70 per year

testModel.lag1<- clm(los ~ n.lag1.lap.log+n.lag1.lap.logsq+n.lag1.lap.logcu+age4cat+race5cat+comorbcat+pay1+mdnum1.r+year, data=nomiss.lag6, link=c('probit'))
tmp.junk(testModel.lag1)

testModel.lag1<- clm(los ~ n.lag1.lap.log+n.lag1.lap.logsq+age4cat+race5cat+comorbcat+pay1+mdnum1.r, data=nomiss.lag6, link=c('probit'))


summary(testModel.lag2)

testModel.08<- clm(los ~ n.lag4.lap.log+n.lag4.lap.logsq+age4cat+race5cat+comorbcat+pay1+mdnum1.r, data=subset(nomiss.lag6,year=='2008'), link=c('probit'))

testModel.09<- clm(los ~ n.lag4.lap.log+n.lag4.lap.logsq+age4cat+race5cat+comorbcat+pay1+mdnum1.r, data=subset(nomiss.lag6,year=='2009'), link=c('probit'))


testModel.07<- clm(los ~ n.lag4.lap.log+n.lag4.lap.logsq+age4cat+race5cat+comorbcat+pay1+mdnum1.r, data=subset(nomiss.lag6,year=='2007'), link=c('probit'))

testModel.06<- clm(los ~ n.lag4.lap.log+n.lag4.lap.logsq+age4cat+race5cat+comorbcat+pay1+mdnum1.r, data=subset(nomiss.lag6,year=='2006'), link=c('probit'))


testModel.05<- clm(los ~ n.lag4.lap.log+n.lag4.lap.logsq+age4cat+race5cat+comorbcat+pay1+mdnum1.r, data=subset(nomiss.lag6,year=='2005'), link=c('probit'))

testModel.10<- clm(los ~ n.lag4.lap.log+n.lag4.lap.logsq+age4cat+race5cat+comorbcat+pay1+mdnum1.r, data=subset(nomiss.lag6,year='2010'), link=c('probit'))

nrow(subset(nomiss.lag6,year=='2008'))

tmp.junk=function(model){
  exp(-summary(model)$coeff[7]/(2*summary(model)$coeff[8]))
  
}

tmp.junk(testModel.05)

table(nomiss.lag6[,'year'])

knots.list$lag1,bdknots
bsMat = as.matrix(gen.bs(seq(0,170),knots.list$lag4,bdknots.list$lag4,3,'lag4.nlap.bs')[,1:6])
plot(bsMat %*% matrix(testModel$beta[1:6],ncol=1),ylim=c(0,100))
lines(lag1.df[,'n.lag1.lap'],lag1.df[,'meanlos'])
summary(testModel)

names(nomiss.lag6)

testModel<- clm(los ~ lag4.nlap.bs1+lag4.nlap.bs2+lag4.nlap.bs3+age4cat+race5cat+comorbcat+pay1+year+mdnum1.r, data=nomiss.lag6, link=c('probit'))

summary(testModel)


bsMat = as.matrix(gen.bs(log1p(seq(0,170)),knots.list$lag4,bdknots.list$lag4,3,'lag4.nlap.bs')[,1:4])
plot(bsMat %*% matrix(testModel$beta[1:4],ncol=1))

bsMat = as.matrix(gen.bs(log1p(seq(0,50)),knots.list$lag1,bdknots.list$lag1,2,'lag4.nlap.bs')[,1:3])
plot(bsMat %*% matrix(testModel$beta[1:3],ncol=1))


which.min(bsMat %*% matrix(testModel$beta[1:4],ncol=1))

summary(testModel)
testModel<- clm(los ~ lag4.nlap.bs2+lag4.nlap.bs3+lag4.nlap.bs4+lag4.nlap.bs5+age4cat+race5cat+year+comorbcat+pay1+year+mdnum1.r, data=nomiss.lag6, link=c('probit'))

#search for the lowest point...


#now need to test these splines...

summary(testModel)

summary(testModel)
summary(riop.lag4.1t2t )
(-riop.lag4.sqrt1st$beta[1]/(2*riop.lag4.sqrt1st$beta[2]))^2




nPatByMd=count(nomiss.lag6,'mdnum1.r')
nPatByMd=rename.vars(nPatByMd,'freq','nPat')
nrow(subset(nPatByMd,nPat==1)) #60 records which corresponds to one single MD

nomiss.lag6.stable=join(intoChar.yz(nomiss.lag6,'mdnum1.r'),intoChar.yz(nPatByMd,'mdnum1.r'),by='mdnum1.r',type='inner')
nomiss.lag6.stable=intoFac.yz(nomiss.lag6.stable,'mdnum1.r')
nomiss.lag6.stable=droplevels(nomiss.lag6.stable[complete.cases(nomiss.lag6.stable[,chkMissVars.lag6]),])
#1 h not good
#riop.lag1 <- clmm(los ~ n.lag1.lap.sqrt+n.lag1.lap+n.lag1.lap.1h+n.lag1.nonLap.cat+agecat+race+year+comorbcat+pay1+hospst+(1|mdnum1.r), data=nomiss.6.le7, link=c('probit'))

namesDf.yz(nomiss)
summary(riop.lag1)
count()



#below is log transformation based on log1p

riop.lag1.log <- clmm(los ~ n.lag1.lap.log+n.lag1.lap.logsq+n.lag1.rap.log+n.lag1.nonLap.5cat+age4cat+race5cat+year+comorbcat+pay1+hospst+(1|mdnum1.r), data=nomiss.lag6, link=c('probit'))
summary(riop.lag1)

#we first ran a random effect model then run a fixed effect model then did a test, our model reject the hypothesis equal, there is potential endogeneity issue, so we adopted fixed effect model. as our final model.


riop.lag1.log.feDoc <- clm(los ~ n.lag1.lap.log+n.lag1.lap.logsq+n.lag1.rap.log+n.lag1.nonLap.5cat+age4cat+race5cat+year+comorbcat+pay1+mdnum1.r, data=subset(nomiss.lag6), link=c('probit'))


riop.lag2.log.feDoc <- clm(los ~ n.lag2.lap.log+n.lag2.lap.logsq+n.lag2.rap.log+n.lag2.nonLap.5cat+age4cat+race5cat+year+comorbcat+pay1+mdnum1.r, data=subset(nomiss.lag6), link=c('probit'))


riop.lag3.log.feDoc <- clm(los ~ n.lag3.lap.log+n.lag3.lap.logsq+n.lag3.rap.log+n.lag3.nonLap.5cat+age4cat+race5cat+year+comorbcat+pay1+mdnum1.r, data=subset(nomiss.lag6), link=c('probit'))

riop.lag4.log.feDoc <- clm(los ~ n.lag4.lap.log+n.lag4.lap.logsq+n.lag4.rap.log+n.lag4.nonLap.5cat+age4cat+race5cat+year+comorbcat+pay1+mdnum1.r, data=subset(nomiss.lag6), link=c('probit'))

riop.lag5.log.feDoc <- clm(los ~ n.lag5.lap.log+n.lag5.lap.logsq+n.lag5.rap.log+n.lag5.nonLap.5cat+age4cat+race5cat+year+comorbcat+pay1+mdnum1.r, data=subset(nomiss.lag6), link=c('probit'))

riop.lag6.log.feDoc <- clm(los ~ n.lag6.lap.log+n.lag6.lap.logsq+n.lag6.rap.log+n.lag6.nonLap.5cat+age4cat+race5cat+year+comorbcat+pay1+mdnum1.r, data=subset(nomiss.lag6), link=c('probit'))


plot(unlist(lapply(list(riop.lag1.log.feDoc, riop.lag2.log.feDoc,riop.lag3.log.feDoc,riop.lag4.log.feDoc, riop.lag5.log.feDoc,riop.lag6.log.feDoc),logLik)),type='b')


#next I somehow to test endogeneity problem. the issue is surgical volume is endogenous.
#phyisican who were good and obtain volume through selective referraol

# used fixed effects estimators to control for unobserved
# hospital heterogeneity that may be correlated with volume (Hamilton and Ho (1998), Hamilton
#                                                            and Hamilton (1997), and Farley and Ozminkowski (1992)).

#---------the above is log transofrmaiton using log1p
# 
# riop.lag1 <- clmm(los ~ n.lag1.lap.sqrt+n.lag1.lap+n.lag1.rap+n.lag1.nonLap.5cat+age4cat+race5cat+year+comorbcat+pay1+hospst+(1|mdnum1.r), data=nomiss.lag6, link=c('probit'))
# summary(riop.lag1)
# #riol.lag2 <- clmm(los ~ n.lag2.lap.sqrt+n.lag2.lap+n.lag2.lap.1h+n.lag2.nonLap.cat+agecat+race+year+comorbcat+pay1+hospst+(1|mdnum1.r), data=nomiss.6.le7, link=c('logit'))
# #not very good only sqrt is sig, do not use 1.5 order
# 
# #use sqrt and lst order it is very strong, this should be good enough, use 1h will not be good
# riop.lag2 <- clmm(los ~ n.lag2.lap.sqrt+n.lag2.lap+n.lag2.rap+n.lag2.nonLap.5cat+age4cat+race5cat+year+comorbcat+pay1+hospst+(1|mdnum1.r), data=nomiss.lag6, link=c('probit'))
# summary(riop.lag2)
# 
# riop.lag2.1h <- clmm(los ~ n.lag2.lap.sqrt+n.lag2.lap+n.lag2.rap+n.lag2.nonLap.5cat+age4cat+race5cat+year+comorbcat+pay1+hospst+(1|mdnum1.r), data=nomiss.lag6, link=c('probit'))
# summary(riop.lag2.1h)
# riop.lag2.sq <- clmm(los ~ n.lag2.lap.sqrt+n.lag2.lap+n.lag2.rap+n.lag2.nonLap.5cat+age4cat+race5cat+year+comorbcat+pay1+hospst+(1|mdnum1.r), data=nomiss.lag6, link=c('probit'))
# summary(riop.lag2.sq)
# 
# 
# riop.lag2.1t1h <- clmm(los ~ n.lag2.lap.1t+n.lag2.lap.1h+n.lag2.rap+n.lag2.nonLap.5cat+age4cat+race5cat+year+comorbcat+pay1+hospst+(1|mdnum1.r), data=nomiss.lag6, link=c('probit'))
# summary(riop.lag2.1t1h)
# 
# 
# riop.lag2.1t2t <- clmm(los ~ n.lag2.lap.1t+n.lag2.lap.2t+n.lag2.rap+n.lag2.nonLap.5cat+age4cat+race5cat+year+comorbcat+pay1+hospst+(1|mdnum1.r), data=nomiss.lag6, link=c('probit'))
# summary(riop.lag2.1t2t)

#not good.
# riop.lag2.1t1h1f <- clmm(los ~ n.lag2.lap.1f+ n.lag2.lap.1t+n.lag2.lap.1h+n.lag2.rap+n.lag2.nonLap.5cat+age4cat+race5cat+year+comorbcat+pay1+hospst+(1|mdnum1.r), data=nomiss.lag6, link=c('probit'))
# summary(riop.lag2.1t1h1f)
# 
# riop.lag2.1fsqrt <- clmm(los ~ n.lag2.lap.1f+n.chlag2.lap.sqrt+n.lag2.rap+n.lag2.nonLap.5cat+age4cat+race5cat+year+comorbcat+pay1+hospst+(1|mdnum1.r), data=nomiss.lag6, link=c('probit'))
# summary(riop.lag2.1fsqrt)


riop.lag4.1t2t <- clm(los ~ n.lag4.lap.1t+n.lag4.lap.2t+n.lag4.rap+n.lag4.nonLap.5cat+age4cat+race5cat+year+comorbcat+pay1+mdnum1.r, data=nomiss.lag6, link=c('probit'))

ddply(nomiss.lag6,c('mdnum1.r','year'),function(x){cbind(nrow(x),x[,'n.lag4.'])})

riop.lag4.1t2t <- clm(los ~ n.lag4.lap+n.lag4.lap.sq+mdnum1.r, data=subset(nomiss.lag6,nPat>4), link=c('probit'))

names(nomiss.lag6)

(-riop.lag4.1t2t$beta[1]/(2*riop.lag4.1t2t$beta[2]))^3
#added int will have problem due ot multcollinear...
riop.lag4.1t2t <- clm(los ~ n.lag4.lap+n.lag4.lap.sq+age4cat+comorbcat+pay1+mdnum1.r, data=nomiss.lag6, link=c('probit'))

testModel<- clm(los ~ lag4.nlap.bs1+lag4.nlap.bs2+lag4.nlap.bs3+lag4.nlap.bs4+lag4.nlap.bs5+age4cat+comorbcat+pay1+mdnum1.r, data=nomiss.lag6, link=c('probit'))
summary(testModel)
summary(riop.lag4.1t2t )
(-riop.lag4.sqrt1st$beta[1]/(2*riop.lag4.sqrt1st$beta[2]))^2

logLik(riop.lag4.1t2t)

riop.lag4.sqrt1st <- clm(los ~ n.lag4.lap.sqrt+n.lag4.lap+n.lag4.nonLap.5cat+age4cat+race5cat+year+comorbcat+pay1+mdnum1.r, data=nomiss.lag6, link=c('probit'))


lapply(list(riop.lag4.1t2t,riop.lag4.sqrt1st),logLik)


riop.lag4.1st2nd <- clm(los ~ n.lag4.lap+n.lag4.lap.sq, data=droplevels(subset(nomiss.lag6.stable,nPat>0)), link=c('probit'))
logLik(riop.lag4.1st2nd)
-riop.lag4.1st2nd$beta[1]/(2*riop.lag4.1st2nd$beta[2])

ddply(nomiss.lag6,'year',function(x){mean(x[,'n.lag4.lap'])})

riop.lag4.1st2nd <- clm(los ~ n.lag4.lap.log+n.lag4.lap.logsq+age3cat+race5cat+mdnum1.r+comorbcat+pay1+n.lag4.nonLap.5cat, data=droplevels(subset(nomiss.lag6.stable,nPat>1)), link=c('probit'))
logLik(riop.lag4.1st2nd)
(1/52)*((-riop.lag4.1st2nd$beta[1]/(2*riop.lag4.1st2nd$beta[2]))^2)

(1/52)*(exp(-riop.lag4.1st2nd$beta[1]/(2*riop.lag4.1st2nd$beta[2]))-1)
summary(riop.lag4.1st2nd)

opRiFit=riop.lag4.log.feDoc
summary(riop.lag4.log.feDoc)
exp(1.9822e-01/(2*4.6151e-02))-1



is.factor(nomiss.lag6.stable[,'mdnum1.r'])
nomiss.lag6.stable[,'n.lag4.lap']

nomiss.lag6[1:10,c('n.lag4.lap','n.lag4.lap.sq')]

nrow(nomiss.lag6.stable)
is.factor(nomiss.lag6[,'mdnum1.r'])


logLik(riop.lag1)
#-5898.6209 (df=37)

logLik(riop.lag2.1fsqrt) #'log Lik.' -5897.7297 (df=37)
logLik(riop.lag2) #'log Lik.' -5897.1422 (df=37)
logLik(riop.lag2.1t2t) #'log Lik.' -5897.0257 (df=37) this is preferred model



#save(nomiss.lag6, riop.lag1,riop.lag2,riop.lag3,riop.lag4,riop.lag5,riop.lag6, file='Z:/j_scrdata/lapLearn/May132013_sensitivity.RData')


# Based on the following we picked lag2, good now.
# [,1]       [,2]
# [1,]    1 -5898.6209
# [2,]    2 -5897.1422 #we then can choose this one based on likelihood
# [3,]    3 -5899.2092
# [4,]    4 -5900.9315
# [5,]    5 -5897.7006
# [6,]    6 -5898.1471

#add sqrt, 1, 2 is not good. second order i.e., 2 is not useful...
# riop.lag2.1 <- clmm(los ~ n.lag2.lap.sqrt+n.lag2.lap+n.lag2.lap.sq+n.lag2.nonLap.cat+agecat+race+year+comorbcat+pay1+hospst+(1|mdnum1.r), data=nomiss.6.le7, link=c('probit'))
# summary(riop.lag2.1)

#interaction terms is hard to write...there some a<0 not easy to write. so use non-interaction term
# riop.lag2.inter <- clmm(los ~ n.lag2.lap.sqrt*n.lag2.nonLap.cat+n.lag2.lap*n.lag2.nonLap.cat+n.lag2.nonLap.cat+agecat+race+year+comorbcat+pay1+hospst+(1|mdnum1.r), data=nomiss.6.le7, link=c('probit'))
# summary(riop.lag2.inter)

# #hard to write it
# riop.lag2.inter.1 <- clmm(los ~ n.lag2.lap.sqrt+n.lag2.lap+n.lag2.lap.sqrt*n.lag2.nonLap+n.lag2.lap*n.lag2.nonLap+agecat+race+year+comorbcat+pay1+hospst+(1|mdnum1.r), data=nomiss.6.le7, link=c('probit'))
# summary(riop.lag2.inter.1)

#the following for sensitivity analysis

logLikVec=unlist(lapply(list(riop.lag1,riop.lag2, riop.lag3, riop.lag4, riop.lag5, riop.lag6),logLik))
#use lag 1 is the best. 
plot(logLikVec,type='b',ylim=c(-5920,-5880))
#the change in loglikelihood is little, so we choose the one that is best for planning, i.e., annual surgery is good for planning.
#so we chose lag4, we will use the same data nomiss.6.1e7

# summary(riop.lag4)
# 
# #use lag1
# #'log Lik.' -6078.2 (df=33)
# oprobit.re.lag1.sensitivity <- clmm(as.factor(los) ~ n.lag1.lap.sqrt+n.lag1.lap+factor(n.lag1.nonLap.cat)+factor(agecat)+factor(race)+factor(year)+factor(comorbcat)+factor(pay1)+(1|mdnum1.r), data=nomiss.4.le7, link=c('probit'))
# #you can pick up the spill over effect using lag2
# #-6081 
# oprobit.re.lag2.sensitivity <- clmm(as.factor(los) ~ n.lag2.lap.sqrt+n.lag2.lap+factor(n.lag2.nonLap.cat)+factor(agecat)+factor(race)+factor(year)+factor(comorbcat)+factor(pay1)+(1|mdnum1.r), data=nomiss.4.le7, link=c('probit'))
# 
# oprobit.re.lag3.sensitivity <- clmm(as.factor(los) ~ n.lag3.lap.sqrt+n.lag3.lap+factor(n.lag3.nonLap.cat)+factor(agecat)+factor(race)+factor(year)+factor(comorbcat)+factor(pay1)+(1|mdnum1.r), data=nomiss.4.le7, link=c('probit'))
# logLik(oprobit.re.lag3.sensitivity )
# #'log Lik.' -6081.2 (df=33)
# oprobit.re.lag4.sensitivity <- clmm(as.factor(los) ~ n.lag4.lap.sqrt+n.lag4.lap+factor(n.lag4.nonLap.cat)+factor(agecat)+factor(race)+factor(year)+factor(comorbcat)+factor(pay1)+(1|mdnum1.r), data=nomiss.4.le7, link=c('probit'))
# logLik(oprobit.re.lag4.sensitivity)
# #'log Lik.' -6081.8 (df=33)
# names(nomiss.4)
# #nomiss.4 has 6360 patients, is the smallest dataset, we will use this one for sensitivity analysis to study which lag is the best.
# (losDensity=density.yz(nomiss.1[,'los']))
# 

# 
# oprobit.re.lag1.sensitivity.1h <- clmm(as.factor(los) ~ n.lag1.lap.sqrt+n.lag1.lap+n.lag1.lap.1h+factor(n.lag1.nonLap.cat)+factor(agecat)+factor(race)+factor(year)+factor(comorbcat)+factor(pay1)+(1|mdnum1.r), data=nomiss.4.le7, link=c('probit'))
# summary(oprobit.re.lag1.sensitivity.1h) #not good
# 
# 
# #lag 2 is good based on nomiss.4, but has one term that is not stat sig, but  I need to increase the sample size a bit...I
# summary(oprobit.re.lag2.sensitivity.1h)
# oprobit.re.lag2.sensitivity.1h <- clmm(as.factor(los) ~ n.lag2.lap.sqrt+n.lag2.lap+n.lag1.lap.1h+factor(n.lag2.nonLap.cat)+factor(agecat)+factor(race)+factor(year)+factor(comorbcat)+factor(pay1)+(1|mdnum1.r), data=nomiss.4.le7, link=c('probit'))
# 
# nomiss.2[,'race5cat']=replaceValJoin.yz(nomiss.2[,'race'] #this is typically a data column
#                   ,list(1,2,3,4,c(5,6)) 
#                   ,c(1,2,3,4,5)
#                   ,origValVn='race'
#                   ,newValVn='race5cat' #if output is vector, this newValVn is inapplicable
#                   ,outputJoinedDf=TRUE #if F means only output the new vector
#                   #if T, then output df             
# )[,'race5cat']
# 
# nomiss.2=intoFac.yz(nomiss.2,c('race5cat','race','year','agecat','comorbcat','pay1'))
# # so I use the larger data (nomiss.2)
# 
# nomiss.2.le7=subset(nomiss.2, los<=7)
# table(nomiss.2[,'los'])
# 
# nomiss.2.le7=intoFac.yz(nomiss.2.le7,c('los','hospst'))
# nomiss.2.le7[,'los']
# 
# 
# 
# table(nomiss.2.le7[,'los'])
# 
# nonLapCuts.lag2= quantile(uniqueRows.yz(nomiss.2.le7[,c('n.lag2.nonLap','mdId')])[,'n.lag2.nonLap'],probs=seq(0,1,0.25),na.rm=T)
# nomiss.2.le7=grpnv.supplycuts.yz(nomiss.2.le7, 'n.lag2.nonLap', nonLapCuts.lag2, 'n.lag2.nonLap.cat')
# is.factor(nomiss.2.le7[,'n.lag2.nonLap.cat'])
# 
# 
# oprobit.re.lag2.1h.final  <- clmm(los ~ n.lag2.lap.sqrt+n.lag2.lap+n.lag1.lap.1h+n.lag2.nonLap.cat+agecat+race+year+comorbcat+pay1+(1|mdnum1.r), data=nomiss.2.le7, link=c('probit'))
# 
# oprobit.re.lag2.1h.final.wState <- clmm(los ~ n.lag2.lap.sqrt+n.lag2.lap+n.lag1.lap.1h+n.lag2.nonLap.cat+agecat+race5cat+year+comorbcat+pay1+hospst+(1|mdnum1.r), data=nomiss.2.le7, link=c('probit'))
# 
# oprobit.re.lag2.1h.final.wState <- clmm(los ~ n.lag2.nonLap.cat+agecat+race5cat+year+comorbcat+pay1+hospst+(1|mdnum1.r)+(1|n.lag2.lap.sqrt+n.lag2.lap+n.lag1.lap.1h, data=nomiss.2.le7, link=c('probit'))
# 


# race
# 1 White 
# 2 Black 
# 3 Hispanic 
# 4 Asian or Pacific Islander 
# 5 Native American 
# 6 Other 

# 1 Medicare 
# 2 Medicaid 
# 3 Private insurance 
# 4 Self-pay 
# 5 No charge 
# 6 Other 


#save the final fit object
# save(oprobit.re.lag2.sensitivity.1h.final #final model
#      ,nomiss.2.le7 #final data
#      ,nomiss.4.le7 #used for sensitvity analysis
#      ,nomiss.1
#      ,nomiss.2
#      ,nomiss.3
#      ,nomiss.4
#      
#      ,oprobit.re.lag1.sensitivity #models for sensititty for lag1 lag2 lag3 lag4
#      ,oprobit.re.lag2.sensitivity
#      ,oprobit.re.lag3.sensitivity
#      ,oprobit.re.lag4.sensitivity
#      ,
#      file="Z:/j_scrdata/lapLearn/May092013Modeling.RData"
#      )

#(load(file="Z:/j_scrdata/lapLearn/May092013Modeling.RData"))


opRiFit=riop.lag4.log.feDoc
summary(riop.lag4.log.feDoc)
exp(1.9822e-01/(2*4.6151e-02))-1
# eblupDf= as.data.frame(opRiFit$ranef)
# names(eblupDf)='eblup'
# eblupDf=rowname2vn.yz(eblupDf,rownameVn=names(opRiFit$ranef))
# head(eblupDf)
# learnCurveCoef=coef(opRiFit)[c('n.lag2.lap.sqrt','n.lag2.lap','n.lag1.lap.1h')]
# lag.nLap.vec=seq(0,50)
# plot(learnCurveCoef[1]*lag.nLap.vec^0.5+learnCurveCoef[2]*lag.nLap.vec+learnCurveCoef[3]*lag.nLap.vec^1.5)
# commonLearn=learnCurveCoef[1]*lag.nLap.vec^0.5+learnCurveCoef[2]*lag.nLap.vec+learnCurveCoef[3]*lag.nLap.vec^1.5

# count(eblupDf,'mdnum1.r')
# 
# glist=list()
# nDoc = nrow(eblupDf)
# for(i in 1:nDoc){
#   
#   glist[[i]]=commonLearn+eblupDf[i,1]
#   
# }
# 
# for(i in 1:length(glist)){
# if(i==1) {plot(glist[[i]],ylim=c(-1.8,1.5))} else
# {lines(glist[[i]])}
# }

#need to generate a population level learning curve for each individual surgeon
surgeonId= mdVec[i]
genExanteIndivLearnCurve(opRiFit, mdVec[i], state.data, seq(0,100,1),whichLag)
genIndivLearnCurve = function(opRiFit, surgeonId, newData, lagVol.vec){
  #basically score each surgeon for all the patients belong to a region (e.g., a state, or a county or something) he resides.  
  eblupDf= as.data.frame(opRiFit$ranef)
  names(eblupDf)='eblup'
  eblupDf=rowname2vn.yz(eblupDf,rownameVn=names(opRiFit$ranef))
  eblupDf[which(eblupDf[,'mdnum1.r']==surgeonId),'eblup']
  
  J=length(opRiFit$y.levels) #number of ordered choices
  cuts=opRiFit$alpha; betaVec=opRiFit$beta
  formu=as.list(opRiFit$call)$formula
  rhs.formu=paste('~',strsplit(as.character(formu), "~")[[3]],sep='')
  meanLosVec=rep(NA,length(lagVol.vec))
  for(i in 1:length(lagVol.vec)){
    cat('i=',i, '\n')
    newData[,'n.lag2.lap']=lagVol.vec[i]
    newData[,'n.lag2.lap.sqrt']=lagVol.vec[i]^0.5
    newData[,'n.lag2.lap.1h']=lagVol.vec[i]^1.5 
    dataMat=model.matrix(as.formula(rhs.formu),data=newData)
    XB=dataMat[,-c(1,ncol(dataMat))] %*% matrix(betaVec,ncol=1)
    #now we change eblupDf eblup into the picked surgeon
    XBPlusRi=XB+eblupDf[which(eblupDf[,2]==surgeonId),'eblup']
    #I did a check on the correlation between eblup and outcome and confirm that it should be PLUS not minus
    cumProbMat=matrix(NA,nrow=length(XBPlusRi),ncol=J)
    for (j in 1:(J-1)){
      cumProbMat[,j]=pnorm(cuts[j]-XBPlusRi)
    }
    cumProbMat[,J]=1
    probMat=array(NA,dim=dim(cumProbMat))
    probMat[,1]=cumProbMat[,1]
    for(j in 2:J){
      probMat[,j]=cumProbMat[,j]-cumProbMat[,j-1]
    }
    
    colnames(probMat)=opRiFit$y.levels
    meanLosVec[i]=mean(probMat %*% matrix(seq(1:7), ncol=1))
  }
  return(meanLosVec)
}

#When I was doing random effect model.

newData=nomiss.lag6        

n.sims=100
lagVol.vec=seq(0,175,1)
range(nomiss.lag6[,'n.lag4.lap']) #0,173

genAvgLearnCurve = function(opRiFit, newData, lagVol.vec, n.sims
                            , whichLag #'lag1', 'lag2' 'lag3' ect
                            ){
  #basically score each surgeon for all the patients belong to a region (e.g., a state, or a county or something) he resides.  
  J=length(opRiFit$y.levels) #number of ordered choices
  betaVec=opRiFit$beta
  coef=opRiFit$coefficients
  covmat=vcov(opRiFit)
  set.seed(1)
  coefMat=mnormt::rmnorm(n = n.sims, mean=opRiFit$coefficients, vcov(opRiFit))
  
  
  formu=as.list(opRiFit$call)$formula
  rhs.formu=paste('~',strsplit(as.character(formu), "~")[[3]],sep='')
  
  dataMat=model.matrix(as.formula(rhs.formu),data=newData)
  
  meanLosMat=matrix(NA,nrow=length(lagVol.vec), ncol=n.sims)
  
  for(s in 1:n.sims){
     cat('s=',s,'\n')
    cuts.s= coefMat[s,names(opRiFit$alpha)]
    betaVec.s=coefMat[s,names(opRiFit$beta)]
    for(i in 1:length(lagVol.vec)){
      #cat('vol',lagVol.vec[i], '\n')
      if (whichLag=='lag1'){
       
        dataMat[,'n.lag1.lap.log']=log1p(lagVol.vec[i])
        dataMat[,'n.lag1.lap.logsq']=log1p(lagVol.vec[i])^2
      }
      
      if (whichLag=='lag2'){
        dataMat[,'n.lag2.lap.log']=log1p(lagVol.vec[i])
        dataMat[,'n.lag2.lap.logsq']=log1p(lagVol.vec[i])^2
      }
      if (whichLag=='lag3'){
      
        dataMat[,'n.lag3.lap.log']=log1p(lagVol.vec[i])
        dataMat[,'n.lag3.lap.logsq']=log1p(lagVol.vec[i])^2
      }
        
        if (whichLag=='lag4'){
          dataMat[,'n.lag4.lap.log']=log1p(lagVol.vec[i])
          dataMat[,'n.lag4.lap.logsq']=log1p(lagVol.vec[i])^2
        }
        
        if (whichLag=='lag5'){
          dataMat[,'n.lag5.lap.log']=log1p(lagVol.vec[i])
          dataMat[,'n.lag5.lap.logsq']=log1p(lagVol.vec[i])^2
        }
      
      #XB=dataMat[,-c(1,ncol(dataMat))] %*% matrix(betaVec.s,ncol=1)
      XB=dataMat[,-1] %*% matrix(betaVec.s,ncol=1)
      #cbind(colnames(dataMat[,-c(1,ncol(dataMat))]),names(betaVec.s))
      #now we change eblupDf eblup into the picked surgeon
      XBPlusRi=XB+0 #average one 
      #I did a check on the correlation between eblup and outcome and confirm that it should be PLUS not minus
      cumProbMat=matrix(NA,nrow=length(XBPlusRi),ncol=J)
      for (j in 1:(J-1)){
        cumProbMat[,j]=pnorm(cuts.s[j]-XBPlusRi)
      }
      cumProbMat[,J]=1
      probMat=array(NA,dim=dim(cumProbMat))
      probMat[,1]=cumProbMat[,1]
      for(j in 2:J){
        probMat[,j]=cumProbMat[,j]-cumProbMat[,j-1]
      }
      colnames(probMat)=opRiFit$y.levels
      meanLosMat[i,s]=mean(probMat %*% matrix(seq(1:J), ncol=1))
      #print(meanLosMat[i,s])
    } #i loop

    } #s loop

  colnames(meanLosMat)=paste('sim', seq(n.bt),sep='.')
  print(meanLosMat)
  outDf=data.frame(lag.lap.vol=lagVol.vec,meanLosMat)
  return(outDf)
  }




#opRiFit=riop.lag2
opRiFit=riop.lag4.log #this is the chosen model


 #300 is large enought to get confidence interval

n.bt=100
volume.vec= seq(0,175,1)
range(nomiss.lag6[,'n.lag4.lap'])
avgLearningCurveDf = genAvgLearnCurve(opRiFit, nomiss.lag6, volume.vec, n.bt, whichLag='lag4')

avgLearningCurveDf
save(avgLearningCurveDf, file='Z:/j_scrdata/lapLearn/avgLearningCurve_lag4_log_may142013.RData')
#save(avgLearningCurveMat, file='Z:/j_scrdata/lapLearn/avgLearningCurve_may102013.RData') 

# save(avgLearningCurveMat, file='Z:/j_scrdata/lapLearn/avgLearningCurve_lag2_1t2t_may132013.RData') 
# save(avgLearningCurveMat, file='Z:/j_scrdata/lapLearn/avgLearningCurve_lag4_log_may142013.RData') 
avgLearningCurveMat=avgLearningCurveDf[,2:ncol(avgLearningCurveDf)]
lbUbMat=apply(avgLearningCurveMat,1,function(x){quantile(x,prob=c(0.10,0.9))}) 
avgLearnCurve=cbind(avgLearningCurveDf[,1],apply(avgLearningCurveMat,1,mean),t(lbUbMat)) 
colnames(avgLearnCurve)=c('n.lag.lap','mean.los','lb.los','ub.los')
#plot(nomiss.lag6[,'n.lag2.lap'],nomiss.lag6[,'los'])
library(ggplot2)
# chk=ddply(nomiss.lag6,'n.lag2.lap',function(x){mean(as.numeric(x[,'los']),na.rm=T)})

#save(avgLearningCurveMat , avgLearnCurve,file='Z:/j_scrdata/lapLearn/avgLearnCurve_may102013.RData')
qplot(n.lag.lap, mean.los, data=as.data.frame(avgLearnCurve), xlab='Number of MIRP in past 6 months', ylab='Expected length of stay for a MIRP')+geom_smooth(aes(ymin=lb.los, ymax=ub.los), data=as.data.frame(avgLearnCurve), stat="identity")+opts(axis.line = theme_segment(colour = "black"),panel.grid.major = theme_blank(), panel.grid.minor = theme_blank(),panel.border = theme_blank(),panel.background = theme_blank()) 
      

newData=state.data
get.FE.model.indivLearnCurve = function(opRiFit, surgeonId, newData, lagVol.vec, whichLag){
  #basically score each surgeon for all the patients belong to a region (e.g., a state, or a county or something) he resides.  
  J=length(opRiFit$y.levels) #number of ordered choices
  cuts=opRiFit$alpha; betaVec=opRiFit$beta
  formu=as.list(opRiFit$call)$formula
  rhs.formu=paste('~',strsplit(as.character(formu), "~")[[3]],sep='')
  
  #just change newData's md into the md of interest
  newData[,'mdnum1.r']=surgeonId
  #levels(newData[,'mdnum1.r'])
  dataMat=model.matrix(as.formula(rhs.formu),data=newData)
  
  meanLosVec=rep(NA,length(lagVol.vec))
  
  for(i in 1:length(lagVol.vec)){
    cat('i=',i, '\n')
    if (whichLag=='lag1'){
      dataMat[,'n.lag1.lap.log']=log1p(lagVol.vec[i])
      dataMat[,'n.lag1.lap.logsq']=log1p(lagVol.vec[i])^2
    }
    if (whichLag=='lag2'){
      dataMat[,'n.lag2.lap.log']=log1p(lagVol.vec[i])
      dataMat[,'n.lag2.lap.logsq']=log1p(lagVol.vec[i])^2
    }
    if (whichLag=='lag3'){
      dataMat[,'n.lag3.lap.log']=log1p(lagVol.vec[i])
      dataMat[,'n.lag3.lap.logsq']=log1p(lagVol.vec[i])^2
    }
    if (whichLag=='lag4'){
      dataMat[,'n.lag4.lap.log']=log1p(lagVol.vec[i])
      dataMat[,'n.lag4.lap.logsq']=log1p(lagVol.vec[i])^2
    }
 
    #XB=dataMat[,-c(1,ncol(dataMat))] %*% matrix(betaVec,ncol=1) #this is for random effect model
    XB=dataMat[,-1] %*% matrix(betaVec,ncol=1) 
    #now we change eblupDf eblup into the picked surgeon
    XBPlusRi=XB+0
    #I did a check on the correlation between eblup and outcome and confirm that it should be PLUS not minus
    cumProbMat=matrix(NA,nrow=length(XBPlusRi),ncol=J)
    for (j in 1:(J-1)){
      cumProbMat[,j]=pnorm(cuts[j]-XBPlusRi)
    }
    cumProbMat[,J]=1
    probMat=array(NA,dim=dim(cumProbMat))
    probMat[,1]=cumProbMat[,1]
    for(j in 2:J){
      probMat[,j]=cumProbMat[,j]-cumProbMat[,j-1]
    }
    
    colnames(probMat)=opRiFit$y.levels
    meanLosVec[i]=mean(probMat %*% matrix(seq(1:7), ncol=1))
  }
  outmat=cbind(lagVol.vec, meanLosVec)
  colnames(outmat)=c('lag.vol','mean.los')
  return(outmat)
}
#for each parametric bootstrap coeffect, you can get a M learning curves for M doctors
#then for each of the M learning curves, you can solve for an allocation, 
gen.g.list=function(inNomissDf, state, opRiFit,lagVol.vec, whichLag){
#   state.data=subset(nomiss.2.le7, hospst=='IA')
  state.data=subset(inNomissDf, hospst==state)
  mdVec=unique(state.data[,'mdnum1.r'])
  g.list=list()
  
  for(i in 1:length(mdVec)){
   g.list=lappend.yz(g.list, get.FE.model.indivLearnCurve(opRiFit, mdVec[i], state.data, lagVol.vec,whichLag))
  }
  names(g.list)=mdVec
  return(g.list)
}
inNomissDf=nomiss.lag6
state='IA'
whichLag='lag4'
g.list.ia=gen.g.list(nomiss.lag6, 'IA', opRiFit ,seq(0,175,1),'lag4')
g.list.ny=gen.g.list(nomiss.lag6, 'NY', opRiFit ,seq(0,175,1),'lag4')
g.list.md=gen.g.list(nomiss.lag6, 'MD', opRiFit ,seq(0,175,1),'lag4')
plot(g.list.ia[[1]])
lines(g.list.ia[[2]])

g.list.ia
                                        
plot(cumsum(g.list.md[[1]]))
                              

#I could have g.list with MdId, but I forget, so I extract them seaprately
#the first elemetn of mdVec.ia corresponds to the first element of g.list.ia
mdVec.ia=unique(subset(nomiss.lag6, hospst=='IA')[,'mdnum1.r'])
mdVec.md=unique(subset(nomiss.lag6, hospst=='MD')[,'mdnum1.r'])
mdVec.ny=unique(subset(nomiss.lag6, hospst=='NY')[,'mdnum1.r'])


#save(g.list.ia, g.list.md, g.list.ny, file='Z:/j_scrdata/lapLearn/glists.RData') 
                    
#May 14 I used log1p and lag4 to fun model and obtained g.list
#save(mdVec.ia, mdVec.md, mdVec.ny, g.list.ia, g.list.md, g.list.ny, file='Z:/j_scrdata/lapLearn/glists_May142013.RData')

load(file='Z:/j_scrdata/lapLearn/glists_May142013.RData')                   
                                        srgSrchFile.ia=xpt2r.yz("Z:/j_scrdata/lapLearn","srgSearchFile_ia")
                                        srgSrchFile.ia[,'key']= as.character(srgSrchFile.ia[,'key'])
                                        
                                        srgSrchFile.md=xpt2r.yz("Z:/j_scrdata/lapLearn","srgSearchFile_md")
                                        srgSrchFile.md[,'key']= as.character(srgSrchFile.md[,'key'])
                                        
                                        srgSrchFile.ny=xpt2r.yz("Z:/j_scrdata/lapLearn","srgSearchFile_ny")
                                        srgSrchFile.ny[,'key']= as.character(srgSrchFile.ny[,'key'])


extract.lap=function(srgSrchFile){
  isLap=rep(NA,length(nrow(srgSrchFile)))                                        
  for(ii in 1:nrow(srgSrchFile)){                                        #  cat('ii=',ii,'\n')
    isRap[ii]=any(srgSrchFile[ii,c('pr1','pr2','pr3','pr4','pr5','pr6')]=='605',na.rm=T)
    isLap[ii]=isRap[ii] & any(srgSrchFile[ii,c('pr1','pr2','pr3','pr4','pr5','pr6')]=='5421',na.rm=T)
  }                                       
  lapRecord=srgSrchFile[isLap,]
  return(lapRecord)
}                                    

                                        
lap.ia=extract.lap(srgSrchFile.ia)
lap.ny=extract.lap(srgSrchFile.ny)
lap.md=extract.lap(srgSrchFile.md)
#save(lap.ia, lap.md, lap.ny, file='Z:/j_scrdata/lapLearn/lapRecords.RData') 
(load('Z:/j_scrdata/lapLearn/lapRecords.RData'))



ddply(lap.ia,'year',nrow)
ddply(lap.md,'year',nrow)
ddply(lap.ny,'year',nrow)

#set coverage integer linear programming, patient allocation
scilpPatAlloc.yz <- function(N, G.list #G is a list of payoff, with 0 patient, 1, 2......a maximum number of patients
                             , allTreated=TRUE){
                                          J <- length(G.list)
                                          #note if TM_OPTIMAL_SOLUTION_FOUND==0 means found optimal, if NULL then not found
                                          Mvec <- unlist(lapply(G.list,length))-1
                                          Mmax <- max(Mvec)
                                          
                                          if (allTreated){stopifnot(sum(Mvec)>=N)}
                                          
                                          constMat <- matrix(rep(0,(J+1)*sum(Mvec+1)),nrow=(J+1))
                                          constrColnameList <- list()
                                          for (j in 1:J){
                                            for (i in 0:Mvec[j]) {constrColnameList<-lappend.yz(constrColnameList, paste(j,':',i,sep=''))}
                                          }                   
                                          
                                          constList.Jrows <-list()
                                          
                                          for(j in 1:J){
                                            constrMat.Jrows.jthDoc = matrix(rep(0,J*(Mvec[j]+1)),nrow=J)
                                            constrMat.Jrows.jthDoc[j,] <- 1
                                            constList.Jrows <- lappend.yz(constList.Jrows,constrMat.Jrows.jthDoc)
                                          }
                                          
                                          Jplus1List <- list()
                                          for(j in 1:J){Jplus1List <- lappend.yz(Jplus1List,c(0,seq(Mvec[j])))}
                                          Jplus1Row <- unlist(Jplus1List)                     
                                          constMat <- rbind(do.call('cbind',constList.Jrows),Jplus1Row)
                                          colnames(constMat) <- unlist(constrColnameList)
                                          rownames(constMat) <- c(paste('doc',seq(J),sep=''),'all.doc')
                                          if (allTreated){constDir <- c(rep('==', J),'==')} else {constDir <- c(rep('==',J),'<=')}
                                          
                                          rhs <- c(rep(1,J),N)
                                          types <- rep("B", J*(Mmax+1))
                                          
                                          Gvec=unlist(G.list)
                                          #ensure when you want, all patints be treated it is possible
                                          stopifnot(length(Gvec) == sum(Mvec+1))
                                          
                                          fit <- Rsymphony_solve_LP(Gvec, constMat, constDir, rhs,type=types, max = F) #false means minimize
                                          opt.n <- subset(data.frame(chosenDecision=fit$solution,n.treated=Jplus1Row),chosenDecision==1)[,'n.treated']
                                          
                                          names(opt.n) <- paste('j=',seq(J),sep='')
                                          fit$opt.n <- opt.n
                                          fit$total.opt.n <- sum(opt.n)
                                          fit$solution <- NULL
                                          return(fit)
                                        }
                                      
# G.list.md=lapply(g.list.md,cumsum) 
# G.list.ny=lapply(g.list.ny,cumsum)
                                            
genSteadyState.Glist=function(g.list){
  g.list.firstElemIs0= lapply(g.list,function(x){x.1=x[,2]; x.1[1]=0;return(x.1)})
  volume.vec=seq(length(g.list.firstElemIs0[[1]]))-1
  G.list=lapply(g.list.firstElemIs0, function(x){volume.vec*x})
  return(G.list)
}

G.list.ia=genSteadyState.Glist(g.list.ia)
G.list.md=genSteadyState.Glist(g.list.md)
G.list.ny=genSteadyState.Glist(g.list.ny)


#mdRange is [1:25] and the like
mdRange=seq(1,25) #use these doctor to solve for optimal
sol.1to25.ny=scilpPatAlloc.yz(1121, G.list.ny[mdRange])
#you can change the sequence of selected patients

#now from optimal solution to predicted LOS
getMdData=function(mdVec){
mdList=list()
for (i in 1:length(mdVec)){
  mdList=lappend.yz(mdList,lastWithin.yz(subset(nomiss.lag6, mdnum1.r==mdVec[i],select=c('mdnum1.r','n.lag4.lap.log','n.lag4.lap.logsq','n.lag4.rap','n.lag4.nonLap.5cat')),'mdnum1.r'))
}
outmdDf=do.call(rbind,mdList)
return(outmdDf)
}
#extract mdDf information
mdDf.ia=getMdData(mdVec.ia)
mdDf.md=getMdData(mdVec.md)
mdDf.ny=getMdData(mdVec.ny)



optSOl.to.los(sol, mdRange, interestYear, opRiFit, stateYearData, mdDf)
#mdRange is the range of MD picked up solution. This is to get around speed problem
optSOl.to.los=function(sol, mdRange, interestYear, opRiFit, stateYearData
                       , mdDf #first column is mdId
                       #this data should have each MD id and MD characteristics, n.lag.rap, the like
                       
){
  
  J=length(opRiFit$y.levels) #number of ordered choices
  formu=as.list(opRiFit$call)$formula
  rhs.formu=paste('~',strsplit(as.character(formu), "~")[[3]],sep='')
  
  #from solution, to mdnum.1
  md.opt.vol=data.frame(mdnum1.r=mdDf[mdRange,'mdnum1.r'], opt.n=sol$opt.n)
  
  n.pats=sum(sol$opt.n) #number of patients optimized
  
  mdRepVec=unlist(mapply(function(x,y){rep(x,each=y)}, as.character(md.opt.vol[,'mdnum1.r']), md.opt.vol[,'opt.n']))
  scrambledMds=mdRepVec[sample(length(mdRepVec),replace=F)]
  #sampling a patient population of the size of interest
  newPop=stateYearData[sample(seq(nrow(stateYearData)),n.pats, replace=T),]#this populaton is bootstrappled population
  
  #optimal allocation case below
  newPop[,'mdnum1.r']=factor(scrambledMds,levels(opRiFit$model[,'mdnum1.r']))
  #now replace newPop mdNum1.r and mdNum's nonLap and nonRad accoringly....
  mdDf.opt=join(mdDf,md.opt.vol,by='mdnum1.r',type='inner')
  
  tojoin=deldfcols.yz(newPop,setdiff(names(mdDf.opt),'mdnum1.r'))
  
  newPopOk = join(tojoin,mdDf.opt,by='mdnum1.r') 
  
  #update newPopOk based volume
  newPopOk[,'n.lag4.lap.log']=log1p(newPopOk[,'opt.n'])
  newPopOk[,'n.lag4.lap.logsq']=log1p(newPopOk[,'opt.n'])^2
  
  newPopOk[,'year']=factor(rep(interestYear,nrow(newPopOk)),levels=levels(opRiFit$model[,'year']))
  #next get los from newPop
  
  dataMat=model.matrix(as.formula(rhs.formu),data=newPopOk)
  cuts=opRiFit$alpha; betaVec=opRiFit$beta
  XB=dataMat[,-1] %*% matrix(betaVec,ncol=1)
  #cbind(colnames(dataMat[,-1]),names(betaVec))
  #now we change eblupDf eblup into the picked surgeon
  XBPlusRi=XB+0 #average one 
  #I did a check on the correlation between eblup and outcome and confirm that it should be PLUS not minus
  cumProbMat=matrix(NA,nrow=length(XBPlusRi),ncol=J)
  for (j in 1:(J-1)){
    cumProbMat[,j]=pnorm(cuts.s[j]-XBPlusRi)
  }
  cumProbMat[,J]=1
  probMat=array(NA,dim=dim(cumProbMat))
  probMat[,1]=cumProbMat[,1]
  for(j in 2:J){
    probMat[,j]=cumProbMat[,j]-cumProbMat[,j-1]
  }
  colnames(probMat)=opRiFit$y.levels
  pred.los=mean(probMat %*% matrix(seq(1:J), ncol=1))
  return(pred.los)
}


interestState='MD'
interestYear='2004'
mdDf=mdDf.md
G.list=G.list.md
mdRange=seq(1,25)
data=nomiss.lag6
G.list[[21]][44]
plot(g.list.md[[21]])

pointEsLosDrop=function(interestState, interestYear, G.list, mdDf
                        , mdRange #you can jitter to get confidence interval 
                        ,data #the dadta use to build model fit
                        ,opRiFit #model fit
                        ){
stateYearData = subset(nomiss.lag6, hospst==interestState & year==interestYear)

#approx.ol below is based on point estimate of G.list and year was NOT controlled see get.FE.model.indivLearnCurve
#it is an approxmiate solution...

approx.sol=scilpPatAlloc.yz(nrow(stateYearData), G.list[mdRange])

doc.location=which(approx.sol$opt.n>0)
ndoc.opt=length(doc.location)
npatPerDoc.opt=mean(approx.sol$opt.n[doc.location])
#predicted minimual based on approxmiate solution...adn this time rescore data with change in year
min.los=optSOl.to.los(approx.sol, mdRange, interestYear, opRiFit, stateYearData, mdDf)
obs.los=mean(as.numeric(stateYearData[,'los']))
diff=obs.los-min.los
diff.pct=100*diff/obs.los
n=nrow(stateYearData)
losRed=n*diff
outVec=c(min.los,obs.los,diff,diff.pct,n,losRed,ndoc.opt,npatPerDoc.opt)
names(outVec)=c('min.los','obs.los','diff','diff.pct','n.patients','losRed','ndoc.opt','npatPerDoc.opt')
return(outVec)
}

pointEsLosDrop('NY','2004',G.list.ny, mdDf.ny,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('NY','2005',G.list.ny, mdDf.ny,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('NY','2006',G.list.ny, mdDf.ny,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('NY','2007',G.list.ny, mdDf.ny,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('NY','2008',G.list.ny, mdDf.ny,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('NY','2009',G.list.ny, mdDf.ny,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('NY','2010',G.list.ny, mdDf.ny,seq(1,25),nomiss.lag6, opRiFit)


pointEsLosDrop('MD','2004',G.list.md, mdDf.md,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('MD','2005',G.list.md, mdDf.md,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('MD','2006',G.list.md, mdDf.md,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('MD','2007',G.list.md, mdDf.md,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('MD','2008',G.list.md, mdDf.md,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('MD','2009',G.list.md, mdDf.md,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('MD','2010',G.list.md, mdDf.md,seq(1,25),nomiss.lag6, opRiFit)

pointEsLosDrop('IA','2004',G.list.ia, mdDf.ia,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('IA','2005',G.list.ia, mdDf.ia,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('IA','2006',G.list.ia, mdDf.ia,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('IA','2007',G.list.ia, mdDf.ia,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('IA','2008',G.list.ia, mdDf.ia,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('IA','2009',G.list.ia, mdDf.ia,seq(1,25),nomiss.lag6, opRiFit)
pointEsLosDrop('IA','2010',G.list.ia, mdDf.ia,seq(1,25),nomiss.lag6, opRiFit)



plot(g.list.md.chgzero[[1]])
ndoc.list=list()
for(n.patients in seq(30,500,1)){
opt.n=scilpPatAlloc.yz(n.patients, G.list.ny[1:10], allTreated=TRUE)$opt.n
ndoc=length(which(opt.n>0))
ndoc.list=lappend.yz(ndoc.list,ndoc)
}

                                         
plot(cbind(seq(30,500,1),unlist(ndoc.list)),type='s',ylim=c(0,12), ylab="number of MIRP surgeons", xlab='number of MIRP in two quarters')
abline(h=10) #averagely 10 surgical taking care pateints per 2 quarters
abline(v=100)                                  
optSolution2Los=function(optSolution, fit){
  return(expLos)
}


                                
                                        
                                        
                                        
#next I want to get learning curve and get a confidence interval for each states
                                        lagVol.vec=seq(0,5,1) #max is 95


density.yz(nomiss.2[,'n.lag2.lap'])
#ia learning curve
ia.data=subset(nomiss.2.le7, hospst=="IA")
ia.docs=unique(ia.data[,'mdnum1.r'])
#IA
ia.g.list=list()
for(i in 1:length(ia.docs)){
  
  ia.g.list= lappend.yz(ia.g.list, genIndivLearnCurve(opRiFit, ia.docs[i], ia.data, lagVol.vec))
  
}
ia.curve=cbind(lagVol.vec,apply(do.call(rbind,ia.g.list),2,mean))


#md learning curve
md.data=subset(nomiss.2.le7, hospst=="MD")
md.docs=unique(md.data[,'mdnum1.r'])
#IA
md.g.list=list()
for(i in 1:length(md.docs)){
  
  md.g.list= lappend.yz(md.g.list, genIndivLearnCurve(opRiFit, md.docs[i], md.data, lagVol.vec))
  
}
md.curve=cbind(lagVol.vec, apply(do.call(rbind,md.g.list),2,mean))

#NY
ny.data=subset(nomiss.2.le7, hospst=="NY")
ny.docs=unique(ny.data[,'mdnum1.r'])
#IA
ny.g.list=list()
for(i in 1:length(ny.docs)){
  
  ny.g.list= lappend.yz(ny.g.list, genIndivLearnCurve(opRiFit, ny.docs[i], ny.data, lagVol.vec))
  
}
ny.curve=cbind(lagVol.vec, apply(do.call(rbind,ny.g.list),2,mean))



plot(md.curve,ylim=c(1.7,2.2))
lines(ia.curve)
lines(ny.curve)

which.min(ny.curve[,2])
which.min(ia.curve[,2])
which.min(md.curve[,2])
lagVol.vec[39]
#still 39, so there is no way, I can get a different ....

#I think I need to add state fixed effect later, it optimal volume would be different across states and it is easier to write it.



#next  I can then run the optimizaton problem to find the solution.





plot(cbind(lagVol.vec, learncurve.i), type='b', cex=0.2)



XBPlusRi=XB+join(newData, eblupDf,by=names(opRiFit$ranef))[,'eblup']





genIndSurgonLearnCurve=function(doc.volDf, nonLapCuts){
  doc.volDf[,'n.lag1.lap.sqrt']=doc.volDf[,'n.lag1.lap']^(0.5)
  doc.volDf[,'n.lag1.lap.1h']=doc.volDf[,'n.lag1.lap']^(1.5)
  doc.volDf = grpnv.supplycuts.yz(doc.volDf, 'n.lag1.nonLap', nonLapCuts, 'n.lag1.nonLap.cat')
  #doc.volDf[,'n.lag1.nonLap.cat']= as.factor(doc.volDf[,'n.lag1.nonLap.cat'])
  outdf=data.frame(doc.volDf,dummyMat.yz(doc.volDf,c('n.lag1.nonLap.cat'),c('[  0, 12)')))
  names(outdf)[7:9]=c()
  head(doc.volDf)
  g.list=list()
  for(j in 1:nrow(doc.volDf)){
    gvec.j=rep(NA,100)
    for(jj in 1:100){
      
      
    }
    
  }
  
  return(doc.volDf)
  
}


  
  names(nomiss)
  #simple model

  oprobit.fe <- clm(as.factor(los) ~ cum.nLap.last2.1+cum.nLap.last2.1.sqrt+factor(agecat)+factor(race)+factor(year)+factor(surgeon.id.1)+factor(comorbcat)+factor(pay1), data=nomiss, link=c('probit'))
  
  summary(oprobit.fe)


nrow(nomiss)
nrow(anaDf.clean)
#there is something wrong with IA state, tons of missing, fine...
head(anaDf.clean[,c('n.lag2.lap','hospst')],1000)



nomiss=grpnv.supplycuts.yz(nomiss, 'n.lag2.nonLap', quantile(nomiss[,'n.lag2.nonLap'],probs=seq(0,1,0.25),na.rm=T), 'n.lag2.nonLap.cat')
nomiss.smallest=grpnv.supplycuts.yz(nomiss.smallest, 'n.lag2.nonLap', quantile(nomiss[,'n.lag2.nonLap'],probs=seq(0,1,0.25),na.rm=T), 'n.lag2.nonLap.cat')

nomiss.smallest=grpnv.supplycuts.yz(nomiss.smallest, 'n.lag1.nonLap', quantile(nomiss[,'n.lag1.nonLap'],probs=seq(0,1,0.25),na.rm=T), 'n.lag1.nonLap.cat')

nomiss.smallest=grpnv.supplycuts.yz(nomiss.smallest, 'n.lag3.nonLap', quantile(nomiss[,'n.lag3.nonLap'],probs=seq(0,1,0.25),na.rm=T), 'n.lag3.nonLap.cat')

nomiss.smallest=grpnv.supplycuts.yz(nomiss.smallest, 'n.lag4.nonLap', quantile(nomiss[,'n.lag4.nonLap'],probs=seq(0,1,0.25),na.rm=T), 'n.lag4.nonLap.cat')




#the following is for sensitity analysis of lag


# AIC(oprobit.re.lag2.sensitivity)
# [1] 8908
# > AIC(oprobit.re.lag3.sensitivity)
# [1] 8905
# > AIC(oprobit.re.lag4.sensitivity)
# [1] 8911
# > AIC(oprobit.re.lag1.sensitivity)
# [1] 8902


#use 5 category would have
# factor(n.lag1.nonLap.cat)[ 10, 17)  0.20244    0.06627    3.05   0.0023 ** 
#   factor(n.lag1.nonLap.cat)[ 17, 26)  0.15735    0.08036    1.96   0.0502 .  
# factor(n.lag1.nonLap.cat)[ 26, 37)  0.24054    0.08875    2.71   0.0067 ** 
#   factor(n.lag1.nonLap.cat)[ 37,147]  0.03679    0.10649    0.35   0.7298    

#it better to use 4 category
nomiss=grpnv.supplycuts.yz(nomiss, 'n.lag1.nonLap', quantile(nomiss[,'n.lag1.nonLap'],probs=seq(0,1,0.25),na.rm=T), 'n.lag1.nonLap.cat')

oprobit.re.lag1 <- clmm(as.factor(los) ~ n.lag1.lap.sqrt+n.lag1.lap+factor(n.lag1.nonLap.cat)+factor(agecat)+factor(race)+factor(year)+factor(comorbcat)+factor(pay1)+(1|mdnum1.r), data=nomiss, link=c('probit'))

oprobit.re.lag1.1 <- clmm(as.factor(los) ~ n.lag1.lap.sqrt+n.lag1.lap+n.lag1.lap.1h+factor(n.lag1.nonLap.cat)+factor(agecat)+factor(race)+factor(year)+factor(comorbcat)+factor(pay1)+(1|mdnum1.r), data=nomiss, link=c('probit'))

oprobit.re.lag1.1 <- clmm(as.factor(los) ~ n.lag1.lap.sqrt+n.lag1.lap+n.lag1.lap.cu+factor(n.lag1.nonLap.cat)+factor(agecat)+factor(race)+factor(year)+factor(comorbcat)+factor(pay1)+(1|mdnum1.r), data=nomiss, link=c('probit'))


summary(oprobit.re.lag1.1)

save(oprobit.re.lag1
     ,oprobit.re.lag1.sensitivity,oprobit.re.lag2.sensitivity,oprobit.re.lag3.sensitivity,oprobit.re.lag4.sensitivity,
    file='Z:/j_scrdata/lapLearn/modelFitMay6th2013.RData' )
                                        load('Z:/j_scrdata/lapLearn/modelFitMay6th2013.RData')

  save(oprobit.re,oprobit.fe,ologit.re,ologit.fe,file='Z:/j_scrdata/lapLearn/modelFit.RData')
  summary(oprobit.re)
  summary(oprobit.fe)


#this provide predition of probabiltiy based on point estimation of betas and cuts
predOrProbitRi.pointEst = function(opRiFit #in formula, you always put random intercpet at the end
                                   ,newData
                                   #need to make sure the new data has the same levels as modelData
                                   ,nlagVn
                                   ,add.eblup=T){
  J=length(opRiFit$y.levels) #number of ordered choices
  cuts=opRiFit$alpha; betaVec=opRiFit$beta
  
  formu=as.list(opRiFit$call)$formula
  rhs.formu=paste('~',strsplit(as.character(formu), "~")[[3]],sep='')
  dataMat=model.matrix(as.formula(rhs.formu),data=newData)
  XB=dataMat[,-c(1,ncol(dataMat))] %*% matrix(betaVec,ncol=1)
  
  
  
  
  
  
  if(add.eblup){
    eblupDf= as.data.frame(opRiFit$ranef)
    names(eblupDf)='eblup'
    eblupDf=rowname2vn.yz(eblupDf,rownameVn=names(opRiFit$ranef))
    XBPlusRi=XB+join(newData, eblupDf,by=names(opRiFit$ranef))[,'eblup']
    #I did a check on the correlation between eblup and outcome and confirm that it should be PLUS not minus
  } else{
    
  }
  
  
  cumProbMat=matrix(NA,nrow=length(XBPlusRi),ncol=J)
  
  for (j in 1:(J-1)){
    cumProbMat[,j]=pnorm(cuts[j]-XBPlusRi)
  }
  cumProbMat[,J]=1
  probMat=array(NA,dim=dim(cumProbMat))
  probMat[,1]=cumProbMat[,1]
  for(j in 2:J){
    probMat[,j]=cumProbMat[,j]-cumProbMat[,j-1]
  }
  colnames(probMat)=opRiFit$y.levels
  return(probMat)
}



  intensityVec = quantile(nomiss[,nlagVn],probs=seq(0.05,0.95,0.05),na.rm=T)
intensityVec = seq(0,50,1)
  options(digits=5)
  popMeanLos = rep(NA,length(intensityVec))
  for(i in 1:length(intensityVec)){
    newData=nomiss
    newData[,'n.lag1.lap']=intensityVec[i]
    newData[,'n.lag1.lap.sqrt']= intensityVec[i]^(0.5)
    probMat=predOrProbitRi.pointEst(oprobit.re.lag1.1  #in formula, you always put random intercpet at the end
                                    ,newData   #need to make sure the new data has the same levels as modelData
                                    ,add.eblup=T)
    popMeanLos[i]=mean(probMat %*% seq(1,5))
  }
  

  plot(x=intensityVec,y=popMeanLos,type='l',ylim=c(1.2,2))



#min is at 10 per quater.

  cbind(intensityVec ,popMeanLos)

  opRiFit=oprobit.re.lag1
  eblupDf= as.data.frame(opRiFit$ranef)
  names(eblupDf)='eblup'
  eblupDf=rowname2vn.yz(eblupDf,rownameVn=names(opRiFit$ranef))
  names(eblupDf)
  eblupDf = rename.vars(eblupDf,'mdnum1.r','surgeon.id')
  head(cum.nLapSrchDf)
  eblup.vol = join(eblupDf, cum.nLapSrchDf, by='surgeon.id')
  names(eblup.vol)
  #eblup means poor qaulity here
  cor(-eblup.vol[,'eblup'],eblup.vol[,'nLap'])
  
  
  
  
  
  
  (-(-1.0176e-01)/(2*1.3157e-02))^2 #14.95
  
  (-(-6.4819)/(2*1.1095))^2 #8.5
  
  hist(cumVol.1)
  names(cumVol.1)
  
  forHistGram=join(data.frame(surgeon.id=nomiss[,'surgeon.id.1']),cum.nLapSrchDf[,c('surgeon.id',  "cum.nLap.last2")])
  
  
  
  
  
  names(cum.nLapSrchDf)
  hist(forHistGram[,'cum.nLap.last2'],100)
  plot(ecdf(forHistGram[,'cum.nLap.last2']))
  
  abline(v = 15, col = "dodgerblue3", lty="dotdash")
  
  #get surgical learning curve for different 
  
  
  
  
  
  
  
  summary(ologit.fe)
  summary(ologit.re)
  summary(oprobit.re)
  summary()
  
  head(predict(ologit.fe))
  #next I ran a post-preidction model.
  head(ologit.fe$fitted.value)
  #the post estimation for random intercept model is add a random intercept(random sample from normal)
  
  
  
  
  
  nrow(nomiss)
  
  
  
  
  
  
  
  
  
  
  #nomiss[,'cum.nLap.last1.1.sq']=nomiss[,'cum.nLap.last1.1']^2
  
  #need to add hopsital fixed effect.....zipcode level effect too.
  #then recover the learning curve.
  #then get cost as next step.
  
  
  #may need to try, the following package
  #gamlss.mx
  
  
  renb=gamlssNP(los ~ cum.nLap.last2.1, data=nomiss,
                random = ~ 1 | surgeon.id.1,
                family = NBI)
  
  summary(renb)
  
  # nbFit = glm.nb(los ~ cum.nLap.last2.1.sq+cum.nLap.last2.1+factor(year)+age+factor(race)+factor(chf)+factor(valve)+factor(pulmcirc)+factor(perivasc)+factor(para)+factor(neuro)+factor(chrnlung)+factor(dm)+factor(dmcx)+factor(hypothy)+factor(renlfail)+factor(liver)+factor(ulcer)+factor(lymph)+factor(mets)+factor(tumor)+factor(arth)+factor(coag)+factor(obese)+factor(wghtloss)+factor(lytes)+factor(bldloss)+factor(anemdef)+factor(alcohol)+factor(drug)+factor(psych)+factor(depress)+factor(htn.c), data=nomiss)
  
  # AIC(opFit)
  #last 1: 10024.276
  #last 2: 10019.367 (best)
  #last 3: 10022.297
  #last 4,: 10022.377
  source('R:/model/orderProbit')
  nomiss =deldfcols.yz(nomiss,c('cum.nLap.last1.1','cum.nLap.last1.1.sq','cum.nLap.last3.1','cum.nLap.last3.1.sq','cum.nLap.last4.1','cum.nLap.last4.1.sq'))
  #we then run a cumulative risk with link of probit
  opFit<- clm(as.factor(los) ~ cum.nLap.last2.1+cum.nLap.last2.1.sq+cum.nLap.last2.1.cu+factor(year)+age+factor(race)+factor(chf)+factor(valve)+factor(pulmcirc)+factor(perivasc)+factor(para)+factor(neuro)+factor(chrnlung)+factor(dm)+factor(dmcx)+factor(hypothy)+factor(renlfail)+factor(liver)+factor(ulcer)+factor(lymph)+factor(mets)+factor(tumor)+factor(arth)+factor(coag)+factor(obese)+factor(wghtloss)+factor(lytes)+factor(bldloss)+factor(anemdef)+factor(alcohol)+factor(drug)+factor(psych)+factor(depress)+factor(htn.c)+factor(surgeon.id.1), data=nomiss, link=c('probit'))
  
  
  #we then run a cumulative risk with link of probit
  #Random effects in ordinal regression models Computational Statistics & Data Analysis 22 (1996) 537-557
  #explains how random intercept actually shifts the cutoffs
  
  
  summary(ologitFit)
  names(nomiss)
  
  #adjust for year, use cub will not get random effect
  ologitFit<- clmm(as.factor(los) ~ cum.nLap.last2.1+cum.nLap.last2.1.sq+cum.nLap.last2.1.sqrt+age+factor(year)+(1|surgeon.id.1), data=nomiss, link=c('logit'))
  summary(ologitFit)
  
  ologitFit<- clmm(as.factor(los) ~ cum.nLap.last2.1+cum.nLap.last2.1.sq+age+factor(year)
                   
                   +factor(race)+factor(chf)+factor(valve)+factor(pulmcirc)+factor(perivasc)+factor(para)+factor(neuro)+factor(chrnlung)+factor(dm)+factor(dmcx)+factor(hypothy)+factor(renlfail)+factor(liver)+factor(ulcer)+factor(lymph)+factor(mets)+factor(tumor)+factor(arth)+factor(coag)+factor(obese)+factor(wghtloss)+factor(lytes)+factor(bldloss)+factor(anemdef)+factor(alcohol)+factor(drug)+factor(psych)+factor(depress)+factor(htn.c)
                   +(1|surgeon.id.1), data=nomiss, link=c('logit'))
  summary(ologitFit)
  
  
  ologitFit<- clm(as.factor(los) ~ cum.nLap.last2.1+cum.nLap.last2.1.sq+cum.nLap.last2.1.qu+age+factor(year)+factor(surgeon.id.1), data=nomiss, link=c('logit'))
  
  count(nomiss,'surgeon.id.1')
  
  
  names(nomiss)
  
  clmm(as.factor(los) ~ cum.nLap.last2.1+cum.nLap.last2.1.cu+(1|surgeon.id.1), data=nomiss, link=c('logit'))
  summary(ologitFit)
  data(soup)
  
  ## Cumulative link mixed model with two random terms:
  mm1 <- clmm(SURENESS ~ PROD + (1|RESP) , data = soup,
              link = "probit", threshold = "equidistant")
  
  ologitFit<- clmm(as.factor(los) ~ cum.nLap.last2.1+cum.nLap.last2.1.sq+cum.nLap.last2.1.cu+factor(year)+age+factor(race)+factor(chf)+factor(valve)+factor(pulmcirc)+factor(perivasc)+factor(para)+factor(neuro)+factor(chrnlung)+factor(dm)+factor(dmcx)+factor(hypothy)+factor(renlfail)+factor(liver)+factor(ulcer)+factor(lymph)+factor(mets)+factor(tumor)+factor(arth)+factor(coag)+factor(obese)+factor(wghtloss)+factor(lytes)+factor(bldloss)+factor(anemdef)+factor(alcohol)+factor(drug)+factor(psych)+factor(depress)+factor(htn.c)+(1|surgeon.id.1), data=nomiss, link=c('logit'))
  
  
  
  ologitFitClmm2<- clmm2(as.factor(los) ~ cum.nLap.last2.1+cum.nLap.last2.1.sq+cum.nLap.last2.1.cu+factor(year)+age+factor(race)+factor(chf)+factor(valve)+factor(pulmcirc)+factor(perivasc)+factor(para)+factor(neuro)+factor(chrnlung)+factor(dm)+factor(dmcx)+factor(hypothy)+factor(renlfail)+factor(liver)+factor(ulcer)+factor(lymph)+factor(mets)+factor(tumor)+factor(arth)+factor(coag)+factor(obese)+factor(wghtloss)+factor(lytes)+factor(bldloss)+factor(anemdef)+factor(alcohol)+factor(drug)+factor(psych)+factor(depress)+factor(htn.c), data=nomiss, random=surgeon.id.1,link=c('logistic'))
  
  unilen.yz(nomiss,'surgeon.id.1')
  
  (load(file='Z:/j_scrdata/lapLearn/ologitFit.RData'))
  sum
  
  save(ologitFit,file='Z:/j_scrdata/lapLearn/ologitFit.RData')
  (load('Z:/j_scrdata/lapLearn/ologitFit.RData'))
  
  summary(ologitFit)
  
  b=opFit$beta['cum.nLap.last2.1']
  a=opFit$beta['cum.nLap.last2.1.sq']
  (opt.nLap.last2=-b/(2*a))
  nLapVals = quantile(nomiss[,'cum.nLap.last2.1'], probs=seq(0,0.95,0.05))
  outVec=rep(NA,length(nLapVals))
  
  i
  for(i in 1:length(nLapVals)){
    newData[,'cum.nLap.last2.1']=nLapVals[i]
    newData[,'cum.nLap.last2.1.sq']=nLapVals[i]^2
    newData[,'cum.nLap.last2.1.cu']=nLapVals[i]^3
    newData[,'cum.nLap.last2.1.qu']=nLapVals[i]^4
    outVec[i]=mean(predProbClmOrdProbit.pointEst(opFit,newData)%*% matrix(seq(5),ncol=1))
  }
  plot(nLapVals,outVec,type='b')
  
  names(newData)
  
  #do not adjust for pstate. to avoid opening the question, but if they really want, then adjust state, leve.
  #count level and state level has huge colloinary issue and wount converge. 
  #the reason that second order is not seen signficant is becuase much of the "busy" is explained away by region. so we cannot #use overly fine region as pstate
  
  table(nomiss[,'pstco'],nomiss[,'cum.nLap.last2.1'])
  
  #you cannot adjust for zipcode fixed effect, it will have trouble
  #you cannot even adjust for pstco regional variation
  
  AIC(nbFit)
  # 1. 14699.205
  # 2: 14696.697
  # 3: 14700.197
  # 4: 14702.66
  summary(nbFit)
  a=coef(nbFit)[2]
  b=coef(nbFit)[3]
  -b/(2*a)
  #4 19068.321
  #3 19062.971
  #2 19060.116 (best)
  #1 19063.619
  #so we need to use last 2
  dataMat = model.matrix(nbFit,data= nomiss)
  predLos = exp(dataMat%*%matrix(coef(nbFit),ncol=1))
  dataMat = model.matrix(nbFit,data= nomiss)
  amtVec =quantile(dataMat[,'cum.nLap.last2.1'],probs=seq(0,1,0.05))
  predLosMat=matrix(NA,nrow=nrow(dataMat),ncol=length(amtVec))
  for(i in 1:length(amtVec)){
    dataMat[,'cum.nLap.last2.1']=amtVec[i]
    dataMat[,'cum.nLap.last2.1.sq']=dataMat[,'cum.nLap.last2.1']^2
    predLosMat[,i]= exp(dataMat%*%matrix(coef(nbFit),ncol=1))
  }
  
  # 0%    1%    2%    3%    4%    5%    6%    7%    8%    9%   10%   11%   12%   13%   14%   15%   16%   17%   18%   19%   20%   21% 
  #   1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00 
  # 22%   23%   24%   25%   26%   27%   28%   29%   30%   31%   32%   33%   34%   35%   36%   37%   38%   39%   40%   41%   42%   43% 
  #   1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00 
  # 44%   45%   46%   47%   48%   49%   50%   51%   52%   53%   54%   55%   56%   57%   58%   59%   60%   61%   62%   63%   64%   65% 
  #   1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  1.00  2.00  2.00  2.00  2.00  2.00  2.00  2.00  2.00  2.00  2.00  2.00  2.00  2.00 
  # 66%   67%   68%   69%   70%   71%   72%   73%   74%   75%   76%   77%   78%   79%   80%   81%   82%   83%   84%   85%   86%   87% 
  #   2.00  2.00  2.00  2.00  2.00  2.00  2.00  2.00  2.00  2.00  2.00  2.00  2.00  2.00  2.00  2.00  2.00  2.00  2.00  2.00  2.00  2.21 
  # 88%   89%   90%   91%   92%   93%   94%   95%   96%   97%   98%   99%  100% 
  # 3.00  3.00  3.00  3.00  3.00  3.00  3.00  4.00  4.00  5.00  5.00  7.00 20.00 
  
  
  
  plot(apply(predLosMat,2,mean),type='b')
  abline(h=mean(predLos))
  
  obsLos = nomiss[,'los']
  quantile(obsLos,seq(0,1,0.01))
  hist(nomiss[,'los'],100)
  quantile(nomiss[,'los'],probs=seq(0,1,0.05))
  
  mean(predLos)
  hist(predLos,100)
  quantile(predLos,probs=seq(0,1,0.05))
  
  plot(predLos,nomiss[,'los'])
  mean(predLos)
  mean(obsLos)
  hist(obsLos,100)
  quantile(obsLos,seq(0,1,0.01))
  
  table(obsLos)
  
  
  str(vgamFit)
  summary(vgamFit)
  Coef(vgamFit)
  vcov(vgamFit)
  coef(vgamFit)
  exp(coef(vgamFit))
  fitted(vgamFit)[1]
  
  # vglm1 = vglm(cover~elev+I(elev^2)+streamdist+elev*streamdist,family=pospoisson,data=dat2) #zero truncated poisson
  # vglm2 = vglm(cover~elev+I(elev^2)+streamdist+elev*streamdist,family=poissonff,data=dat2) #regular
  # AIC(vglm1)
  # [1] 1490.624
  # AIC(vglm2)
  # [1] 1532.216 
  
  #smaller is better
  
  # Unfortunately the anova function is not yet implemented in the VGAM library, but AICs suggest we have a vast improvement in model building with the zero-truncated model.  Also note that Poisson counts assume equal mean and variance.  If the variance increases faster than the mean, a negative binomial model is more appropriate, because it includes an additional parameter for the added variance (overdispersion).  
  
  
  vgamFit@fitted.values
  table(anaDf[,'los'])
  
  sort(names(anaDf))
  #need to run poisson model
  vglm(stay ~ age + hmo + died, family = posnegbinomial(), data = dat)
  
  
  #only two people has aid, lack variation. factor(aids)
  
  
  
  
}