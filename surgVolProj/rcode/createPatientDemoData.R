
#need to add hospital effect by add hospital ID
#insurance type there?

#create surgical patient level demo data.
demoRaw = xpt2r.yz("Z:/j_scrdata/lapLearn",'_demo_iamdny')
demoRaw[,'key']=as.character(demoRaw[,'key'])
demoRaw[,'x.iny'] = NULL
demoRaw[,'quarterSince2003'] = 4*(demoRaw[,'year']-2003)+demoRaw[,'dqtr']
names(demoRaw)
names(demoRaw)
(load(file="Z:/j_scrdata/lapLearn/cum.nLap.RData"))
names(cum.nLap.ia)

cum.nLapSrchDf = rbind(cum.nLap.ia, cum.nLap.md, cum.nLap.ny)
names(cum.nLapSrchDf)
cum.nLapSrchDf[,'cum.nLap.last2']

grab.cum.nLap = function(demoRaw,cum.nLapSrchDf){
  
  tmp.1=demoRaw[,c('key','quarterSince2003','hospst','mdnum1.r')]
  tmp.1=rename.vars(tmp.1,'mdnum1.r',"surgeon.id")
  cumVol.1=join(tmp.1,cum.nLapSrchDf[,c('surgeon.id','hospst','quarterSince2003', "nLap","cum.nLap.last1", "cum.nLap.last2" ,  "cum.nLap.last3" ,  "cum.nLap.last4")], by=c('surgeon.id','hospst','quarterSince2003'))
  cumVol.1[,'quarterSince2003'] = NULL
  cumVol.1[,'hospst'] = NULL
  cumVol.1=rename.vars(cumVol.1,c( "surgeon.id", "nLap","cum.nLap.last1","cum.nLap.last2",  "cum.nLap.last3","cum.nLap.last4" )
  ,c( "surgeon.id.1", "nLap.1","cum.nLap.last1.1","cum.nLap.last2.1",  "cum.nLap.last3.1","cum.nLap.last4.1" )
  )
  tmp.2=demoRaw[,c('key','quarterSince2003','hospst','mdnum2.r')]
  tmp.2=rename.vars(tmp.2,'mdnum2.r',"surgeon.id")
  cumVol.2=join(tmp.2,cum.nLapSrchDf[,c('surgeon.id','hospst','quarterSince2003', "nLap","cum.nLap.last1", "cum.nLap.last2" ,  "cum.nLap.last3" ,  "cum.nLap.last4")], by=c('surgeon.id','hospst','quarterSince2003'))
  cumVol.2[,'quarterSince2003'] = NULL
  cumVol.2[,'hospst'] = NULL
  
  cumVol.2=rename.vars(cumVol.2,c( "surgeon.id", "nLap","cum.nLap.last1","cum.nLap.last2",  "cum.nLap.last3","cum.nLap.last4" )
              ,
              
              c( "surgeon.id.2", "nLap.2","cum.nLap.last1.2","cum.nLap.last2.2",  "cum.nLap.last3.2","cum.nLap.last4.2" )
              )
  
  addSrgVol=join(join(demoRaw,cumVol.1,by='key'),cumVol.2,by='key')
  return(addSrgVol)
}


addSrgVol=grab.cum.nLap(demoRaw,cum.nLapSrchDf)
names(cum.nLapSrchDf)


names(addSrgVol)
#next I need to add los variable
keyLos.ia = xpt2r.yz("Z:/j_scrdata/lapLearn",'lap_ia_20032010')[c('key','hospst','los')]
keyLos.ny = xpt2r.yz("Z:/j_scrdata/lapLearn",'lap_ny_20032010')[c('key','hospst','los')]
keyLos.md = xpt2r.yz("Z:/j_scrdata/lapLearn",'lap_md_20032010')[c('key','hospst','los')]

losDf=rbind(keyLos.ia,keyLos.ny,keyLos.md)

comorb = xpt2r.yz('Z:/j_scrdata/lapLearn','comorb_iamymd')
comorb[,'key'] = as.character(comorb[,'key'])

comorb[,'comorbSum'] = rowSums(comorb[,c('chf','valve','pulmcirc','perivasc','para','neuro','chrnlung','dm','dmcx','hypothy','renlfail','liver','ulcer','lymph','mets','tumor','arth','coag','obese','wghtloss','lytes','bldloss','anemdef','alcohol','drug','psych','depress','htn.c')])


anaDf=join(join(addSrgVol,losDf,by=c('key','hospst')),comorb, by=c('key','hospst')) #key is not unique across states
anaDf[,'zip3or5'] =NA
anaDf[which(!is.na(anaDf[,'zip3'])),'zip3or5'] = anaDf[which(!is.na(anaDf[,'zip3'])),'zip3']
anaDf[which(is.na(anaDf[,'zip3'])),'zip3or5'] = anaDf[which(is.na(anaDf[,'zip3'])),'zip'] 
anaDf[,'zip3cleaned'] = substr(anaDf[,'zip3or5'],1,3)
anaDf[which(anaDf[,'zip3cleaned']=='F'),'zip3cleaned'] = NA
useVars = c('los','cum.nLap.last4.1','cum.nLap.last3.1','cum.nLap.last2.1','cum.nLap.last1.1','nLap.1','age','race','chf','valve', 'pulmcirc', 'perivasc', 'para', 'neuro', 'chrnlung', 'dm', 'dmcx', 'hypothy', 'renlfail', 'liver', 'ulcer', 'lymph', 'mets', 'tumor', 'arth', 'coag', 'obese', 'wghtloss', 'lytes', 'bldloss', 'anemdef', 'alcohol', 'drug', 'psych', 'depress', 'htn.c','year','pstate','pstco','surgeon.id.1','comorbSum','pay1')
names(anaDf)

anaDf=subset(anaDf,los>0 & los<=5, select=useVars)
nomiss = anaDf[complete.cases(anaDf),]

#there are two people who stay in hospital for zero days.
#so we remove them.

nomiss[,'cum.nLap.last4.1.sqrt']=nomiss[,'cum.nLap.last4.1']^(0.5)
nomiss[,'cum.nLap.last3.1.sqrt']=nomiss[,'cum.nLap.last3.1']^(0.5)
nomiss[,'cum.nLap.last2.1.sqrt']=nomiss[,'cum.nLap.last2.1']^(0.5)
nomiss[,'cum.nLap.last1.1.sqrt']=nomiss[,'cum.nLap.last1.1']^(0.5)


nomiss[,'cum.nLap.last4.1.1h']=nomiss[,'cum.nLap.last4.1']^(1.5)
nomiss[,'cum.nLap.last3.1.1h']=nomiss[,'cum.nLap.last3.1']^(1.5)
nomiss[,'cum.nLap.last2.1.1h']=nomiss[,'cum.nLap.last2.1']^(1.5)
nomiss[,'cum.nLap.last1.1.1h']=nomiss[,'cum.nLap.last1.1']^(1.5)

nomiss[,'cum.nLap.last4.1.sq']=nomiss[,'cum.nLap.last4.1']^2
nomiss[,'cum.nLap.last3.1.sq']=nomiss[,'cum.nLap.last3.1']^2
nomiss[,'cum.nLap.last2.1.sq']=nomiss[,'cum.nLap.last2.1']^2
nomiss[,'cum.nLap.last1.1.sq']=nomiss[,'cum.nLap.last1.1']^2

nomiss[,'cum.nLap.last4.1.cu']=nomiss[,'cum.nLap.last4.1']^3
nomiss[,'cum.nLap.last3.1.cu']=nomiss[,'cum.nLap.last3.1']^3
nomiss[,'cum.nLap.last2.1.cu']=nomiss[,'cum.nLap.last2.1']^3
nomiss[,'cum.nLap.last1.1.cu']=nomiss[,'cum.nLap.last1.1']^3

nomiss[,'cum.nLap.last4.1.qu']=nomiss[,'cum.nLap.last4.1']^4
nomiss[,'cum.nLap.last3.1.qu']=nomiss[,'cum.nLap.last3.1']^4
nomiss[,'cum.nLap.last2.1.qu']=nomiss[,'cum.nLap.last2.1']^4
nomiss[,'cum.nLap.last1.1.qu']=nomiss[,'cum.nLap.last1.1']^4

ageCuts=quantile(nomiss[,'age'],prob=c(0,0.333,0.666,1))
nomiss<- grpnv.supplycuts.yz (nomiss, 'age', ageCuts, 'agecat')
table(nomiss[,'agecat'])

comorbCuts=quantile(nomiss[,'comorbSum'],prob=c(0,0.333,0.666,1))
comorbCuts=c(0,1,2,3,Inf)
nomiss<- grpnv.supplycuts.yz (nomiss, 'comorbSum', comorbCuts, 'comorbcat')


names(nomiss)
#simple model
ologit.re <- clmm(as.factor(los) ~ cum.nLap.last2.1+cum.nLap.last2.1.sqrt+(1|surgeon.id.1), data=nomiss, link=c('logit'))
summary(ologitFit)

ologit.fe <- clm(as.factor(los) ~ cum.nLap.last2.1+cum.nLap.last2.1.sqrt+ factor(surgeon.id.1), data=nomiss, link=c('logit'))


ologit.re <- clmm(as.factor(los) ~ cum.nLap.last2.1+cum.nLap.last2.1.sqrt+(1|surgeon.id.1), data=nomiss, link=c('logit'))
summary(ologit.fe)


#full fe
#fixed effect is not that good (second order term is marginally signficant)
ologit.fe <- clm(as.factor(los) ~ cum.nLap.last2.1+cum.nLap.last2.1.sqrt+factor(agecat)+factor(race)+factor(year)+factor(surgeon.id.1)+factor(comorbcat), data=nomiss, link=c('logit'))

predProbClmOrdProbit.pointEst(oprobit.fe,nomiss)

oprobit.fe <- clm(as.factor(los) ~ cum.nLap.last2.1+cum.nLap.last2.1.sqrt+factor(agecat)+factor(race)+factor(year)+factor(surgeon.id.1)+factor(comorbcat)+factor(pay1), data=nomiss, link=c('probit'))

summary(oprobit.fe)

#for a medical jouranl this is good enough (i.e., random effect)
#also note I do not need to worry about symmetric learning curve anymore because I used sqrt...that is great!
ologit.re <- clmm(as.factor(los) ~ cum.nLap.last2.1+cum.nLap.last2.1.sqrt+factor(agecat)+factor(race)+factor(year)+factor(comorbcat)+(1|surgeon.id.1), data=nomiss, link=c('logit'))


oprobit.re <- clmm(as.factor(los) ~ cum.nLap.last2.1+cum.nLap.last2.1.sqrt+factor(agecat)+factor(race)+factor(year)+factor(comorbcat)+factor(pay1)+(1|surgeon.id.1), data=nomiss, link=c('probit'))


save(oprobit.re,oprobit.fe,ologit.re,ologit.fe,file='Z:/j_scrdata/lapLearn/modelFit.RData')
summary(oprobit.re)
summary(oprobit.fe)

intensityVec = quantile(nomiss[,'cum.nLap.last2.1'],probs=seq(0.05,0.95,0.05))

popMeanLos = rep(NA,length(intensityVec))
for(i in 1:length(intensityVec)){
  newData=nomiss
  newData[,'cum.nLap.last2.1']=intensityVec[i]
  newData[,'cum.nLap.last2.1.sqrt']= intensityVec[i]^(0.5)
  probMat=predOrProbitRi.pointEst(oprobit.re  #in formula, you always put random intercpet at the end
                          ,newData   #need to make sure the new data has the same levels as modelData
                          ,add.eblup=T)
  popMeanLos[i]=mean(probMat %*% seq(1,5))
}

plot(intensityVec ,popMeanLos,type='b')


opRiFit=oprobit.re
eblupDf= as.data.frame(opRiFit$ranef)
names(eblupDf)='eblup'
eblupDf=rowname2vn.yz(eblupDf,rownameVn=names(opRiFit$ranef))
names(eblupDf)
eblupDf = rename.vars(eblupDf,'surgeon.id.1','surgeon.id')
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
newData=nomiss
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
