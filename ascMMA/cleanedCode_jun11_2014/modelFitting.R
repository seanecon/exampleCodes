#new compile


#first fit a model for total expenditure

fit.totexp=glm(log(totexp) ~ qtr + mean.age + .avg.comorbsum 
               + mma2008.impact 
               + fascsurgeongrpannual
               #+ firstqtraftermma
               #          +.qtr.since2006+.qtr.since2006.sqrt
               +.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4
               +.qtr.since2006.bs5
               +.qtr.since2006.bs6
               +.qtr.since2006.bs7+.qtr.since2006.bs8
               +.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11
               , data=anaDfwithBs.less)

summary(fit.totexp)
#presence of MMA tends to increase expenditure. at least it did not slow down spending
#in places with more fasc it increases more

#this may be drven by change in number of beneficiaries
#so we fit a model on per capital expenditrue, we still see that it failed to stop
#in places with more fasc, it increases more

fit.percap.totexp=glm(log(percap.totexp) ~ qtr + mean.age + .avg.comorbsum 
               + mma2008.impact 
               + fascsurgeongrpannual
               + mma2008.impact *fascsurgeongrpannual
               #+ firstqtraftermma
               #          +.qtr.since2006+.qtr.since2006.sqrt
               +.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4
               +.qtr.since2006.bs5
               +.qtr.since2006.bs6
               +.qtr.since2006.bs7+.qtr.since2006.bs8
               +.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11
               , data=anaDfwithBs.less)

summary(fit.percap.totexp)

#per capita increase and in high penetration places, the increase is faster
#this model already adjusted for fasc, so...it may mean that upcoding is more significant in high pentration region

subset(anaDfwithBs.less[,c('mma2008.impact','.qtr.since2006')],mma2008.impact==0)

#how about just ouppatient care
#we MMA has little impact (sign is positive though)
#but for high FASC places outpatient cost dropped signficantly
fit.totopexp=glm(log(totopexp) ~ qtr + mean.age + .avg.comorbsum 
               + mma2008.impact 
               + fascsurgeongrpannual
               #+ firstqtraftermma
               #          +.qtr.since2006+.qtr.since2006.sqrt
               +.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4
               +.qtr.since2006.bs5
               +.qtr.since2006.bs6
               +.qtr.since2006.bs7+.qtr.since2006.bs8
               +.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11
               , data=anaDfwithBs.less)

summary(fit.totopexp)

#what about per cap total expenditure on outpatinet care
fit.percap.totopexp=glm(log(percap.totopexp) ~ qtr + mean.age + .avg.comorbsum 
                 + mma2008.impact 
                 + fascsurgeongrpannual
                 #+ firstqtraftermma
                 #          +.qtr.since2006+.qtr.since2006.sqrt
                 +.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4
                 +.qtr.since2006.bs5
                 +.qtr.since2006.bs6
                 +.qtr.since2006.bs7+.qtr.since2006.bs8
                 +.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11
                 , data=anaDfwithBs.less)

summary(fit.percap.totopexp)

#per capita outpatient volume
fit.percap.outpvol=glm(
  totaloutpvol ~  mma2008.impact+fascsurgeongrp+mean.age+.avg.comorbsum+race.0+race.1+race.2+race.3+race.4+race.5+sex.1+.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4+.qtr.since2006.bs5+.qtr.since2006.bs6,
  , offset=log(n.bene)
  , family = poisson
  , data=anaDfwithBs.less)
summary(fit.percap.outpvol)

