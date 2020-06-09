(load('Z:/j_scrdata/ascMMA/result/policySimulation05172014.RData'))
okData=modelFit.allvol$data
nrow(modelFit.allvol$data)
tail(okData[,c('hrrnum','.qtr.since2006','totaloutpvol','mma2008.impact')],100)
tail(anaDfwithBs[,c('hrrnum','.qtr.since2006','totaloutpvol','mma2008.impact'),100])
nrow(anaDfwithBs)
names(okData)

summary(modelFit.allvol)
nrow(okData)
count(okData,'.qtr.since2006')





modelFit.allvol.ck=glm(
  totaloutpvol ~  mma2008.impact+fascsurgeongrp+ mma2008.impact*fascsurgeongrp+mean.age+.avg.comorbsum+race.0+race.1+race.2+race.3+race.4+race.5+sex.1+.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4+.qtr.since2006.bs5+.qtr.since2006.bs6,
  , offset=log(n.bene)
  , family = poisson
  , data=okData)
summary(modelFit.allvol.ck)

summary(modelFit.allvol)