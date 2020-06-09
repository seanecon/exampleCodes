

(load(file='Z:/j_scrdata/ascMMA/result/anaDfNoBs.Jan262015.RData'))
addedVars=anaDfNoBs[,c('hrrnum','.qtr.since2006','collegeAbovePct','povpct','percaincome','per10kMd')]

(load(file='Z:/j_scrdata/ascMMA/result/anaDfNoBs.Jan062015.RData'))

anaDfNoBs=join(anaDfNoBs,addedVars)

addTimeTrendVar=function(df,knots.tiles.2004=
                           #c(0.2,0.4,0.6,0.8)
                           0.5
                         , knots.tiles.2008=c(0.5)){
  names(df)=tolower(names(df))
  
  df[,'.qtr.since2004'] = df[,'.qtr.since2006'] + 8
  df[,'.qtr.since2004.sq'] = df[,'.qtr.since2004']^2
  df[,'.qtr.since2004.sqrt'] = df[,'.qtr.since2004']^0.5
  
  df[,'.qtr.since2008'] = df[,'.qtr.since2006'] - 8
  
  df[,'.qtr.since2008.nonpos']= (df[,'.qtr.since2008']<=0)
  df[,'.qtr.since2008.pos']= (df[,'.qtr.since2008']>0)*df[,'.qtr.since2008']
  df[,'.qtr.since2008.pos.sq']= df[,'.qtr.since2008.pos']^2
  df[,'.qtr.since2008.pos.cu']= df[,'.qtr.since2008.pos']^3
  df[,'.qtr.since2008.pos.sqrt']= df[,'.qtr.since2008.pos']^0.5
  #df[,'.qtr.since2008.pos.sqrt']= (df[,'.qtr.since2008']>0)*sqrt(df[,'.qtr.since2008'])
  
  df[,'firstQtrAfterMMA']=0
  df[which(df[,'.qtr.since2006']==9),'firstQtrAfterMMA']=1
  
  df[,'firstYrAfterMMA']=0
  df[which(df[,'.qtr.since2006']==9),'firstYrAfterMMA']=1
  
  df[,'firstYr.qtr.since2008.pos']=df[,'firstYrAfterMMA']*df[,'.qtr.since2008.pos']
  df[,'firstYr.qtr.since2008.pos']=df[,'firstYrAfterMMA']*df[,'.qtr.since2008.pos.sq']
  df[,'firstQtr.qtr.since2008.pos']=df[,'firstQtrAfterMMA']*df[,'.qtr.since2008.pos']
  df[,'firstQtr.qtr.since2008.pos']=df[,'firstQtrAfterMMA']*df[,'.qtr.since2008.pos.sq']
  
  #how about use bs spline to capture nonlinear trend
  
  #since 2004 bs
  bslist.04=bsWrapper1.yz(df[,'.qtr.since2004'] #this is the x in bs function
                          , quantile(df[,'.qtr.since2004'],probs=knots.tiles.2004) #inner knots, if it contains boundary knots, function will stop and issue erro message
                          , paste('.qtr.since2004','.bs',sep='') #output data's columen name stem
                          , degree=2
                          , dataType='data.frame'
                          , Boundary.knots=quantile(df[,'.qtr.since2004'],prob=c(0,1))
  )
  n.basis.04=bslist.04$n.basis
  df=cbind(df,bslist.04$bsdata)
  
  
  #since 2008 bs
  bslist.08=bsWrapper1.yz(df[,'.qtr.since2008.pos'] #this is the x in bs function
                          , quantile(df[which(df[,'.qtr.since2008.pos']>0),'.qtr.since2008.pos'],probs=knots.tiles.2008) #inner knots, if it contains boundary knots, function will stop and issue erro message
                          , paste('.qtr.since2008.pos','.bs',sep='') #output data's columen name stem
                          , degree=2
                          , dataType='data.frame'
                          , Boundary.knots=quantile(df[,'.qtr.since2008.pos'],prob=c(0,1))
  )
  n.basis.08=bslist.08$n.basis
  df=cbind(df,bslist.08$bsdata)
  
  names(df)=tolower(names(df))
  
  return(df)
  
}

anaDfDev=addTimeTrendVar(anaDfNoBs,knots.tiles.2004=
                           c(0.5)
                          
                         , knots.tiles.2008=c(0.5))
names(anaDfDev)

unique(subset(anaDfWithBs, year %in% c('2004','2005','2006','2007'))[,'year'])
names(anaDfDev)

anaDfDev=cbind(anaDfDev,dummyEffectCoding.yz(anaDfDev,'fascsurgeongrpannual',levelList=list(levels(anaDfDev[,'fascsurgeongrpannual'])),typeVec='dummy',returnList=TRUE,codedVnList=list(c('mid','high'))))
names(anaDfDev)

plot(ddply(anaDfDev,c('.qtr.since2004'),function(x){mean(x[,'percap.outpvol'])}))




names(anaDfDev)
anaDfDev.till2011=subset(anaDfDev, year %in% seq(2004,2011))


#we need plot for outpatient utilization and like to see a drop in utilization. and per captial spending go us and cancels the utilization drop effect


#per capital outpatient vol
fit.percap.outpVol=glm(
  totaloutpvol ~
     + qtr + mean.age + .avg.comorbsum +race.1+sex.1
  #+percapinc+familyfemalehead+collegeedu+numhospbeds.per100k
  +collegeabovepct+povpct+percaincome+per10kmd
  #+ fascsurgeongrpannual
  +fascsurgeongrpannual.mid
  +fascsurgeongrpannual.high
  #.qtr.since2004
  #+.qtr.since2004.sq  not good
  #+.qtr.since2004.sqrt #you really needs this term. without this sqrt term it will not look good
  #once this terms is marginal signficant (actully it is 0.05), you can justify the linear term utilization
  +.qtr.since2004.bs1+.qtr.since2004.bs2+ .qtr.since2004.bs3  # +.qtr.since2004.bs4+.qtr.since2004.bs5+.qtr.since2004.bs6 
  
  # +.qtr.since2004.sq #this will cause really back noMMAprediction for year after 2008
  #+firstqtraftermma
  #+firstyraftermma
  #+firstyr.qtr.since2008.pos
  +.qtr.since2008.nonpos
  #   +.qtr.since2008.pos
  #+.qtr.since2008.pos.sqrt
  #+.qtr.since2008.pos.sq
  #                +.qtr.since2008.pos.cu
  +.qtr.since2008.pos.bs1+.qtr.since2008.pos.bs2+.qtr.since2008.pos.bs3
  #+.qtr.since2008.pos.bs3 #+.qtr.since2008.pos.bs4+.qtr.since2008.pos.bs5+.qtr.since2008.pos.bs6
  ,
  , offset=log(n.bene)
  , family = poisson
  , data=anaDfDev.till2011)
summary(fit.percap.outpVol)

# Call:
#   glm(formula = totaloutpvol ~ +qtr + mean.age + .avg.comorbsum + 
#         race.1 + sex.1 + collegeabovepct + povpct + percaincome + 
#         per10kmd + fascsurgeongrpannual.mid + fascsurgeongrpannual.high + 
#         .qtr.since2004.bs1 + .qtr.since2004.bs2 + .qtr.since2004.bs3 + 
#         .qtr.since2008.nonpos + .qtr.since2008.pos.bs1 + .qtr.since2008.pos.bs2 + 
#         .qtr.since2008.pos.bs3, family = poisson, data = anaDfDev.till2011, 
#       offset = log(n.bene))
# 
# Deviance Residuals: 
#   Min        1Q    Median        3Q       Max  
# -34.2815   -6.5911   -1.6163    4.0354   65.2943  
# 
# Coefficients:
#   Estimate  Std. Error   z value Pr(>|z|)    
# (Intercept)               -6.1821e+00  1.3061e-02 -473.3362  < 2e-16 ***
#   qtr2                       8.9139e-02  4.0295e-04  221.2148  < 2e-16 ***
#   qtr3                       7.2147e-02  4.0858e-04  176.5795  < 2e-16 ***
#   qtr4                       6.1928e-02  4.1700e-04  148.5078  < 2e-16 ***
#   mean.age                   4.8201e-02  1.5076e-04  319.7206  < 2e-16 ***
#   .avg.comorbsum             3.2584e-01  5.4713e-04  595.5417  < 2e-16 ***
#   race.1                     2.8069e-04  1.5525e-05   18.0802  < 2e-16 ***
#   sex.1                      8.8411e-03  1.2216e-04   72.3739  < 2e-16 ***
#   collegeabovepct           -1.0848e-03  2.9218e-05  -37.1288  < 2e-16 ***
#   povpct                     1.0510e-02  5.9356e-05  177.0695  < 2e-16 ***
#   percaincome                7.1259e-03  4.4329e-05  160.7507  < 2e-16 ***
#   per10kmd                  -4.7849e-03  2.9679e-05 -161.2225  < 2e-16 ***
#   fascsurgeongrpannual.mid   3.1401e-02  3.2291e-04   97.2433  < 2e-16 ***
#   fascsurgeongrpannual.high  7.4449e-02  3.8770e-04  192.0265  < 2e-16 ***
#   .qtr.since2004.bs1         6.1998e-02  1.4209e-03   43.6342  < 2e-16 ***
#   .qtr.since2004.bs2         2.0956e-01  2.5586e-03   81.9041  < 2e-16 ***
#   .qtr.since2004.bs3         4.5740e-01  1.7823e-01    2.5664  0.01028 *  
#   .qtr.since2008.nonposTRUE  3.4599e-02  2.8363e-03   12.1986  < 2e-16 ***
#   .qtr.since2008.pos.bs1     3.9820e-02  1.3917e-02    2.8612  0.00422 ** 
#   .qtr.since2008.pos.bs2    -1.7890e-01  7.8508e-02   -2.2787  0.02268 *  
#   .qtr.since2008.pos.bs3    -2.6231e-01  1.7622e-01   -1.4886  0.13660    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# 
# (Dispersion parameter for poisson family taken to be 1)
# 
# Null deviance: 1867976  on 9791  degrees of freedom
# Residual deviance:  792063  on 9771  degrees of freedom
# AIC: 891231
# 
# Number of Fisher Scoring iterations: 4


#this is observed with MMA
predVal=exp(btXbWithShock.yz( coef(fit.percap.outpVol) 
                              , vcov(fit.percap.outpVol)  
                              , changedBetas=NULL
                              , 0 #for poission model, the random shock follows a poission 
                              , model.matrix(formula(fit.percap.outpVol),fit.percap.outpVol$data) 
                              , 100                        
                              , changedBetasHaveVariation=TRUE
                              , seed=1  
                              , sameShockVec=FALSE   
                              
))


locList=valLoc.yz(anaDfDev.till2011$'.qtr.since2008')
meanMat=do.call(rbind,lapply(locList,function(x){apply(predVal[x,],2,mean)}))
percap.outpvol.mma.hat=apply(meanMat,1,mean)
percap.outpvol.mma.ci=apply(meanMat,1,function(x){quantile(x,probs=c(0.025,0.975))})
#percap.outpvol.true=unlist(lapply(locList,function(x){mean(anaDfWithBs$percap.outpvol[x])}))

#this is counterfactural without MMA
changedBetas=c(.qtr.since2008.pos.bs1=0,.qtr.since2008.pos.bs2=0,.qtr.since2008.pos.bs3=0)
predVal=exp(btXbWithShock.yz( coef(fit.percap.outpVol) 
                              , vcov(fit.percap.outpVol)  
                              , changedBetas=changedBetas
                              , 0 #for poission model, the random shock follows a poission 
                              , model.matrix(formula(fit.percap.outpVol),fit.percap.outpVol$data) 
                              , 100                        
                              , changedBetasHaveVariation=TRUE
                              , seed=1  
                              , sameShockVec=FALSE   
                              
))

meanMat=do.call(rbind,lapply(locList,function(x){apply(predVal[x,],2,mean)}))
percap.outpvol.noMma.hat=apply(meanMat,1,mean)
percap.outpvol.noMma.ci=apply(meanMat,1,function(x){quantile(x,probs=c(0.025,0.975))})

percap.outpvol.mma.nomma=list(
  data.frame(as.numeric(names(locList)),percap.outpvol.mma.hat,percap.outpvol.mma.ci[1,],percap.outpvol.mma.ci[2,])
  , data.frame(as.numeric(names(locList)),percap.outpvol.noMma.hat,percap.outpvol.noMma.ci[1,],percap.outpvol.noMma.ci[2,])
)

plotLineCi.yz(percap.outpvol.mma.nomma
              #each element of the list is a data.frame has four columns, the first column x, the second columen is mean.y, third columen is lowerbound.y, the four column is upperbound.y
              ,xlim=c(-15,16)
              ,ylim=c(0.25,0.5)
              ,pchVec=c(1,2) #this should be the same lenght as plotdata.list
              ,ciBar=TRUE #if FALSE then there is no CI bar, then it beomes nonerror bar line point
              ,jit.size=0 #to avoid overlapping
              ,ciBarLwd=1 #width of error bar
              ,type='b' #can line 'l' or both 'b'
              ,gap=0
              ,sfrac=0.01
              ,maintitle=c('per capita outpatient volume: MMA vs. no MMA') #title of plot
              ,x.label=c('quarter since 2008')
              ,y.label=c('per capita outpatient volume (encounter day)')
              ,legendLoc='bottomright'
              ,legend.txt.vec=c('MMA','no MMA') #the legend text characteri vector
              ,cex.legend.text=1
              ,#pdf.path=NULL
              pdf.path=pdf('Z:/j_scrdata/ascMMA/result/fig/percapOutPatientVolMmaNoMma_Jan282014.pdf')
                #pdf('Z:/j_scrdata/ascMMA/result/fig/percapOutPatientVolMmaNoMma_Jan072014.pdf') #with e.g., C;/abc.pdf
              
              #pdf('Z:/j_scrdata/ascMMA/result/fig/percapOutPatientVolMmaNoMma_sep302014.pdf')
)



percap.outpvol.mma.nomma

> percap.outpvol.mma.nomma
[[1]]
as.numeric.names.locList.. percap.outpvol.mma.hat percap.outpvol.mma.ci.1... percap.outpvol.mma.ci.2...
-15                        -15             0.29525156                 0.29492585                 0.29555072
-14                        -14             0.32512539                 0.32485944                 0.32546268
-13                        -13             0.32194102                 0.32170855                 0.32215126
-12                        -12             0.32097767                 0.32071375                 0.32119986
-11                        -11             0.30429320                 0.30406382                 0.30449976
-10                        -10             0.33518490                 0.33496440                 0.33537495
-9                          -9             0.33200404                 0.33180046                 0.33220345
-8                          -8             0.33111234                 0.33083516                 0.33134892
-7                          -7             0.31842848                 0.31818110                 0.31864549
-6                          -6             0.35086301                 0.35062721                 0.35109198
-5                          -5             0.34764020                 0.34741853                 0.34786300
-4                          -4             0.34681308                 0.34655087                 0.34705789
-3                          -3             0.32922323                 0.32901691                 0.32946530
-2                          -2             0.36286881                 0.36257069                 0.36313713
-1                          -1             0.35964625                 0.35932111                 0.35997164
0                            0             0.35890087                 0.35849734                 0.35932794
1                            1             0.33852089                 0.33795279                 0.33905398
2                            2             0.37445640                 0.37404346                 0.37485602
3                            3             0.37150868                 0.37116554                 0.37184289
4                            4             0.37042360                 0.37017644                 0.37076280
5                            5             0.35408086                 0.35378316                 0.35437051
6                            6             0.38869137                 0.38839466                 0.38902387
7                            7             0.38301309                 0.38267295                 0.38335593
8                            8             0.37934549                 0.37902547                 0.37957896
9                            9             0.35831615                 0.35807761                 0.35854875
10                          10             0.39071533                 0.39046453                 0.39100697
11                          11             0.38259423                 0.38228737                 0.38291422
12                          12             0.37763514                 0.37731981                 0.37803597
13                          13             0.35846234                 0.35811530                 0.35879145
14                          14             0.39209592                 0.39172032                 0.39243096
15                          15             0.38624993                 0.38594243                 0.38653746
16                          16             0.38368797                 0.38325202                 0.38415853

[[2]]
as.numeric.names.locList.. percap.outpvol.noMma.hat percap.outpvol.noMma.ci.1... percap.outpvol.noMma.ci.2...
-15                        -15               0.29525156                   0.29492585                   0.29555072
-14                        -14               0.32512539                   0.32485944                   0.32546268
-13                        -13               0.32194102                   0.32170855                   0.32215126
-12                        -12               0.32097767                   0.32071375                   0.32119986
-11                        -11               0.30429320                   0.30406382                   0.30449976
-10                        -10               0.33518490                   0.33496440                   0.33537495
-9                          -9               0.33200404                   0.33180046                   0.33220345
-8                          -8               0.33111234                   0.33083516                   0.33134892
-7                          -7               0.31842848                   0.31818110                   0.31864549
-6                          -6               0.35086301                   0.35062721                   0.35109198
-5                          -5               0.34764020                   0.34741853                   0.34786300
-4                          -4               0.34681308                   0.34655087                   0.34705789
-3                          -3               0.32922323                   0.32901691                   0.32946530
-2                          -2               0.36286881                   0.36257069                   0.36313713
-1                          -1               0.35964625                   0.35932111                   0.35997164
0                            0               0.35890087                   0.35849734                   0.35932794
1                            1               0.33643456                   0.33586997                   0.33696437
2                            2               0.37089409                   0.37048508                   0.37128991
3                            3               0.36776426                   0.36742458                   0.36809510
4                            4               0.36751027                   0.36726506                   0.36784680
5                            5               0.35307091                   0.35277406                   0.35335973
6                            6               0.39063524                   0.39033704                   0.39096940
7                            7               0.38905012                   0.38870462                   0.38939836
8                            8               0.39054461                   0.39021515                   0.39078498
9                            9               0.37494217                   0.37469256                   0.37518556
10                          10               0.41671478                   0.41644729                   0.41702582
11                          11               0.41690669                   0.41657231                   0.41725538
12                          12               0.42041160                   0.42006055                   0.42085783
13                          13               0.40752153                   0.40712700                   0.40789568
14                          14               0.45499538                   0.45455952                   0.45538416
15                          15               0.45729201                   0.45692795                   0.45763243
16                          16               0.46325149                   0.46272515                   0.46381963





#figure 2

# With fasc effect outpatient per capita volume modling 
#__________________________



#per capital outpatient  vol
# fit.percap.outpVol.fasc=glm(
#   totaloutpvol ~
#     mma2008.impact +fascsurgeongrpannual+fascsurgeongrpannual*mma2008.impact
#  # + qtr 
#   + mean.age + .avg.comorbsum +mean.age
#   +race.0+race.1+race.2+race.3+race.4+race.5
#   +sex.1
#   +.qtr.since2006.bs1+.qtr.since2006.bs2+.qtr.since2006.bs3+.qtr.since2006.bs4+.qtr.since2006.bs5+.qtr.since2006.bs6+.qtr.since2006.bs7+.qtr.since2006.bs8+.qtr.since2006.bs8+.qtr.since2006.bs9+.qtr.since2006.bs10+.qtr.since2006.bs11,
#   , offset=log(n.bene)
#   , family = poisson
#   , data=anaDfWithBs)

#names(anaDfWithBs)
#summary(fit.percap.outpVol.nofasc)
summary(fit.percap.outpVol)

# 
# predVal=exp(btXbWithShock.yz( coef(fit.percap.outpVol) 
#                               , vcov(fit.percap.outpVol)  
#                               , changedBetas=NULL
#                               , 0 #for poission model, the random shock follows a poission 
#                               , model.matrix(formula(fit.percap.outpVol),fit.percap.outpVol$data) 
#                               , 100                        
#                               , changedBetasHaveVariation=TRUE
#                               , seed=1  
#                               , sameShockVec=FALSE   
#                               
# ))
# 
# meanMat=do.call(rbind,lapply(locList,function(x){apply(predVal[x,],2,mean)}))
# percap.outpvol.mma.fasc.hat=apply(meanMat,1,mean)
# percap.outpvol.mma.fasc.ci=apply(meanMat,1,function(x){quantile(x,probs=c(0.025,0.975))})
# #percap.outpvol.true=unlist(lapply(locList,function(x){mean(anaDfWithBs$percap.outpvol[x])}))


#FASC group 1 MMA
# testdf=fit.percap.outpVol.fasc$data
# chgLoc=which(testdf[,'.qtr.since2006']>8)
# testdf[chgLoc,'fascsurgeongrpannual']=factor(levels(testdf[,'fascsurgeongrpannual'])[1],levels=levels(testdf[,'fascsurgeongrpannual']))



anaDfDev.lowfasc.mat=model.matrix(formula(fit.percap.outpVol),anaDfDev.till2011)
anaDfDev.lowfasc.mat[,'fascsurgeongrpannual.mid']=anaDfDev.lowfasc.mat[,'fascsurgeongrpannual.high']=0
changedBetas=NULL


predVal=exp(btXbWithShock.yz( coef(fit.percap.outpVol) 
                              , vcov(fit.percap.outpVol)  
                              , changedBetas=NULL
                              , 0 #for poission model, the random shock follows a poission 
                              , anaDfDev.lowfasc.mat
                              , 100                        
                              , changedBetasHaveVariation=TRUE
                              , seed=1  
                              , sameShockVec=FALSE   
                              
))


meanMat=do.call(rbind,lapply(locList,function(x){apply(predVal[x,],2,mean)}))
percap.outpvol.mma.lowfasc.hat=apply(meanMat,1,mean)
percap.outpvol.mma.lowfasc.ci=apply(meanMat,1,function(x){quantile(x,probs=c(0.025,0.975))})

anaDfDev.midfasc.mat=model.matrix(formula(fit.percap.outpVol),anaDfDev.till2011)
anaDfDev.midfasc.mat[,'fascsurgeongrpannual.mid']=1; anaDfDev.midfasc.mat[,'fascsurgeongrpannual.high']=0
changedBetas=NULL


predVal=exp(btXbWithShock.yz( coef(fit.percap.outpVol) 
                              , vcov(fit.percap.outpVol)  
                              , changedBetas=NULL
                              , 0 #for poission model, the random shock follows a poission 
                              , anaDfDev.midfasc.mat
                              , 100                        
                              , changedBetasHaveVariation=TRUE
                              , seed=1  
                              , sameShockVec=FALSE   
                              
))


meanMat=do.call(rbind,lapply(locList,function(x){apply(predVal[x,],2,mean)}))
percap.outpvol.mma.midfasc.hat=apply(meanMat,1,mean)
percap.outpvol.mma.midfasc.ci=apply(meanMat,1,function(x){quantile(x,probs=c(0.025,0.975))})


anaDfDev.highfasc.mat=model.matrix(formula(fit.percap.outpVol),anaDfDev.till2011)
anaDfDev.highfasc.mat[,'fascsurgeongrpannual.mid']=0; anaDfDev.highfasc.mat[,'fascsurgeongrpannual.high']=1
changedBetas=NULL


predVal=exp(btXbWithShock.yz( coef(fit.percap.outpVol) 
                              , vcov(fit.percap.outpVol)  
                              , changedBetas=NULL
                              , 0 #for poission model, the random shock follows a poission 
                              , anaDfDev.highfasc.mat
                              , 100                        
                              , changedBetasHaveVariation=TRUE
                              , seed=1  
                              , sameShockVec=FALSE   
                              
))


meanMat=do.call(rbind,lapply(locList,function(x){apply(predVal[x,],2,mean)}))
percap.outpvol.mma.highfasc.hat=apply(meanMat,1,mean)
percap.outpvol.mma.highfasc.ci=apply(meanMat,1,function(x){quantile(x,probs=c(0.025,0.975))})


percap.outpvol.mma.byFasc=list(
  data.frame(as.numeric(names(locList)),percap.outpvol.mma.lowfasc.hat,percap.outpvol.mma.lowfasc.ci[1,],percap.outpvol.mma.lowfasc.ci[2,])
  ,  data.frame(as.numeric(names(locList)),percap.outpvol.mma.midfasc.hat,percap.outpvol.mma.midfasc.ci[1,],percap.outpvol.mma.midfasc.ci[2,])
  ,  data.frame(as.numeric(names(locList)),percap.outpvol.mma.highfasc.hat,percap.outpvol.mma.highfasc.ci[1,],percap.outpvol.mma.highfasc.ci[2,])
)


plotLineCi.yz(percap.outpvol.mma.byFasc
              #each element of the list is a data.frame has four columns, the first column x, the second columen is mean.y, third columen is lowerbound.y, the four column is upperbound.y
              ,xlim=c(-15,16)
              ,ylim=c(0.25,0.41)
              ,pchVec=c(1,2,4) #this should be the same lenght as plotdata.list
              ,ciBar=TRUE #if FALSE then there is no CI bar, then it beomes nonerror bar line point
              ,jit.size=0 #to avoid overlapping
              ,ciBarLwd=1 #width of error bar
              ,type='b' #can line 'l' or both 'b'
              ,gap=0
              ,sfrac=0.01
              ,maintitle=c('per capita outpatient volume: with MMA by FASC penetration') #title of plot
              ,x.label=c('quarter since 2008')
              ,y.label=c('per capita outpatient volume (encounter day)')
              ,legendLoc='bottomright'
              ,legend.txt.vec=c('low FASC','medium FASC', 'high FASC') #the legend text characteri vector
              ,cex.legend.text=1
              ,
              #pdf.path=NULL
              pdf.path=pdf('Z:/j_scrdata/ascMMA/result/fig/percapOutPatientVolMmaByFasc_sep302014.pdf') #with e.g., C;/abc.pdf
)
dev.off()


