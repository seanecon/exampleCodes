

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


#(load(file='Z:/j_scrdata/ascMMA/result/anaDfNoBs.Jan062015.RData'))
(load(file='Z:/j_scrdata/ascMMA/result/anaDfNoBs.Jan262015.RData'))
addedVars=anaDfNoBs[,c('hrrnum','.qtr.since2006','collegeAbovePct','povpct','percaincome','per10kMd')]
(load(file='Z:/j_scrdata/ascMMA/result/anaDfNoBs.Jan062015.RData'))
anaDfNoBs=join(anaDfNoBs,addedVars)

#the following is an ad-hoc correction
anaDfNoBs[,'per.outp.entday.exp']=anaDfNoBs[,'percap.totOpExp']/anaDfNoBs[,'percap.outpVol']

anaDfDev=addTimeTrendVar(anaDfNoBs,knots.tiles.2004=
                           c(0.5)
                          
                         , knots.tiles.2008=c(0.2,0.4,0.6,0.8))
names(anaDfDev)

unique(subset(anaDfWithBs, year %in% c('2004','2005','2006','2007'))[,'year'])
names(anaDfDev)

anaDfDev=cbind(anaDfDev,dummyEffectCoding.yz(anaDfDev,'fascsurgeongrpannual',levelList=list(levels(anaDfDev[,'fascsurgeongrpannual'])),typeVec='dummy',returnList=TRUE,codedVnList=list(c('mid','high'))))
names(anaDfDev)

plot(ddply(anaDfDev,c('.qtr.since2004'),function(x){mean(x[,'per.outp.entday.exp'])}))

names(anaDfDev)
anaDfDev.till2011=subset(anaDfDev, year %in% seq(2004,2011))


#we need plot for outpatient utilization and like to see a drop in utilization. and per captial spending go us and cancels the utilization drop effect

fit.peroutpentdayexp.fasc=glm(
  log(per.outp.entday.exp)  ~
    #.qtr.since2004
   # +.qtr.since2004.sq  #not good
    #+.qtr.since2004.sqrt #you really needs this term. without this sqrt term it will not look good
    #once this terms is marginal signficant (actully it is 0.05), you can justify the linear term utilization
    qtr + mean.age + .avg.comorbsum +race.1+sex.1
  #+percapinc+familyfemalehead+collegeedu+numhospbeds.per100k
  +collegeabovepct+povpct+percaincome+per10kmd
  #+ fascsurgeongrpannual
  +fascsurgeongrpannual.mid
  +fascsurgeongrpannual.high
 +.qtr.since2004
  +.qtr.since2004.sqrt
#  +.qtr.since2004.bs1+.qtr.since2004.bs2+ .qtr.since2004.bs3  
  #+.qtr.since2004.bs4+.qtr.since2004.bs5+.qtr.since2004.bs6 
  # +.qtr.since2004.sq #this will cause really back noMMAprediction for year after 2008
  #+firstqtraftermma
  #+firstyraftermma
  #+firstyr.qtr.since2008.pos
  +.qtr.since2008.nonpos
  +.qtr.since2008.pos
  #+.qtr.since2008.pos.sq
  #+.qtr.since2008.pos.sq
  #                +.qtr.since2008.pos.cu
  #+.qtr.since2008.pos.bs1+.qtr.since2008.pos.bs2+.qtr.since2008.pos.bs3 #+.qtr.since2008.pos.bs4+.qtr.since2008.pos.bs5+.qtr.since2008.pos.bs6
  
  , data=anaDfDev.till2011)

summary(fit.peroutpentdayexp.fasc)

# Coefficients:
#   Estimate  Std. Error  t value  Pr(>|t|)    
# (Intercept)                5.63595005  0.08231009  68.4722 < 2.2e-16 ***
#   qtr2                      -0.02917212  0.00264546 -11.0273 < 2.2e-16 ***
#   qtr3                      -0.03147709  0.00268010 -11.7447 < 2.2e-16 ***
#   qtr4                      -0.04514879  0.00272580 -16.5635 < 2.2e-16 ***
#   mean.age                  -0.00077497  0.00097273  -0.7967 0.4256504    
# .avg.comorbsum            -0.03369606  0.00370019  -9.1066 < 2.2e-16 ***
#   race.1                    -0.00010928  0.00010848  -1.0074 0.3137671    
# sex.1                      0.01021275  0.00074774  13.6582 < 2.2e-16 ***
#   collegeabovepct            0.00016221  0.00018406   0.8813 0.3781884    
# povpct                    -0.00145895  0.00036245  -4.0252 5.735e-05 ***
#   percaincome                0.00275166  0.00030085   9.1462 < 2.2e-16 ***
#   per10kmd                  -0.00099722  0.00019341  -5.1560 2.572e-07 ***
#   fascsurgeongrpannual.mid   0.01406844  0.00235838   5.9653 2.526e-09 ***
#   fascsurgeongrpannual.high -0.00216190  0.00247151  -0.8747 0.3817434    
# .qtr.since2004             0.00112483  0.00176830   0.6361 0.5247202    
# .qtr.since2004.sqrt        0.04149854  0.00921156   4.5050 6.713e-06 ***
#   .qtr.since2008.nonposTRUE -0.01348452  0.00401310  -3.3601 0.0007821 ***
#   .qtr.since2008.pos         0.00817199  0.00090077   9.0722 < 2.2e-16 ***
#   ---

anaDfNoBs$'per.outp.entday.exp'

#no MMA
changedBetas=c(.qtr.since2008.pos=0)
predVal=btXbWithShock.yz( coef(fit.peroutpentdayexp.fasc) 
                          , vcov(fit.peroutpentdayexp.fasc)  
                          , changedBetas=changedBetas
                          , sd(fit.peroutpentdayexp.fasc$residuals)
                          , model.matrix(formula(fit.peroutpentdayexp.fasc),fit.peroutpentdayexp.fasc$data) 
                          , 100                        
                          , changedBetasHaveVariation=TRUE
                          , seed=1  
                          , sameShockVec=FALSE   
                          
)

locList=valLoc.yz(anaDfDev.till2011$'.qtr.since2008')
meanMat=do.call(rbind,lapply(locList,function(x){apply(exp(predVal[x,]),2,mean)}))

perentday.exp.nomma.hat=apply(meanMat,1,mean)
perentday.exp.nomma.ci=apply(meanMat,1,function(x){quantile(x,probs=c(0.025,0.975))})

#MMA
changedBetas=NULL
predVal=btXbWithShock.yz( coef(fit.peroutpentdayexp.fasc) 
                          , vcov(fit.peroutpentdayexp.fasc)  
                          , changedBetas=changedBetas
                          , sd(fit.peroutpentdayexp.fasc$residuals)
                          , model.matrix(formula(fit.peroutpentdayexp.fasc),fit.peroutpentdayexp.fasc$data) 
                          , 100                        
                          , changedBetasHaveVariation=TRUE
                          , seed=1  
                          , sameShockVec=FALSE   
                          
)

locList=valLoc.yz(anaDfDev.till2011$'.qtr.since2008')
meanMat=do.call(rbind,lapply(locList,function(x){apply(exp(predVal[x,]),2,mean)}))

perentday.exp.mma.hat=apply(meanMat,1,mean)
perentday.exp.mma.ci=apply(meanMat,1,function(x){quantile(x,probs=c(0.025,0.975))})


percap.perentday.mma.noMma=list(data.frame(as.numeric(names(locList)),perentday.exp.mma.hat, perentday.exp.mma.ci[1,],perentday.exp.mma.ci[2,])
                                 ,data.frame(as.numeric(names(locList)),perentday.exp.nomma.hat, perentday.exp.nomma.ci[1,],perentday.exp.nomma.ci[2,])
)

plotLineCi.yz(percap.perentday.mma.noMma
              #each element of the list is a data.frame has four columns, the first column x, the second columen is mean.y, third columen is lowerbound.y, the four column is upperbound.y
              ,xlim=c(-15,16)
              ,ylim=c(400,600)
              ,pchVec=c(1,2) #this should be the same lenght as plotdata.list
              ,ciBar=TRUE #if FALSE then there is no CI bar, then it beomes nonerror bar line point
              ,jit.size=0 #to avoid overlapping
              ,ciBarLwd=1 #width of error bar
              ,type='b' #can line 'l' or both 'b'
              ,maintitle=c('per outpatient encounter day spending: MMA vs. no MMA') #title of plot
              ,x.label=c('quarter since 2006')
              ,y.label=c('per outpatient encounter day spending on surgery (dollar)')
              ,legendLoc='bottomright'
              ,legend.txt.vec=c('MMA','no MMA') #the legend text characteri vector
              ,cex.legend.text=1
              ,sfrac=0.002
              ,
              #pdf.path=pdf('Z:/j_scrdata/ascMMA/result/fig/peroutpentdayexpMmaNoMmaOct012014.pdf') #with e.g., C;/abc.pdf
              pdf.path=pdf('Z:/j_scrdata/ascMMA/result/fig/peroutpentdayexpMmaNoMmaJan282014.pdf') #with e.g., C;/abc.pdf
)

> percap.perentday.mma.noMma
[[1]]
as.numeric.names.locList.. perentday.exp.mma.hat perentday.exp.mma.ci.1... perentday.exp.mma.ci.2...
-15                        -15             413.91086                 408.84855                 420.17338
-14                        -14             409.10850                 404.51624                 413.44341
-13                        -13             414.04856                 409.29784                 418.78239
-12                        -12             414.10505                 409.32156                 419.15844
-11                        -11             438.61572                 433.83793                 442.79180
-10                        -10             429.68396                 426.31368                 434.56322
-9                          -9             433.32005                 429.05897                 438.28342
-8                          -8             430.93833                 426.31069                 435.71820
-7                          -7             457.60099                 452.59786                 462.10219
-6                          -6             448.01619                 443.03116                 452.29975
-5                          -5             450.65074                 445.61377                 455.67686
-4                          -4             447.20678                 442.88114                 451.49337
-3                          -3             475.22715                 469.77766                 480.08077
-2                          -2             464.24518                 458.90844                 469.95492
-1                          -1             466.23978                 460.87891                 471.47605
0                            0             463.22448                 458.09099                 468.49042
1                            1             500.38807                 495.47511                 505.71362
2                            2             492.65194                 487.47114                 498.17092
3                            3             499.03888                 494.54723                 504.30012
4                            4             499.17981                 493.83293                 505.11624
5                            5             529.20856                 523.20291                 535.37353
6                            6             521.45957                 516.41965                 527.19698
7                            7             527.56334                 522.45355                 533.67389
8                            8             527.17012                 522.13286                 531.92248
9                            9             559.77580                 553.95136                 565.70518
10                          10             550.67326                 545.20527                 557.15542
11                          11             556.97456                 550.90267                 563.15577
12                          12             556.84758                 550.51017                 562.84112
13                          13             593.40665                 586.32410                 600.02441
14                          14             584.58180                 577.88614                 589.97102
15                          15             590.39466                 584.22884                 597.74008
16                          16             590.06903                 582.48127                 598.68420

[[2]]
as.numeric.names.locList.. perentday.exp.nomma.hat perentday.exp.nomma.ci.1... perentday.exp.nomma.ci.2...
-15                        -15               413.91086                   408.84855                   420.17338
-14                        -14               409.10850                   404.51624                   413.44341
-13                        -13               414.04856                   409.29784                   418.78239
-12                        -12               414.10505                   409.32156                   419.15844
-11                        -11               438.61572                   433.83793                   442.79180
-10                        -10               429.68396                   426.31368                   434.56322
-9                          -9               433.32005                   429.05897                   438.28342
-8                          -8               430.93833                   426.31069                   435.71820
-7                          -7               457.60099                   452.59786                   462.10219
-6                          -6               448.01619                   443.03116                   452.29975
-5                          -5               450.65074                   445.61377                   455.67686
-4                          -4               447.20678                   442.88114                   451.49337
-3                          -3               475.22715                   469.77766                   480.08077
-2                          -2               464.24518                   458.90844                   469.95492
-1                          -1               466.23978                   460.87891                   471.47605
0                            0               463.22448                   458.09099                   468.49042
1                            1               496.31557                   491.44260                   501.59777
2                            2               484.66549                   479.56868                   490.09500
3                            3               486.95322                   482.57034                   492.08704
4                            4               483.12645                   477.95152                   488.87197
5                            5               508.02095                   502.25574                   513.93910
6                            6               496.50812                   491.70935                   501.97099
7                            7               498.23161                   493.40591                   504.00241
8                            8               493.80832                   489.08983                   498.25993
9                            9               520.08303                   514.67159                   525.59197
10                          10               507.46198                   502.42306                   513.43548
11                          11               509.09148                   503.54159                   514.74129
12                          12               504.83303                   499.08759                   510.26671
13                          13               533.59872                   527.23000                   539.54950
14                          14               521.38510                   515.41328                   526.19171
15                          15               522.28397                   516.82947                   528.78199
16                          16               517.74755                   511.08978                   525.30680

