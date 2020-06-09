

#to get nice plot, you still like to old variables (not good presentation, because they do not change over time), but for table you like to use new ASC and ARF variables. 

#save(anaDfNoBs, file='Z:/j_scrdata/ascMMA/result/anaDfNoBs.Aug082014.RData')
#load(file='Z:/j_scrdata/ascMMA/result/anaDfNoBs.Aug082014.RData')
#(load(file='Z:/j_scrdata/ascMMA/result/anaDfNoBs.Jan062015.RData'))

(load(file='Z:/j_scrdata/ascMMA/result/anaDfNoBs.Jan262015.RData'))
addedVars=anaDfNoBs[,c('hrrnum','.qtr.since2006','collegeAbovePct','povpct','percaincome','per10kMd')]
(load(file='Z:/j_scrdata/ascMMA/result/anaDfNoBs.Jan062015.RData'))
anaDfNoBs=join(anaDfNoBs,addedVars)

#the following is an ad-hoc correction
anaDfNoBs[,'per.outp.entday.exp']=anaDfNoBs[,'percap.totOpExp']/anaDfNoBs[,'percap.outpVol']
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
                         
                         , knots.tiles.2008=c(0.2,0.4,0.6,0.8))
names(anaDfDev)

#unique(subset(anaDfWithBs, year %in% c('2004','2005','2006','2007'))[,'year'])
names(anaDfDev)

anaDfDev=cbind(anaDfDev,dummyEffectCoding.yz(anaDfDev,'fascsurgeongrpannual',levelList=list(levels(anaDfDev[,'fascsurgeongrpannual'])),typeVec='dummy',returnList=TRUE,codedVnList=list(c('mid','high'))))
names(anaDfDev)

anaDfDev.till2011=subset(anaDfDev, year %in% seq(2004,2011))


fit.percap.outopexp=glm(
  log(percap.totopexp) ~
    #.qtr.since2004
    # +.qtr.since2004.sq  #not good
    #+.qtr.since2004.sqrt #you really needs this term. without this sqrt term it will not look good
    #once this terms is marginal signficant (actully it is 0.05), you can justify the linear term utilization
    qtr + mean.age + .avg.comorbsum +race.1+sex.1
 # +percapinc+familyfemalehead+collegeedu+numhospbeds.per100k #you can instead use these variable than collegeabovepct+povpct+percaincome+per10kmd to get a plot slightly better
  +collegeabovepct+povpct+percaincome+per10kmd
  #+collegeabovepct+povpct+percaincome+per10kmd
  #+ fascsurgeongrpannual
  +fascsurgeongrpannual.mid
  +fascsurgeongrpannual.high
  +.qtr.since2004.bs1+.qtr.since2004.bs2+ .qtr.since2004.bs3  
  # +.qtr.since2004.bs4+.qtr.since2004.bs5+.qtr.since2004.bs6 
  #.qtr.since2004 +.qtr.since2004.sq #this will cause really back noMMAprediction for year after 2008
  #+firstqtraftermma
  #+firstyraftermma
  #+firstyr.qtr.since2008.pos
  +.qtr.since2008.nonpos
  +.qtr.since2008.pos
  #+.qtr.since2008.pos.sqrt
  #+.qtr.since2008.pos.sq
  #                +.qtr.since2008.pos.cu
  #+.qtr.since2008.pos.bs1+.qtr.since2008.pos.bs2+.qtr.since2008.pos.bs3 #+.qtr.since2008.pos.bs4+.qtr.since2008.pos.bs5+.qtr.since2008.pos.bs6
  
  , data=anaDfDev.till2011)





range(anaDfDev.till2011[,'.qtr.since2008'])
unique(anaDfDev.till2011[,'year'])
#names(anaDfWithBs)
summary(fit.percap.outopexp)

# 
# 
# 
# Call:
#   glm(formula = log(percap.totopexp) ~ qtr + mean.age + .avg.comorbsum + 
#         race.1 + sex.1 + collegeabovepct + povpct + percaincome + 
#         per10kmd + fascsurgeongrpannual.mid + fascsurgeongrpannual.high + 
#         .qtr.since2004.bs1 + .qtr.since2004.bs2 + .qtr.since2004.bs3 + 
#         .qtr.since2008.nonpos + .qtr.since2008.pos, data = anaDfDev.till2011)
# 
# Deviance Residuals: 
#   Min         1Q     Median         3Q        Max  
# -0.509811  -0.078744   0.004960   0.082437   0.464205  
# 
# Coefficients:
#   Estimate  Std. Error  t value  Pr(>|t|)    
# (Intercept)                2.06798429  0.11136493  18.5694 < 2.2e-16 ***
#   qtr2                       0.05870764  0.00359158  16.3459 < 2.2e-16 ***
#   qtr3                       0.04178368  0.00363243  11.5030 < 2.2e-16 ***
#   qtr4                       0.01378827  0.00369866   3.7279 0.0001942 ***
#   mean.age                   0.02190546  0.00132433  16.5407 < 2.2e-16 ***
#   .avg.comorbsum             0.20694165  0.00503615  41.0913 < 2.2e-16 ***
#   race.1                    -0.00016579  0.00014764  -1.1229 0.2615001    
# sex.1                      0.00883205  0.00101772   8.6783 < 2.2e-16 ***
#   collegeabovepct            0.00070784  0.00025047   2.8261 0.0047216 ** 
#   povpct                     0.00422534  0.00049334   8.5648 < 2.2e-16 ***
#   percaincome                0.00583051  0.00040968  14.2318 < 2.2e-16 ***
#   per10kmd                  -0.00298785  0.00026331 -11.3473 < 2.2e-16 ***
#   fascsurgeongrpannual.mid   0.04526492  0.00320983  14.1020 < 2.2e-16 ***
#   fascsurgeongrpannual.high  0.08579224  0.00336450  25.4993 < 2.2e-16 ***
#   .qtr.since2004.bs1         0.16836416  0.01292968  13.0215 < 2.2e-16 ***
#   .qtr.since2004.bs2         0.41178773  0.02378546  17.3126 < 2.2e-16 ***
#   .qtr.since2004.bs3         0.24751684  0.05010558   4.9399 7.946e-07 ***
#   .qtr.since2008.nonposTRUE  0.03371786  0.00738410   4.5663 5.025e-06 ***
#   .qtr.since2008.pos         0.01326858  0.00228564   5.8052 6.629e-09 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
# 
# (Dispersion parameter for gaussian family taken to be 0.015648075)
# 
# Null deviance: 514.116  on 9791  degrees of freedom
# Residual deviance: 152.929  on 9773  degrees of freedom
# AIC: -12899.9
# 
# Number of Fisher Scoring iterations: 2


#this is observed with MMA
predVal=btXbWithShock.yz( coef(fit.percap.outopexp) 
                          , vcov(fit.percap.outopexp)  
                          , changedBetas=NULL
                          , 0 #for poission model, the random shock follows a poission 
                              , model.matrix(formula(fit.percap.outopexp),fit.percap.outopexp$data) 
                              , 100                        
                              , changedBetasHaveVariation=TRUE
                              , seed=1  
                              , sameShockVec=FALSE   
                              
)


locList=valLoc.yz(anaDfDev.till2011$'.qtr.since2008')
meanMat=do.call(rbind,lapply(locList,function(x){apply(exp(predVal[x,]),2,mean)}))
percap.outpexp.mma.hat=apply(meanMat,1,mean)
percap.outpexp.mma.ci=apply(meanMat,1,function(x){quantile(x,probs=c(0.025,0.975))})
#percap.outpvol.true=unlist(lapply(locList,function(x){mean(anaDfWithBs$percap.outpvol[x])}))

#this is counterfactural without MMA
#changedBetas=c(.qtr.since2008.pos.bs1=0,.qtr.since2008.pos.bs2=0,.qtr.since2008.pos.bs3=0)
changedBetas=c(.qtr.since2008.pos=0)
predVal=btXbWithShock.yz( coef(fit.percap.outopexp) 
                              , vcov(fit.percap.outopexp)  
                              , changedBetas=changedBetas
                              , 0 #for poission model, the random shock follows a poission 
                              , model.matrix(formula(fit.percap.outopexp),fit.percap.outopexp$data) 
                              , 100                        
                              , changedBetasHaveVariation=TRUE
                              , seed=1  
                              , sameShockVec=FALSE   
                              
)

meanMat=do.call(rbind,lapply(locList,function(x){apply(exp(predVal[x,]),2,mean)}))
percap.outpexp.nomma.hat=apply(meanMat,1,mean)
percap.outpexp.nomma.ci=apply(meanMat,1,function(x){quantile(x,probs=c(0.025,0.975))})
percap.outpexp.mma.nomma=list(data.frame(as.numeric(names(locList)),percap.outpexp.mma.hat, percap.outpexp.mma.ci[1,],percap.outpexp.mma.ci[2,])
                                 ,data.frame(as.numeric(names(locList)),percap.outpexp.nomma.hat, percap.outpexp.nomma.ci[1,],percap.outpexp.nomma.ci[2,])
)

plotLineCi.yz(percap.outpexp.mma.nomma
              #each element of the list is a data.frame has four columns, the first column x, the second columen is mean.y, third columen is lowerbound.y, the four column is upperbound.y
              ,xlim=c(-15,16)
              ,ylim=c(100,250)
              ,pchVec=c(1,2) #this should be the same lenght as plotdata.list
              ,ciBar=TRUE #if FALSE then there is no CI bar, then it beomes nonerror bar line point
              ,jit.size=0 #to avoid overlapping
              ,ciBarLwd=1 #width of error bar
              ,type='b' #can line 'l' or both 'b'
              ,gap=0
              ,sfrac=0.01
              ,maintitle=c('per capita outpatient expenditure: MMA vs. no MMA') #title of plot
              ,x.label=c('quarter since 2008')
              ,y.label=c('per capita outpatient expenditure')
              ,legendLoc='bottomright'
              ,legend.txt.vec=c('MMA','no MMA') #the legend text characteri vector
              ,cex.legend.text=1
              ,#pdf.path=NULL
              pdf.path=pdf('Z:/j_scrdata/ascMMA/result/fig/percapOutPatientExpMmaNoMma_jan282014.pdf') #with e.g., C;/abc.pdf
)


#now we use percapital outp spending to predict total spending.
(load(file= "Z:/j_scrdata/ascMMA/result/encounterDayDf.20042012.RData"))
#save(encounterDayDf.20062011,file= "Z:/j_scrdata/ascMMA/result/encounterDayDf.20062011.RData")
#(load(file= "Z:/j_scrdata/ascMMA/result/encounterDayVarDf.20062011.RData"))
out=ddply(subset(encounterDayDf.20042012, year<2012),c('.qtr.since2006'),function(x){c(n.bene=sum(x[,'n.bene']))})
out[,'.qtr.since2008']=out[,'.qtr.since2006']-8
qtr.pop.size=subset(out,.qtr.since2008 >=-15)
qtr.pop.size[,c(2,3)]


save(fit.percap.outopexp,percap.outpexp.mma.nomma,file='')

#note I times 5 in code below, it is for 20% sample
plotList=list()
tmp=rename.vars(percap.outpexp.mma.nomma[[1]],'as.numeric.names.locList..','.qtr.since2008')
junk=join(tmp,qtr.pop.size)
plotList=lappend.yz(plotList,cbind(.qtr.since2008=tmp[,1],junk[,c(2:4)]*junk[,c(6)]*5/1e9))

tmp=rename.vars(percap.outpexp.mma.nomma[[2]],'as.numeric.names.locList..','.qtr.since2008')
junk=join(tmp,qtr.pop.size)
plotList=lappend.yz(plotList,cbind(.qtr.since2008=tmp[,1],junk[,c(2:4)]*junk[,c(6)]*5/1e9))

plotLineCi.yz(plotList
              #each element of the list is a data.frame has four columns, the first column x, the second columen is mean.y, third columen is lowerbound.y, the four column is upperbound.y
              ,xlim=c(-15,16)
              ,ylim=c(3,5.5)
              ,pchVec=c(1,2) #this should be the same lenght as plotdata.list
              ,ciBar=TRUE #if FALSE then there is no CI bar, then it beomes nonerror bar line point
              ,jit.size=0 #to avoid overlapping
              ,ciBarLwd=1 #width of error bar
              ,type='b' #can line 'l' or both 'b'
              ,gap=0
              ,sfrac=0.01
              ,maintitle=c('per capita outpatient expenditure: MMA vs. no MMA') #title of plot
              ,x.label=c('quarter since 2008')
              ,y.label=c('per capita outpatient expenditure (billion dollars)')
              ,legendLoc='bottomright'
              ,legend.txt.vec=c('MMA','no MMA') #the legend text characteri vector
              ,cex.legend.text=1
              ,#pdf.path=NULL
              pdf.path=pdf('Z:/j_scrdata/ascMMA/result/fig/totalOutPatientExpMmaNoMma_jan282014.pdf') #with e.g., C;/abc.pdf
)




> percap.outpexp.mma.nomma
[[1]]
as.numeric.names.locList.. percap.outpexp.mma.hat percap.outpexp.mma.ci.1... percap.outpexp.mma.ci.2...
-15                        -15              120.29157                  118.87443                  121.36735
-14                        -14              130.05927                  128.98022                  130.98166
-13                        -13              130.30364                  129.27321                  131.42856
-12                        -12              129.07365                  128.37358                  130.05064
-11                        -11              130.20936                  129.47228                  131.00670
-10                        -10              140.60484                  139.72517                  141.72298
-9                          -9              140.69057                  139.58364                  141.71559
-8                          -8              139.18595                  138.28230                  139.99579
-7                          -7              140.89370                  139.95578                  141.88734
-6                          -6              151.94927                  150.79460                  153.41340
-5                          -5              151.84879                  150.66908                  152.76488
-4                          -4              150.03415                  149.15587                  150.82898
-3                          -3              151.01256                  150.00816                  152.23662
-2                          -2              162.65577                  161.32232                  164.11453
-1                          -1              162.34224                  161.21227                  163.62452
0                            0              160.19954                  158.91217                  161.48320
1                            1              161.05186                  159.29828                  162.60396
2                            2              175.58953                  173.89117                  177.05972
3                            3              177.35981                  175.93863                  178.89838
4                            4              176.88447                  175.92688                  178.06575
5                            5              179.30256                  178.38055                  180.30259
6                            6              194.33739                  192.94667                  195.88994
7                            7              194.87674                  193.69079                  196.14792
8                            8              192.91207                  191.66997                  194.41102
9                            9              193.34806                  192.23665                  194.47608
10                          10              208.00450                  206.70420                  209.70346
11                          11              207.03255                  205.93355                  208.24186
12                          12              203.42363                  202.24885                  204.95896
13                          13              204.09181                  202.34781                  205.41784
14                          14              217.93231                  216.33956                  219.54189
15                          15              215.30392                  213.32023                  217.45551
16                          16              209.98074                  208.06995                  212.39139

[[2]]
as.numeric.names.locList.. percap.outpexp.nomma.hat percap.outpexp.nomma.ci.1... percap.outpexp.nomma.ci.2...
-15                        -15                120.29157                    118.87443                    121.36735
-14                        -14                130.05927                    128.98022                    130.98166
-13                        -13                130.30364                    129.27321                    131.42856
-12                        -12                129.07365                    128.37358                    130.05064
-11                        -11                130.20936                    129.47228                    131.00670
-10                        -10                140.60484                    139.72517                    141.72298
-9                          -9                140.69057                    139.58364                    141.71559
-8                          -8                139.18595                    138.28230                    139.99579
-7                          -7                140.89370                    139.95578                    141.88734
-6                          -6                151.94927                    150.79460                    153.41340
-5                          -5                151.84879                    150.66908                    152.76488
-4                          -4                150.03415                    149.15587                    150.82898
-3                          -3                151.01256                    150.00816                    152.23662
-2                          -2                162.65577                    161.32232                    164.11453
-1                          -1                162.34224                    161.21227                    163.62452
0                            0                160.19954                    158.91217                    161.48320
1                            1                158.92904                    157.19858                    160.46068
2                            2                170.99117                    169.33728                    172.42285
3                            3                170.43853                    169.07281                    171.91707
4                            4                167.74123                    166.83313                    168.86145
5                            5                167.79311                    166.93028                    168.72895
6                            6                179.46573                    178.18144                    180.89948
7                            7                177.59171                    176.51095                    178.75015
8                            8                173.48407                    172.36706                    174.83207
9                            9                171.58431                    170.59800                    172.58535
10                          10                182.15789                    181.01917                    183.64574
11                          11                178.91693                    177.96717                    179.96201
12                          12                173.48092                    172.47906                    174.79026
13                          13                171.75660                    170.28891                    172.87254
14                          14                180.98685                    179.66411                    182.32356
15                          15                176.44723                    174.82155                    178.21051
16                          16                169.81651                    168.27120                    171.76606

> plotList
[[1]]
.qtr.since2008 percap.outpexp.mma.hat percap.outpexp.mma.ci.1... percap.outpexp.mma.ci.2...
1             -15              3.0390245                  3.0032222                  3.0662030
2             -14              3.2857940                  3.2585332                  3.3090971
3             -13              3.2919677                  3.2659351                  3.3203876
4             -12              3.2608934                  3.2432069                  3.2855759
5             -11              3.2660511                  3.2475629                  3.2860508
6             -10              3.5268017                  3.5047369                  3.5548481
7              -9              3.5289522                  3.5011869                  3.5546627
8              -8              3.4912115                  3.4685453                  3.5115249
9              -7              3.4487432                  3.4257851                  3.4730650
10             -6              3.7193573                  3.6910937                  3.7551956
11             -5              3.7168979                  3.6880213                  3.7393215
12             -4              3.6724796                  3.6509814                  3.6919352
13             -3              3.6312556                  3.6071038                  3.6606894
14             -2              3.9112287                  3.8791647                  3.9463062
15             -1              3.9036897                  3.8765183                  3.9345234
16              0              3.8521661                  3.8212099                  3.8830331
17              1              3.8116642                  3.7701618                  3.8483983
18              2              4.1557319                  4.1155362                  4.1905272
19              3              4.1976295                  4.1639940                  4.2340434
20              4              4.1863796                  4.1637159                  4.2143373
21              5              4.2235425                  4.2018243                  4.2470988
22              6              4.5776940                  4.5449351                  4.6142650
23              7              4.5903985                  4.5624630                  4.6203418
24              8              4.5441199                  4.5148617                  4.5794284
25              9              4.6491953                  4.6224705                  4.6763193
26             10              5.0016200                  4.9703534                  5.0424727
27             11              4.9782488                  4.9518225                  5.0073275
28             12              4.8914696                  4.8632212                  4.9283878
29             13              4.9497702                  4.9074736                  4.9819302
30             14              5.2854394                  5.2468109                  5.3244760
31             15              5.2216940                  5.1735842                  5.2738755
32             16              5.0925927                  5.0462509                  5.1510573

[[2]]
.qtr.since2008 percap.outpexp.nomma.hat percap.outpexp.nomma.ci.1... percap.outpexp.nomma.ci.2...
1             -15                3.0390245                    3.0032222                    3.0662030
2             -14                3.2857940                    3.2585332                    3.3090971
3             -13                3.2919677                    3.2659351                    3.3203876
4             -12                3.2608934                    3.2432069                    3.2855759
5             -11                3.2660511                    3.2475629                    3.2860508
6             -10                3.5268017                    3.5047369                    3.5548481
7              -9                3.5289522                    3.5011869                    3.5546627
8              -8                3.4912115                    3.4685453                    3.5115249
9              -7                3.4487432                    3.4257851                    3.4730650
10             -6                3.7193573                    3.6910937                    3.7551956
11             -5                3.7168979                    3.6880213                    3.7393215
12             -4                3.6724796                    3.6509814                    3.6919352
13             -3                3.6312556                    3.6071038                    3.6606894
14             -2                3.9112287                    3.8791647                    3.9463062
15             -1                3.9036897                    3.8765183                    3.9345234
16              0                3.8521661                    3.8212099                    3.8830331
17              1                3.7614229                    3.7204676                    3.7976727
18              2                4.0469010                    4.0077580                    4.0807851
19              3                4.0338216                    4.0014987                    4.0688145
20              4                3.9699836                    3.9484915                    3.9964962
21              5                3.9524329                    3.9321088                    3.9744771
22              6                4.2273861                    4.1971341                    4.2611586
23              7                4.1832429                    4.1577852                    4.2105303
24              8                4.0864859                    4.0601743                    4.1182384
25              9                4.1258700                    4.1021534                    4.1499408
26             10                4.3801195                    4.3527382                    4.4158959
27             11                4.3021881                    4.2793506                    4.3273179
28             12                4.1714754                    4.1473849                    4.2029594
29             13                4.1655552                    4.1299598                    4.1926199
30             14                4.3894134                    4.3573334                    4.4218322
31             15                4.2793157                    4.2398884                    4.3220798
32             16                4.1185029                    4.0810252                    4.1657847




#the following is using old social econ variables (they do not change over time so trouble)

> percap.outpexp.mma.nomma
[[1]]
as.numeric.names.locList.. percap.outpexp.mma.hat percap.outpexp.mma.ci.1... percap.outpexp.mma.ci.2...
-15                        -15              120.43752                  119.03014                  121.58185
-14                        -14              130.02294                  129.01969                  131.11861
-13                        -13              130.07892                  129.17243                  131.12005
-12                        -12              128.67506                  127.93266                  129.63367
-11                        -11              130.61452                  129.72542                  131.49522
-10                        -10              140.95159                  140.03376                  142.01857
-9                          -9              140.95335                  139.87397                  141.86667
-8                          -8              139.37410                  138.32750                  140.55542
-7                          -7              140.73404                  139.77597                  141.67840
-6                          -6              151.80825                  150.89779                  152.98735
-5                          -5              151.74628                  150.71137                  152.64572
-4                          -4              149.98324                  148.98462                  151.21659
-3                          -3              150.72308                  149.56856                  151.59233
-2                          -2              162.51527                  161.42994                  163.54786
-1                          -1              162.38129                  161.23467                  163.49943
0                            0              160.42815                  158.94608                  162.38264
1                            1              161.18481                  159.80056                  162.81456
2                            2              175.62693                  174.22911                  177.01099
3                            3              177.29497                  175.98122                  178.51362
4                            4              176.71902                  175.31333                  177.88456
5                            5              179.21429                  178.13812                  180.29204
6                            6              194.23793                  192.89079                  195.38405
7                            7              194.76654                  193.39732                  196.18041
8                            8              192.79131                  191.14143                  194.23067
9                            9              193.55190                  191.99551                  194.82219
10                          10              208.32614                  206.91939                  209.72131
11                          11              207.44763                  206.26231                  208.90973
12                          12              203.92311                  202.56294                  205.15633
13                          13              203.67077                  202.19429                  204.97847
14                          14              217.70150                  216.06789                  219.61227
15                          15              215.28390                  213.44719                  216.85526
16                          16              210.16290                  208.31959                  212.14171

[[2]]
as.numeric.names.locList.. percap.outpexp.nomma.hat percap.outpexp.nomma.ci.1... percap.outpexp.nomma.ci.2...
-15                        -15                120.43752                    119.03014                    121.58185
-14                        -14                130.02294                    129.01969                    131.11861
-13                        -13                130.07892                    129.17243                    131.12005
-12                        -12                128.67506                    127.93266                    129.63367
-11                        -11                130.61452                    129.72542                    131.49522
-10                        -10                140.95159                    140.03376                    142.01857
-9                          -9                140.95335                    139.87397                    141.86667
-8                          -8                139.37410                    138.32750                    140.55542
-7                          -7                140.73404                    139.77597                    141.67840
-6                          -6                151.80825                    150.89779                    152.98735
-5                          -5                151.74628                    150.71137                    152.64572
-4                          -4                149.98324                    148.98462                    151.21659
-3                          -3                150.72308                    149.56856                    151.59233
-2                          -2                162.51527                    161.42994                    163.54786
-1                          -1                162.38129                    161.23467                    163.49943
0                            0                160.42815                    158.94608                    162.38264
1                            1                159.46318                    158.09371                    161.07553
2                            2                171.89519                    170.52707                    173.24984
3                            3                171.67433                    170.40223                    172.85434
4                            4                169.28893                    167.94234                    170.40546
5                            5                169.84556                    168.82565                    170.86697
6                            6                182.11760                    180.85452                    183.19221
7                            7                180.66272                    179.39265                    181.97421
8                            8                176.92042                    175.40636                    178.24129
9                            9                175.72124                    174.30823                    176.87451
10                          10                187.11427                    185.85075                    188.36738
11                          11                184.33505                    183.28179                    185.63425
12                          12                179.26777                    178.07204                    180.35188
13                          13                177.13353                    175.84943                    178.27085
14                          14                187.31382                    185.90823                    188.95787
15                          15                183.25518                    181.69172                    184.59276
16                          16                176.98525                    175.43294                    178.65167

> plotList #this is for ploting total expenditure for these elgible benes (after multiplying 5)
[[1]]
.qtr.since2008 percap.outpexp.mma.hat percap.outpexp.mma.ci.1... percap.outpexp.mma.ci.2...
1             -15              3.0427118                  3.0071560                  3.0716220
2             -14              3.2848761                  3.2595302                  3.3125569
3             -13              3.2862904                  3.2633889                  3.3125932
4             -12              3.2508236                  3.2320677                  3.2750416
5             -11              3.2762138                  3.2539125                  3.2983045
6             -10              3.5354992                  3.5124772                  3.5622625
7              -9              3.5355435                  3.5084692                  3.5584523
8              -8              3.4959310                  3.4696789                  3.5255621
9              -7              3.4448350                  3.4213839                  3.4679507
10             -6              3.7159054                  3.6936196                  3.7447671
11             -5              3.7143885                  3.6890565                  3.7364047
12             -4              3.6712335                  3.6467898                  3.7014229
13             -3              3.6242946                  3.5965331                  3.6451967
14             -2              3.9078503                  3.8817525                  3.9326800
15             -1              3.9046287                  3.8770570                  3.9315154
16              0              3.8576634                  3.8220254                  3.9046611
17              1              3.8148109                  3.7820493                  3.8533827
18              2              4.1566170                  4.1235343                  4.1893739
19              3              4.1960951                  4.1650022                  4.2249371
20              4              4.1824638                  4.1491950                  4.2100490
21              5              4.2214634                  4.1961137                  4.2468502
22              6              4.5753512                  4.5436188                  4.6023486
23              7              4.5878027                  4.5555502                  4.6211071
24              8              4.5412755                  4.5024120                  4.5751801
25              9              4.6540967                  4.6166722                  4.6846418
26             10              5.0093542                  4.9755279                  5.0429019
27             11              4.9882297                  4.9597278                  5.0233870
28             12              4.9034801                  4.8707737                  4.9331336
29             13              4.9395591                  4.9037503                  4.9712743
30             14              5.2798417                  5.2402221                  5.3261828
31             15              5.2212084                  5.1766631                  5.2593180
32             16              5.0970105                  5.0523053                  5.1450018

[[2]]
.qtr.since2008 percap.outpexp.nomma.hat percap.outpexp.nomma.ci.1... percap.outpexp.nomma.ci.2...
1             -15                3.0427118                    3.0071560                    3.0716220
2             -14                3.2848761                    3.2595302                    3.3125569
3             -13                3.2862904                    3.2633889                    3.3125932
4             -12                3.2508236                    3.2320677                    3.2750416
5             -11                3.2762138                    3.2539125                    3.2983045
6             -10                3.5354992                    3.5124772                    3.5622625
7              -9                3.5355435                    3.5084692                    3.5584523
8              -8                3.4959310                    3.4696789                    3.5255621
9              -7                3.4448350                    3.4213839                    3.4679507
10             -6                3.7159054                    3.6936196                    3.7447671
11             -5                3.7143885                    3.6890565                    3.7364047
12             -4                3.6712335                    3.6467898                    3.7014229
13             -3                3.6242946                    3.5965331                    3.6451967
14             -2                3.9078503                    3.8817525                    3.9326800
15             -1                3.9046287                    3.8770570                    3.9315154
16              0                3.8576634                    3.8220254                    3.9046611
17              1                3.7740646                    3.7416530                    3.8122244
18              2                4.0682969                    4.0359171                    4.1003577
19              3                4.0630696                    4.0329624                    4.0909973
20              4                4.0066135                    3.9747435                    4.0330388
21              5                4.0007793                    3.9767548                    4.0248390
22              6                4.2898521                    4.2600997                    4.3151648
23              7                4.2555816                    4.2256647                    4.2864743
24              8                4.1674304                    4.1317662                    4.1985439
25              9                4.2253454                    4.1913686                    4.2530767
26             10                4.4992992                    4.4689171                    4.5294311
27             11                4.4324709                    4.4071444                    4.4637111
28             12                4.3106244                    4.2818723                    4.3366926
29             13                4.2959603                    4.2648172                    4.3235432
30             14                4.5428593                    4.5087700                    4.5827320
31             15                4.4444265                    4.4065084                    4.4768664
32             16                4.2923639                    4.2547163                    4.3327790


names(percap.outpexp.mma.nomma[[1]])
names(qtr.pop.size)

names(anaDfDev.till2011)
n.bene.hrr.qtr=ddply(anaDfDev.till2011,c('.qtr.since2006','hrrnum'),function(x){mean(x[,'n.bene'])})
head(n.bene.hrr.qtr)
subset(n.bene.hrr.qtr,hrrnum==1)


n.bene.byQtr=ddply(n.bene.hrr.qtr,'.qtr.since2006',function(x){n.bene.byQtr=sum(x[,'V1'])})
plot(n.bene.byQtr)

