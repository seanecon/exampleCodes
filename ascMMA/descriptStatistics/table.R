
#now we use the following data to get some basic statistics

#save(anaDfNoBs, file='Z:/j_scrdata/ascMMA/result/anaDfNoBs.Aug082014.RData')
#load(file='Z:/j_scrdata/ascMMA/result/anaDfNoBs.Aug082014.RData')

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

plot(ddply(anaDfDev,c('.qtr.since2004'),function(x){mean(x[,'percap.totopexp'])}))



names(anaDfDev)
nrow(anaDfDev.till2011)/1224
names(anaDfDev.till2011)

anaDfDev.till2011=subset(anaDfDev, year %in% seq(2004,2011))

names(anaDfDev.till2011)
".avg.comorbsum" 

head(anaDfDev.till2011[,'sex.1'])

ddply(anaDfDev.till2011,c('year'),function(x)(apply(x[,c('race.1','sex.1',".avg.comorbsum" )],2,)))
#simple mean(percentage) and std, not by dependent variable
tableNotByDep <- function(df
                          , varVec
                          , categroicalInd # a vector 1 means it is cateogrical 0 means it is continous
                          , byVar #can be missing, usually this is dependent variable
){
  outList <- list()
  for(i in 1:length(varVec)){
    if (categroicalInd[i]){
      cat('processing categorical variable', varVec[i], '\n')
      if (missing(byVar)){
        
        tabout <- t(table(df[,varVec[i]],useNA='ifany'))
        chisq <- chisq.test(tabout)$p.value   
        
      } else{
        tabout <- t(table(df[,byVar],df[,varVec[i]],useNA='ifany'))
        chisq <- chisq.test(tabout)$p.value            
      }
      
      out.i <- list(tabout,chisq)
      
    } else{ #continous variable
      cat('processing contionuous variable', varVec[i], '\n')
      if(missing(byVar)){
        out.i <- c(mean(df[,varVec[i]]), sd(df[,varVec[i]]))
        names(out.i) <- c('mean','sd')
      } else{ 
        out.i <- ddply(df,byVar,function(x){c(mean=x[,varVec[i]],sd=sd(x[,varVec[i]]))})
      }
    }
    outList <- lappend.yz(outList,out.i)
  }
  
  names(outList) <- varVec
  return(outList)
}


getTable = function(df){
  nrow.df=nrow(df)
  outTab=tableNotByDep(df
                       , 
                       c('collegeabovepct','povpct','percaincome','per10kmd','race.1','sex.1','mean.age',".avg.comorbsum",'totopexp',"percap.totopexp","percap.outpvol","per.outp.entday.exp")
                       
                       #c('percapinc','familyfemalehead','collegeedu','numhospbeds.per100k','race.1','sex.1','mean.age',".avg.comorbsum",'totopexp',"percap.totopexp","percap.outpvol","per.outp.entday.exp")
                       , rep(0,12) # a vector 1 means it is cateogrical 0 means it is continous
                       , #can be missing, usually this is dependent variable
  )
  outlist=list(nrow=nrow.df,outTab)
  return(outlist)
}

per.outp.entday.exp

head(anaDfDev.till2011[,c('per.outp.entday.exp')])

anaDfDev.till2011[,'n.bene']

sum(subset(anaDfDev.till2011, year==2008)[,'n.bene'])/4

getTable(subset(anaDfDev.till2011, year==2005))
getTable(subset(anaDfDev.till2011, year==2008))
getTable(subset(anaDfDev.till2011, year==2011))


subset(anaDfDev.till2011, year==2005)[,'hrrnum']

names(anaDfDev.till2011)

#get bene size
(load(file= "Z:/j_scrdata/ascMMA/result/encounterDayDf.20042012.RData"))
#save(encounterDayDf.20062011,file= "Z:/j_scrdata/ascMMA/result/encounterDayDf.20062011.RData")
#(load(file= "Z:/j_scrdata/ascMMA/result/encounterDayVarDf.20062011.RData"))
out=ddply(subset(encounterDayDf.20042012, year<2012),c('.qtr.since2006'),function(x){c(n.bene=sum(x[,'n.bene']))})
out[,'.qtr.since2008']=out[,'.qtr.since2006']-8
qtr.pop.size=subset(out,.qtr.since2008 >=-15)
qtr.pop.size[,c(2,3)]



