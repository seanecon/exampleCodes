#use for data cleaning
genAnaDf.lapLearn=function(maxLos){
  #need to add hospital effect by add hospital ID
  #insurance type there?
  
  #create surgical patient level demo data.
  demoRaw = xpt2r.yz("Z:/j_scrdata/lapLearn",'_demo_iamdny')
  demoRaw[,'key']=as.character(demoRaw[,'key'])
  demoRaw[,'x.iny'] = NULL
  demoRaw[,'quarterSince2003'] = 4*(demoRaw[,'year']-2003)+demoRaw[,'dqtr']
  names(demoRaw) #6928, ???not 100% sure whey there is more here (6928 > 6821)
  # from C:\Dropbox\K\lapLearningCurve\rcode\genVolMeasurement_1.R
  
  
  (load(file='Z:/j_scrdata/lapLearn/surgVolLag1_may082013.RData'))
  (load(file='Z:/j_scrdata/lapLearn/surgVolLag2_may082013.RData'))
  (load(file='Z:/j_scrdata/lapLearn/surgVolLag3_may082013.RData'))
  (load(file='Z:/j_scrdata/lapLearn/surgVolLag4_may082013.RData'))
  (load(file='Z:/j_scrdata/lapLearn/surgVolLag5_may082013.RData'))
  (load(file='Z:/j_scrdata/lapLearn/surgVolLag6_may082013.RData'))
  
  
  lag1Vol.ia.1=data.frame(hospst="IA",lag1Vol.ia)
  lag1Vol.md.1=data.frame(hospst="MD",lag1Vol.md)
  lag1Vol.ny.1=data.frame(hospst="NY",lag1Vol.ny)
  
  
  
  lag2Vol.ia.1=data.frame(hospst="IA",lag2Vol.ia)
  lag2Vol.md.1=data.frame(hospst="MD",lag2Vol.md)
  lag2Vol.ny.1=data.frame(hospst="NY",lag2Vol.ny)
  
  
  lag3Vol.ia.1=data.frame(hospst="IA",lag3Vol.ia)
  lag3Vol.md.1=data.frame(hospst="MD",lag3Vol.md)
  lag3Vol.ny.1=data.frame(hospst="NY",lag3Vol.ny)
  
  lag4Vol.ia.1=data.frame(hospst="IA",lag4Vol.ia)
  lag4Vol.md.1=data.frame(hospst="MD",lag4Vol.md)
  lag4Vol.ny.1=data.frame(hospst="NY",lag4Vol.ny)
  
  lag5Vol.ia.1=data.frame(hospst="IA",lag5Vol.ia)
  lag5Vol.md.1=data.frame(hospst="MD",lag5Vol.md)
  lag5Vol.ny.1=data.frame(hospst="NY",lag5Vol.ny)
  
  lag6Vol.ia.1=data.frame(hospst="IA",lag6Vol.ia)
  lag6Vol.md.1=data.frame(hospst="MD",lag6Vol.md)
  lag6Vol.ny.1=data.frame(hospst="NY",lag6Vol.ny)
  
  
  okKeys=join(rbind(lag1Vol.ia.1[,c('key','hospst','mdId')]
                    ,lag1Vol.ny.1[,c('key','hospst','mdId')]
                    ,lag1Vol.md.1[,c('key','hospst','mdId')])
              ,demoRaw[,c('key','hospst')]
              ,by=c('key','hospst')
              ,type='left'
              )
  
  
  
  
  test=rbind(lag1Vol.ia.1[,c('key','hospst','mdId')]
             ,lag1Vol.ny.1[,c('key','hospst','mdId')]
             ,lag1Vol.md.1[,c('key','hospst','mdId')])
  demoRaw[,'key']=as.character(demoRaw[,'key'])
  
#   nrow(test)
#   nrow(demoRaw)
# 
#   
#   varTypeCompare.yz(test,problemKeys)
#   
varType.yz(demoRaw)
  problemKeys=subset(demoRaw, hospst=='MD' & year==2009 & dqtr %in% c(1,2))[,c('key','hospst')]
  join(problemKeys,test)[,'key']
  
setdiff.yz(lag1Vol.md.1[,'key'] , problemKeys[,'key'], type='inboth' )
  
  
  subset(demoRaw, hospst=='IA' & year==2009 & dqtr %in% c(1,2))
  
  
  varTypeCompare.yz(okKeys,demoRaw)
  demoRaw.2=join(demoRaw, okKeys)
  demoRaw.2=join(okKeys,demoRaw)
  nrow(demoRaw.2)
  
  
  #troube stems from demoRaw to demoRaw.1
  demoRaw.1=join(okKeys, demoRaw)
  
  
  
  
  
  
  nrow(demoRaw.1)
  subset(demoRaw,hospst=='MD' & year=='2009')[,'dqtr']
  names(demoRaw.1)
  nrow(demoRaw) 
  head(demoRaw)
  
  #   subset(demoRaw, hospst=='MD' & year==2009)[,'dqtr']
  #   nrow(demoRaw.1)
  #   [1] 6821
  #   > nrow(demoRaw)
  #   [1] 6928
  
  #we will use the final keys in okKeys
  #I did surgical volumen twice. lag1Vol.ny, lag1Vol.ia,lag1Vol.md are obtrained later
  #use them as reference is better.
  #so demoRaw should be updated. just assume demoRaw 
  
  lag1Vol.ia.2=rename.vars(lag1Vol.ia.1,c("n.lagk.lap",   "n.lagk.rap",    "n.lagk.allsrg", "n.lagk.nonLap"),c("n.lag1.lap",   "n.lag1.rap",    "n.lag1.allsrg", "n.lag1.nonLap"))
  lag2Vol.ia.2=rename.vars(lag2Vol.ia.1,c("n.lagk.lap",   "n.lagk.rap",    "n.lagk.allsrg", "n.lagk.nonLap"),c("n.lag2.lap",   "n.lag2.rap",    "n.lag2.allsrg", "n.lag2.nonLap"))
  lag3Vol.ia.2=rename.vars(lag3Vol.ia.1,c("n.lagk.lap",   "n.lagk.rap",    "n.lagk.allsrg", "n.lagk.nonLap"),c("n.lag3.lap",   "n.lag3.rap",    "n.lag3.allsrg", "n.lag3.nonLap"))
  lag4Vol.ia.2=rename.vars(lag4Vol.ia.1,c("n.lagk.lap",   "n.lagk.rap",    "n.lagk.allsrg", "n.lagk.nonLap"),c("n.lag4.lap",   "n.lag4.rap",    "n.lag4.allsrg", "n.lag4.nonLap"))
  lag5Vol.ia.2=rename.vars(lag5Vol.ia.1,c("n.lagk.lap",   "n.lagk.rap",    "n.lagk.allsrg", "n.lagk.nonLap"),c("n.lag5.lap",   "n.lag5.rap",    "n.lag5.allsrg", "n.lag5.nonLap"))
  lag6Vol.ia.2=rename.vars(lag6Vol.ia.1,c("n.lagk.lap",   "n.lagk.rap",    "n.lagk.allsrg", "n.lagk.nonLap"),c("n.lag6.lap",   "n.lag6.rap",    "n.lag6.allsrg", "n.lag6.nonLap")) 
  
  
  lag1Vol.md.2=rename.vars(lag1Vol.md.1,c("n.lagk.lap",   "n.lagk.rap",    "n.lagk.allsrg", "n.lagk.nonLap"),c("n.lag1.lap",   "n.lag1.rap",    "n.lag1.allsrg", "n.lag1.nonLap"))
  lag2Vol.md.2=rename.vars(lag2Vol.md.1,c("n.lagk.lap",   "n.lagk.rap",    "n.lagk.allsrg", "n.lagk.nonLap"),c("n.lag2.lap",   "n.lag2.rap",    "n.lag2.allsrg", "n.lag2.nonLap"))
  lag3Vol.md.2=rename.vars(lag3Vol.md.1,c("n.lagk.lap",   "n.lagk.rap",    "n.lagk.allsrg", "n.lagk.nonLap"),c("n.lag3.lap",   "n.lag3.rap",    "n.lag3.allsrg", "n.lag3.nonLap"))
  lag4Vol.md.2=rename.vars(lag4Vol.md.1,c("n.lagk.lap",   "n.lagk.rap",    "n.lagk.allsrg", "n.lagk.nonLap"),c("n.lag4.lap",   "n.lag4.rap",    "n.lag4.allsrg", "n.lag4.nonLap"))
  lag5Vol.md.2=rename.vars(lag5Vol.md.1,c("n.lagk.lap",   "n.lagk.rap",    "n.lagk.allsrg", "n.lagk.nonLap"),c("n.lag5.lap",   "n.lag5.rap",    "n.lag5.allsrg", "n.lag5.nonLap"))
  lag6Vol.md.2=rename.vars(lag6Vol.md.1,c("n.lagk.lap",   "n.lagk.rap",    "n.lagk.allsrg", "n.lagk.nonLap"),c("n.lag6.lap",   "n.lag6.rap",    "n.lag6.allsrg", "n.lag6.nonLap")) 
  
  lag1Vol.ny.2=rename.vars(lag1Vol.ny.1,c("n.lagk.lap",   "n.lagk.rap",    "n.lagk.allsrg", "n.lagk.nonLap"),c("n.lag1.lap",   "n.lag1.rap",    "n.lag1.allsrg", "n.lag1.nonLap"))
  lag2Vol.ny.2=rename.vars(lag2Vol.ny.1,c("n.lagk.lap",   "n.lagk.rap",    "n.lagk.allsrg", "n.lagk.nonLap"),c("n.lag2.lap",   "n.lag2.rap",    "n.lag2.allsrg", "n.lag2.nonLap"))
  lag3Vol.ny.2=rename.vars(lag3Vol.ny.1,c("n.lagk.lap",   "n.lagk.rap",    "n.lagk.allsrg", "n.lagk.nonLap"),c("n.lag3.lap",   "n.lag3.rap",    "n.lag3.allsrg", "n.lag3.nonLap"))
  lag4Vol.ny.2=rename.vars(lag4Vol.ny.1,c("n.lagk.lap",   "n.lagk.rap",    "n.lagk.allsrg", "n.lagk.nonLap"),c("n.lag4.lap",   "n.lag4.rap",    "n.lag4.allsrg", "n.lag4.nonLap"))
  lag5Vol.ny.2=rename.vars(lag5Vol.ny.1,c("n.lagk.lap",   "n.lagk.rap",    "n.lagk.allsrg", "n.lagk.nonLap"),c("n.lag5.lap",   "n.lag5.rap",    "n.lag5.allsrg", "n.lag5.nonLap"))
  lag6Vol.ny.2=rename.vars(lag6Vol.ny.1,c("n.lagk.lap",   "n.lagk.rap",    "n.lagk.allsrg", "n.lagk.nonLap"),c("n.lag6.lap",   "n.lag6.rap",    "n.lag6.allsrg", "n.lag6.nonLap"))
  
  #use 6821 observation in demoRaw.1
  
 
  
  
  
#   > count(subset( demoRaw, !is.na(dqtr) & hospst=="MD" & year=='2004'),'dqtr')
#   dqtr freq
#   1    1   20
#   2    2   16
#   3    3   50
#   4    4   46
#   > count(subset( demoRaw, !is.na(dqtr) & hospst=="MD" & year=='2009'),'dqtr')
#   dqtr freq
#   1    3   36
#   2    4   44
  
  
  withVol= joinReduce.yz(demoRaw.1
                         , do.call(rbind,list(lag1Vol.ny.2,lag1Vol.ia.2,lag1Vol.md.2))
                         , do.call(rbind,list(lag2Vol.ny.2,lag2Vol.ia.2,lag2Vol.md.2))
                         , do.call(rbind,list(lag3Vol.ny.2,lag3Vol.ia.2,lag3Vol.md.2))
                         , do.call(rbind,list(lag4Vol.ny.2,lag4Vol.ia.2,lag4Vol.md.2))
                         , do.call(rbind,list(lag5Vol.ny.2,lag5Vol.ia.2,lag5Vol.md.2))
                         , do.call(rbind,list(lag6Vol.ny.2,lag6Vol.ia.2,lag6Vol.md.2))
                         , keysList=list(c('key','hospst'),c('key','hospst'),c('key','hospst'),c('key','hospst') ,c('key','hospst'),c('key','hospst'))
                         , typeVec=rep('left',6), matchVec=rep('first',6)
  )
  
  #next I need to add los variable
  keyLos.ia = xpt2r.yz("Z:/j_scrdata/lapLearn",'lap_ia_20032010')[c('key','hospst','los')]
  keyLos.ny = xpt2r.yz("Z:/j_scrdata/lapLearn",'lap_ny_20032010')[c('key','hospst','los')]
  keyLos.md = xpt2r.yz("Z:/j_scrdata/lapLearn",'lap_md_20032010')[c('key','hospst','los')]
  
  losDf=subset(rbind(keyLos.ia,keyLos.ny,keyLos.md), los>0)
  
  nrow(losDf) #6926
  
  comorb = xpt2r.yz('Z:/j_scrdata/lapLearn','comorb_iamymd')
  comorb[,'key'] = as.character(comorb[,'key'])
  
  comorb[,'comorbSum'] = rowSums(comorb[,c('chf','valve','pulmcirc','perivasc','para','neuro','chrnlung','dm','dmcx','hypothy','renlfail','liver','ulcer','lymph','mets','tumor','arth','coag','obese','wghtloss','lytes','bldloss','anemdef','alcohol','drug','psych','depress','htn.c')])
  #no missing in comorb
  #   count(comorb,'comorbSum')
  #   comorbSum freq
  #   1         0 2522
  #   2         1 2703
  #   3         2 1236
  #   4         3  367
  #   5         4   85
  #   6         5   11
  #   7         6    2
  #   8         7    1
  #   9         8    1
  
  anaDf=join(join(withVol,losDf,by=c('key','hospst')),comorb, by=c('key','hospst')) #key is not unique across states
  anaDf[,'zip3or5'] =NA
  anaDf[which(!is.na(anaDf[,'zip3'])),'zip3or5'] = anaDf[which(!is.na(anaDf[,'zip3'])),'zip3']
  anaDf[which(is.na(anaDf[,'zip3'])),'zip3or5'] = anaDf[which(is.na(anaDf[,'zip3'])),'zip'] 
  anaDf[,'zip3cleaned'] = substr(anaDf[,'zip3or5'],1,3)
  anaDf[which(anaDf[,'zip3cleaned']=='F'),'zip3cleaned'] = NA
  useVars = c(
    'key'
    ,'dqtr'
    ,'mdId'
    ,'mdnum1.r' #this is the attending physiican id (99% percent change attending physician id are the same as secondary/operation physician)
    ,'los'
    ,"n.lag1.lap",   "n.lag1.rap",    "n.lag1.allsrg", "n.lag1.nonLap"
    ,"n.lag2.lap",   "n.lag2.rap",    "n.lag2.allsrg", "n.lag2.nonLap"
    ,"n.lag3.lap",   "n.lag3.rap",    "n.lag3.allsrg", "n.lag3.nonLap"
    ,"n.lag4.lap",   "n.lag4.rap",    "n.lag4.allsrg", "n.lag4.nonLap"
    ,"n.lag5.lap",   "n.lag5.rap",    "n.lag5.allsrg", "n.lag5.nonLap"
    ,"n.lag6.lap",   "n.lag6.rap",    "n.lag6.allsrg", "n.lag6.nonLap"
    
    ,'age','race','chf','valve', 'pulmcirc', 'perivasc', 'para', 'neuro', 'chrnlung', 'dm', 'dmcx', 'hypothy', 'renlfail', 'liver', 'ulcer', 'lymph', 'mets', 'tumor', 'arth', 'coag', 'obese', 'wghtloss', 'lytes', 'bldloss', 'anemdef', 'alcohol', 'drug', 'psych', 'depress', 'htn.c','year','hospst','pstco','comorbSum','pay1')

  
  anaDf.1toMaxLos=subset(anaDf,los<=maxLos & los>0, select=useVars)
  
  
  anaDf.1toMaxLos[,'n.lag6.lap.log']=log1p(anaDf.1toMaxLos[,'n.lag6.lap'])
  anaDf.1toMaxLos[,'n.lag5.lap.log']=log1p(anaDf.1toMaxLos[,'n.lag5.lap'])
  anaDf.1toMaxLos[,'n.lag4.lap.log']=log1p(anaDf.1toMaxLos[,'n.lag4.lap'])
  anaDf.1toMaxLos[,'n.lag3.lap.log']=log1p(anaDf.1toMaxLos[,'n.lag3.lap'])
  anaDf.1toMaxLos[,'n.lag2.lap.log']=log1p(anaDf.1toMaxLos[,'n.lag2.lap'])
  anaDf.1toMaxLos[,'n.lag1.lap.log']=log1p(anaDf.1toMaxLos[,'n.lag1.lap'])
  
  anaDf.1toMaxLos[,'n.lag6.lap.log.sq']=(log1p(anaDf.1toMaxLos[,'n.lag6.lap']))^2
  anaDf.1toMaxLos[,'n.lag5.lap.log.sq']=(log1p(anaDf.1toMaxLos[,'n.lag5.lap']))^2
  anaDf.1toMaxLos[,'n.lag4.lap.log.sq']=(log1p(anaDf.1toMaxLos[,'n.lag4.lap']))^2
  anaDf.1toMaxLos[,'n.lag3.lap.log.sq']=(log1p(anaDf.1toMaxLos[,'n.lag3.lap']))^2
  anaDf.1toMaxLos[,'n.lag2.lap.log.sq']=(log1p(anaDf.1toMaxLos[,'n.lag2.lap']))^2
  anaDf.1toMaxLos[,'n.lag1.lap.log.sq']=(log1p(anaDf.1toMaxLos[,'n.lag1.lap']))^2
  
  
  #pay1=5 is no charge, only 1 patient is like this. so delete this observation'
  
  anaDf.1toMaxLos=subset(anaDf.1toMaxLos, !pay1=='5')
  
  return(anaDf.1toMaxLos)
}




#used for data cleaning
cubic.bs.tmp=function(x,knots,vnStem,degree, boundary.knots){
  if (missing(boundary.knots))
  {outD=as.data.frame(splines::bs(x, knots=knots,degree=degree))} else{
    outD=as.data.frame(splines::bs(x, knots=knots,degree=degree,Boundary.knots=boundary.knots))
  }
  names(outD)=paste(vnStem,seq(ncol(outD)),sep='')
  return(outD)
}



finalCleaning=function(anaDf.raw
                       , log.lag1.inner.knots
                       ,bsDegree){ 
  
  completeCase.vec=complete.cases(anaDf.raw[,c('age','race')])
  cat('dropped (due to missing age or race) ', length(completeCase.vec)-sum(completeCase.vec),'\n')
  anaDf.raw=anaDf.raw[completeCase.vec,]
  (losDensity=density.yz(anaDf.raw[,'los']))
  print(losDensity)
  
  #we can use bbase.yz to get the spline design matrix
  
  #fine  the range for all the lags.
  #then feed bbase.yz
  # bs.percentils
  
  
#   knots.lag1 = quantile(anaDf.raw[,'n.lag1.lap.log'],prob=bs.percentiles,na.rm=T) 
#   boundary.knots.lag1=range(anaDf.raw[,'n.lag1.lap.log'],na.rm=T)
#   anaDf.raw = cbind(anaDf.raw, cubic.bs.tmp(anaDf.raw[,'n.lag1.lap.log'],knots.lag1,'lag1.nlap.bs',degree))
#  
  

  bslist=bsWrapper.yz(anaDf.raw[,'n.lag1.lap.log'] #this is the x in bs function
                        , log.lag1.inner.knots #inner knots, if it contains boundary knots, function will stop and issue erro message
                        , 'n.lag1.lap.log.bs' #output data's columen name stem
                        , degree=bsDegree
                        , dataType='data.frame'
                        #or ='data.frame
  )
  
  anaDf.raw=cbind(anaDf.raw,bslist$bsdata)
  
  boundary.knots.lag1=bslist$boundary.knots
  n.basis=bslist$n.basis
#   
#   knots.lag2 = quantile(anaDf.raw[,'n.lag2.lap.log'],prob=bs.percentiles,na.rm=T)
#   boundary.knots.lag2=range(anaDf.raw[,'n.lag2.lap.log'],na.rm=T)
#   anaDf.raw = cbind(anaDf.raw, cubic.bs.tmp(anaDf.raw[,'n.lag2.lap.log'],knots.lag2,'lag2.nlap.bs',degree))
#   
#   knots.lag3 = quantile(anaDf.raw[,'n.lag3.lap.log'],prob=bs.percentiles,na.rm=T) 
#   boundary.knots.lag3=range(anaDf.raw[,'n.lag3.lap.log'],na.rm=T)
#   anaDf.raw = cbind(anaDf.raw, cubic.bs.tmp(anaDf.raw[,'n.lag3.lap.log'],knots.lag3,'lag3.nlap.bs',degree))
#   
#   knots.lag4 = quantile(anaDf.raw[,'n.lag4.lap.log'],prob=bs.percentiles,na.rm=T)
#   boundary.knots.lag4=range(anaDf.raw[,'n.lag4.lap.log'],na.rm=T)
#   anaDf.raw = cbind(anaDf.raw, cubic.bs.tmp(anaDf.raw[,'n.lag4.lap.log'],knots.lag4,'lag4.nlap.bs',degree))
#   
#   knots.lag5 = quantile(anaDf.raw[,'n.lag5.lap.log'],prob=bs.percentiles,na.rm=T) 
#   boundary.knots.lag5=range(anaDf.raw[,'n.lag5.lap.log'],na.rm=T)
#   anaDf.raw = cbind(anaDf.raw, cubic.bs.tmp(anaDf.raw[,'n.lag5.lap.log'],knots.lag5,'lag5.nlap.bs',degree))
#   
#   knots.lag6 = quantile(anaDf.raw[,'n.lag6.lap.log'],prob=bs.percentiles,na.rm=T) 
#   boundary.knots.lag6=range(anaDf.raw[,'n.lag6.lap.log'],na.rm=T)
#   anaDf.raw = cbind(anaDf.raw, cubic.bs.tmp(anaDf.raw[,'n.lag6.lap.log'],knots.lag6,'lag6.nlap.bs',degree))
#   
  inner.knots.list=list(inner.knots.lag1=log.lag1.inner.knots
#                   , knots.lag2=knots.lag2, knots.lag3=knots.lag3, knots.lag4=knots.lag4, knots.lag5=knots.lag5, knots.lag6=knots.lag6
                  
                  )
  boundary.knots.list=list(boundary.knots.lag1=boundary.knots.lag1
#                     , boundary.knots.lag2=boundary.knots.lag2, boundary.knots.lag3=boundary.knots.lag3, boundary.knots.lag4=boundary.knots.lag4, boundary.knots.lag5=boundary.knots.lag5, boundary.knots.lag6=boundary.knots.lag6
                    )
#   names(knots.list)=paste('lag',seq(6),sep='')
#   names(boundary.knots.list)=paste('lag',seq(6),sep='')
  names(inner.knots.list)=paste('lag',seq(1),sep='')
  names(boundary.knots.list)=paste('lag',seq(1),sep='')
  
  #   knots.lag1 = quantile(anaDf.raw[,'n.lag1.lap'],prob=bs.percentiles,na.rm=T) 
  #   boundary.knots.lag1=range(anaDf.raw[,'n.lag1.lap'],na.rm=T)
  #   anaDf.raw = cbind(anaDf.raw, cubic.bs.tmp(anaDf.raw[,'n.lag1.lap'],knots.lag1,'lag1.nlap.bs',degree))
  #      
  #    
  #   knots.lag2 = quantile(anaDf.raw[,'n.lag2.lap'],prob=bs.percentiles,na.rm=T)
  #   boundary.knots.lag2=range(anaDf.raw[,'n.lag2.lap'],na.rm=T)
  #   anaDf.raw = cbind(anaDf.raw, cubic.bs.tmp(anaDf.raw[,'n.lag2.lap'],knots.lag2,'lag2.nlap.bs',degree))
  #   
  #   knots.lag3 = quantile(anaDf.raw[,'n.lag3.lap'],prob=bs.percentiles,na.rm=T) 
  #   boundary.knots.lag3=range(anaDf.raw[,'n.lag3.lap'],na.rm=T)
  #   anaDf.raw = cbind(anaDf.raw, cubic.bs.tmp(anaDf.raw[,'n.lag3.lap'],knots.lag3,'lag3.nlap.bs',degree))
  #   
  #   knots.lag4 = quantile(anaDf.raw[,'n.lag4.lap'],prob=bs.percentiles,na.rm=T)
  #    boundary.knots.lag4=range(anaDf.raw[,'n.lag4.lap'],na.rm=T)
  #   anaDf.raw = cbind(anaDf.raw, cubic.bs.tmp(anaDf.raw[,'n.lag4.lap'],knots.lag4,'lag4.nlap.bs',degree))
  #   
  #   knots.lag5 = quantile(anaDf.raw[,'n.lag5.lap'],prob=bs.percentiles,na.rm=T) 
  #    boundary.knots.lag5=range(anaDf.raw[,'n.lag5.lap'],na.rm=T)
  #   anaDf.raw = cbind(anaDf.raw, cubic.bs.tmp(anaDf.raw[,'n.lag5.lap'],knots.lag5,'lag5.nlap.bs',degree))
  #   
  #   knots.lag6 = quantile(anaDf.raw[,'n.lag6.lap'],prob=bs.percentiles,na.rm=T) 
  #   boundary.knots.lag6=range(anaDf.raw[,'n.lag6.lap'],na.rm=T)
  #   anaDf.raw = cbind(anaDf.raw, cubic.bs.tmp(anaDf.raw[,'n.lag6.lap'],knots.lag6,'lag6.nlap.bs',degree))
  #   
  #   knots.list=list(knots.lag1=knots.lag1, knots.lag2=knots.lag2, knots.lag3=knots.lag3, knots.lag4=knots.lag4, knots.lag5=knots.lag5, knots.lag6=knots.lag6)
  #    boundary.knots.list=list(boundary.knots.lag1=boundary.knots.lag1, boundary.knots.lag2=boundary.knots.lag2, boundary.knots.lag3=boundary.knots.lag3, boundary.knots.lag4=boundary.knots.lag4, boundary.knots.lag5=boundary.knots.lag5, boundary.knots.lag6=boundary.knots.lag6)
  #   names(knots.list)=paste('lag',seq(6),sep='')
  #   names(boundary.knots.list)=paste('lag',seq(6),sep='')
  
#   
#   
#   anaDf.raw[,'n.lag6.lap.logsq']=log1p(anaDf.raw[,'n.lag6.lap'])^2
#   anaDf.raw[,'n.lag5.lap.logsq']=log1p(anaDf.raw[,'n.lag5.lap'])^2
#   anaDf.raw[,'n.lag4.lap.logsq']=log1p(anaDf.raw[,'n.lag4.lap'])^2
#   anaDf.raw[,'n.lag3.lap.logsq']=log1p(anaDf.raw[,'n.lag3.lap'])^2
#   anaDf.raw[,'n.lag2.lap.logsq']=log1p(anaDf.raw[,'n.lag2.lap'])^2
#   anaDf.raw[,'n.lag1.lap.logsq']=log1p(anaDf.raw[,'n.lag1.lap'])^2
#   
#   anaDf.raw[,'n.lag6.lap.logcu']=log1p(anaDf.raw[,'n.lag6.lap'])^3
#   anaDf.raw[,'n.lag5.lap.logcu']=log1p(anaDf.raw[,'n.lag5.lap'])^3
#   anaDf.raw[,'n.lag4.lap.logcu']=log1p(anaDf.raw[,'n.lag4.lap'])^3
#   anaDf.raw[,'n.lag3.lap.logcu']=log1p(anaDf.raw[,'n.lag3.lap'])^3
#   anaDf.raw[,'n.lag2.lap.logcu']=log1p(anaDf.raw[,'n.lag2.lap'])^3
#   anaDf.raw[,'n.lag1.lap.logcu']=log1p(anaDf.raw[,'n.lag1.lap'])^3
#   
#   anaDf.raw[,'n.lag6.lap.1f']=anaDf.raw[,'n.lag6.lap']^(1/4)
#   anaDf.raw[,'n.lag5.lap.1f']=anaDf.raw[,'n.lag5.lap']^(1/4)
#   anaDf.raw[,'n.lag4.lap.1f']=anaDf.raw[,'n.lag4.lap']^(1/4)
#   anaDf.raw[,'n.lag3.lap.1f']=anaDf.raw[,'n.lag3.lap']^(1/4)
#   anaDf.raw[,'n.lag2.lap.1f']=anaDf.raw[,'n.lag2.lap']^(1/4)
#   anaDf.raw[,'n.lag1.lap.1f']=anaDf.raw[,'n.lag1.lap']^(1/4)
#   
#   anaDf.raw[,'n.lag6.lap.1t']=anaDf.raw[,'n.lag6.lap']^(1/3)
#   anaDf.raw[,'n.lag5.lap.1t']=anaDf.raw[,'n.lag5.lap']^(1/3)
#   anaDf.raw[,'n.lag4.lap.1t']=anaDf.raw[,'n.lag4.lap']^(1/3)
#   anaDf.raw[,'n.lag3.lap.1t']=anaDf.raw[,'n.lag3.lap']^(1/3)
#   anaDf.raw[,'n.lag2.lap.1t']=anaDf.raw[,'n.lag2.lap']^(1/3)
#   anaDf.raw[,'n.lag1.lap.1t']=anaDf.raw[,'n.lag1.lap']^(1/3)
#   
#   anaDf.raw[,'n.lag6.lap.2t']=anaDf.raw[,'n.lag6.lap']^(2/3)
#   anaDf.raw[,'n.lag5.lap.2t']=anaDf.raw[,'n.lag5.lap']^(2/3)
#   anaDf.raw[,'n.lag4.lap.2t']=anaDf.raw[,'n.lag4.lap']^(3/3)
#   anaDf.raw[,'n.lag3.lap.2t']=anaDf.raw[,'n.lag3.lap']^(2/3)
#   anaDf.raw[,'n.lag2.lap.2t']=anaDf.raw[,'n.lag2.lap']^(2/3)
#   anaDf.raw[,'n.lag1.lap.2t']=anaDf.raw[,'n.lag1.lap']^(2/3)
#   
#   
#   anaDf.raw[,'n.lag6.lap.sqrt']=anaDf.raw[,'n.lag6.lap']^(0.5)
#   anaDf.raw[,'n.lag5.lap.sqrt']=anaDf.raw[,'n.lag5.lap']^(0.5)
#   anaDf.raw[,'n.lag4.lap.sqrt']=anaDf.raw[,'n.lag4.lap']^(0.5)
#   anaDf.raw[,'n.lag3.lap.sqrt']=anaDf.raw[,'n.lag3.lap']^(0.5)
#   anaDf.raw[,'n.lag2.lap.sqrt']=anaDf.raw[,'n.lag2.lap']^(0.5)
#   anaDf.raw[,'n.lag1.lap.sqrt']=anaDf.raw[,'n.lag1.lap']^(0.5)
#   
#   
#   anaDf.raw[,'n.lag6.lap.1h']=anaDf.raw[,'n.lag6.lap']^(1.5)
#   anaDf.raw[,'n.lag5.lap.1h']=anaDf.raw[,'n.lag5.lap']^(1.5)
#   anaDf.raw[,'n.lag4.lap.1h']=anaDf.raw[,'n.lag4.lap']^(1.5)
#   anaDf.raw[,'n.lag3.lap.1h']=anaDf.raw[,'n.lag3.lap']^(1.5)
#   anaDf.raw[,'n.lag2.lap.1h']=anaDf.raw[,'n.lag2.lap']^(1.5)
#   anaDf.raw[,'n.lag1.lap.1h']=anaDf.raw[,'n.lag1.lap']^(1.5)
#   
#   
#   anaDf.raw[,'n.lag6.lap.sq']=anaDf.raw[,'n.lag6.lap']^2
#   anaDf.raw[,'n.lag5.lap.sq']=anaDf.raw[,'n.lag5.lap']^2
#   anaDf.raw[,'n.lag4.lap.sq']=anaDf.raw[,'n.lag4.lap']^2
#   anaDf.raw[,'n.lag3.lap.sq']=anaDf.raw[,'n.lag3.lap']^2
#   anaDf.raw[,'n.lag2.lap.sq']=anaDf.raw[,'n.lag2.lap']^2
#   anaDf.raw[,'n.lag1.lap.sq']=anaDf.raw[,'n.lag1.lap']^2
#   
#   anaDf.raw[,'n.lag6.lap.cu']=anaDf.raw[,'n.lag6.lap']^3
#   anaDf.raw[,'n.lag5.lap.cu']=anaDf.raw[,'n.lag5.lap']^3
#   anaDf.raw[,'n.lag4.lap.cu']=anaDf.raw[,'n.lag4.lap']^3
#   anaDf.raw[,'n.lag3.lap.cu']=anaDf.raw[,'n.lag3.lap']^3
#   anaDf.raw[,'n.lag2.lap.cu']=anaDf.raw[,'n.lag2.lap']^3
#   anaDf.raw[,'n.lag1.lap.cu']=anaDf.raw[,'n.lag1.lap']^3
#   
#   anaDf.raw[,'n.lag6.lap.qu']=anaDf.raw[,'n.lag6.lap']^4
#   anaDf.raw[,'n.lag5.lap.qu']=anaDf.raw[,'n.lag5.lap']^4
#   anaDf.raw[,'n.lag4.lap.qu']=anaDf.raw[,'n.lag4.lap']^4
#   anaDf.raw[,'n.lag3.lap.qu']=anaDf.raw[,'n.lag3.lap']^4
#   anaDf.raw[,'n.lag2.lap.qu']=anaDf.raw[,'n.lag2.lap']^4
#   anaDf.raw[,'n.lag1.lap.qu']=anaDf.raw[,'n.lag1.lap']^4
#   
#   #---------------------------------
#   
#   
#   anaDf.raw[,'n.lag6.nonLap.sqrt']=anaDf.raw[,'n.lag6.nonLap']^(0.5)
#   anaDf.raw[,'n.lag5.nonLap.sqrt']=anaDf.raw[,'n.lag5.nonLap']^(0.5)
#   anaDf.raw[,'n.lag4.nonLap.sqrt']=anaDf.raw[,'n.lag4.nonLap']^(0.5)
#   anaDf.raw[,'n.lag3.nonLap.sqrt']=anaDf.raw[,'n.lag3.nonLap']^(0.5)
#   anaDf.raw[,'n.lag2.nonLap.sqrt']=anaDf.raw[,'n.lag2.nonLap']^(0.5)
#   anaDf.raw[,'n.lag1.nonLap.sqrt']=anaDf.raw[,'n.lag1.nonLap']^(0.5)
#   
#   anaDf.raw[,'n.lag6.nonLap.1h']=anaDf.raw[,'n.lag6.nonLap']^(1.5)
#   anaDf.raw[,'n.lag5.nonLap.1h']=anaDf.raw[,'n.lag5.nonLap']^(1.5)
#   anaDf.raw[,'n.lag4.nonLap.1h']=anaDf.raw[,'n.lag4.nonLap']^(1.5)
#   anaDf.raw[,'n.lag3.nonLap.1h']=anaDf.raw[,'n.lag3.nonLap']^(1.5)
#   anaDf.raw[,'n.lag2.nonLap.1h']=anaDf.raw[,'n.lag2.nonLap']^(1.5)
#   anaDf.raw[,'n.lag1.nonLap.1h']=anaDf.raw[,'n.lag1.nonLap']^(1.5)
#   
#   anaDf.raw[,'n.lag6.nonLap.sq']=anaDf.raw[,'n.lag6.nonLap']^2
#   anaDf.raw[,'n.lag5.nonLap.sq']=anaDf.raw[,'n.lag5.nonLap']^2
#   anaDf.raw[,'n.lag4.nonLap.sq']=anaDf.raw[,'n.lag4.nonLap']^2
#   anaDf.raw[,'n.lag3.nonLap.sq']=anaDf.raw[,'n.lag3.nonLap']^2
#   anaDf.raw[,'n.lag2.nonLap.sq']=anaDf.raw[,'n.lag2.nonLap']^2
#   anaDf.raw[,'n.lag1.nonLap.sq']=anaDf.raw[,'n.lag1.nonLap']^2
#   
#   anaDf.raw[,'n.lag6.nonLap.cu']=anaDf.raw[,'n.lag6.nonLap']^3
#   anaDf.raw[,'n.lag5.nonLap.cu']=anaDf.raw[,'n.lag5.nonLap']^3
#   anaDf.raw[,'n.lag4.nonLap.cu']=anaDf.raw[,'n.lag4.nonLap']^3
#   anaDf.raw[,'n.lag3.nonLap.cu']=anaDf.raw[,'n.lag3.nonLap']^3
#   anaDf.raw[,'n.lag2.nonLap.cu']=anaDf.raw[,'n.lag2.nonLap']^3
#   anaDf.raw[,'n.lag1.nonLap.cu']=anaDf.raw[,'n.lag1.nonLap']^3
#   
#   anaDf.raw[,'n.lag6.nonLap.qu']=anaDf.raw[,'n.lag6.nonLap']^4
#   anaDf.raw[,'n.lag5.nonLap.qu']=anaDf.raw[,'n.lag5.nonLap']^4
#   anaDf.raw[,'n.lag4.nonLap.qu']=anaDf.raw[,'n.lag4.nonLap']^4
#   anaDf.raw[,'n.lag3.nonLap.qu']=anaDf.raw[,'n.lag3.nonLap']^4
#   anaDf.raw[,'n.lag2.nonLap.qu']=anaDf.raw[,'n.lag2.nonLap']^4
#   anaDf.raw[,'n.lag1.nonLap.qu']=anaDf.raw[,'n.lag1.nonLap']^4
#   
#   #-------------------------------------
#   anaDf.raw[,'n.lag6.rap.log']=log1p(anaDf.raw[,'n.lag6.rap'])
#   anaDf.raw[,'n.lag5.rap.log']=log1p(anaDf.raw[,'n.lag5.rap'])
#   anaDf.raw[,'n.lag4.rap.log']=log1p(anaDf.raw[,'n.lag4.rap'])
#   anaDf.raw[,'n.lag3.rap.log']=log1p(anaDf.raw[,'n.lag3.rap'])
#   anaDf.raw[,'n.lag2.rap.log']=log1p(anaDf.raw[,'n.lag2.rap'])
#   anaDf.raw[,'n.lag1.rap.log']=log1p(anaDf.raw[,'n.lag1.rap'])
#   
#   anaDf.raw[,'n.lag6.rap.sqrt']=anaDf.raw[,'n.lag6.rap']^(0.5)
#   anaDf.raw[,'n.lag5.rap.sqrt']=anaDf.raw[,'n.lag5.rap']^(0.5)
#   anaDf.raw[,'n.lag4.rap.sqrt']=anaDf.raw[,'n.lag4.rap']^(0.5)
#   anaDf.raw[,'n.lag3.rap.sqrt']=anaDf.raw[,'n.lag3.rap']^(0.5)
#   anaDf.raw[,'n.lag2.rap.sqrt']=anaDf.raw[,'n.lag2.rap']^(0.5)
#   anaDf.raw[,'n.lag1.rap.sqrt']=anaDf.raw[,'n.lag1.rap']^(0.5)
#   
#   anaDf.raw[,'n.lag6.rap.1h']=anaDf.raw[,'n.lag6.rap']^(1.5)
#   anaDf.raw[,'n.lag5.rap.1h']=anaDf.raw[,'n.lag5.rap']^(1.5)
#   anaDf.raw[,'n.lag4.rap.1h']=anaDf.raw[,'n.lag4.rap']^(1.5)
#   anaDf.raw[,'n.lag3.rap.1h']=anaDf.raw[,'n.lag3.rap']^(1.5)
#   anaDf.raw[,'n.lag2.rap.1h']=anaDf.raw[,'n.lag2.rap']^(1.5)
#   anaDf.raw[,'n.lag1.rap.1h']=anaDf.raw[,'n.lag1.rap']^(1.5)
#   
#   anaDf.raw[,'n.lag6.rap.sq']=anaDf.raw[,'n.lag6.rap']^2
#   anaDf.raw[,'n.lag5.rap.sq']=anaDf.raw[,'n.lag5.rap']^2
#   anaDf.raw[,'n.lag4.rap.sq']=anaDf.raw[,'n.lag4.rap']^2
#   anaDf.raw[,'n.lag3.rap.sq']=anaDf.raw[,'n.lag3.rap']^2
#   anaDf.raw[,'n.lag2.rap.sq']=anaDf.raw[,'n.lag2.rap']^2
#   anaDf.raw[,'n.lag1.rap.sq']=anaDf.raw[,'n.lag1.rap']^2
#   
#   anaDf.raw[,'n.lag6.rap.cu']=anaDf.raw[,'n.lag6.rap']^3
#   anaDf.raw[,'n.lag5.rap.cu']=anaDf.raw[,'n.lag5.rap']^3
#   anaDf.raw[,'n.lag4.rap.cu']=anaDf.raw[,'n.lag4.rap']^3
#   anaDf.raw[,'n.lag3.rap.cu']=anaDf.raw[,'n.lag3.rap']^3
#   anaDf.raw[,'n.lag2.rap.cu']=anaDf.raw[,'n.lag2.rap']^3
#   anaDf.raw[,'n.lag1.rap.cu']=anaDf.raw[,'n.lag1.rap']^3
#   
#   anaDf.raw[,'n.lag6.rap.qu']=anaDf.raw[,'n.lag6.rap']^4
#   anaDf.raw[,'n.lag5.rap.qu']=anaDf.raw[,'n.lag5.rap']^4
#   anaDf.raw[,'n.lag4.rap.qu']=anaDf.raw[,'n.lag4.rap']^4
#   anaDf.raw[,'n.lag3.rap.qu']=anaDf.raw[,'n.lag3.rap']^4
#   anaDf.raw[,'n.lag2.rap.qu']=anaDf.raw[,'n.lag2.rap']^4
#   anaDf.raw[,'n.lag1.rap.qu']=anaDf.raw[,'n.lag1.rap']^4
#   
  #-----------------------------------------
  
  age3catcuts=quantile(anaDf.raw[,'age'],prob=c(0,0.333,0.666,1))
  anaDf.raw=grpnv.supplycuts.yz(anaDf.raw, 'age', age3catcuts, 'age3cat')
  
  age4catcuts=quantile(anaDf.raw[,'age'],prob=c(0,0.25,0.5,0.75,1))
  anaDf.raw=grpnv.supplycuts.yz(anaDf.raw, 'age', age4catcuts, 'age4cat')
  
  #comorbcuts=quantile(anaDf.raw[,'comorbSum'],prob=c(0,0.333,0.666,1))
  comorbcuts=c(0,1,2,3,Inf)
  anaDf.raw=grpnv.supplycuts.yz(anaDf.raw, 'comorbSum', comorbcuts, 'comorbcat')
  
  
  nonLapCuts.lag1= quantile(uniqueRows.yz(anaDf.raw[,c('n.lag1.nonLap','mdId')])[,'n.lag1.nonLap'],probs=seq(0,1,0.2),na.rm=T)
  
  anaDf.raw=grpnv.supplycuts.yz(anaDf.raw, 'n.lag1.nonLap', nonLapCuts.lag1, 'n.lag1.nonLap.5cat')
  
#   nonLapCuts.lag2= quantile(uniqueRows.yz(anaDf.raw[,c('n.lag2.nonLap','mdId')])[,'n.lag2.nonLap'],probs=seq(0,1,0.2),na.rm=T)
#   anaDf.raw=grpnv.supplycuts.yz(anaDf.raw, 'n.lag2.nonLap', nonLapCuts.lag2, 'n.lag2.nonLap.5cat')
#   
#   nonLapCuts.lag3= quantile(uniqueRows.yz(anaDf.raw[,c('n.lag3.nonLap','mdId')])[,'n.lag3.nonLap'],probs=seq(0,1,0.2),na.rm=T)
#   anaDf.raw=grpnv.supplycuts.yz(anaDf.raw, 'n.lag3.nonLap', nonLapCuts.lag3, 'n.lag3.nonLap.5cat')
#   
#   nonLapCuts.lag4= quantile(uniqueRows.yz(anaDf.raw[,c('n.lag4.nonLap','mdId')])[,'n.lag4.nonLap'],probs=seq(0,1,0.2),na.rm=T)
#   anaDf.raw=grpnv.supplycuts.yz(anaDf.raw, 'n.lag4.nonLap', nonLapCuts.lag4, 'n.lag4.nonLap.5cat')
#   
#   nonLapCuts.lag5= quantile(uniqueRows.yz(anaDf.raw[,c('n.lag5.nonLap','mdId')])[,'n.lag5.nonLap'],probs=seq(0,1,0.2),na.rm=T)
#   anaDf.raw=grpnv.supplycuts.yz(anaDf.raw, 'n.lag5.nonLap', nonLapCuts.lag5, 'n.lag5.nonLap.5cat')
#   
#   nonLapCuts.lag6= quantile(uniqueRows.yz(anaDf.raw[,c('n.lag6.nonLap','mdId')])[,'n.lag6.nonLap'],probs=seq(0,1,0.2),na.rm=T)
  
#   
#   anaDf.raw=grpnv.supplycuts.yz(anaDf.raw, 'n.lag6.nonLap', nonLapCuts.lag6, 'n.lag6.nonLap.5cat')
#   
  
  anaDf.raw[,'race5cat']=replaceValJoin.yz(anaDf.raw[,'race'] #this is typically a data column
                                           ,list(1,2,3,4,c(5,6)) 
                                           ,c(1,2,3,4,5)
                                           ,origValVn='race'
                                           ,newValVn='race5cat' #if output is vector, this newValVn is inapplicable
                                           ,outputJoinedDf=TRUE #if F means only output the new vector
                                           #if T, then output df             
  )[,'race5cat']
  
  
  #to run fixed effect, you have to delete physician who have only one patient record
  count(anaDf.raw,'mdnum1.r')
  
  anaDf.raw=intoFac.yz(anaDf.raw,c('race','race5cat','year','age3cat','age4cat','comorbcat','pay1','n.lag1.nonLap.5cat'
#,'n.lag2.nonLap.5cat','n.lag3.nonLap.5cat','n.lag4.nonLap.5cat','n.lag5.nonLap.5cat','n.lag6.nonLap.5cat'
                                   ,'hospst','mdnum1.r'))
  anaDf.raw=subset(anaDf.raw, los<=7)
  anaDf.raw=intoFac.yz(anaDf.raw,'los')
  #year 2003 will be missing if we use lag6, so after running this function, you need to droplevels for year
  
  outlist=list(inner.knots=inner.knots.list,boundary.knots=boundary.knots.list,n.basis=n.basis, df=anaDf.raw)
  
  return(outlist)
}


#use to generate spliine basis spline function in post estateimation.

gen.bs=function(lagvol.vec
                , knots
                , boundary.knots
                ,degree
                , vnStem #e.g., 'lag6.nlap.bs'   
){
  outdf=cubic.bs.tmp(lagvol.vec,knots,vnStem,degree,boundary.knots)
  return(outdf)
}

#the following is post-estimation functions

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

genSteadyState.Glist=function(g.list){
  g.list.firstElemIs0= lapply(g.list,function(x){x.1=x[,2]; x.1[1]=0;return(x.1)})
  volume.vec=seq(length(g.list.firstElemIs0[[1]]))-1
  G.list=lapply(g.list.firstElemIs0, function(x){volume.vec*x})
  return(G.list)
}
getMdData=function(mdVec){
  mdList=list()
  for (i in 1:length(mdVec)){
    mdList=lappend.yz(mdList,lastWithin.yz(subset(nomiss.lag6, mdnum1.r==mdVec[i],select=c('mdnum1.r','n.lag4.lap.log','n.lag4.lap.logsq','n.lag4.rap','n.lag4.nonLap.5cat')),'mdnum1.r'))
  }
  outmdDf=do.call(rbind,mdList)
  return(outmdDf)
}
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


pointEsLosDrop=function(interestState
                        , interestYear
                        , G.list
                        , mdDf
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
