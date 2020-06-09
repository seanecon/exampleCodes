#the analysis is the HRR level.
libVec=c('ascMMA')
pathVec=c('Z:/j_scrdata/ascMMA')
libPathDf=data.frame(lib=libVec, path=pathVec)

#next, get denominator file var

asciiDataVn.yz("Z:/j_scrdata/ascMMA/denVarsHrr_2006.csv")
asciiDataVn.yz("Z:/j_scrdata/ascMMA/denVarsHrr_2004.csv")

asciiDataVn.yz("Z:/j_scrdata/ascMMA/denVarsHrr_2008.csv")
asciiDataVn.yz("Z:/j_scrdata/ascMMA/denVarsHrr_2012.csv")

denAggByHrr=function(denDataPath,year){
  indf = readTableCols.yz(
    denDataPath
    #"Z:/j_scrdata/ascMMA/denVarsHrr_2006.csv"
    , c("AGE", "RACE","SEX","hrrnum")
  )
  # asciiDataVn.yz("Z:/j_scrdata/ascMMA/denVarsHrr_2006.csv")
  names(indf)=tolower(names(indf))
  indf[,'age']=as.numeric(indf[,'age'])
  
  okRows=rowsWithInterestValues.yz(indf
                                   ,c('sex')
                                   #there is k vns then the interestValuesList has a length of k
                                   , list(c(0))
  )$rowsWithoutInterestValues
  
  aggHrrList = aggregateVarsBy.yz( indf[okRows,]
                                   ,'hrrnum' #by variable
                                   ,c("race", "sex")
                                   ,'percent'
                                   #either 'freq' or 'percent'
                                   ,'age' #these are numerical vars, will generate mean.varname, sd.varname, '.' is the vnSep
                                   #meanSdVars can be empty
                                   ,
                                   #these are numerical vars, will generate median.varname
                                   #medianVars can be empty
                                   ,tabNA=TRUE
                                   ,na.rm.quantile=TRUE
  )
  
  denVarsDf = data.frame(join(aggHrrList[[1]],aggHrrList[[2]],by='hrrnum'),year=year)
  return(denVarsDf)
}

indf = readTableCols.yz(
  "Z:/j_scrdata/ascMMA/denVarsHrr_2010.csv"
  #"Z:/j_scrdata/ascMMA/denVarsHrr_2006.csv"
  , c("AGE", "RACE","SEX","hrrnum")
)
count(indf,'SEX')


denHrrLevel.2004 = denAggByHrr("Z:/j_scrdata/ascMMA/denVarsHrr_2004.csv",2004)
denHrrLevel.2005 = denAggByHrr("Z:/j_scrdata/ascMMA/denVarsHrr_2005.csv",2005)
denHrrLevel.2012 = denAggByHrr("Z:/j_scrdata/ascMMA/denVarsHrr_2012.csv",2012)

denHrrLevel.2006 = denAggByHrr("Z:/j_scrdata/ascMMA/denVarsHrr_2006.csv",2006)
denHrrLevel.2007 = denAggByHrr("Z:/j_scrdata/ascMMA/denVarsHrr_2007.csv",2007)
denHrrLevel.2008 = denAggByHrr("Z:/j_scrdata/ascMMA/denVarsHrr_2008.csv",2008)
denHrrLevel.2009 = denAggByHrr("Z:/j_scrdata/ascMMA/denVarsHrr_2009.csv",2009)
denHrrLevel.2010 = denAggByHrr("Z:/j_scrdata/ascMMA/denVarsHrr_2010.csv",2010)
denHrrLevel.2011 = denAggByHrr("Z:/j_scrdata/ascMMA/denVarsHrr_2011.csv",2011)


denHrr.20042012 = do.call(rbind,list(denHrrLevel.2004,denHrrLevel.2005,denHrr.20062011,denHrrLevel.2012))

(load(file= "Z:/j_scrdata/ascMMA/result/denHrr.20062011.RData"))
denHrr.20042012=do.call(rbind,list(denHrrLevel.2004,denHrrLevel.2005,denHrrLevel.2006,denHrrLevel.2007,denHrrLevel.2008,denHrrLevel.2009,denHrrLevel.2010,denHrrLevel.2011,denHrrLevel.2012))
denHrr.20042012=do.call(rbind,list(denHrrLevel.2004,denHrrLevel.2005,denHrr.20062011))
#save(denHrr.20042012,file= "Z:/j_scrdata/ascMMA/result/denHrr.20042012.RData")
#denHrr.20062011=do.call(rbind,list(denHrrLevel.2006,denHrrLevel.2007,denHrrLevel.2008,denHrrLevel.2009,denHrrLevel.2010,denHrrLevel.2011))
load(file= "Z:/j_scrdata/ascMMA/result/denHrr.20042012.RData")
#comorbidity 
hrrMeanComorb=xpt2r.yz("Z:/j_scrdata/ascMMA/",'ran_meanComorbHrr_2010')[,c('hrrnum','.avg.comorbsum')]

hrrMeanComorb.2004=data.frame(hrrMeanComorb,year=2004)
hrrMeanComorb.2005=data.frame(hrrMeanComorb,year=2005)
hrrMeanComorb.2012=data.frame(hrrMeanComorb,year=2012)


hrrMeanComorb.2006=data.frame(hrrMeanComorb,year=2006)
hrrMeanComorb.2007=data.frame(hrrMeanComorb,year=2007)
hrrMeanComorb.2008=data.frame(hrrMeanComorb,year=2008)
hrrMeanComorb.2009=data.frame(hrrMeanComorb,year=2009)
hrrMeanComorb.2010=data.frame(hrrMeanComorb,year=2010)
hrrMeanComorb.2011=data.frame(hrrMeanComorb,year=2011)


#hrrMeanComorb.20062011=do.call(rbind,list(hrrMeanComorb.2006,hrrMeanComorb.2007,hrrMeanComorb.2008,hrrMeanComorb.2009, hrrMeanComorb.2010,hrrMeanComorb.2011))



hrrMeanComorb.20042012=do.call(rbind,list(hrrMeanComorb.2004,hrrMeanComorb.2005, hrrMeanComorb.2006,hrrMeanComorb.2007,hrrMeanComorb.2008,hrrMeanComorb.2009, hrrMeanComorb.2010,hrrMeanComorb.2011,hrrMeanComorb.2012))

#indepVarDf.20062011=join(posVarsDf.20062011,join(denHrr.20062011,hrrMeanComorb.20062011))
#no need for pos file anymore

# indepVarDf.20062011=join(denHrr.20062011,hrrMeanComorb.20062011)
# save(denHrr.20062011,file= "Z:/j_scrdata/ascMMA/result/denHrr.20062011.RData")

indepVarDf.20042012=join(denHrr.20042012,hrrMeanComorb.20042012)

load(file='Z:/j_scrdata/ascMMA/result/indepVarDf.20042012.RData')

#save(indepVarDf.20042012,file='Z:/j_scrdata/ascMMA/result/indepVarDf.20042012.RData')
#save(indepVarDf.20062011,file= "Z:/j_scrdata/ascMMA/result/indepVarDf.20062011.RData")
#(load(file= "Z:/j_scrdata/ascMMA/result/indepVarDf.20062011.RData"))
