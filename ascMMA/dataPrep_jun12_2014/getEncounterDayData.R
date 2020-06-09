

encounterDay2004=xpt2r.yz("Z:/j_scrdata/ascMMA/",'fascOfOpVol_2004')
encounterDay2005=xpt2r.yz("Z:/j_scrdata/ascMMA/",'fascOfOpVol_2005')
encounterDay2012=xpt2r.yz("Z:/j_scrdata/ascMMA/",'fascOfOpVol_2012')

encounterDay2006=xpt2r.yz("Z:/j_scrdata/ascMMA/",'fascOfOpVol_2006')
encounterDay2007=xpt2r.yz("Z:/j_scrdata/ascMMA/",'fascOfOpVol_2007')
encounterDay2008=xpt2r.yz("Z:/j_scrdata/ascMMA/",'fascOfOpVol_2008')
encounterDay2009=xpt2r.yz("Z:/j_scrdata/ascMMA/",'fascOfOpVol_2009')
encounterDay2010=xpt2r.yz("Z:/j_scrdata/ascMMA/",'fascOfOpVol_2010')
encounterDay2011=xpt2r.yz("Z:/j_scrdata/ascMMA/",'fascOfOpVol_2011')

names(encounterDay2012)
names(encounterDay2006)

encounterDayDf.20062011 = do.call(rbind,list(encounterDay2006,encounterDay2007,encounterDay2008,encounterDay2009,encounterDay2010,encounterDay2011))

encounterDayDf.20042012 = do.call(rbind,list(encounterDay2004,encounterDay2005, encounterDay2006,encounterDay2007,encounterDay2008,encounterDay2009,encounterDay2010,encounterDay2011,encounterDay2012))


#save(encounterDayDf.20042012,file= "Z:/j_scrdata/ascMMA/result/encounterDayDf.20042012.RData")
(load(file= "Z:/j_scrdata/ascMMA/result/encounterDayDf.20042012.RData"))
#save(encounterDayDf.20062011,file= "Z:/j_scrdata/ascMMA/result/encounterDayDf.20062011.RData")
#(load(file= "Z:/j_scrdata/ascMMA/result/encounterDayVarDf.20062011.RData"))