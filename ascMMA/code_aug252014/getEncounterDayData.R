

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


#study total popluation by quarter


#save(encounterDayDf.20042012,file= "Z:/j_scrdata/ascMMA/result/encounterDayDf.20042012.RData")
(load(file= "Z:/j_scrdata/ascMMA/result/encounterDayDf.20042012.RData"))
#save(encounterDayDf.20062011,file= "Z:/j_scrdata/ascMMA/result/encounterDayDf.20062011.RData")
#(load(file= "Z:/j_scrdata/ascMMA/result/encounterDayVarDf.20062011.RData"))




out=ddply(encounterDayDf.20042012,c('.qtr.since2006'),function(x){sum(x[,'n.bene'])})

rownames(out)=NULL

ddply(encounterDayDf.20042012,c('year'),function(x){sum(x[,'entday.fasc'])+sum(x[,'entday.of'])+sum(x[,'entday.op'])})

subset(encounterDayDf.20042012,year==2004)

names(encounterDayDf.20042012)


# 2              -7 5052764
# 3              -6 5052764
# 4              -5 5052764
# 5              -4 5052764
# 6              -3 5016615
# 7              -2 5016615
# 8              -1 5016615
# 9               0 5016615
# 10              1 4895525
# 11              2 4895525
# 12              3 4895525
# 13              4 4895525
# 14              5 4809210
# 15              6 4809210
# 16              7 4809210
# 17              8 4809210
# 18              9 4733462
# 19             10 4733462
# 20             11 4733462
# 21             12 4733462
# 22             13 4711079
# 23             14 4711079
# 24             15 4711079
# 25             16 4711079
# 26             17 4809146
# 27             18 4809146
# 28             19 4809146
# 29             20 4809146
# 30             21 4850533
# 31             22 4850533
# 32             23 4850533
# 33             24 4850533
# 34             25 4894297
# 35             26 4894297
# 36             27 4894297
# 37             28 4894297
# 
