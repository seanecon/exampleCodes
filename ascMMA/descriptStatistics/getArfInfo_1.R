#get ARF information from ARF
arf.raw=xpt2r.yz('Z:/j_rawdata/ARF/raw_sas','zipArfZipPopSize')[,c('zipchar',c('fips','medianhhinc08','collegeedu00','percapitaincome2007',"familyfemalehead2000","numsrgip.per100k.2007","numsrgop.per100k.2007","numhospbeds.per100k.2007","numoprooms.per100k.2007",'fippopsize2009'))]


countyHrr.xw=uniqueRows.yz(ziphrr.arf[,c('fips','hrrnum')]) #get this data
indf=read.csv('F:/j_scrdata/americanCommunitySurvey/educationAttain20052011/ACS_10_edu.csv')
rename.vars(indf,'HC01_EST_VC05','college')
indf.1=data.frame(indf,fips=substr(indf[,1],10,14))

join(indf.1,countyHrr.xw)
names(join(indf.1,countyHrr.xw))


names(xpt2r.yz('Z:/j_rawdata/ARF/raw_sas','zipArfZipPopSize'))
#note it can only be at fips level not at zip level.




getBachelorOrHigher=function( dataPath
                              ,what.year
                             ,age1824.pct.var
                             ,age25more.pct.var
                             ,age1824.popsize.var
                             ,age25more.popsize.var
                             ){
all_content = readLines(dataPath)
skip_second = all_content[-2]
dat = read.csv(textConnection(skip_second), header = TRUE, stringsAsFactors = FALSE)
dat[,'fips']=substr(dat[,'GEO.id'],10,14)
#keep the following variable
dat[,'totpop']=dat[,age1824.popsize.var]+dat[,age25more.popsize.var]
dat[,'bachelorhigherPct']=dat[,age25more.pct.var]*(dat[,age25more.popsize.var]/dat[,'totpop'])+dat[,age1824.pct.var]*(dat[,age1824.popsize.var]/dat[,'totpop'])
out=join(dat[,c('bachelorhigherPct','totpop','fips')], countyHrr.xw, by='fips')
out.1=out[which(!is.na(out[,'hrrnum'])),]
out.2=ddply(out.1,'hrrnum',function(x){c(collegeAbovePct=sum(x[,'bachelorhigherPct']*x[,'totpop'])/sum(x[,'totpop']))})
outdf=data.frame(out.2,year=what.year)
return(outdf)
}



collegeAbove.2005=getBachelorOrHigher("F:/j_scrdata/americanCommunitySurvey/educationAttain20052011/ACS_05_EST_S1501_with_ann.csv",2005
                                    , 'HC01_EST_VC05'
                                      ,'HC01_EST_VC15'
                                      ,'HC01_EST_VC01'
                                      ,'HC01_EST_VC06')
collegeAbove.2006=getBachelorOrHigher("F:/j_scrdata/americanCommunitySurvey/educationAttain20052011/ACS_06_EST_S1501_with_ann.csv",2006, 'HC01_EST_VC05'
                                      ,'HC01_EST_VC15'
                                      ,'HC01_EST_VC01'
                                      ,'HC01_EST_VC06')
collegeAbove.2007=getBachelorOrHigher("F:/j_scrdata/americanCommunitySurvey/educationAttain20052011/ACS_07_1YR_S1501_with_ann.csv",2007, 'HC01_EST_VC05'
                                      ,'HC01_EST_VC15'
                                      ,'HC01_EST_VC01'
                                      ,'HC01_EST_VC06')
collegeAbove.2008=getBachelorOrHigher("F:/j_scrdata/americanCommunitySurvey/educationAttain20052011/ACS_08_1YR_S1501_with_ann.csv",2008, 'HC01_EST_VC05'
                                      ,'HC01_EST_VC15'
                                      ,'HC01_EST_VC01'
                                      ,'HC01_EST_VC06')
collegeAbove.2009=getBachelorOrHigher("F:/j_scrdata/americanCommunitySurvey/educationAttain20052011/ACS_09_1YR_S1501_with_ann.csv",2009, 'HC01_EST_VC05'
                                      ,'HC01_EST_VC15'
                                      ,'HC01_EST_VC01'
                                      ,'HC01_EST_VC06')
collegeAbove.2010=getBachelorOrHigher("F:/j_scrdata/americanCommunitySurvey/educationAttain20052011/ACS_10_1YR_S1501_with_ann.csv",2010, 'HC01_EST_VC05'
                                      ,'HC01_EST_VC17'
                                      ,'HC01_EST_VC01'
                                      ,'HC01_EST_VC07')
collegeAbove.2011=getBachelorOrHigher("F:/j_scrdata/americanCommunitySurvey/educationAttain20052011/ACS_11_1YR_S1501_with_ann.csv",2011, 'HC01_EST_VC05'
                                      ,'HC01_EST_VC17'
                                      ,'HC01_EST_VC01'
                                      ,'HC01_EST_VC07')

bachelorAboveHrrYear=do.call(rbind,list(collegeAbove.2011,collegeAbove.2010,collegeAbove.2009,collegeAbove.2008,collegeAbove.2007,collegeAbove.2006,collegeAbove.2005))
#there are some missing. we need to handle it.
save(bachelorAboveHrrYear,file='F:/j_scrdata/americanCommunitySurvey/educationForAscProj/bachelorAboveHrrYear.RData')



te.2011=subset(bachelorAboveHrrYear,year==2011)
imput.2011=imputOneVar.yz(
  data.frame(hrrnum=unique(anaDfNoBs[,'hrrnum'])) #the universe of ids
  , te.2011[,c('hrrnum','collegeAbovePct')]
  #the data frame contains ids and one value
  , c('hrrnum') #the variable define an observation
  , c('collegeAbovePct')
  #one var, I make a point to not use many vars here
  , 'median'
  #either 'median' or 'mean' or 'random' or most 'mostFreq'
  #if value variale is character, then it has to be either random or most frequent
  , randomSeed=1
  #if random then use randomSeed is useful
  , imputIndicatorVn='isImput'
)
te.2010=subset(bachelorAboveHrrYear,year==2010)
imput.2010=imputOneVar.yz(
  data.frame(hrrnum=unique(anaDfNoBs[,'hrrnum'])) #the universe of ids
  , te.2010[,c('hrrnum','collegeAbovePct')]
  #the data frame contains ids and one value
  , c('hrrnum') #the variable define an observation
  , c('collegeAbovePct')
  #one var, I make a point to not use many vars here
  , 'median'
  #either 'median' or 'mean' or 'random' or most 'mostFreq'
  #if value variale is character, then it has to be either random or most frequent
  , randomSeed=1
  #if random then use randomSeed is useful
  , imputIndicatorVn='isImput'
)

te.2009=subset(bachelorAboveHrrYear,year==2009)
imput.2009=imputOneVar.yz(
  data.frame(hrrnum=unique(anaDfNoBs[,'hrrnum'])) #the universe of ids
  , te.2009[,c('hrrnum','collegeAbovePct')]
  #the data frame contains ids and one value
  , c('hrrnum') #the variable define an observation
  , c('collegeAbovePct')
  #one var, I make a point to not use many vars here
  , 'median'
  #either 'median' or 'mean' or 'random' or most 'mostFreq'
  #if value variale is character, then it has to be either random or most frequent
  , randomSeed=1
  #if random then use randomSeed is useful
  , imputIndicatorVn='isImput'
)

te.2008=subset(bachelorAboveHrrYear,year==2008)
imput.2008=imputOneVar.yz(
  data.frame(hrrnum=unique(anaDfNoBs[,'hrrnum'])) #the universe of ids
  , te.2008[,c('hrrnum','collegeAbovePct')]
  #the data frame contains ids and one value
  , c('hrrnum') #the variable define an observation
  , c('collegeAbovePct')
  #one var, I make a point to not use many vars here
  , 'median'
  #either 'median' or 'mean' or 'random' or most 'mostFreq'
  #if value variale is character, then it has to be either random or most frequent
  , randomSeed=1
  #if random then use randomSeed is useful
  , imputIndicatorVn='isImput'
)
te.2007=subset(bachelorAboveHrrYear,year==2007)
imput.2007=imputOneVar.yz(
  data.frame(hrrnum=unique(anaDfNoBs[,'hrrnum'])) #the universe of ids
  , te.2007[,c('hrrnum','collegeAbovePct')]
  #the data frame contains ids and one value
  , c('hrrnum') #the variable define an observation
  , c('collegeAbovePct')
  #one var, I make a point to not use many vars here
  , 'median'
  #either 'median' or 'mean' or 'random' or most 'mostFreq'
  #if value variale is character, then it has to be either random or most frequent
  , randomSeed=1
  #if random then use randomSeed is useful
  , imputIndicatorVn='isImput'
)

te.2006=subset(bachelorAboveHrrYear,year==2006)
imput.2006=imputOneVar.yz(
  data.frame(hrrnum=unique(anaDfNoBs[,'hrrnum'])) #the universe of ids
  , te.2006[,c('hrrnum','collegeAbovePct')]
  #the data frame contains ids and one value
  , c('hrrnum') #the variable define an observation
  , c('collegeAbovePct')
  #one var, I make a point to not use many vars here
  , 'median'
  #either 'median' or 'mean' or 'random' or most 'mostFreq'
  #if value variale is character, then it has to be either random or most frequent
  , randomSeed=1
  #if random then use randomSeed is useful
  , imputIndicatorVn='isImput'
)

te.2005=subset(bachelorAboveHrrYear,year==2005)
imput.2005=imputOneVar.yz(
  data.frame(hrrnum=unique(anaDfNoBs[,'hrrnum'])) #the universe of ids
  , te.2005[,c('hrrnum','collegeAbovePct')]
  #the data frame contains ids and one value
  , c('hrrnum') #the variable define an observation
  , c('collegeAbovePct')
  #one var, I make a point to not use many vars here
  , 'median'
  #either 'median' or 'mean' or 'random' or most 'mostFreq'
  #if value variale is character, then it has to be either random or most frequent
  , randomSeed=1
  #if random then use randomSeed is useful
  , imputIndicatorVn='isImput'
)
nrow(data.frame(hrrnum=unique(anaDfNoBs[,'hrrnum'])) )

te.2004=te.2005
te.2004[,'year']=2004


imput.2004=imputOneVar.yz(
  data.frame(hrrnum=unique(anaDfNoBs[,'hrrnum'])) #the universe of ids
  , te.2004[,c('hrrnum','collegeAbovePct')]
  #the data frame contains ids and one value
  , c('hrrnum') #the variable define an observation
  , c('collegeAbovePct')
  #one var, I make a point to not use many vars here
  , 'median'
  #either 'median' or 'mean' or 'random' or most 'mostFreq'
  #if value variale is character, then it has to be either random or most frequent
  , randomSeed=1
  #if random then use randomSeed is useful
  , imputIndicatorVn='isImput'
)



collegeEduFromACS.20042011=rbindAdddBlockValue.yz(imput.2004,imput.2005,imput.2006,imput.2007,imput.2008,imput.2009,imput.2010,imput.2011, vn='year',valueVec=seq(2004,2011))

#save(collegeEduFromACS.20042011, file='Z:/j_scrdata/ascMMA/collegeEduFromACS.20042011.RData')










# HC01_EST_VC01  Total; Estimate; Population 18 to 24 years
# HC01_EST_VC05  Total; Estimate; Population 18 to 24 years - Bachelor's degree or higher

# HC01_EST_VC06  Total; Estimate; Population 25 years and over
# HC01_EST_VC15  Total; Estimate; Population 25 years and over - Percent bachelor's degree or higher

#2011
#HC01_EST_VC05  Total; Estimate; Bachelor's degree or higher

#2010
#HC01_EST_VC01  Total; Estimate; Population 18 to 24 years
#HC01_EST_VC05  Total; Estimate; Bachelor's degree or higher Population 18 to 24 years ?
#HC01_EST_VC07  Total; Estimate; Population 25 years and over
#HC01_EST_VC17  Total; Estimate; Percent bachelor's degree or higher , Population 25 years and over?








#you have to get fips level data then aggregate
head(arf.raw)

#hrrhsastate=xpt2r.yz('Z:/j_rawdata/zip_hsa_hrr_state/hsaHrrStateXw','hsaHrrStateXw')


zip.hrr=read.csv(file='Z:/j_rawdata/zip_hsa_hrr_state/ZipHsaHrr12.csv')
zip.hrr.1 = fill.ini.char.yz(zip.hrr,'zipcode12','0',5,'zipchar')[,c('zipchar','hrrnum')]

length(unique(zip.hrr[,'hrrnum']))
nrow(zip.hrr)
nrow(arf.raw)
ziphrr.arf=join(arf.raw,zip.hrr.1)
head(ziphrr.arf)




ziphrr.arf.1=uniqueRows.yz(deldfcols.yz(ziphrr.arf,'zipchar'))
percapinc=ddply(ziphrr.arf.1,'hrrnum',function(x){c(percapinc=wtd.mean(x[,'percapitaincome2007'],x[,'fippopsize2009']/sum(x[,'fippopsize2009'])))})
collegeedu=ddply(ziphrr.arf.1,'hrrnum',function(x){c(collegeedu=wtd.mean(x[,'collegeedu00'],x[,'fippopsize2009']/sum(x[,'fippopsize2009'])))})
numhospbeds.per100k=ddply(ziphrr.arf.1,'hrrnum',function(x){c(numhospbeds.per100k=wtd.mean(x[,'numhospbeds.per100k.2007'],x[,'fippopsize2009']/sum(x[,'fippopsize2009'])))})
familyfemalehead=ddply(ziphrr.arf.1,'hrrnum',function(x){c(familyfemalehead=wtd.mean(x[,'familyfemalehead2000'],x[,'fippopsize2009']/sum(x[,'fippopsize2009'])))})

arfIndep=joinReduce.yz(percapinc, collegeedu, numhospbeds.per100k, familyfemalehead,keysList=list('hrrnum','hrrnum','hrrnum'),typeVec=c('left','left','left'),matchVec=c('all','all','all'))
arfIndepClean=arfIndep[!is.na(arfIndep[,1]),]
#save(arfIndepClean,file= "Z:/j_scrdata/ascMMA/result/arfIndepClean.RData")



