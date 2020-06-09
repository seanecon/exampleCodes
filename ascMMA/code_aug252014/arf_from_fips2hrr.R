#next, Add ARF variables
arfVars=xpt2r.yz('Z:/j_scrdata/ascMMA','arf_jan262015')
table(nchar(arfVars[,'fips']))

names(arfVars)

#from fips to hrr
(load('E:/HouseUrbanDevelop/zip.fips.20102014.RData'))

zip.fips=rename.vars(zip.fips.20102014,c('ZIP'),c('zip5'))


#   zip.hrr=xpt2r.yz("Z:/j_rawdata/zip_hsa_hrr_state",'hrrhsazip20012007_cleaned')[,c('zip5','hrrnum')]
#   length(unique(zip.hrr[,'hrrnum']))

length(unique(zip.hrr[,'zip5']))

missingSeq.yz(x)

(load(file='Z:/j_rawdata/zip_hsa_hrr_state/zip.hrr.20042012.RData'))

zip.hrr.20042012= rename.vars(zip.hrr.20042012,'zipchar','zip5')
out=join(zip.fips,zip.hrr.20042012,by='zip5')

fips.hrrnum= uniqueRows.yz(out[,c('fips','hrrnum')])

arfVars.hrr=join(arfVars,fips.hrrnum,by='fips')
arfVars.hrr.1=arfVars.hrr[!is.na(arfVars.hrr[,'hrrnum']),]

length(unique(arfVars.hrr[,'hrrnum'])) #295 hrr in arf file


x=subset(arfVars.hrr,hrrnum==150)
missing(x[1,])
arf.hrrLevel=ddply(arfVars.hrr.1,'hrrnum',function(x){
  
  naRowsOut=rowsWithNa.yz(x)
  x1=x[naRowsOut$rowsNoNA,]
  per10kMd2011=10000*sum(x1[,'numactmd2011'])/sum(x1[,'popsize2011']);
  per10kMd2010=10000*sum(x1[,'numactmd2010'])/sum(x1[,'popsize2010']);
  per10kMd2009=10000*sum(x1[,'numactmd2009'])/sum(x1[,'popsize2009']);
  per10kMd2008=10000*sum(x1[,'numactmd2011'])/sum(x1[,'popsize2008']);
  per10kMd2007=10000*sum(x1[,'numactmd2007'])/sum(x1[,'popsize2007']);
  per10kMd2006=10000*sum(x1[,'numactmd2006'])/sum(x1[,'popsize2006']);
  per10kMd2005=10000*sum(x1[,'numactmd2005'])/sum(x1[,'popsize2005']);
  
  percaincome2005=sum(x1[,'percaincome2005']/1000*x1[,'popsize2005'])/sum(x1[,'popsize2005']);
  percaincome2006=sum(x1[,'percaincome2006']/1000*x1[,'popsize2006'])/sum(x1[,'popsize2006']);
  percaincome2007=sum(x1[,'percaincome2007']/1000*x1[,'popsize2007'])/sum(x1[,'popsize2007']);
  percaincome2008=sum(x1[,'percaincome2008']/1000*x1[,'popsize2008'])/sum(x1[,'popsize2008']);
  percaincome2009=sum(x1[,'percaincome2009']/1000*x1[,'popsize2009'])/sum(x1[,'popsize2009']);
  percaincome2010=sum(x1[,'percaincome2010']/1000*x1[,'popsize2010'])/sum(x1[,'popsize2010']);
  percaincome2011=sum(x1[,'percaincome2011']/1000*x1[,'popsize2011'])/sum(x1[,'popsize2011']);
  
  povpct2005=sum(x1[,'povpct2005']*x1[,'popsize2005'])/sum(x1[,'popsize2005']);
  povpct2006=sum(x1[,'povpct2006']*x1[,'popsize2006'])/sum(x1[,'popsize2006']);
  povpct2007=sum(x1[,'povpct2007']*x1[,'popsize2007'])/sum(x1[,'popsize2007']);
  povpct2008=sum(x1[,'povpct2008']*x1[,'popsize2008'])/sum(x1[,'popsize2008']);
  povpct2009=sum(x1[,'povpct2009']*x1[,'popsize2009'])/sum(x1[,'popsize2009']);
  povpct2010=sum(x1[,'povpct2010']*x1[,'popsize2010'])/sum(x1[,'popsize2010']);
  povpct2011=sum(x1[,'povpct2011']*x1[,'popsize2011'])/sum(x1[,'popsize2011']);

  out=c(povpct2005=povpct2005,povpct2006=povpct2006,povpct2007=povpct2007,povpct2008=povpct2008,povpct2009=povpct2009,povpct2010=povpct2010,povpct2011=povpct2011
        ,percaincome2005=percaincome2005,percaincome2006=percaincome2006,percaincome2007=percaincome2007,percaincome2008=percaincome2008,percaincome2009=percaincome2009,percaincome2010=percaincome2010,percaincome2011=percaincome2011
        ,per10kMd2005=per10kMd2005,per10kMd2006=per10kMd2006,per10kMd2007=per10kMd2007,per10kMd2008=per10kMd2008,per10kMd2009=per10kMd2009,per10kMd2010=per10kMd2010,per10kMd2011=per10kMd2011
  )
  return(out)
})

arf.hrrLevel[,'povpct2004']=arf.hrrLevel[,'povpct2005']
arf.hrrLevel[,'percaincome2004']=arf.hrrLevel[,'percaincome2005']
arf.hrrLevel[,'per10kMd2004']=arf.hrrLevel[,'per10kMd2005']

medianOut=apply(arf.hrrLevel[,seq(2,ncol(arf.hrrLevel))],2,median)
names(medianOut)

#only 294 HRR found
allHrr=
  ?reshape


summary(Indometh)
wide <- reshape(Indometh, v.names = "conc", idvar = "Subject",
                timevar = "time", direction = "wide")
wide

reshape(wide, direction = "long")
reshape(wide, idvar = "Subject", varying = list(2),
        v.names = "conc", direction = "long")

  
missingHrrFromArf=setdiff(unique(anaDfNoBs[,'hrrnum']),arf.hrrLevel[,'hrrnum'])

imput.arf=data.frame(hrrnum=missingHrrFromArf,matrix(medianOut,nrow=1))
names(imput.arf)=c('hrrnum',names(medianOut))

arf.hrr.imput=rbind(arf.hrrLevel,imput.arf)

names(arf.hrr.imput)


povpct.tmp=melt(arf.hrr.imput[,c("hrrnum","povpct2004","povpct2005", "povpct2006", "povpct2007", "povpct2008","povpct2009", "povpct2010", "povpct2011")], id=c('hrrnum')) 
povpct.df.tmp=data.frame(povpct.tmp,year=as.numeric(substr(povpct.tmp[,'variable'],7,12)))[,c('hrrnum','value','year')]
povpct.df=rename.vars(povpct.df.tmp,c('value'),c('povpct'))

names(arf.hrr.imput)

percaincome.tmp=melt(arf.hrr.imput[,c("hrrnum",'percaincome2004',"percaincome2005", "percaincome2006", "percaincome2007", "percaincome2008", "percaincome2009" ,"percaincome2010", "percaincome2011")], id=c('hrrnum')) 
percaincome.df.tmp=data.frame(percaincome.tmp,year=as.numeric(substr(percaincome.tmp[,'variable'],12,17)))[,c('hrrnum','value','year')]
percaincome.df=rename.vars(percaincome.df.tmp,c('value'),c('percaincome'))

per10kMd.tmp=melt(arf.hrr.imput[,c("hrrnum","per10kMd2004","per10kMd2005", "per10kMd2006", "per10kMd2007", "per10kMd2008", "per10kMd2009" ,"per10kMd2010", "per10kMd2011")], id=c('hrrnum')) 
per10kMd.df.tmp=data.frame(per10kMd.tmp,year=as.numeric(substr(per10kMd.tmp[,'variable'],9,14)))[,c('hrrnum','value','year')]
per10kMd.df=rename.vars(per10kMd.df.tmp,c('value'),c('per10kMd'))

arfCleanedHrrYear20042011=join(join(per10kMd.df,percaincome.df),povpct.df)

save(arfCleanedHrrYear20042011,file='Z:/j_scrdata/ascMMA/arfCleanedHrrYear20042011.RData')














