#get ARF information from ARF
arf.raw=xpt2r.yz('Z:/j_rawdata/ARF/raw_sas','zipArfZipPopSize')[,c('zipchar',c('fips','medianhhinc08','collegeedu00','percapitaincome2007',"familyfemalehead2000","numsrgip.per100k.2007","numsrgop.per100k.2007","numhospbeds.per100k.2007","numoprooms.per100k.2007",'fippopsize2009'))]




names(xpt2r.yz('Z:/j_rawdata/ARF/raw_sas','zipArfZipPopSize'))
#note it can only be at fips level not at zip level.


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



