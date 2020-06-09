# 
# %_libyz(te,E:\medicareRaw\posAscCategory)
# %genxpt(te.poshsa)

# 
# * ASC Categories per region.sas                                                                                    *
#   *                                                                                                                  *
#   * This program creates a file of ASC existence within a market.  The file has the following characteristics:       *
#   *  - Resulting file has one line per region (either HSA or HRR)                                                    *
#   *  - ASC category defined as ASC never, always, or introduced within a market                                      *
#   *	- Year variable is the year an ASC was introduced into a market.  For the                                       *
#   * 	  other 2 categories, the year variable is probability-matched to allow for comparisons.                        *
#   *                                                                                                                  *
#   * This resulting dataset is processed from a SAS dataset called POSHSA.  The POSHSA                                *
#   * file contains one line per unique ASC from the POS files from 2001 to 2010 and contains                          *
#   * facility ID, zip code, state, HSA/HRR info, and indicators for an ASC existing overall (FAC01-FAC10)             *
#   * and by specialty (CV01--UROLOGY10).  This dataset can be used for further refined information.                   *
#   *                                                                                                                  *
#   * To use this macro, you need to do the following:                                                                 *
#   * 	1) Update the ASC libname statement to point to the location where you places the POSHSA dataset.               *
#   *	2) FIRSTYR and LASTYR are the first and last years (YYYY format) of the time period of interest.                *
#   *	3) By default, the macro will use HSA as the region.  If you would prefer HRR, add REGVAR=HRR to the macro call *
#   *	4) By default, the macro will use the overall indicators.  If a specialty is desired, add SPEC=specialty        *
#   *	   to the macro call.  Specialties available are:                                                               *
#   *		cv=Cardiovascular                                                                                            *
#   *		foot=Foot Surgery                                                                                            *
#   *		gensurg=General Surgery                                                                                      *
#   *		neuro=Neurological                                                                                           *
#   *		obgyn=Obstetrics/Gynecology                                                                                  *
#   *		optha=Opthamology                                                                                            *
#   *		oral=Oral Surgery                                                                                            *
#   *		ortho=Orthopedics                                                                                            *
#   *		oto=Otolaryngology                                                                                           *
#   *		plastic=Plastic Surgery                                                                                      *
#   *		thoracic=Thoracic Surgery                                                                                    *
#   *		urology=Urology                                                                                              *
#   *     other=Other specialties
# *                                                                                                                  *
#   * Example macro calls:                                                                                             *
#   *                                                                                                                  *
#   * %hsacat(2001,2010) - ASC categories for HSA from 2001-2010	                                                    *
#   * %hsacat(2002,2007) - ASC categories for HSA from 2002-2007	                                                    *
#   * %hsacat(2003,2008,spec=cv)	 - Cardiovascular ASC categories for HSA from 2003-2008	                            *
#   * %hsacat(2004,2009,regvar=hrr) - ASC categories for HRR from 2004-2009	                                           *
#   * %hsacat(2005,2010,spec=foot,regvar=hrr) - Foot surgery ASC categories for HRR from 2005-2010	                   *
#   *                                                                                                                  *
#   * Created by: 			Rod Dunn                                                                                      *
#   * Intially created:	December 2012  
nrow(posZipLevelAsc) # 6760 zipcode level


#if the data only contains these facilities that were ASC in at least one year of 2001 and 2010, then a HSA that has no ASC facilities
#would then not in the poshsa file. then they should be in risk set.

posZipLevelAsc <- xpt2r.yz('E:/medicareRaw/posAscCategory/new/','poshsa')
names(posZipLevelAsc)

head(posZipLevelAsc)


test=xpt2r.yz('Z:/j_scrdata/ascMMA','posVars_2006')

hrr.fascind=ddply(test,'hrrnum',function(x){vec=as.logical(x[,'fascind']);out=any(vec);return(out)})
table(hrr.fascind[,'V1'])



rawDf <- subset(posZipLevelAsc,select=c('zip','state','hrr','facid',paste('fac',yearLast2.yz(seq(2001,2010)),sep='')))
head(rawDf)


TF.mat=apply(as.matrix(rawDf[,paste('fac',substr(seq(2001,2010),3,4),sep='')]),2,as.logical)

apply(TF.mat,2,function(x){cumAny.yz(x,includeCurrent=T)})


?as.logical
min(rowSums(rawDf[,paste('fac',yearLast2.yz(seq(2001,2010)),sep='')]))
#need to dig out the arf-HSA and zip code crosswalk 
#arf is fips level data. then go to use sasfipszip 
#then append arf an zipcode. then further append
names(rawDf)

lapply(apply(matrix(c(1,2,3,4),nrow=2),1,function(x){which(x==1)}),)
unlist(apply(matrix(c(1,2,3,4),nrow=2),1,function(x){which(x==1)}))

#note if someone has not adopt in any year between 2001 and 2010 then it will not work.
firstAdoptYrIndx <- apply(rawDf[,paste("fac",yearLast2.yz(seq(2001,2010)),sep='')],1,function(x){whichOut <- which(x==1); if (length(whichOut)==0){re=NA} else{re=min(whichOut)};return(re)})

adoptYear <- indexVec2valueVec.yz(seq(2001,2010),firstAdoptYrIndx, troubleVal=NaN)[,'val'] #every zip adopts

rawDfNotClose <- subset(rawDf.1,notClose)
#6752

rawDfNotClose[,'firstAdoptYear']
rawDf.1[32,]

min(rowSums(rawDf.1[,paste("fac",yearLast2.yz(seq(2001,2010)),sep='')]))
#every one adopt ASC in 2010. this is strange...

#the following hsa is hospital service area, ARF is health servcies area
# zipHsaXw <- rename.vars(xpt2r.yz('Z:/j_rawdata/zip_hsa_hrr_state','hrrhsazip20012007_cleaned'),c('hsanum','zip5'),c('hsa','zip'))[,c('hsa','zip')]

names()
#%genxpt(ascscr.zipLevelArf)
zipHlthServiceAreaXw <- subset(xpt2r.yz('Z:/j_rawdata/ARF/raw_sas','zipArfZipPopSize'),select=c("hlthservicearea",'zipchar'))
zipHlthServiceAreaXw <- rename.vars(zipHlthServiceAreaXw,'zipchar','zip')
names(zipHlthServiceAreaXw)
head(zipHlthServiceAreaXw)
nrow(uniqueRows.yz(zipHlthServiceAreaXw))

hsaSet <- unique(zipHlthServiceAreaXw[,'hlthservicearea'])
length(hsaSet) #804 HSAs, in the national there are 824 HSAs in total http://www.cdc.gov/niosh/pdfs/98-157-l.pdf
#at least in 2009 ARF file, we identifed these 804 HSA

rawDf.1 <- subset(join(rawDf,zipHlthServiceAreaXw,c('zip')),!is.na(hlthservicearea))
head(rawDf.1)
adopt <- ddply(rawDf.2,'hsa',function(x){min(x[,'firstAdoptYear'],na.rm=T)>2007})

adopt <- ddply(rawDf.1,'hlthservicearea',function(x){min(x[,paste('fac',substr(seq(2001,2009),3,4),sep='')],na.rm=T)>2007})

min(which(subset(rawDf.1,hlthservicearea=='007')[,paste('fac',substr(seq(2001,2009),3,4),sep='')]==1,arr.ind=T)[,'col'])


nrow(rawDf.1.riskSet)
rawDf.1.riskSet[,'adopt20082009'] <- rawDf.1.riskSet[,'firstAdoptYear'] %in% seq(2008,2009)
count.yz(rawDf.1.riskSet,'adopt20082009')

#zip and HSA crosswalk


sum(is.na(rawDf.2[,'hsa'])) #only 4 missing zip code, who cares only 4
rawDf.3 <- subset(rawDf.2,!is.na(hsa),select=c('hsa','firstAdoptYear','notClose','adopt20082009'))
nrow(rawDf.3)

hsaAdoptOutcome <- ddply(rawDf.2,'hsa',function(x){c(adopt20082009=any(x[,'adopt20082009']))})
hasAdoptOutcome <- subset(hsaAdoptOutcome, !is.na(hsa))
#485 risk set HSA, which has not adopted robotic pro



#hsaLevelArf
(load(file='F:/medicareScr/asc/hsaLevelArf.RData'))
#hsaLevelRace and Sex
(load('F:/medicareScr/asc/hsaMedicareBeneAgeRaceSex.RData'))
#hhi 2007
load(file='F:/medicareScr/asc/hhiOp2007.RData')










