stop()
#read in comorobdity data
comorb <- xpt2r_1.yz('Z:/j_scrdata/pcaOverTreatment','comorb_test10k')[,c('hicbic','chrlson')]
#read in ses data
ses <- xpt2r_1.yz('Z:/j_rawdata/seer_SES','zipses')


yearMax <- 2009
names(comorb)
urbanVars <- paste('urban',seq(1986,yearMax,1),sep='')
stateVars <- paste('st',seq(1986,yearMax,1),sep='')
countyVars <- paste('cnty',seq(1986,yearMax,1),sep='')
registryVars <- paste('registry',seq(1986,yearMax,1),sep='')
registryVars2 <- paste('reg2cd',seq(1986,yearMax,1),sep='')
zipVars <- paste('zip',seq(1986,yearMax,1),sep='')
keptVars <- c('hicbic','sex','birthm','birthd','birthyr','sex','race','med.dodm','med.dodd','med.dody' 
              ,'indexmonth','indexyear', 'indexday'
              , urbanVars
              ,stateVars
              ,countyVars
              ,zipVars
              ,registryVars
              , registryVars2
              )

test <- xpt2r_1.yz('Z:/j_scrdata/pcaOverTreatment','sumnc_demoraw')
if (!'indexday' %in% names(test)){test[,'indexday'] <- '15'} #index day does not exist then we need to add it
if (!'birthd' %in% names(test)){test[,'birthd'] <- '15'} #index day does not exist then we need to add it



varOfPickedYrSumnc.yz<-function(indf,yearmax,vns,outputVn,timeVarVn='indexyear',idVn='hicbic'){
  longDf <-  stats::reshape(indf[,c('hicbic',vns)], direction="long", varying=list(vns), v.names=outputVn, idvar=c(idVn), timevar=timeVarVn, times=seq(1986,yearMax))
  outDf <- plyr::join(indf[,c('hicbic','indexyear')],longDf,by=c('hicbic','indexyear'))
  return(outDf)
}

hicbicZip <- varOfPickedYrSumnc.yz(test,yearmax=2009,vns=zipVars,outputVn='zip',timeVarVn='indexyear',idVn='hicbic')
hicbicCounty <- varOfPickedYrSumnc.yz(test,yearmax=2009,vns=countyVars,outputVn='county',timeVarVn='indexyear',idVn='hicbic')
hicbicState <- varOfPickedYrSumnc.yz(test,yearmax=2009,vns=stateVars,outputVn='state',timeVarVn='indexyear',idVn='hicbic')
hicbicUrban <- varOfPickedYrSumnc.yz(test,yearmax=2009,vns=urbanVars,outputVn='urban',timeVarVn='indexyear',idVn='hicbic')
hicbicRegistry <- varOfPickedYrSumnc.yz(test,yearmax=2009,vns=registryVars,outputVn='registry',timeVarVn='indexyear',idVn='hicbic')
hicbicRegistry2 <- varOfPickedYrSumnc.yz(test,yearmax=2009,vns=registryVars2,outputVn='registry2',timeVarVn='indexyear',idVn='hicbic')
#now collect all location variables state, county and zip
locationDf <- joinReduce.yz(hicbicZip, deldfcols.yz(hicbicCounty,'indexyear'),deldfcols.yz(hicbicState,'indexyear'),deldfcols.yz(hicbicUrban,'indexyear'),deldfcols.yz(hicbicRegistry,'indexyear'),deldfcols.yz(hicbicRegistry2,'indexyear'), keysList=rep('hicbic',5), typeVec=rep('left',5), matchVec=rep('all',5))

demo.1 <- joinReduce.yz(locationDf, deldfcols.yz(test,c(urbanVars
                                           ,stateVars
                                           ,countyVars
                                           ,zipVars
                                           ,registryVars
                                           , registryVars2,'indexyear')),comorb, keysList=rep('hicbic',2), typeVec=rep('left',2), matchVec=rep('all',2))

demo.1[,'zipclean'] <- adply(demo.1[,c('zip'),drop=F],1,function(x){ifelse(nchar(x[,'zip'])<=5, zip5 <- x[,'zip'],zip5<-substr(x[,'zip'],1,5)); return(zip5)})[,2]
demo.1 <- deldfcols.yz(demo.1,'zip')
demo.1 <- rename.vars(demo.1,'zipclean','zip')
#now append ses scrore
demo.1 <- join(demo.1,ses,by='zip')
hist(demo.1[,'ses'])

#now need to caculate age at index time.
demo.1[,'indexYMD'] <- paste(demo.1[,'indexyear'],demo.1[,'indexmonth'],demo.1[,'indexday'],sep='-')
demo.1[,'birthYMD'] <- paste(demo.1[,'birthyr'],demo.1[,'birthm'],demo.1[,'birthd'],sep='-')
sourceFileOneByOne.yz('C:/Dropbox/Q_new/datemani_new')
demo.1[,'indexage'] <- numDaysBtCharYmd.yz(demo.1[,'indexYMD'],demo.1[,'birthYMD'])/365
demo.1 <- deldfcols.yz(demo.1,c('birthm','birthd','birthyr','indexyear','indexmonth','indexday'))
#next, we need to remove missing and recategorize the variables.

#first missing comorb should be put to 0
demo.1[is.na(demo.1[,'chrlson']),'chrlson'] <- 0
#766 missing SES
df.miss.pct(demo.1)

#generate outcome variable
#align death to dec 31, 2009.
demo.1[,'med.dYMD'] <- paste(demo.1[,'med.dody'],demo.1[,'med.dodm'],demo.1[,'med.dodd'],sep='-')
deathSinceLastObs <- numDaysBtCharYmd.yz(demo.1[,'med.dYMD'],'2009-12-31')
#for these died after 2009 dec 31, we assume we do not observed death
demo.1[which(deathSinceLastObs>0),'med.dYMD'] <- NA

#check death occurred between index time and 10 year
endWin10Yr.num <- unclass(as.Date(demo.1[,'indexYMD']))+365*10
dieIn10Yr <- unclass(as.Date(demo.1[,'med.dYMD'])) <= endWin10Yr.num
#now change NA in dieIn10Yr into FALSE, NA means death not observed even at dec 31, 2009, so surely not occured in the window
dieIn10Yr[is.na(dieIn10Yr)] <- FALSE
table(dieIn10Yr)
demo.1[,'dieIn10Yr'] <- dieIn10Yr
#I found no one died from 2010 and on (make sense) after only 40 died in 2009 and their death dates are. 
subset(demo.1,dieIn10Yr & med.dody=='2009',select=c('med.dody','med.dodm','med.dodd'))
#I have not explicitly algin med.dYMD to 12-31-2009, so do not trust the value you see in med.dYMD that much.
demo.1 <- deldfcols.yz(demo.1,c('med.dodm','med.dodd','med.dody','med.dYMD')) 

df.miss.pct(demo.1) 

#we alreday code missing comorbidity into 0.
#the rest missing can be removed.

demo.nomiss.1 <- demo.1[complete.cases(demo.1),]
nrow(demo.nomiss)

#now let us do categorication.
#for ses, just do 10 tiles.
#for comorbidity group 7 8 9 together, the rest stay the same
#for race, 0 is unknown 6 is native american, two few, we drop them
#urban in 99 means missing

demo.nomiss.2 <- subset(demo.nomiss.1,race %in% c(1,2,3,4,5,6) & !(urban %in%('99')))
nrow(demo.nomiss.2)

sourceFileOneByOne.yz('C:/Dropbox/Q_new/datamani_new')
demo.nomiss.2[,'chrlsonRecat'] <- replaceValJoin.yz(demo.nomiss.2[,'chrlson'] #this is typically a data column
                  ,list(c(0),c(1),c(2),c(3),c(4),c(5),c(6),seq(7,20))
                  ,c('0','1','2','3','4','5','6','7+')
                  ,origValVn='chrlson'
                  ,newValVn='chrsonRecat' #if output is vector, this newValVn is inapplicable
                  ,outputJoinedDf=TRUE #if F means only output the new vector
                  #if T, then output df             
)[,2]

snc.cleaned <- demo.nomiss.2

#save(snc.cleaned,file='Z:/j_scrdata/pcaOverTreatment/snc.cleaned.RData')
(load(file='Z:/j_scrdata/pcaOverTreatment/snc.cleaned.RData'))





