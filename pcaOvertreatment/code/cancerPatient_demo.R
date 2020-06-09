#load table1.yz function
source('C:/Dropbox/R_new/example/table1_improved.R')
#for cancer pateints
#read in ses data
ses <- xpt2r_.yz('Z:/j_rawdata/seer_SES','zipses')
demo.1 <- xpt2r.yz('Z:/j_scrdata/pcaOverTreatment','cancerPatientDs_07052012')
nrow(demo.1)
#N=69249 dx 2004-2007 meet entitlemnt bf and after 12 rmove die within 6mon

table(demo.1[,'.first.dx.year'])
demoOnlyPca <- subset(demo.1,firstonlypca==1)
table(demoOnlyPca[,'.first.dx.year'])
nrow(demoOnlyPca)
#N=69249 dx 2004-2007 meet entitlemnt bf and after 12 rmove die within 6mon
#N=60472 add pCA only 
demo.1.trtOk <- demoOnlyPca[which(rowSums(demoOnlyPca[,c('hasrobotnoradia','hasimrtnosrg','observation')])==1),]
nrow(demo.1.trtOk)
#N=69249 dx 2004-2007 meet entitlemnt bf and after 12 rmove die within 6mon
#N=60472 add pCA only
#N=32005 after treatment crtierion
count(demo.1.trtOk,'observation') #12316
count(demo.1.trtOk,'hasimrtnosrg') #16271
count(demo.1.trtOk,'hasrobotnoradia') #3418

demo.1.trtOk <- rename.vars(demo.1.trtOk,c('.first.dx.year','.first.dx.month'),c('indexyear','indexmonth'))


if (!'indexday' %in% names(demo.1.trtOk)){demo.1.trtOk[,'indexday'] <- '15'} #index day does not exist then we need to add it
if (!'birthd' %in% names(demo.1.trtOk)){demo.1.trtOk[,'birthd'] <- '15'} #index day does not exist then we need to add it

demo.1.trtOk[,'zipclean'] <- adply(demo.1.trtOk[,c('zip'),drop=F],1,function(x){ifelse(nchar(x[,'zip'])<=5, zip5 <- x[,'zip'],zip5<-substr(x[,'zip'],1,5)); return(zip5)})[,2]
demo.1.trtOk <- deldfcols.yz(demo.1.trtOk,'zip')
demo.1.trtOk <- rename.vars(demo.1.trtOk,'zipclean','zip')
#now append ses scrore
demo.1.trtOk <- join(demo.1.trtOk,ses,by='zip')

#now need to caculate age at index time.
demo.1.trtOk[,'indexYMD'] <- paste(demo.1.trtOk[,'indexyear'],demo.1.trtOk[,'indexmonth'],demo.1.trtOk[,'indexday'],sep='-')
demo.1.trtOk[,'birthYMD'] <- paste(demo.1.trtOk[,'birthyr'],demo.1.trtOk[,'birthm'],demo.1.trtOk[,'birthd'],sep='-')
sourceFileOneByOne.yz('C:/Dropbox/Q_new/datemani_new')
demo.1.trtOk[,'indexage'] <- numDaysBtCharYmd.yz(demo.1.trtOk[,'indexYMD'],demo.1.trtOk[,'birthYMD'])/365
demo.1.trtOk <- deldfcols.yz(demo.1.trtOk,c('birthm','birthd','birthyr'))
#next, we need to remove missing and recategorize the variables.

#first missing comorb should be put to 0
demo.1.trtOk[is.na(demo.1.trtOk[,'chrlson']),'chrlson'] <- 0
#further append grade and stage information from seer
#getDemoTable.sas
stageGrade <-  xpt2r_1.yz('Z:/j_scrdata/pcaOverTreatment', 'cancer_dxstageGrade')
# 
# '1' = White
# '2' = Black
# '3' = Other
# '4' = Asian
# '5' = Hispanic
# '6' = N. Am. Native
# '0' = Unknown
demo.2 <- join(demo.1.trtOk,stageGrade,by='regcase')
nrow(demo.2)

demo.2[,'raceCat'] <- replaceValJoin.yz(demo.2[,'race'] #this is typically a data column
                                       ,list(c('1'),c('2'),c('3','6'),'4','5','0')
                                       ,c('white','black','other','asian','hispanic',NA)
                                       ,origValVn='race'
                                       ,newValVn='raceCat' #if output is vector, this newValVn is inapplicable
                                       ,outputJoinedDf=TRUE #if F means only output the new vector
                                       #if T, then output df             
)[,2]

demo.2[,'grdCat'] <- replaceValJoin.yz(demo.2[,'.gradecat'] #this is typically a data column
                                        ,list('poorUndiff','restType','wellmodDiff')
                                        ,c('poorUndiff',NA,'wellmodDiff')
                                        ,origValVn='.gradecat'
                                        ,newValVn= #if output is vector, this newValVn is inapplicable
                                        ,outputJoinedDf=TRUE #if F means only output the new vector
                                        #if T, then output df             
)[,2]

sesCuts <-c(-Inf,quantile(demo.2[,'ses'],probs=c(0.333,0.667,1),na.rm=T))
demo.2 <- grpnv.supplycuts.yz (demo.2, 'ses', sesCuts, 'sesCat')


ageCuts <-c(-Inf,70,75,80,85,Inf)
demo.2 <- grpnv.supplycuts.yz (demo.2, 'indexage', ageCuts, 'ageCat')

count(demo.2,'raceCat')
count(demo.2,'grdCat')
count(demo.2,'sesCat')
count(demo.2,'ageCat')

demo.2[,'chrlsonRecat'] <- replaceValJoin.yz(demo.2[,'chrlson'] #this is typically a data column
                                                    ,list(c(0),c(1),c(2),c(3),c(4),c(5),c(6),seq(7,20))
                                                    ,c('0','1','2','3','4','5','6','7+')
                                                    ,origValVn='chrlson'
                                                    ,newValVn='chrsonRecat' #if output is vector, this newValVn is inapplicable
                                                    ,outputJoinedDf=TRUE #if F means only output the new vector
                                                    #if T, then output df             
)[,2]


demo.2[,'stgCat'] <- replaceValJoin.yz(demo.2[,'.stggrp.simple'] #this is typically a data column
                                             ,list('<=T1','T1','T2','T3','T4','missing')
                                             ,c('<=T1','T1','T2','T3','T4',NA)
                                             ,origValVn='.stggrp.simple'
                                             ,newValVn='stgCat' #if output is vector, this newValVn is inapplicable
                                             ,outputJoinedDf=TRUE #if F means only output the new vector
                                             #if T, then output df             
)[,2]



#now further remove mets
demo.3 <- subset(demo.2,x.metsind==0)
nrow(demo.3)
#
#N=69249 dx 2004-2007 meet entitlemnt bf and after 12 rmove die within 6mon
#N=60472 add pCA only
#N=32005 after treatment crtierion
count(demo.1.trtOk,'observation') #12316
count(demo.1.trtOk,'hasimrtnosrg') #16271
count(demo.1.trtOk,'hasrobotnoradia') #3418
#nrow 32005-29991=2014 mets

demo.3 <- dummy2factor.yz (demo.3, c('hasimrtnosrg','hasrobotnoradia','observation'), "observation"
                           , 'trtType'
                           , update.dfr=T)

finalSample <- demo.3[complete.cases(demo.3[,c('ageCat','raceCat','grdCat','stgCat','.damicorisk','sesCat','chrlsonRecat','urban','trtType','registry')]),]

#N=69249 dx 2004-2007 meet entitlemnt bf and after 12 rmove die within 6mon
#N=60472 add pCA only
#N=32005 after treatment crtierion
#N=29991 remove missing values for variables as shown below
# addedVar left.N lost.N
# 1            NA  29991      0
# 2     indexyear  29991      0
# 3        ageCat  29991      0
# 4       raceCat  29967    -24
# 5        grdCat  29535   -432
# 6        stgCat  29373   -162
# 7   .damicorisk  29373      0
# 8        sesCat  28317  -1056
# 9  chrlsonRecat  28317      0
# 10        urban  28316     -1
# 11     registry  28316      0
# 12      trtType  28316      0
missingSeq.yz(demo.3,c('indexyear','ageCat','raceCat','grdCat','stgCat','.damicorisk','sesCat','chrlsonRecat','urban','registry','trtType'))

tab1Output <- table.yz(c('indexyear','ageCat','raceCat','grdCat','stgCat','.damicorisk','sesCat','chrlsonRecat','registry'),'trtType',finalSample,outDf.indepVn='indepVn')
str(tab1Output)
tab1Output$tabDf[,c(1,5,6,7,8)]
tab1Output$sample.byDep

count(finalSample,'indexyear')

names(finalSample)


#save(finalSample,file='Z:/j_scrdata/pcaOverTreatment/finalSample.RData')

(load(file='Z:/j_scrdata/pcaOverTreatment/finalSample.RData'))

#now I need to get predicted 10 year life expectancy 

# 
# 
# #the rest missing can be removed.
# 
# #now let us do categorication.
# #for ses, just do 10 tiles.
# #for comorbidity group 7 8 9 together, the rest stay the same
# #for race, 0 is unknown 6 is native american, two few, we drop them
# #urban in 99 means missing
# 
# demo.nomiss.2 <- subset(demo.nomiss.1,race %in% c(1,2,3,4,5) & !(urban %in%('99')))
# nrow(demo.nomiss.2)
# 
# sourceFileOneByOne.yz('C:/Dropbox/Q_new/datamani_new')
# 
# #this data pcadx20042007Mets is generated in getDemoTable.sas
# nonmetsReg <- subset(xpt2r_1.yz('Z:/j_scrdata/pcaOverTreatment','pcadx20042007Mets'),x.metsind==0)[,'regcase',drop=F]
# # nrow of demo.nomiss.2=66099
# 
# 
# nometsDf <- join(demo.nomiss.2, nonmetsReg, by='regcase', type='inner')
# 
# #further append grade and stage information from seer
# #getDemoTable.sas
# stageGrade <-  xpt2r_1.yz('Z:/j_scrdata/pcaOverTreatment', 'cancer_dxstageGrade')
# 
# cancer.cleaned <- join(nometsDf, stageGrade,by='regcase')
# # nrow(cancer.cleaned)==60394, so 66099-60394 has either mets or missing
# 
# #save(cancer.cleaned,file='Z:/j_scrdata/pcaOverTreatment/cancer.cleaned.RData')
# (load(file='Z:/j_scrdata/pcaOverTreatment/cancer.cleaned.RData'))
# 
# rm(demo.nomiss.1)
# rm(demo.nomiss.2)
# #now we need to figure out IMRT and Robot and observational see getMoreRelaxedCohort_06252012.sas
# #robotRegcase can be dx year can be 01 to 07, but cancer.cleaned  
# robotRegcase <- xpt2r_1.yz('Z:/j_scrdata/pcaOverTreatment','robot_notRadiaBfTrt')[,'regcase',drop=F]
# robotPca <- join(robotRegcase,cancer.cleaned,by='regcase',type='inner')
# 
# imrtRegcase <- xpt2r_1.yz('Z:/j_scrdata/pcaOverTreatment','imrt_notSrgBfTrt')[,'regcase',drop=F]
# imrtPca <- join(imrtRegcase,cancer.cleaned,by='regcase',type='inner')
# 
# 
# names(robotPca)
# indepVars <- c('indexyear','comorbCat', 'registry','.stggrp.simple','x.grade','race','.damicorisk','indexyear')
# 
# table1Df <- robotPca
# table1Df <- imrtPca
# 
# tablelist <- list()
# for (i in 1:length(indepVars)){
#   tablelist <- lappend.yz(tablelist,count(table1Df,indepVars[i]))
# }
# tablelist
# 
# 
# 
# 
# 
# join()
# 
# 
# 
# 
# #cancerPatientDs is for year 2004 and later
# 
# #note cancer.cleaned are these patients who meets entitlement criteria see lowRiskPCaCohort.sas
# #see getMoreRelaxedCohort_06252012.sas
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
