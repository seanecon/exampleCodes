

#load table1.yz function
source('C:/Dropbox/R_new/example/table1_improved.R')
#for cancer pateints
#read in ses data

interestIniTreatments <- c('abdomen_radpromy','robot_radpromy','ebrt','imrt','obs')

genRevisionData <- function(interestIniTreatments){


#cancerPatientDs_03222013 is already first and only and meet entitlement and dx 2004-2009
demo.1 <- subset(xpt2r.yz('Z:/j_scrdata/pcaOverTreatment','cancerPatientDs_03222013'),!registry=='47' & x.dxage>=66)
nrow(demo.1)
#83811 (removing reg 47 and meet entitlement) (not removing reg47 is 91609 )
#further remove age<66 get 83191
cat('remove reg47 and meet entitlement left',nrow(demo.1),'\n')

demo.1 <- rename.vars(demo.1,c('.first.dx.year','.first.dx.month'),c('indexyear','indexmonth'),info=FALSE)
if (!'indexday' %in% names(demo.1)){demo.1[,'indexday'] <- '15'} #index day does not exist then we need to add it
if (!'birthd' %in% names(demo.1)){demo.1[,'birthd'] <- '15'} #index day does not exist then we need to add it

#now need to caculate age at index time.
demo.1[,'indexYMD'] <- paste(demo.1[,'indexyear'],demo.1[,'indexmonth'],demo.1[,'indexday'],sep='-')
demo.1[,'birthYMD'] <- paste(demo.1[,'birthyr'],demo.1[,'birthm'],demo.1[,'birthd'],sep='-')
sourceFileOneByOne.yz('C:/Dropbox/Q_new/datemani_new')
demo.1[,'indexage'] <- numDaysBtCharYmd.yz(demo.1[,'indexYMD'],demo.1[,'birthYMD'])/365
demo.1 <- deldfcols.yz(demo.1,c('birthm','birthd','birthyr'))
#next, we need to remove missing and recategorize the variables.

#first missing comorb should be put to 0
demo.1[is.na(demo.1[,'chrlson']),'chrlson'] <- 0
#further append grade and stage information from seer
#getDemoTable.sas
# 
# '1' = White
# '2' = Black
# '3' = Other
# '4' = Asian
# '5' = Hispanic
# '6' = N. Am. Native
# '0' = Unknown

demo.1[,'raceCat'] <- replaceValJoin.yz(demo.1[,'race'] #this is typically a data column
                                       ,list(c('1'),c('2'),c('3','6'),'4','5','0')
                                       ,c('white','black','other','asian','hispanic',NA)
                                       ,origValVn='race'
                                       ,newValVn='raceCat' #if output is vector, this newValVn is inapplicable
                                       ,outputJoinedDf=TRUE #if F means only output the new vector
                                       #if T, then output df             
)[,2]

demo.1[,'grdCat'] <- replaceValJoin.yz(demo.1[,'.gradecat'] #this is typically a data column
                                        ,list('poorUndi','restType','wellmodD')
                                        ,c('poorUndiff',NA,'wellmodDiff')
                                        ,origValVn='.gradecat'
                                        ,newValVn= #if output is vector, this newValVn is inapplicable
                                        ,outputJoinedDf=TRUE #if F means only output the new vector
                                        #if T, then output df             
)[,2]

sesCuts <-c(-Inf,quantile(demo.1[,'x.ses'],probs=c(0.333,0.667,1),na.rm=T))
demo.1 <- grpnv.supplycuts.yz (demo.1, 'x.ses', sesCuts, 'sesCat')


ageCuts <-c(-Inf,70,75,80,85,Inf)
demo.1 <- grpnv.supplycuts.yz (demo.1, 'indexage', ageCuts, 'ageCat')


demo.1[,'chrlsonRecat'] <- replaceValJoin.yz(demo.1[,'chrlson'] #this is typically a data column
                                                    ,list(c(0),c(1),c(2),c(3),c(4),c(5),c(6),seq(7,20))
                                                    ,c('0','1','2','3','4','5','6','7+')
                                                    ,origValVn='chrlson'
                                                    ,newValVn='chrsonRecat' #if output is vector, this newValVn is inapplicable
                                                    ,outputJoinedDf=TRUE #if F means only output the new vector
                                                    #if T, then output df             
)[,2]


demo.1[,'stgCat'] <- replaceValJoin.yz(demo.1[,'.tstage.grp.simple'] #this is typically a data column
                                             ,list('<=T1','T1','T2','T3','T4','missing')
                                             ,c('<=T1','T1','T2','T3','T4',NA)
                                             ,origValVn='.stggrp.simple'
                                             ,newValVn='stgCat' #if output is vector, this newValVn is inapplicable
                                             ,outputJoinedDf=TRUE #if F means only output the new vector
                                             #if T, then output df             
)[,2]




#now further remove mets
demo.2 <- subset(demo.1,x.metsind==0)

cat('remove mets left',nrow(demo.2),'\n')

cat('by dx year sample size, after removing mets, after entitlement and age66pluse')
print(count(demo.2,'indexyear'))


(load(file='Z:/j_scrdata/pcaOverTreatmentRevision/iniTrtDf.RData'))

demo.3 <- subset(join(demo.2, iniTrtDf,by='regcase'),iniTrt %in% interestIniTreatments)
cat('restricting sample to interest treatment further remove',nrow(demo.2)-nrow(demo.3),'\n')

demo.3[which(is.na(demo.3[,'stgCat'])),'.damicorisk'] <- NA

rm(demo.1)

#88781 to 81168 by removing reg=47
demo.3[which(demo.3[,'.damicorisk']=='missing'),'.damicorisk'] <- NA

missingReport <- missingSeq.yz(demo.3,c('indexyear','.damicorisk','ageCat','raceCat','grdCat','stgCat','sesCat','chrlsonRecat','urban','registry'))
print(missingReport)

missingReport <- missingSeq.yz(demo.3,c('indexyear','ageCat','raceCat','.damicorisk','grdCat','stgCat','sesCat','chrlsonRecat','urban','registry'))

print(missingReport)

demo.4 <- demo.3[complete.cases(demo.3[,c('ageCat','raceCat','grdCat','stgCat','.damicorisk','sesCat','chrlsonRecat','urban','registry','iniTrt')]),]

demo.4[,'race'] <- replaceValJoin.yz(demo.4[,'race'] #this is typically a data column
                                                       ,list(c('1'),c('2'),c('3'),c('4'),c('5','6'))
                                                       ,c('1','2','3','4','5'))[,2]

cat('loading nonCaner survival model','\n')
(load(file='Z:/j_scrdata/pcaOverTreatment/allSampleFit.RData'))
demo.4 <- rename.vars(demo.4,'x.ses','ses',info=FALSE)
predProb <- predict(allSampleFit,demo.4,type='prob') #predProb[,2] is 10 year death prob

cat('creating predicted prob 3 groups','\n')
demo.4 <- grpnv.yz(data.frame(demo.4,surv10YrProb=predProb[,1]),'surv10YrProb',3,'surv10YrProb3Cat')
demo.4[,'surv10YrProb3Cat'] <- replaceValJoin.yz(demo.4[,'surv10YrProb3Cat'] #this is typically a data column
                                                       ,names(table(demo.4[,'surv10YrProb3Cat']))
                                                       ,c('surv10Yr.low','surv10Yr.mid','surv10Yr.high')
)[,2]
cat('creating 9 risk strata based on .damico risk and survival','\n')


#regroup chrlson and age
demo.4 <- remove.vars(demo.4,c('ageCat','chrlsonRecat'))
ageCuts <-c(-Inf,70,75,80,Inf)
demo.4 <- grpnv.supplycuts.yz (demo.4, 'indexage', ageCuts, 'ageCat')


demo.4[,'chrlsonRecat'] <- replaceValJoin.yz(demo.4[,'chrlson'] #this is typically a data column
                                             ,list(c(0),c(1),c(2),seq(3,20))
                                             ,c('0','1','2','3+')
                                             ,origValVn='chrlson'
                                             ,newValVn='chrsonRecat' #if output is vector, this newValVn is inapplicable
                                             ,outputJoinedDf=TRUE #if F means only output the new vector
                                             #if T, then output df             
)[,2]


# 
 demo.4[,'riskStrata'] <- catvars2onecatvar.yz(demo.4, c('.damicorisk','surv10YrProb3Cat'), ' | ')
return(demo.4)
}


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
