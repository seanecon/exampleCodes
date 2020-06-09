
# 
# bene_id Level Volume
# -----------------------------------------
#   
# ascMMA.entMajorSrgPatientsBid_2005 has 2100396 observations.
# ascMMA.entMajorSrgPatientsBid_2006 has 2074938 observations.
# ascMMA.entMajorSrgPatientsBid_2007 has 2054495 observations.
# ascMMA.entMajorSrgPatientsBid_2008 has 2029987 observations.
# ascMMA.entMajorSrgPatientsBid_2009 has 2037019 observations.
# ascMMA.entMajorSrgPatientsBid_2010 has 2043433 observations.
# ascMMA.entMajorSrgPatientsBid_2011 has 2047757 observations.
# 
# ----------------------------------------
#   

# 
# -----------------------------------------
#   
# ascMMA.abNoHmoPid_2005 has 5043779 observations.
# ascMMA.abNoHmoPid_2006 has 4897840 observations.
# ascMMA.abNoHmoPid_2007 has 4808497 observations.
# ascMMA.abNoHmoPid_2008 has 4733505 observations.
# ascMMA.abNoHmoPid_2009 has 4712034 observations.
# ascMMA.abNoHmoPid_2010 has 4808342 observations.
# ascMMA.abNoHmoPid_2011 has 4848712 observations.
# 
# ----------------------------------------
  
  
  
#   
  
  

path='Z:/j_scrdata/ascMMA/ipFacCostByQtr.csv'
costVn='X_ipFacCost'



cleanCost=function(path,costVn){
  
indf=readTableCols.yz(path
                        ,  #the variables to be kept or dropped, ifa variable specified here is not in the data, then it will stop running
                        #vns are case insensitive, if vns are missing all variables will be called
                        , keep=TRUE
                        #whether to keep the specififed vns or drop them, if FALSE then drop them
                        
                        , sep="," #if it is tab separate, then it is sep=""
                        , nrowsForCheckColClass=200 #if the data has less than 200 rows it is fine
                        , header=TRUE #I only handle cases with header=TRUE, but I just have this as an option for future
                        , unseenStop=FALSE #if FALSE, variables unseen, I still allow it to run, if TRUE then it will stop the program. 
  )


indf[,costVn]=as.numeric(indf[,costVn])
indf=rename.vars(indf,'X_qtrSince2006','qtr')
indf[,'qtr']=as.integer(indf[,'qtr'])
indf=subset(indf,qtr>0)
outdf=ddply(indf,'qtr', function(x){sum(x[,costVn])})
outdf=sortDf.yz(outdf,'qtr',c(TRUE))
return(outdf)
}






labList=seq(1,24)
pdf('C:/Dropbox/K/ascMMA/result/fig/01082014/fascByQtrNoEntRes.pdf')
plot(cleanCost('Z:/j_scrdata/ascMMA/fascCostByQtr_noEntRestriction.csv','X_fascCost'),type='b', axes=F,xlab='quarter since 2006',ylab='freestanding ASC from 20% sample ($)')
axis(side=1,at=seq(1,24),labels=F)
text(seq(1,24),par("usr")[3] - 1,labels = labList, srt = 45, pos = 1, xpd = TRUE,cex=0.9)
axis(2)
box()
dev.off()


labList=seq(1,24)
pdf('C:/Dropbox/K/ascMMA/result/fig/01082014/ipFacByQtr.pdf')
plot(cleanCost('Z:/j_scrdata/ascMMA/ipFacCostByQtr.csv','X_ipFacCost'),type='b', axes=F,xlab='quarter since 2006',ylab='inpatient facility payment among beneficiaries who meet entitlement criteria from 20% sample ($)')
axis(side=1,at=seq(1,24),labels=F)
text(seq(1,24),par("usr")[3] - 1,labels = labList, srt = 45, pos = 1, xpd = TRUE,cex=0.9)
axis(2)
box()
dev.off()

labList=seq(1,24)
pdf('C:/Dropbox/K/ascMMA/result/fig/01082014/ipFacByQtr_1010.pdf')
plot(cleanCost('Z:/j_scrdata/ascMMA/ipFacCostByQtr_1010.csv','X_ipFacCost'),type='b', axes=F,xlab='quarter since 2006',ylab='inpatient facility payment among beneficiaries who meet entitlement criteria from 20% sample ($)')
axis(side=1,at=seq(1,24),labels=F)
text(seq(1,24),par("usr")[3] - 1,labels = labList, srt = 45, pos = 1, xpd = TRUE,cex=0.9)
axis(2)
box()
dev.off()

pdf('C:/Dropbox/K/ascMMA/result/fig/01082014/ipFacByQtr_1010_differentScale.pdf')
plot(cleanCost('Z:/j_scrdata/ascMMA/ipFacCostByQtr_1010.csv','X_ipFacCost'),type='b', axes=F,xlab='quarter since 2006',ylab='inpatient facility payment among beneficiaries who meet entitlement criteria from 20% sample ($)', ylim=c(0,10e9))
axis(side=1,at=seq(1,24),labels=F)
text(seq(1,24),par("usr")[3] - 1,labels = labList, srt = 45, pos = 1, xpd = TRUE,cex=0.9)
axis(2)
box()
dev.off()



labList=seq(24)
pdf('C:/Dropbox/K/ascMMA/result/fig/01082014/costByServiceType.pdf')
plot((ofVol[,2]/qtrOkEnt)*100000, type='b', ylab='Number of claims per 100k beneficiaries who meet entitlement criteria', ylim=c(30000,111000), axes=F, pch=1,xlab='quarter since 2006')
lines((ipVol[,2]/qtrOkEnt)*100000, type='b', pch=2)
lines((fascVol[,2]/qtrOkEnt)*100000, type='b',pch=3)
lines((opVol[,2]/qtrOkEnt)*100000, type='b',pch=4)
legend(21,70000,c("office", "inpatient", "fasc", 'outpatient'), pch=seq(4))
text(seq(1,24),par("usr")[3] - 1,labels = labList, srt = 45, pos = 1, xpd = TRUE,cex=0.9)
abline(v=9,lty=3)
axis(2)
box()
dev.off()




labList=seq(1,24)
pdf('C:/Dropbox/K/ascMMA/result/fig/01082014/ipProByQtr.pdf')
plot(cleanCost('Z:/j_scrdata/ascMMA/ipProCostByQtr.csv','X_ipProCost'),type='b', axes=F,xlab='quarter since 2006',ylab='inpatient professional payment among beneficiaries who meet entitlement criteria from 20% sample ($)')
axis(side=1,at=seq(1,24),labels=F)
text(seq(1,24),par("usr")[3] - 1,labels = labList, srt = 45, pos = 1, xpd = TRUE,cex=0.9)
axis(2)
box()
dev.off()


labList=seq(1,24)
pdf('C:/Dropbox/K/ascMMA/result/fig/01082014/ipProByQtr_1010.pdf')
plot(cleanCost('Z:/j_scrdata/ascMMA/ipProCostByQtr_1010.csv','X_ipProCost'),type='b', axes=F,xlab='quarter since 2006',ylab='inpatient professional payment among beneficiaries who meet entitlement criteria from 20% sample ($)')
axis(side=1,at=seq(1,24),labels=F)
text(seq(1,24),par("usr")[3] - 1,labels = labList, srt = 45, pos = 1, xpd = TRUE,cex=0.9)
axis(2)
box()
dev.off()

labList=seq(1,24)
pdf('C:/Dropbox/K/ascMMA/result/fig/01082014/ipProByQtr_1010_differentScale.pdf')
plot(cleanCost('Z:/j_scrdata/ascMMA/ipProCostByQtr_1010.csv','X_ipProCost'),type='b', axes=F,xlab='quarter since 2006'
     ,ylim=c(0,4e8)
     ,ylab='inpatient professional payment among beneficiaries who meet entitlement criteria from 20% sample ($)')
axis(side=1,at=seq(1,24),labels=F)
text(seq(1,24),par("usr")[3] - 1,labels = labList, srt = 45, pos = 1, xpd = TRUE,cex=0.9)
axis(2)
box()
dev.off()


pdf('C:/Dropbox/K/ascMMA/result/fig/01082014/ipByQtrVol.pdf')
plot(ipVol,type='b', axes=F,xlab='quarter since 2006',ylab='inpatient Volume among beneficiaries who meet entitlement criteria from 20% sample ($)')
axis(side=1,at=seq(1,24),labels=F)
text(seq(1,24),par("usr")[3] - 1,labels = labList, srt = 45, pos = 1, xpd = TRUE,cex=0.9)
axis(2)
box()
dev.off()


labList=seq(1,24)
pdf('C:/Dropbox/K/ascMMA/result/fig/01082014/opProByQtr.pdf')
plot(cleanCost('Z:/j_scrdata/ascMMA/opProCostByQtr.csv','X_opProCost'),type='b', axes=F,xlab='quarter since 2006',ylab='outpatient professional payment from 20% sample ($)')
axis(side=1,at=seq(1,24),labels=F)
text(seq(1,24),par("usr")[3] - 1,labels = labList, srt = 45, pos = 1, xpd = TRUE,cex=0.9)
axis(2)
box()
dev.off()

labList=seq(1,24)
pdf('C:/Dropbox/K/ascMMA/result/fig/01082014/opFacByQtr.pdf')
plot(cleanCost('Z:/j_scrdata/ascMMA/opFacCostByQtr.csv','X_opFacCost'),type='b', axes=F,xlab='quarter since 2006',ylab='outpatient facility payment from 20% sample ($)')
axis(side=1,at=seq(1,24),labels=F)
text(seq(1,24),par("usr")[3] - 1,labels = labList, srt = 45, pos = 1, xpd = TRUE,cex=0.9)
axis(2)
box()
dev.off()
pdf('C:/Dropbox/K/ascMMA/result/fig/01082014/opByQtrVol.pdf')
plot(opVol,type='b', axes=F,xlab='quarter since 2006',ylab='outpatient Volume from 20% sample ($)')
axis(side=1,at=seq(1,24),labels=F)
text(seq(1,24),par("usr")[3] - 1,labels = labList, srt = 45, pos = 1, xpd = TRUE,cex=0.9)
axis(2)
box()
dev.off()



#of only reimbursed in carrier
labList=seq(1,24)
pdf('C:/Dropbox/K/ascMMA/result/fig/01082014/ofByQtr.pdf')
plot(cleanCost('Z:/j_scrdata/ascMMA/ofCostByQtr.csv','X_ofCost'),type='b', axes=F,xlab='quarter since 2006',ylab='of payment from 20% sample ($)')
axis(side=1,at=seq(1,24),labels=F)
text(seq(1,24),par("usr")[3] - 1,labels = labList, srt = 45, pos = 1, xpd = TRUE,cex=0.9)
axis(2)
box()
dev.off()

pdf('C:/Dropbox/K/ascMMA/result/fig/01082014/ofByQtrVol.pdf')
plot(ofVol,type='b', axes=F,xlab='quarter since 2006',ylab='office visit Volume from 20% sample ($)')
axis(side=1,at=seq(1,24),labels=F)
text(seq(1,24),par("usr")[3] - 1,labels = labList, srt = 45, pos = 1, xpd = TRUE,cex=0.9)
axis(2)
box()
dev.off()

#free-stadning asc only reimbursed in carrier but you can still differeniate fac and professional...I guss
labList=seq(1,24)
pdf('C:/Dropbox/K/ascMMA/result/fig/01082014/fascByQtr.pdf')
plot(cleanCost('Z:/j_scrdata/ascMMA/fascCostByQtr.csv','X_fascCost'),type='b', axes=F,xlab='quarter since 2006',ylab='free-standing ASC payment from 20% sample ($)')
axis(side=1,at=seq(1,24),labels=F)
text(seq(1,24),par("usr")[3] - 1,labels = labList, srt = 45, pos = 1, xpd = TRUE,cex=0.9)
axis(2)
box()
dev.off()



pdf('C:/Dropbox/K/ascMMA/result/fig/01082014/fascByQtrVol.pdf')
plot(fascVol,type='b', axes=F,xlab='quarter since 2006',ylab='free-standing Volume from 20% sample ($)')
axis(side=1,at=seq(1,24),labels=F)
text(seq(1,24),par("usr")[3] - 1,labels = labList, srt = 45, pos = 1, xpd = TRUE,cex=0.9)
axis(2)
box()
dev.off()



opOfAscCost=
cleanCost('Z:/j_scrdata/ascMMA/opProCostByQtr.csv','X_opProCost')[,2]+cleanCost('Z:/j_scrdata/ascMMA/opFacCostByQtr.csv','X_opFacCost')[,2]+cleanCost('Z:/j_scrdata/ascMMA/fascCostByQtr.csv','X_fascCost')[,2]+cleanCost('Z:/j_scrdata/ascMMA/ofCostByQtr.csv','X_ofCost')[,2]

pdf('C:/Dropbox/K/ascMMA/result/fig/01082014/opOfAscCostByQtrVol.pdf')
plot(cbind(seq(24),opOfAscCost/1e9),type='b', axes=F,xlab='quarter since 2006',ylab='spending on surgeries in outpatient, office visit, and freestanding ASC among beneficaries meeting entitlement criteria in 20% sample (billion dollars)')
axis(side=1,at=seq(1,24),labels=F)
text(seq(1,24),par("usr")[3] - 1,labels = labList, srt = 45, pos = 1, xpd = TRUE,cex=0.9)
axis(2)
abline(v=9,lty=3)
box()
dev.off()

pdf('C:/Dropbox/K/ascMMA/result/fig/01082014/opOfAscCostByQtrVol.pdf')
plot(cbind(seq(24),opOfAscCost/),type='b', axes=F,xlab='quarter since 2006',ylab='spending on surgeries in outpatient, office visit, and freestanding ASC among beneficaries meeting entitlement criteria in 20% sample (billion dollars)')
axis(side=1,at=seq(1,24),labels=F)
text(seq(1,24),par("usr")[3] - 1,labels = labList, srt = 45, pos = 1, xpd = TRUE,cex=0.9)
axis(2)
abline(v=9,lty=3)
box()
dev.off()


entSrgVol=rep(c(2074938, 2054495,2029987 ,2037019,2043433,2047757)/4,each=4)

entVol=rep(c(4897840,4808497,4733505,4712034,4808342,4848712)/4,each=4)

pdf('C:/Dropbox/K/ascMMA/result/fig/01082014/opOfAscCostEntSrgByQtr.pdf')
plot(cbind(seq(24),opOfAscCost/1e9),type='b', axes=F,xlab='quarter since 2006',ylab='billion dollars', main='')
title(main = list('spending on surgeries in outpatient, office visit, and freestanding ASC among beneficaries meeting entitlement criteria in 20% sample', cex = 0.7,
                  font = 3))
axis(side=1,at=seq(1,24),labels=F)
text(seq(1,24),par("usr")[3] - 1,labels = labList, srt = 45, pos = 1, xpd = TRUE,cex=0.9)
axis(2)
abline(v=9,lty=3)
box()
dev.off()

pdf('C:/Dropbox/K/ascMMA/result/fig/01082014/opOfAscCostPercaEntByQtr.pdf')
plot(cbind(seq(24),opOfAscCost/entVol),type='b', axes=F,xlab='quarter since 2006',main='',  ylab='dollar')
title(main = list('per capita surgery spending among beneficaries meeting entitlement', cex = 0.8,
                  font = 3))
axis(side=1,at=seq(1,24),labels=F)
text(seq(1,24),par("usr")[3] - 1,labels = labList, srt = 45, pos = 1, xpd = TRUE,cex=0.9)
axis(2)
abline(v=9,lty=3)
box()
dev.off()

pdf('C:/Dropbox/K/ascMMA/result/fig/01082014/opOfAscCostPercaEntSurgBeneByQtrVol.pdf')
plot(cbind(seq(24),opOfAscCost/entSrgVol),type='b', axes=F,xlab='quarter since 2006', main ='', ylab='dollar')
title(main = list('per capita surgery spending among beneficaries meeting entitlement and having surgery', cex = 0.8,
                  font = 3))
axis(side=1,at=seq(1,24),labels=F)
text(seq(1,24),par("usr")[3] - 1,labels = labList, srt = 45, pos = 1, xpd = TRUE,cex=0.9)
axis(2)
abline(v=9,lty=3)
box()
dev.off()


indf=readTableCols.yz('Z:/j_scrdata/ascMMA/ipFacCostByQtr.csv'
                      ,  #the variables to be kept or dropped, ifa variable specified here is not in the data, then it will stop running
                      #vns are case insensitive, if vns are missing all variables will be called
                      , keep=TRUE
                      #whether to keep the specififed vns or drop them, if FALSE then drop them
                      
                      , sep="," #if it is tab separate, then it is sep=""
                      , nrowsForCheckColClass=200 #if the data has less than 200 rows it is fine
                      , header=TRUE #I only handle cases with header=TRUE, but I just have this as an option for future
                      , unseenStop=FALSE #if FALSE, variables unseen, I still allow it to run, if TRUE then it will stop the program. 
)

#plot volume
path='Z:/j_scrdata/ascMMA/ofClmVolume2010'
path='Z:/j_scrdata/ascMMA/ofClmVolume2006'
#csvFileName='fascClmVol2008'

csvFileName='ofClmVol2010'
list.files(path)

path='Z:/j_scrdata/ascMMA'

csvFileList=paste(path,list.files(path),sep='/')
csvFileList[1]

qtrVn=tolower('qtrSince2006')




fascTest=getClaimLevelVol('Z:/j_scrdata/ascMMA/fascClmVolume2006', qtrVn, c("hic", "claimindex"))
opTest=getClaimLevelVol('Z:/j_scrdata/ascMMA/opClmVolume2006', qtrVn, c("hic", "claimindex"))

path.fasc='Z:/j_scrdata/ascMMA/fascClmVolume2006'
path.op='Z:/j_scrdata/ascMMA/opClmVolume2006'
csvFileList.op=paste(path.op,list.files(path.op),sep='/')
csvFileList.fasc=paste(path.fasc,list.files(path.fasc),sep='/')

fasc.1=read.csv(file=csvFileList.fasc[1], head=T, sep=',',  stringsAsFactors=F)
op.1=read.csv(file=csvFileList.op[1], head=T, sep=',',  stringsAsFactors=F)

nrow(op.1)
nrow(fasc.1)

head(op.1)
head(fasc.1)

getClaimLevelVol=function(path, qtrVn, idVns){
csvFileList=paste(path,list.files(path),sep='/')
nFile=length(csvFileList)
#indf=read.csv.yz1(csvFileName,path)
outList=list()
for (i in 1:nFile){
  cat('process', i, '\n')
  indf = read.csv(file=csvFileList[i], head=T, sep=',',  stringsAsFactors=F)
  
  indf1=readTableCols.yz(csvFileList[i]
                            ,  #the variables to be kept or dropped, ifa variable specified here is not in the data, then it will stop running
                            #vns are case insensitive, if vns are missing all variables will be called
                            , keep=TRUE
                            #whether to keep the specififed vns or drop them, if FALSE then drop them
                            
                            , sep="," #if it is tab separate, then it is sep=""
                            , nrowsForCheckColClass=200 #if the data has less than 200 rows it is fine
                            , header=TRUE #I only handle cases with header=TRUE, but I just have this as an option for future
                            , unseenStop=FALSE #if FALSE, variables unseen, I still allow it to run, if TRUE then it will stop the program. 
  )
  
  names(indf)=tolower(names(indf))
  outList=lappend.yz(outList, ddply(indf, qtrVn, function(x){c(volume=nrow(uniqueRows.yz(x[,idVns])))}) )
}
volumeOut=ddply(do.call(rbind,outList),qtrVn,function(x){c(volume=sum(x[,'volume']))})
return(volumeOut)
}


ofVol2006=getClaimLevelVol('Z:/j_scrdata/ascMMA/ofClmVolume2006', qtrVn, c("hic", "claimindex"))
ofVol2007=getClaimLevelVol('Z:/j_scrdata/ascMMA/ofClmVolume2007', qtrVn, c("hic", "claimindex"))
ofVol2008=getClaimLevelVol('Z:/j_scrdata/ascMMA/ofClmVolume2008', qtrVn, c("bene_id", "claimindex"))
ofVol2009=getClaimLevelVol('Z:/j_scrdata/ascMMA/ofClmVolume2009', qtrVn, c("bene_id", "claimindex"))
ofVol2010=getClaimLevelVol('Z:/j_scrdata/ascMMA/ofClmVolume2010', qtrVn, c("bene_id", "clm_id"))
ofVol2011=getClaimLevelVol('Z:/j_scrdata/ascMMA/ofClmVolume2011', qtrVn, c("bene_id", "clm_id"))

fascVol2006=getClaimLevelVol('Z:/j_scrdata/ascMMA/fascClmVolume2006', qtrVn, c("hic", "claimindex"))
fascVol2007=getClaimLevelVol('Z:/j_scrdata/ascMMA/fascClmVolume2007', qtrVn, c("hic", "claimindex"))
fascVol2008=getClaimLevelVol('Z:/j_scrdata/ascMMA/fascClmVolume2008', qtrVn, c("bene_id", "claimindex"))
fascVol2009=getClaimLevelVol('Z:/j_scrdata/ascMMA/fascClmVolume2009', qtrVn, c("bene_id", "claimindex"))
fascVol2010=getClaimLevelVol('Z:/j_scrdata/ascMMA/fascClmVolume2010', qtrVn, c("bene_id", "clm_id"))
fascVol2011=getClaimLevelVol('Z:/j_scrdata/ascMMA/fascClmVolume2011', qtrVn, c("bene_id", "clm_id"))

opVol2006=getClaimLevelVol('Z:/j_scrdata/ascMMA/opClmVolume2006', qtrVn, c("hic", "claimindex"))
opVol2007=getClaimLevelVol('Z:/j_scrdata/ascMMA/opClmVolume2007', qtrVn, c("hic", "claimindex"))
opVol2008=getClaimLevelVol('Z:/j_scrdata/ascMMA/opClmVolume2008', qtrVn, c("bene_id", "claimindex"))
opVol2009=getClaimLevelVol('Z:/j_scrdata/ascMMA/opClmVolume2009', qtrVn, c("bene_id", "claimindex"))
opVol2010=getClaimLevelVol('Z:/j_scrdata/ascMMA/opClmVolume2010', qtrVn, c("bene_id", "clm_id"))
opVol2011=getClaimLevelVol('Z:/j_scrdata/ascMMA/opClmVolume2011', qtrVn, c("bene_id", "clm_id"))

getClaimLevelVol.ip=function(path, qtrVn){
  csvFileList=paste(path,list.files(path),sep='/')
  nFile=length(csvFileList)
  #indf=read.csv.yz1(csvFileName,path)
  outList=list()
  for (i in 1:nFile){
    cat('process', i, '\n')
    indf = read.csv(file=csvFileList[i],head=T, sep=',',  stringsAsFactors=F)
    names(indf)=tolower(names(indf))
    outList=lappend.yz(outList, ddply(indf, qtrVn, function(x){c(volume=nrow(x))}) )
  }
  volumeOut=ddply(do.call(rbind,outList),qtrVn,function(x){c(volume=sum(x[,'volume']))})
  return(volumeOut)
}
ipVol2006=getClaimLevelVol.ip('Z:/j_scrdata/ascMMA/ipClmVolume2006', qtrVn)
ipVol2007=getClaimLevelVol.ip('Z:/j_scrdata/ascMMA/ipClmVolume2007', qtrVn)
ipVol2008=getClaimLevelVol.ip('Z:/j_scrdata/ascMMA/ipClmVolume2008', qtrVn)
ipVol2009=getClaimLevelVol.ip('Z:/j_scrdata/ascMMA/ipClmVolume2009', qtrVn)
ipVol2010=getClaimLevelVol.ip('Z:/j_scrdata/ascMMA/ipClmVolume2010', qtrVn)
ipVol2011=getClaimLevelVol.ip('Z:/j_scrdata/ascMMA/ipClmVolume2011', qtrVn)


fascVol=rbind(fascVol2006,fascVol2007,fascVol2008,fascVol2009,fascVol2010,fascVol2011)
ofVol=rbind(ofVol2006,ofVol2007,ofVol2008,ofVol2009,ofVol2010,ofVol2011)
opVol=rbind(opVol2006,opVol2007,opVol2008,opVol2009,opVol2010,opVol2011)
ipVol=subset(ddply(rbind(ipVol2006,ipVol2007,ipVol2008,ipVol2009,ipVol2010,ipVol2011)
            ,qtrVn
            ,function(x){c(volume=sum(x))}), qtrsince2006>0)

max(fascVol)
max(ofVol)
min(fascVol[,2])
plot(ofVol, type='l', ylim=c(min(ipVol[,2]),max(ofVol)))
points(ipVol[,2], type='l')
lines(fascVol[,2])
lines(opVol[,2])

plot((ofVol[,2]/qtrOkEnt)*100000, type='b', ylab='Number of office visit claims per 100k beneficiaries who meet entitlement criteria')
plot((ipVol[,2]/qtrOkEnt)*100000, type='b', ylab='Number of inpatient claims per 100k beneficiaries who meet entitlement criteria')
plot((fascVol[,2]/qtrOkEnt)*100000, type='b', ylab='Number of freestanding ASC claims per 100k beneficiaries who meet entitlement criteria')
plot((opVol[,2]/qtrOkEnt)*100000, type='b', ylab='Number of outpatient claims per 100k beneficiaries who meet entitlement criteria')

labList=seq(24)
pdf('C:/Dropbox/K/ascMMA/result/fig/01082014/volByQtr.pdf')
plot((ofVol[,2]/qtrOkEnt)*100000, type='b', ylab='Number of claims per 100k beneficiaries who meet entitlement criteria', ylim=c(30000,111000), axes=F, pch=1,xlab='quarter since 2006')
lines((ipVol[,2]/qtrOkEnt)*100000, type='b', pch=2)
lines((fascVol[,2]/qtrOkEnt)*100000, type='b',pch=3)
lines((opVol[,2]/qtrOkEnt)*100000, type='b',pch=4)
legend(21,70000,c("office", "inpatient", "fasc", 'outpatient'), pch=seq(4))
text(seq(1,24),par("usr")[3] - 1,labels = labList, srt = 45, pos = 1, xpd = TRUE,cex=0.9)
abline(v=9,lty=3)
axis(2)
box()
dev.off()

changePct=function(invec){
  outvec=rep(NA,length(invec))
  for(i in 1:length(invec)){
    outvec[i]=100*(invec[i]-invec[1])/invec[1]
  }
  return(outvec)
}

labList=seq(24)
pdf('C:/Dropbox/K/ascMMA/result/fig/01082014/volChangePctByQtr.pdf')
plot(changePct((ofVol[,2]/qtrOkEnt)*100000), type='b', ylab='volume change percent (%) (reference period is 2006 quarter 1)', ylim=c(-30,30), axes=F, pch=1,xlab='quarter since 2006')
lines(changePct((ipVol[,2]/qtrOkEnt)*100000), type='b',pch=2)
lines(changePct((fascVol[,2]/qtrOkEnt)*100000), type='b', pch=3)
lines(changePct((opVol[,2]/qtrOkEnt)*100000), type='b',pch=4)
legend(0.9,30,c("office", "inpatient", "fasc", 'outpatient'), pch=seq(4))
axis(side=1,at=seq(1,24),labels=F)
text(seq(1,24),par("usr")[3] - 1,labels = labList, srt = 45, pos = 1, xpd = TRUE,cex=0.9)
abline(v=9,lty=3)
axis(2)
box()
dev.off()



# -----------------------------------------
#   
#   ascMMA.abNoHmoPid_2006 has 4897840 observations.
# ascMMA.abNoHmoPid_2007 has 4808497 observations.
# ascMMA.abNoHmoPid_2008 has 4733505 observations.
# ascMMA.abNoHmoPid_2009 has 4712034 observations.
# ascMMA.abNoHmoPid_2010 has 4808342 observations.
# ascMMA.abNoHmoPid_2011 has 4848712 observations.
# 
# ----------------------------------------
#   
#partA and B and no HMO
qtrOkEnt=rep(round(c(4897840 ,4808497, 4733505 ,4712034 , 4808342, 4848712)/4),each=4)



# C:\Dropbox\K\ascMMA\addEntitlement.sas
# load(file="C:/Dropbox/K/ascMMA/result/vol.RData")
# save(fascVol,ofVol,opVol,ipVol,qtrOkEnt, file="C:/Dropbox/K/ascMMA/result/vol.RData")
load(file="C:/Dropbox/K/ascMMA/result/vol.RData")

# for inpatient
# -----------------------------------------
#   
#   ascMMA.ip_fac_2006 has 2110111 observations.
# ascMMA.ip_fac_2007 has 2007526 observations.
# ascMMA.ip_fac_2008 has 1941281 observations.
# ascMMA.ip_fac_2009 has 1889065 observations.
# ascMMA.ip_fac_2010 has 1851819 observations.
# ascMMA.ip_fac_2011 has 1485319 observations.
# 
# ----------------------------------------
  
# service type



cleanCostByType=function(dsPath,srvType,costVn){
  outds=readTableCols.yz(dsPath
                         ,  #the variables to be kept or dropped, ifa variable specified here is not in the data, then it will stop running
                         #vns are case insensitive, if vns are missing all variables will be called
                         , keep=TRUE
                         #whether to keep the specififed vns or drop them, if FALSE then drop them
                         
                         , sep="," #if it is tab separate, then it is sep=""
                         , nrowsForCheckColClass=200 #if the data has less than 200 rows it is fine
                         , header=TRUE #I only handle cases with header=TRUE, but I just have this as an option for future
                         , unseenStop=FALSE #if FALSE, variables unseen, I still allow it to run, if TRUE then it will stop the program. 
  )
  
  
  tmp=subset(outds,X_serviceType==srvType)[,c('X_qtrSince2006',costVn)]
  tmp[,costVn]=as.numeric(tmp[,costVn])
  outtmp=ddply(tmp, 'X_qtrSince2006', function(x){c(costSum=sum(x[,costVn]))}) 
  return(outtmp[,'costSum'])
}


pdf('C:/Dropbox/K/ascMMA/result/fig/01082014/ipProCostBySrvTypeByQtr.pdf')
plot(seq(24), cleanCostByType('Z:/j_scrdata/ascMMA/ipProCostByQtrBySvcType.csv','evaluationManagement','X_ipProCost')/10e6, type='b', ylim=c(0,20), axes=F, pch=1, xlab='quarter since 2006', ylab='Inpatient professional cost by service type (million dollars')
lines( cleanCostByType('Z:/j_scrdata/ascMMA/ipProCostByQtrBySvcType.csv','radiology','X_ipProCost')/10e6, type='b', pch=2 )
lines(  cleanCostByType('Z:/j_scrdata/ascMMA/ipProCostByQtrBySvcType.csv','pathology','X_ipProCost')/10e6, type='b', pch=3  )
lines( cleanCostByType('Z:/j_scrdata/ascMMA/ipProCostByQtrBySvcType.csv','others','X_ipProCost')/10e6, type='b', pch=4 )
legend(18,5,c("evaluation/management", "radiology", "pathology", 'others'), pch=seq(4),cex=0.75)
text(seq(1,24),par("usr")[3] - 1,labels = labList, srt = 45, pos = 1, xpd = TRUE, cex=0.9)
abline(v=9,lty=3)
axis(2)
box()
dev.off()



#office visit
pdf('C:/Dropbox/K/ascMMA/result/fig/01082014/ofCostBySrvTypeByQtr.pdf')
plot(seq(24), cleanCostByType('Z:/j_scrdata/ascMMA/ofCostByQtrBySvcType.csv','evaluationManagement','X_ofCost')/10e6, type='b', ylim=c(0,20), axes=F, pch=1, xlab='quarter since 2006', ylab='Office-visit cost by service type (million dollars')
lines( cleanCostByType('Z:/j_scrdata/ascMMA/ofCostByQtrBySvcType.csv','radiology','X_ofCost')/10e6, type='b', pch=2 )
lines(  cleanCostByType('Z:/j_scrdata/ascMMA/ofCostByQtrBySvcType.csv','pathology','X_ofCost')/10e6, type='b', pch=3  )
lines( cleanCostByType('Z:/j_scrdata/ascMMA/ofCostByQtrBySvcType.csv','others','X_ofCost')/10e6, type='b', pch=4 )
legend(18,11,c("evaluation/management", "radiology", "pathology", 'others'), pch=seq(4),cex=0.75)
text(seq(1,24),par("usr")[3] - 1,labels = labList, srt = 45, pos = 1, xpd = TRUE, cex=0.9)
abline(v=9,lty=3)
axis(2)
box()
dev.off()


#outpatient
pdf('C:/Dropbox/K/ascMMA/result/fig/01082014/opProCostBySrvTypeByQtr.pdf')
plot(seq(24), cleanCostByType('Z:/j_scrdata/ascMMA/opProCostByQtrBySvcType.csv','evaluationManagement','X_opProCost')/10e6, type='b', ylim=c(0,20), axes=F, pch=1, xlab='quarter since 2006', ylab='Outpatient professional cost by service type (million dollars')
lines( cleanCostByType('Z:/j_scrdata/ascMMA/opProCostByQtrBySvcType.csv','radiology','X_opProCost')/10e6, type='b', pch=2 )
lines(  cleanCostByType('Z:/j_scrdata/ascMMA/opProCostByQtrBySvcType.csv','pathology','X_opProCost')/10e6, type='b', pch=3  )
lines( cleanCostByType('Z:/j_scrdata/ascMMA/opProCostByQtrBySvcType.csv','others','X_opProCost')/10e6, type='b', pch=4 )
legend(18,20,c("evaluation/management", "radiology", "pathology", 'others'), pch=seq(4),cex=0.75)
text(seq(1,24),par("usr")[3] - 1,labels = labList, srt = 45, pos = 1, xpd = TRUE, cex=0.9)
abline(v=9,lty=3)
axis(2)
box()
dev.off()


#fasc
pdf('C:/Dropbox/K/ascMMA/result/fig/01082014/fascCostBySrvTypeByQtr.pdf')
plot(seq(24), cleanCostByType('Z:/j_scrdata/ascMMA/fascCostByQtrBySvcType.csv','evaluationManagement','X_fascCost')/10e6, type='b', ylim=c(0,20), axes=F, pch=1, xlab='quarter since 2006', ylab='Freestadning ASC cost by service type (million dollars')
lines( cleanCostByType('Z:/j_scrdata/ascMMA/fascCostByQtrBySvcType.csv','radiology','X_fascCost')/10e6, type='b', pch=2 )
lines(  cleanCostByType('Z:/j_scrdata/ascMMA/fascCostByQtrBySvcType.csv','pathology','X_fascCost')/10e6, type='b', pch=3  )
lines( cleanCostByType('Z:/j_scrdata/ascMMA/fascCostByQtrBySvcType.csv','others','X_fascCost')/10e6, type='b', pch=4 )
legend(18,8,c("evaluation/management", "radiology", "pathology", 'others'), pch=seq(4),cex=0.75)
text(seq(1,24),par("usr")[3] - 1,labels = labList, srt = 45, pos = 1, xpd = TRUE, cex=0.9)
abline(v=9,lty=3)
axis(2)
box()
dev.off()