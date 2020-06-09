#overtrtr.bf12af12_firstOnly_dx0409 has 91609 observations.

iniTrtList <- list()
for(i in 1:100){
profileDf <- xpt2r.yz('Z:/j_scrdata/pcaOverTreatmentRevision/treatmentProfileBatch',paste('trtProfileBat',i,sep=''))
iniTrtList <- lappend.yz(iniTrtList,ddply(profileDf,'regcase',assignIniTreatment))
print(i)
}



iniTrtDf.withCpt <- do.call('rbind',iniTrtList)
iniTrtDf.withCpt <- rename.vars(iniTrtDf.withCpt,'V1','iniTrt')

allReg <- xpt2r.yz('Z:/j_scrdata/pcaOverTreatmentRevision','bf12af12_firstOnly_dx0409')[,'patient.id']

nonTrtReg <- setdiff(allReg,iniTrtDf.withCpt[,'regcase'])

iniTrtDf.withoutCpt <-  data.frame(regcase=nonTrtReg,iniTrt='obs')
iniTrtDf <- rbind(iniTrtDf.withCpt,iniTrtDf.withoutCpt)
#save(iniTrtDf,iniTrtList, file='Z:/j_scrdata/pcaOverTreatmentRevision/iniTrtDf.RData')
set.seed(1)
plotDf <- iniTrtDf[sample(nrow(iniTrtDf),3000),]

count.yz(plotDf,'V1',getPercent=T)
vecpct <- count.yz(plotDf,'V1',getPercent=T)[,3]
names(vecpct)<-count.yz(plotDf,'V1',getPercent=T)[,1]

pdf(file='Z:/j_scrdata/pcaOverTreatmentRevision/trtPct.pdf')
barplot(vecpct,las=3,cex.names=0.53)
dev.off()


which(is.na(iniTrtDf[,'iniTrt']))

