demoRaw = xpt2r.yz("Z:/j_scrdata/lapLearn",'_demo_iamdny')
demoRaw[,'key']=as.character(demoRaw[,'key'])
demoRaw[,'x.iny'] = NULL
demoRaw[,'quarterSince2003'] = 4*(demoRaw[,'year']-2003)+demoRaw[,'dqtr']

names(demoRaw)




srgSrchFile.ia=xpt2r.yz("Z:/j_scrdata/lapLearn","srgSearchFile_ia")
srgSrchFile.ia[,'key']= as.character(srgSrchFile.ia[,'key'])

srgSrchFile.md=xpt2r.yz("Z:/j_scrdata/lapLearn","srgSearchFile_md")
srgSrchFile.md[,'key']= as.character(srgSrchFile.md[,'key'])

srgSrchFile.ny=xpt2r.yz("Z:/j_scrdata/lapLearn","srgSearchFile_ny")
srgSrchFile.ny[,'key']= as.character(srgSrchFile.ny[,'key'])


keyVec.raw.ia=unlist(subset(demoRaw,pstate=='IA',select='key'))
keyVec.raw.md=unlist(subset(demoRaw,pstate=='MD',select='key'))
keyVec.raw.ny=unlist(subset(demoRaw,pstate=='NY',select='key'))

keyVec.ia=setdiff.yz(keyVec.raw.ia,srgSrchFile.ia[,'key'],'inboth')
keyVec.md=setdiff.yz(keyVec.raw.md,srgSrchFile.md[,'key'],'inboth')
keyVec.ny=setdiff.yz(keyVec.raw.ny,srgSrchFile.ny[,'key'],'inboth')


lag1Vol.ia = grab.surgAmount(keyVec.ia,1,srgSrchFile.ia)
lag1Vol.md = grab.surgAmount(keyVec.md,1,srgSrchFile.md)
lag1Vol.ny = grab.surgAmount(keyVec.ny,1,srgSrchFile.ny)
#save(lag1Vol.ia,lag1Vol.md,lag1Vol.ny,file='Z:/j_scrdata/lapLearn/surgVolLag1.RData')


lag2Vol.ia = grab.surgAmount(keyVec.ia,2,srgSrchFile.ia)
lag2Vol.md = grab.surgAmount(keyVec.md,2,srgSrchFile.md)
lag2Vol.ny = grab.surgAmount(keyVec.ny,2,srgSrchFile.ny)

#save(lag2Vol.ia,lag2Vol.md,lag2Vol.ny,file='Z:/j_scrdata/lapLearn/surgVolLag2.RData')

lag3Vol.ia = grab.surgAmount(keyVec.ia,3,srgSrchFile.ia)
lag3Vol.md = grab.surgAmount(keyVec.md,3,srgSrchFile.md)
lag3Vol.ny = grab.surgAmount(keyVec.ny,3,srgSrchFile.ny)
#save(lag3Vol.ia,lag3Vol.md,lag3Vol.ny,file='Z:/j_scrdata/lapLearn/surgVolLag3.RData')

lag4Vol.ia = grab.surgAmount(keyVec.ia,4,srgSrchFile.ia)
lag4Vol.md = grab.surgAmount(keyVec.md,4,srgSrchFile.md)
lag4Vol.ny = grab.surgAmount(keyVec.ny,4,srgSrchFile.ny)


lag5Vol.ia = grab.surgAmount(keyVec.ia,5,srgSrchFile.ia)
lag5Vol.md = grab.surgAmount(keyVec.md,5,srgSrchFile.md)
lag5Vol.ny = grab.surgAmount(keyVec.ny,5,srgSrchFile.ny)


lag6Vol.ia = grab.surgAmount(keyVec.ia,6,srgSrchFile.ia)
lag6Vol.md = grab.surgAmount(keyVec.md,6,srgSrchFile.md)
lag6Vol.ny = grab.surgAmount(keyVec.ny,6,srgSrchFile.ny)


#save(lag4Vol.ia,lag4Vol.md,lag4Vol.ny,file='Z:/j_scrdata/lapLearn/surgVolLag4.RData')

# save(lag1Vol.ia,lag1Vol.md,lag1Vol.ny,file='Z:/j_scrdata/lapLearn/surgVolLag1_may082013.RData')
# save(lag2Vol.ia,lag2Vol.md,lag2Vol.ny,file='Z:/j_scrdata/lapLearn/surgVolLag2_may082013.RData')
# save(lag3Vol.ia,lag3Vol.md,lag3Vol.ny,file='Z:/j_scrdata/lapLearn/surgVolLag3_may082013.RData')
# save(lag4Vol.ia,lag4Vol.md,lag4Vol.ny,file='Z:/j_scrdata/lapLearn/surgVolLag4_may082013.RData')

# save(lag5Vol.ia,lag5Vol.md,lag5Vol.ny,file='Z:/j_scrdata/lapLearn/surgVolLag5_may082013.RData')
# save(lag6Vol.ia,lag6Vol.md,lag6Vol.ny,file='Z:/j_scrdata/lapLearn/surgVolLag6_may082013.RData')

sum(sapply(list(lag4Vol.ia, lag4Vol.md, lag4Vol.ny),nrow)) #6821
sum(sapply(list(lag6Vol.ia, lag6Vol.md, lag6Vol.ny),nrow)) #6821

sum(sapply(list(lag4Vol.ia, lag4Vol.md, lag4Vol.ny),function(x){unilen.yz(x[,'mdId'])})) #223

head(lag4Vol.ia)
keyVec=keyVec.ny[3053]
srgSrchFile=srgSrchFile.ny

subset(srgSrchFile.ia,key=='19200410226511')
subset(lag1Vol.ia.1, key=='19200410226511')
subset(withVol, key=='19200410226511')
keyVec=keyVec.md[1]


lagk=2
srgSrchFile=srgSrchFile.md

grab.surgAmount=function(keyVec,lagK,srgSrchFile){
  outList=list()
  
  #there are very few which has missing in dqtr
  #if so we set dqtr to 1
  srgSrchFile[which(is.na(srgSrchFile[,'dqtr'])),'dqtr']=1
  
srgSrchFile[,'quarterSince2003'] = 4*(srgSrchFile[,'year']-2003) + srgSrchFile[,'dqtr']
  mdIdVec=rep(NA,length(keyVec))
for(i in 1:length(keyVec)){ 
 #cat('processing',i,'\n')
  rowIndex = min(which(srgSrchFile[,'key']==keyVec[i]))
  mdID.i=srgSrchFile[rowIndex,'mdnum'] #I could have obtain mdnum1_r here, but demo file also has it. so I grab it there.
  refQuarter = srgSrchFile[rowIndex,c('quarterSince2003')]
  if (refQuarter<(lagK+1) ){n.lagk.lap=n.lagk.rap=n.lagk.allsrg=n.lagk.nonLap=NA} else{
    lagk.allSrg = subset(srgSrchFile, mdnum==mdID.i & quarterSince2003 < refQuarter & quarterSince2003 >= refQuarter-lagK )
    if (!is.null(lagk.allSrg) & nrow(lagk.allSrg)>0){ 
      isLap=isRap=rep(0,nrow(lagk.allSrg))
      for(ii in 1:nrow(lagk.allSrg)){
      #  cat('ii=',ii,'\n')
        isRap[ii]=any(lagk.allSrg[ii,c('pr1','pr2','pr3','pr4','pr5','pr6')]=='605',na.rm=T)
        isLap[ii]=isRap[ii] & any(lagk.allSrg[ii,c('pr1','pr2','pr3','pr4','pr5','pr6')]=='5421',na.rm=T)
      }     
      n.lagk.allsrg=nrow(lagk.allSrg)
      n.lagk.lap=sum(isLap)
      n.lagk.rap=sum(isRap)
      n.lagk.nonLap=n.lagk.allsrg-n.lagk.lap   
    } else{n.lagk.lap=n.lagk.rap=n.lagk.allsrg=n.lagk.nonLap=0}      
  }
outvec=c(n.lagk.lap,n.lagk.rap,n.lagk.allsrg,n.lagk.nonLap)
names(outvec)=c('n.lagk.lap','n.lagk.rap','n.lagk.allsrg','n.lagk.nonLap')  
outList=lappend.yz(outList,outvec)
mdIdVec[i]=mdID.i
  
}
outDf=data.frame(mdId=mdIdVec, key=keyVec,do.call(rbind,outList)) 
return(outDf)
}

rm(outList)
