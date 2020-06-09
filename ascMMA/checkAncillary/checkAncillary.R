
indf=xpt2r.yz('Z:/j_scrdata/ascMMA','_anci_66984_2005')

head(indf)

sortRatePerEncounterDay=function(dfn,code){
  #dfn='_anci_66984_2004'
  #code='66984' 
  indf=xpt2r.yz('Z:/j_scrdata/ascMMA',dfn)
  uniqBidDay=uniqueRows.yz(indf[,'bid.day',drop=F])
  non_code=subset(indf,!hcpcs.cd==code)  
  freqdf=count(non_code,'hcpcs.cd')
  freqdf[,'userate.perencounter']=freqdf[,'freq']/nrow(uniqBidDay)
  outdf=sortDf.yz(freqdf,'userate.perencounter',F)
  
  return(outdf)
  
}

year=2004
yearList=seq(2004,2011)

allyear_code=function(code, yearList){
  
  outlist=list()
  
for (year in yearList){
  cat('processing', year,'\n')
  dfn=paste('_anci_',code,'_',year,sep='')
  outlist=lappend.yz(outlist,sortRatePerEncounterDay(dfn,code))
}
 return(outlist)
}

list_66984=allyear_code('66984',seq(2004,2011))

list_66984[[1]]
yearVec=seq(2004,2011)

fromList2Df=function(inList, yearVec){

  for(i in 1:length(yearVec)){
    inList[[i]]=data.frame(inList[[i]],year=yearVec[i])
  }
  outdf=do.call(rbind,inList)
}

chgAmt=function(freqDf){ 
  rangeDf=ddply(freqDf,'hcpcs.cd',function(x){
    if (nrow(x)==8){
  rate.2011=x[which(x[,'year']==2011),'userate.perencounter']; 
  rate.2004=x[which(x[,'year']==2004),'userate.perencounter'];
  rate.diff.2011vs2004=rate.2011-rate.2004;
    }
    else {rate.2011=rate.2004=  rate.diff.2011vs2004=NA }
  return(c(rate.2011=rate.2011,rate.2004=rate.2004
           ,rate.diff.2011vs2004=rate.diff.2011vs2004
           ))
})
  
  outdf.des=sortDf.yz(rangeDf,'rate.diff.2011vs2004',F)
  outdf.asc=sortDf.yz(rangeDf,'rate.diff.2011vs2004',T)
  outList=list(des=outdf.des,asc=outdf.asc)
  return(outList) 
}


freqDf.66984=fromList2Df(allyear_code('66984',seq(2004,2011)), seq(2004,2011))
#freqDf.93458=fromList2Df(allyear_code('93458',seq(2004,2011)), seq(2004,2011))
freqDf.43239=fromList2Df(allyear_code('43239',seq(2004,2011)), seq(2004,2011))
freqDf.45380=fromList2Df(allyear_code('45380',seq(2004,2011)), seq(2004,2011))
freqDf.45385=fromList2Df(allyear_code('45385',seq(2004,2011)), seq(2004,2011))


freqDf.45378=fromList2Df(allyear_code('45378',seq(2004,2011)), seq(2004,2011))
freqDf.66982=fromList2Df(allyear_code('66982',seq(2004,2011)), seq(2004,2011))
freqDf.52000=fromList2Df(allyear_code('52000',seq(2004,2011)), seq(2004,2011))
freqDf.29826=fromList2Df(allyear_code('29826',seq(2004,2011)), seq(2004,2011))
freqDf.47562=fromList2Df(allyear_code('47562',seq(2004,2011)), seq(2004,2011))

freqDf.49505=fromList2Df(allyear_code('49505',seq(2004,2011)), seq(2004,2011))
freqDf.29827=fromList2Df(allyear_code('29827',seq(2004,2011)), seq(2004,2011))
freqDf.50590=fromList2Df(allyear_code('50590',seq(2004,2011)), seq(2004,2011))


# save(freqDf.66984, freqDf.43239,freqDf.45380,freqDf.45385,freqDf.45378,freqDf.66982,freqDf.52000,freqDf.29826,freqDf.47562,
# freqDf.49505,freqDf.29827,freqDf.50590,file='Z:/j_scrdata/ascMMA/freqDf.RData')
load('Z:/j_scrdata/ascMMA/freqDf.RData')

chg.66984.list=chgAmt(freqDf.66984)
chg.43239.list=chgAmt(freqDf.43239)
chg.45380.list=chgAmt(freqDf.45380)
chg.45385.list=chgAmt(freqDf.45385)
chg.45378.list=chgAmt(freqDf.45378)
chg.66982.list=chgAmt(freqDf.66982)
chg.52000.list=chgAmt(freqDf.52000)
chg.29826.list=chgAmt(freqDf.29826)
chg.47562.list=chgAmt(freqDf.47562)

head(chg.66984.list[[1]],10)
head(chg.66984.list[[2]],10)
ddply(freqDf.66984,'year',function(x){c(num.anci=sum(x[,'userate.perencounter']))})

head(chg.43239.list[[1]],10)
head(chg.43239.list[[2]],10)
ddply(freqDf.43239,'year',function(x){c(num.anci=sum(x[,'userate.perencounter']))})

head(chg.45380.list[[1]],10)
head(chg.45380.list[[2]],10)
ddply(freqDf.45380,'year',function(x){c(num.anci=sum(x[,'userate.perencounter']))})

head(chg.45385.list[[1]],10)
head(chg.45385.list[[2]],10)
ddply(freqDf.45385,'year',function(x){c(num.anci=sum(x[,'userate.perencounter']))})

head(chg.45378.list[[1]],10)
head(chg.45378.list[[2]],10)
ddply(freqDf.45378,'year',function(x){c(num.anci=sum(x[,'userate.perencounter']))})

head(chg.66982.list[[1]],10)
head(chg.66982.list[[2]],10)
ddply(freqDf.66982,'year',function(x){c(num.anci=sum(x[,'userate.perencounter']))})

head(chg.52000.list[[1]],10)
head(chg.52000.list[[2]],10)
ddply(freqDf.52000,'year',function(x){c(num.anci=sum(x[,'userate.perencounter']))})


head(chg.29826.list[[1]],10)
head(chg.29826.list[[2]],10)
ddply(freqDf.29826,'year',function(x){c(num.anci=sum(x[,'userate.perencounter']))})


head(chg.47562.list[[1]],10)
head(chg.47562.list[[2]],10)
ddply(freqDf.47562,'year',function(x){c(num.anci=sum(x[,'userate.perencounter']))})

