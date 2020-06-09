
#admission month may not be available for some people
#so we are going to use quarter....past year.

#attending and operation are actually very much the same. all should be counted into experience.
#what  I need is a by physician id, by quarter, surgical amount data

genLapVolByQuarter = function(lapRec){
srgVolDf = lapRec[,c('key','whatyear','dqtr','mdnum1.r','mdnum2.r')]
srgVolDf=srgVolDf[complete.cases(srgVolDf),]

srgVolDf <- rename.vars(srgVolDf,c('mdnum1.r','mdnum2.r'),c('mdnum.1','mdnum.2'))
srgVolDf.long = melt(srgVolDf, id=c('whatyear','dqtr','key'))
srgVolDf.long[,'variable'] = NULL
srgVolDf.long[,'key'] = as.character(srgVolDf.long[,'key'])
unirowDf = uniqueRows.yz(srgVolDf.long) #one single surgeon id may be used twice for one single surgeory
unirowDf= rename.vars(unirowDf,'value','surgeon.id')

output = ddply(unirowDf,c('whatyear','dqtr','surgeon.id'),function(x){c(nLap=nrow(x))})
surgeon.ids = unique(output[,'surgeon.id'])
length(surgeon.ids) #246 surgeons caring 6928 LAP
gridDf = expand.grid.yz(seq(2003,2010),c(1,2,3,4),surgeon.ids)
gridDf = rename.vars(gridDf,c('Var1','Var2','Var3'),c('whatyear','dqtr','surgeon.id'))
lapVolDf = join(gridDf,output,by=c('whatyear','dqtr','surgeon.id'))
lapVolDf[is.na(lapVolDf[,'nLap']),'nLap'] = 0

lapVolDf[,'quarterSince2003']=4*(lapVolDf[,'whatyear']-2003)+lapVolDf[,'dqtr']
lapVolDf= lapVolDf[,c('surgeon.id','quarterSince2003','whatyear','dqtr','nLap')]
nLapByQuarter= sortDf.yz(lapVolDf,c('surgeon.id','quarterSince2003'))
return(nLapByQuarter)
}


genLapVolCumPastKQuarter = function(nLapByQuarter,k){
outvec = rep(NA,nrow(nLapByQuarter))
 for(i in 1:nrow(nLapByQuarter)){
   if  (nLapByQuarter[i,'quarterSince2003']>k) {subDf = subset(nLapByQuarter,surgeon.id==nLapByQuarter[i,'surgeon.id'] & quarterSince2003<nLapByQuarter[i,'quarterSince2003'] & quarterSince2003>=nLapByQuarter[i,'quarterSince2003']-k);
   outvec[i]=sum(subDf[,'nLap'])
   }
 }
outdf=data.frame(nLapByQuarter,outvec)
names(outdf)[length(names(outdf))]=paste('cum.nLap.last',k,sep='')
return(outdf)
}

sum(is.na(genLapVolByQuarter(lapRec.ny)[,'nLap']))
#read in lap record
lapRec.ny = xpt2r.yz("Z:/j_scrdata/lapLearn",'lap_ny_20032010')
cum.nLap.ny = genLapVolCumPastKQuarter(
  genLapVolCumPastKQuarter(
    genLapVolCumPastKQuarter(
      genLapVolCumPastKQuarter(
        genLapVolByQuarter(lapRec.ny)
        ,1)
      ,2)
    ,3)
  ,4)

cum.nLap.ny[,'hospst']='NY'


lapRec.ia = xpt2r.yz("Z:/j_scrdata/lapLearn",'lap_ia_20032010')
cum.nLap.ia = genLapVolCumPastKQuarter(
  genLapVolCumPastKQuarter(
    genLapVolCumPastKQuarter(
      genLapVolCumPastKQuarter(
        genLapVolByQuarter(lapRec.ia)
        ,1)
      ,2)
    ,3)
  ,4)

cum.nLap.ia[,'hospst']='IA'


lapRec.md = xpt2r.yz("Z:/j_scrdata/lapLearn",'lap_md_20032010')
cum.nLap.md = genLapVolCumPastKQuarter(
  genLapVolCumPastKQuarter(
    genLapVolCumPastKQuarter(
      genLapVolCumPastKQuarter(
        genLapVolByQuarter(lapRec.md)
        ,1)
      ,2)
    ,3)
  ,4)

cum.nLap.md[,'hospst']='MD'

#now we have obtain, surgeon cumulative (past k quarter) vol data).
#this data has a surgeon's past k quarter volumn by each quarter since 2003
#save(cum.nLap.ia, cum.nLap.md, cum.nLap.ny, file="Z:/j_scrdata/lapLearn/cum.nLap.RData")

load(file="Z:/j_scrdata/lapLearn/cum.nLap.RData")

