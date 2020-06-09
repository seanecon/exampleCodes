indf=xpt2r.yz('Z:/j_scrdata/ascMMA','anci_cath_2004')
head(indf,10)
idVn.1='hic'
idVn.2='sexpndt1'
vnVec=c('linepmt','sexpndt1', 'revpmt', 'srev.dt')

vnVec=c('linepmt','revpmt')

inpath='Z:/j_scrdata/ascMMA'
indsn='anci_cath_2004'
codesVec=c('93510', '93543','93545','93555','93556')
freqVec=c(1,1,1,1,1)
get.nAnci.cost=function( codesVec
                        #c('93510', '93543','93545','93555','93556')
                        ,freqVec
                        #c(1,1,1,1,1)
                        ,inpath
                        #'Z:/j_scrdata/ascMMA'
                        ,indsn
                        #'anci_cath_2004'
                        ){
  raw=xpt2r.yz(inpath,indsn)
  #numericVnVec=c('sexpndt1', 'srev.dt')
  pmtVec=maxOrMinNumericValByRow.yz(raw,c('linepmt','revpmt'),'max' ,list(0,0))
  dateVec=maxOrMinNumericValByRow.yz(raw,c('sexpndt1', 'srev.dt'),'max' ,list(0,0))  
  indf.1=cbind(raw,payAmt=pmtVec, dateVn=dateVec)
  indf=subset(indf.1,payAmt>0)
  testout=dlply(indf,c(idVn.1, idVn.2),function(x){
    vec=x[,'hcpcs.cd'] 
    #print(x[,c(idVn.1,idVn.2)])
   
    out.delete=deleteVecValue.yz(vec,codesVec,freqVec)
    leftOverRows=out.delete$leftOverLoc
    return(x[leftOverRows,])
  })
  
  testout[1:3]
  
  head(indf)
  
  
  
  n.ancillary=unlist(lapply(testout,nrow))
  mean(n.ancillary)
  #2.528
  median(n.ancillary)
  #1
  ancillary.cost=unlist(lapply(testout,function(x){sum(x[,'payAmt'])}))
  median(ancillary.cost)
  
  trouble=testout[which(ancillary.cost>200)]
  lapply(trouble[1:100],function(x){x[,c(1,2,7,8)]})
  
}


#258.44
#25.06
#25.06