




#this provide predition of probabiltiy based on point estimation of betas and cuts
predOrProbitRi.pointEst = function(opRiFit #in formula, you always put random intercpet at the end
                                   ,newData
                                   #need to make sure the new data has the same levels as modelData
                                   ,nlagVn
                                   ,add.eblup=T){
  J=length(opRiFit$y.levels) #number of ordered choices
  cuts=opRiFit$alpha; betaVec=opRiFit$beta
  
  formu=as.list(opRiFit$call)$formula
  rhs.formu=paste('~',strsplit(as.character(formu), "~")[[3]],sep='')
  dataMat=model.matrix(as.formula(rhs.formu),data=newData)
  XB=dataMat[,-c(1,ncol(dataMat))] %*% matrix(betaVec,ncol=1)
  
  
  
  
  
  
  if(add.eblup){
    eblupDf= as.data.frame(opRiFit$ranef)
    names(eblupDf)='eblup'
    eblupDf=rowname2vn.yz(eblupDf,rownameVn=names(opRiFit$ranef))
    XBPlusRi=XB+join(newData, eblupDf,by=names(opRiFit$ranef))[,'eblup']
    #I did a check on the correlation between eblup and outcome and confirm that it should be PLUS not minus
  } else{
    
  }
  
  
  cumProbMat=matrix(NA,nrow=length(XBPlusRi),ncol=J)
  
  for (j in 1:(J-1)){
    cumProbMat[,j]=pnorm(cuts[j]-XBPlusRi)
  }
  cumProbMat[,J]=1
  probMat=array(NA,dim=dim(cumProbMat))
  probMat[,1]=cumProbMat[,1]
  for(j in 2:J){
    probMat[,j]=cumProbMat[,j]-cumProbMat[,j-1]
  }
  colnames(probMat)=opRiFit$y.levels
  return(probMat)
}
