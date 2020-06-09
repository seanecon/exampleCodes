
# 
# ageEodSpecTranprob <- array(NA,c(length(dx.hs.vec),NP,NP, length(hs.vec),length(hs.vec)),dimnames=list(ini.dx.hs=dx.hs.vec, dxDur=seq(0,NP-1), currentAge=seq(NP), from.hs=hs.vec, to.hs=hs.vec))
# 
# ageEodSpecTranprob[1,2,3,,]
# #test assigning this big tranProb
# testProbMat <- rbind(c(0.95,0.03,0,0.02),c(0,0.80,0.17,0.03),c(0,0,0.9,0.1),c(0,0,0,1))
# dimnames(testProbMat) <-list(from.hs=hs.vec, to.hs=hs.vec) 
# 
# afill(ageEodSpecTranprob[1,1,1,,]) <- testProbMat
# 
# 
# N=5
# dx.hs.vec <- hs.vec <- c('A','B','C','D')
# n.hs <-length(hs.vec)
# disease.hs.vec <- dx.hs.vec[2:(length(dx.hs.vec)-1)]
# NP <- 3
# 
# iniDxHsIndxVec <- c(NA,2,3,3,2)
# dxDurVec <- c(NA,0,1,2,3) #NA means not diagnosed yet, 0 means instantly diangosed
# # iniDxHsIndxVec <- rep(NA,N)
# # currentAgeVec <- rep(NA,N)
# # dxDurVec <- rep(NA,N)
# #eodMat is a logical matrix.
# currentAgeVec <- c(3,3,3,3,3)
# iniDxHsIndxVec #should be a number >=2 and <=n.h-1
# dxDurVec
# currentHsIndxVec

eod2tranprob <- function( iniDxHsIndxVec
                         ,dxDurVec
                         ,currentAgeVec
                         ,currentHsIndxVec
                         ,ageEodSpecTranprob){
N <- length(iniDxHsIndxVec)
hs.vec <- dimnames(ageEodSpecTranprob)$'to.hs'
n.hs <- length(hs.vec)
probMat <- array(NA,nrow=N,ncol=n.hs,dimnames=list(sid=seq(N),hs=hs.vec))
for (i in 1:N){  
# tranprobMat <- ageEodSpecTranprob[iniDxHsIndxVec[i],dxDurVec[i],currentAgeVec[i],1:n.hs,1:n.hs,drop=T]
# probMat[i,] <- tranprobMat[currentHsIndxVec[i],]
probMat[i,]<- ageEodSpecTranprob[iniDxHsIndxVec[i],dxDurVec[i],currentAgeVec[i],1:n.hs,1:n.hs,drop=T][currentHsIndxVec[i],]
}
return(probMat)
}













  
  






