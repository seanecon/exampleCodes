
n=100000
vec=rnorm(n)
meanVec=rep(NA,100)
for (i in 1:100){
meanVec[i]=mean( sample(vec,round(n/5)) )
}

sd(meanVec)

