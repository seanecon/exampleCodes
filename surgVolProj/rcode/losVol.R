we probably need to argue that 

Robotic surgery starts from 2003. 
If I see very few operation prior to 2003, then I would be safe to argue
the most lap was actually robot

if so, I can then use cumulative learning curve.
Otherwise, I can only argue for equilibrium level proficiency.




testLos <- xpt2r.yz('Z:/j_scrdata/nisRobotAdoption', 'losLapRpVolume20032009')

testLos <- xpt2r.yz('Z:/j_scrdata/lapLearn','_allLap')
testLos[,'x.nisyear'] <- as.numeric(testLos[,'nisyear'])


testLos[,'x.nisyear'] <- as.numeric(testLos[,'x.nisyear'])
testLos <- rename.vars(testLos,'x.nisyear','nisyear')
testLos <- subset(testLos,hospst!="FL")
head(testLos)
# 
# testLos <- xpt2rDfList.yz('Z:/j_scrdata/nisRobotAdoption',c('losLapRpVolume2008','losLapRpVolume2009'))

head(testLos)
col.type.yz(testLos)
outlist <- lapply(split(testLos,testLos[,'mdnum1.r']),function(x){count(x,'nisyear')})
freqVec <- unlist(lapply(outlist,nrow))
numSrgPred <- subset(testLos,mdnum1.r %in% as.numeric(names(outlist)[which(freqVec >1)]))
names(testLos)

testLos[,'aday'] <- 1

testLos[,'srgCharDate'] <- cancatVars.yz(testLos[,c('nisyear','amonth','aday')], sep='-')

testLos[,'srgDaySinceJan12003'] <- numDaysBtCharYmd.yz(testLos[,'srgCharDate'],'2003-1-1')


testLos[,'srgMonSinceJan12003'] <- floor(testLos[,'srgDaySinceJan12003']/30)

numSrgByPersonMonth <- ddply(testLos,c('mdnum1.r','srgMonSinceJan12003'),function(x){c(numSrg=nrow(x))})

head(numSrgByPersonMonth)
numSrgByPersonMonth[,'mdnum1.r'] <- as.factor(numSrgByPersonMonth[,'mdnum1.r'])
numSrgByPersonMonth[,'srgMonSinceJan12003.sq'] <- numSrgByPersonMonth[,'srgMonSinceJan12003']^2
numSrgByPersonMonth[,'srgMonSinceJan12003.cu'] <- numSrgByPersonMonth[,'srgMonSinceJan12003']^3



fitObj <- lm(log(numSrg) ~ srgMonSinceJan12003+srgMonSinceJan12003.cu+srgMonSinceJan12003.sq+mdnum1.r,data=numSrgByPersonMonth)


names(numSrgByPersonMonth)
fitObj <- lm(log(numSrg) ~ srgMonSinceJan12003+srgMonSinceJan12003.sq+mdnum1.r,data=numSrgByPersonMonth)

summary(fitObj)
hist(log(numSrgByPersonMonth[,'numSrg']))

trend <- ddply(numSrgByPersonMonth,'srgMonSinceJan12003',function(x){sum(x[,'numSrg'])/nrow(x)})
plot(trend)

x <- seq(1,max(numSrgByPersonMonth[,'srgMonSinceJan12003'],na.rm=T))
x.sq=x^2
x.cu=x^3

coef(fitObj)[1:4]
#fit a poisson regression
head(numSrgByPersonMonth)

lm(~srgMonSinceJan12003,data=testLos)



#remove FL, which does not report admission months
#we should be able to use cumulative (before surgory month) volumn and current los to figure out the issue.

#since NIS sample is 20 percent sample, we cannot get surgeon prsotate cancer record contiounously...

ddply()



numDaysBtCharYmd.yz(charYMD,charYMD.original)



col.type.yz(testLos)
str(testLos)
names(testLos)
# testLos <- do.call(rbind,testLos$dfList)
volLos <- ddply(subset(testLos,nisyear %in% c(2003,2010)),'mdnum1.r',function(x){c(mean(x[,'los']), nrow(x))})
volLos <- rename.vars(volLos,c('V1','V2'),c('avgLos','annual.vol'))
plot(volLos[,'annual.vol'],volLos[,'avgLos'])

#LOS is a continous integer not a continous variable...this a problem/
  
table(testLos[,'los'])
hist(volLos[,'avgLos'],100)

modelDf <- data.frame(los=volLos[,'avgLos'],vol=volLos[,'annual.vol'], volSq=volLos[,'annual.vol']^2)



qudraticFit <- lm(log(los)~vol+volSq,data=modelDf)
qudraticFit <- lm(los~vol+volSq,data=modelDf)
coef(qudraticFit)
summary(qudraticFit)
c <- coef(qudraticFit)[1]
b <- coef(qudraticFit)[2]
a <- coef(qudraticFit)[3]
minLos.vol <- -0.5*b/a

x <- seq(0,floor(minLos.vol))
y.hat <- (a*x^2+b*x+c)
plot(x,y.hat)

max(modelDf[,'annual.vol'])
cor(volLos[,'annual.vol'],volLos[,'avgLos'],method='kendall')

testLos2008 <- xpt2r.yz('Z:/j_scrdata/nisRobotAdoption', 'losLapRpVolume2008')
volLos2008 <- ddply(testLos2008,'mdnum1.r',function(x){c(mean(x[,'los']), nrow(x))})
volLos2008 <- rename.vars(volLos2008,c('V1','V2'),c('avgLos','annual.vol'))
cor(volLos2008[,'annual.vol'],volLos2008[,'avgLos'],method='kendall')
