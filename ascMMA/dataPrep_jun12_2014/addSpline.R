# 
# save(anaDfNoBs,file='Z:/j_scrdata/ascMMA/result/anaDfNoBs.RData')

(load(file='Z:/j_scrdata/ascMMA/result/anaDfNoBs.RData'))

xtabs(~MMA2008.impact+year,data=anaDfNoBs)

addBs=function(knots.tiles, anaDfNoBs){
  
  
  bslist=bsWrapper1.yz(anaDfNoBs[,'.qtr.since2006'] #this is the x in bs function
                       , quantile(anaDfNoBs[,'.qtr.since2006'],probs=knots.tiles) #inner knots, if it contains boundary knots, function will stop and issue erro message
                       , paste('.qtr.since2006','.bs',sep='') #output data's columen name stem
                       , degree=2
                       , dataType='data.frame'
                       , Boundary.knots=quantile(anaDfNoBs[,'.qtr.since2006'],prob=c(0,1))
  )
  n.basis=bslist$n.basis
  anaDfWithBs=cbind(anaDfNoBs,bslist$bsdata)
  
  names(anaDfWithBs)=tolower(names(anaDfWithBs))
  
  return(anaDfWithBs)
}


#first add bs
anaDfWithBs=addBs(c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9), anaDfNoBs)
