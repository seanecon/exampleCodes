
solveOptAllocation_noPrivateInforamtion=function(n.surgery, G.list, random.seeds, picked.n.md=10){
  #you cannot solve the problem by feeding all the MD
  #however, you can randomly pick MDs assume you have no prior information about MD quality
  avgVol=optNumMd=minLos=rep(NA,length(random.seeds))
  optSolution.list=list()
  for(i in 1:length(random.seeds)){
    set.seed(random.seeds[i])
    pickedMdIndx = sample(seq(length(G.list)), picked.n.md, replace=FALSE)  
    G.list.picked=G.list[pickedMdIndx]
    opt.obj=scilpPatAlloc.yz(n.surgery
                             , G.list.picked #G is a list of payoff, with 0 patient, 1, 2......a maximum number of patients
                             , allTreated=TRUE)
    minLos[i]=opt.obj$objval
    optNumMd[i]=length(which(opt.obj$opt.n>0))
    avgVol[i]=sum(opt.obj$opt.n)/optNumMd[i]
    optSolution.list=lappend.yz(optSolution.list,opt.obj)
  }
  summaryDf=cbind(random.seeds, minLos, optNumMd, avgVol)
  names(summaryDf)=c('random.seeds', 'minLos', 'optNumMd', 'avgVol')
  return(list(optSolution.summary=summaryDf, optSolution.list=optSolution.list))
}

