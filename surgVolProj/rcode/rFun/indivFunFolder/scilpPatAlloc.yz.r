#older version is down at the bottom
#internally it solves a minimization problem not maxmization (G list is length of stay, so solve minimiization)

scilpPatAlloc.yz <- function( N
                              , G.list #G is a list of payoff, with 0 patient, 1, 2......a maximum number of patients
                              , allTreated=TRUE
                              , time_limit=10
                              , node_limit=500
                              , first_feasible=TRUE){
  J <- length(G.list)
  #note if TM_OPTIMAL_SOLUTION_FOUND==0 means found optimal, if NULL then not found
  Mvec <- unlist(lapply(G.list,length))-1
  Mmax <- max(Mvec)
  
  if (allTreated){stopifnot(sum(Mvec)>=N)}
  
  constMat <- matrix(rep(0,(J+1)*sum(Mvec+1)),nrow=(J+1))
  constrColnameList <- list()
  for (j in 1:J){
    for (i in 0:Mvec[j]) {constrColnameList<-lappend.yz(constrColnameList, paste(j,':',i,sep=''))}
  }                   
  
  constList.Jrows <-list()
  
  for(j in 1:J){
    constrMat.Jrows.jthDoc = matrix(rep(0,J*(Mvec[j]+1)),nrow=J)
    constrMat.Jrows.jthDoc[j,] <- 1
    constList.Jrows <- lappend.yz(constList.Jrows,constrMat.Jrows.jthDoc)
  }
  
  Jplus1List <- list()
  for(j in 1:J){Jplus1List <- lappend.yz(Jplus1List,c(0,seq(Mvec[j])))}
  Jplus1Row <- unlist(Jplus1List)                     
  constMat <- rbind(do.call('cbind',constList.Jrows),Jplus1Row)
  colnames(constMat) <- unlist(constrColnameList)
  rownames(constMat) <- c(paste('doc',seq(J),sep=''),'all.doc')
  if (allTreated){constDir <- c(rep('==', J),'==')} else {constDir <- c(rep('==',J),'<=')}
  
  rhs <- c(rep(1,J),N)
  types <- rep("B", J*(Mmax+1))
  
  Gvec=unlist(G.list)
  #ensure when you want, all patints be treated it is possible
  stopifnot(length(Gvec) == sum(Mvec+1))
  
  fit <- Rsymphony_solve_LP(Gvec, constMat, constDir, rhs, type=types, max = F, time_limit=time_limit, node_limit=node_limit,first_feasible=first_feasible) #false means minimize
  
  opt.n <- subset(data.frame(chosenDecision=fit$solution, n.treated=Jplus1Row),chosenDecision==1)[,'n.treated']
  
  #names(opt.n) <- paste('j=',seq(J),sep='')
  
  fit$opt.n <- opt.n
  fit$total.opt.n <- sum(opt.n)
  fit$solution <- NULL
  return(fit)
}








#internally it solves a minimization problem not maxmization (G list is length of stay, so solve minimiization)

scilpPatAlloc.yz.older <- function(N, G.list #G is a list of payoff, with 0 patient, 1, 2......a maximum number of patients
                             , allTreated=TRUE){
  J <- length(G.list)
  #note if TM_OPTIMAL_SOLUTION_FOUND==0 means found optimal, if NULL then not found
  Mvec <- unlist(lapply(G.list,length))-1
  Mmax <- max(Mvec)
  
  if (allTreated){stopifnot(sum(Mvec)>=N)}
  
  constMat <- matrix(rep(0,(J+1)*sum(Mvec+1)),nrow=(J+1))
  constrColnameList <- list()
  for (j in 1:J){
    for (i in 0:Mvec[j]) {constrColnameList<-lappend.yz(constrColnameList, paste(j,':',i,sep=''))}
  }                   
  
  constList.Jrows <-list()
  
  for(j in 1:J){
    constrMat.Jrows.jthDoc = matrix(rep(0,J*(Mvec[j]+1)),nrow=J)
    constrMat.Jrows.jthDoc[j,] <- 1
    constList.Jrows <- lappend.yz(constList.Jrows,constrMat.Jrows.jthDoc)
  }
  
  Jplus1List <- list()
  for(j in 1:J){Jplus1List <- lappend.yz(Jplus1List,c(0,seq(Mvec[j])))}
  Jplus1Row <- unlist(Jplus1List)                     
  constMat <- rbind(do.call('cbind',constList.Jrows),Jplus1Row)
  colnames(constMat) <- unlist(constrColnameList)
  rownames(constMat) <- c(paste('doc',seq(J),sep=''),'all.doc')
  if (allTreated){constDir <- c(rep('==', J),'==')} else {constDir <- c(rep('==',J),'<=')}
  
  rhs <- c(rep(1,J),N)
  types <- rep("B", J*(Mmax+1))
  
  Gvec=unlist(G.list)
  #ensure when you want, all patints be treated it is possible
  stopifnot(length(Gvec) == sum(Mvec+1))
  
  fit <- Rsymphony_solve_LP(Gvec, constMat, constDir, rhs,type=types, max = F) #false means minimize
  opt.n <- subset(data.frame(chosenDecision=fit$solution,n.treated=Jplus1Row),chosenDecision==1)[,'n.treated']
  
  names(opt.n) <- paste('j=',seq(J),sep='')
  fit$opt.n <- opt.n
  fit$total.opt.n <- sum(opt.n)
  fit$solution <- NULL
  return(fit)
}
