
#usually you use this to change value of a named vector by names instead of location

changeNamedVecValue.yz=function(namedVec,namedNewValVec=NULL){
  
  if (!is.null(namedNewValVec)) {
    
    match.loc=match(names(namedNewValVec),names(namedVec))
    if(any(is.na(match.loc))){
      naloc=which(is.na(match.loc))
      warning('The following values can not be replaced', namedNewValVec[naloc], '\n')
    }
    namedVec[match.loc]=namedNewValVec
  }
  return(namedVec)
}

#changeNamedVecValue.yz(c(a=1,b=2,c=3),c(a=3))
#changeNamedVecValue.yz(c(a=1,b=2,c=3))
#changeNamedVecValue.yz(c(a=1,b=2,c=3),1)
