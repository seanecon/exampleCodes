gen.bs=function(lagvol.vec
                , knots
                , bdknots
                ,degree
                , vnStem #e.g., 'lag6.nlap.bs'   
){
  outdf=cubic.bs.tmp(lagvol.vec,knots,vnStem,degree,bdknots)
  return(outdf)
}