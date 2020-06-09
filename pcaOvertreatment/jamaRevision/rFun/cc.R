# i am writing this code to solve a modular problem
# this function will solve the counter clockwise case.

cClock = function(b,s,r,nc){
  # calcuate the total steps in r runs
  n=s*r
  # the position after r runs is p
  p = b-n
  
  #the final location is actually mod of p
  
  e= p %% nc
  
  return(e)
}






