naiveRebalance = function(x, bond=.60, us=.15, intl=.15, commod=.1){ 
for(i in 1: nrow(x)) {
  x[i,1] = bond
  x[i,2] = us 
  x[i,3] = intl
  x[i,4] = commod
}
  return(x)
}
