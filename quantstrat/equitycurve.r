ec <- function(x){

  x[1,2] = x[1,1] * 100
  for(i in 2:nrow(x))

    x[i, 2] = x[i-1,2] * x[i,1]
   
  return(x) 
}


