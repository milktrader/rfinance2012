 plotEquityCurve <- function(x) { 
   
   nam  = deparse(substitute(x)) 
   x$eq = NA
   
   for( i in 1:nrow(x)){
       
       x$eq[i,] =  apply(x[,1][1:i], 2, Return.cumulative)}

   plot(x$eq + 1, main=nam, las=1, xaxt='n')

 }
   
