 plotEquityCurve <- function(x) { 
   
   x$eq = NA
   
   for( i in 1:nrow(x)){
       
       x$eq[i,] =  apply(x[,1][1:i], 2, Return.cumulative)}

   plot(x$eq + 1)

 }
   
