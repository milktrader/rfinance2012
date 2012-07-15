require(quantmod)
 
getSymbols("GLD")
 
#for(i in seq(5, 15, 5))
#  for(j in seq(50, 80, 15))
 
{
    GLD$fast     <- SMA(Cl(GLD), n=i)     
    GLD$slow     <- SMA(Cl(GLD), n=j)         
 
    golden_cross <- Lag(ifelse(GLD$fast > GLD$slow, 1, -1))
    golden_cross <- na.locf(golden_cross, na.rm=TRUE)
         
    coin         <- ROC(Cl(GLD))*golden_cross
    best_coin    <- max(coin)
    worst_coin   <- min(coin)
    last_coin    <- cumprod(1+coin)[NROW(coin),]  
     
    annual_coin   <- round((last_coin-1)*100, digits=2)/(NROW(coin)/252)
 
    cat(i,j,annual_coin, best_coin, worst_coin, "\n", file="~/goldcat", append=TRUE)  
    cat(i,j,annual_coin, best_coin, worst_coin, "\n")  
}
