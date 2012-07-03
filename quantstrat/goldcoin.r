require(quantmod)
 
getSymbols("GLD")
 
for(i in seq(5, 15, 5))
    for(j in seq(50, 80, 15))
       
    {
        GLD$fast     <- SMA(Cl(GLD), n=i)     
        GLD$slow     <- SMA(Cl(GLD), n=j)         
         
         goldencross <- Lag(ifelse(GLD$fast > GLD$slow, 1, -1))
         goldencross <- na.locf(goldencross, na.rm=TRUE)
                     
         coin         <- ROC(Cl(GLD))*goldencross
         bestcoin    <- max(coin)
         worstcoin   <- min(coin)
         lastcoin    <- cumprod(1+coin)[NROW(coin),]  
                         
         annualcoin   <- round((lastcoin-1)*100, digits=2)/(NROW(coin)/252)
                     
         cat(i,j,annualcoin, bestcoin, worstcoin, "\n", file="~/goldcat", append=TRUE)  
         cat(i,j,annualcoin, bestcoin, worstcoin, "\n")  
    }
