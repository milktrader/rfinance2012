require(quantmod)
getSymbols('spy')
SPY$bull = ifelse(SMA(Cl(SPY), n=50) > SMA(Cl(SPY), n=200), 1, 0)
SPY$bear = ifelse(SMA(Cl(SPY), n=30) < SMA(Cl(SPY), n=100), -1, 0)
SPY$pos = SPY$bull + SPY$bear
spy = na.omit(SPY)
nrow(spy[spy$pos == 1])/nrow(spy) 
nrow(spy[spy$pos == -1])/nrow(spy) 

