osEquityCurve <- function (timestamp,orderqty,portfolio,symbol,ruletype,...) {

  tempPortfolio = getPortfolio(portfolio)
  dummy         = updatePortf(Portfolio=portfolio, Dates=paste( :: ,as.Date(timestamp),sep= ))
  trading.pl    =  sum(getPortfolio(portfolio)$summary$Net.Trading.PL) 
  assign(paste("portfolio.",portfolio,sep=""),tempPortfolio,pos=.blotter) 
  total.equity  = initEq+trading.pl 
  tradeSize     = total.equity * trade.percent
  ClosePrice    = as.numeric(Cl(mktdata[timestamp,])) 
  orderqty      =  sign(orderqty)*round(tradeSize/ClosePrice) 
  return(orderqty)
}
