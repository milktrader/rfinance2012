osSandbox <- function (timestamp,orderqty,portfolio,symbol,ruletype,...) {

  suppressMessages(require(PerformanceAnalytics))

  updatePortf(port, sym, Date=paste0('::',as.Date(Sys.time())))
  updateAcct(acct)

#  tempPort      = getPortfolio(port)
#  dummy         = updatePortf(port, Dates=paste0('::' ,as.Date(timestamp)))
#  trading.pl    = sum(getPortfolio(port)$summary$Net.Trading.PL) 

  assign(paste0("portfolio.",portfolio), tempPort, pos=.blotter) 


  returns       = PortfReturns(acct)
  eCurve        = returns + 1
  full          = SMA(eCurve, n=10)
  half          = SMA(eCurve, n=10)
  flat          = SMA(eCurve, n=10)
  
#  total.equity  = initEq+trading.pl 
  tradeSize     = total.equity * trade.percent
  ClosePrice    = as.numeric(Cl(mktdata[timestamp,])) 
  orderqty      =  sign(orderqty)*round(tradeSize/ClosePrice) 

  return(orderqty)
}
