funstats <- function(returns) {

  print('A histogram is being plotted now ...')
  chart.Histogram(returns)

  ifelse(last(SMA(returns, n=10)) > last(SMA(returns, n=30)), 
         print('Your system is winning!'), 
         print('Your system is losing!'))
        
  cat('The Annualized Sharpe Ratio is: ',  
       SharpeRatio.annualized(returns), 
       '\n')








}
