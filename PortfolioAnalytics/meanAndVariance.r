meanAndVariance = function(sym='GLD')
{
  require(quantmod)
  ticker = getSymbols(sym, from='1901-01-01', auto.assign=FALSE)
  ret    = monthlyReturn(Ad(ticker))
  myvar  = var(ret)
  mymean = mean(ret)

  cat(sym,  'mean is: ', mymean, '\n', 'variance is: ', myvar, '\n')
}



