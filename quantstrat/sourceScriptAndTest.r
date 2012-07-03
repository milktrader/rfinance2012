require(svUnit)

source('bee.SPY.r')

TradeStats.SPY = 'Testing consistent trade stats'

test(TradeStats.SPY) = function() {

  stats = tradeStats(port)

  checkEquals( stats$Num.Txns, 75)
  checkEquals( stats$Num.Trades, 37)
  checkEquals( stats$Net.Trading.PL, 2468)
  checkEquals( stats$Largest.Winner, 3302)
  checkEquals( stats$Largest.Loser, -1099)
  checkEquals( stats$Gross.Profits, 12914)
  checkEquals( stats$maxDrawdown, -4346)
  checkEquals( stats$Max.Equity, 3902)
  checkEquals( stats$Min.Equity, -1506)
}


clearLog()
runTest(TradeStats.SPY)

########################## CLEANUP ENVIRONMENT #####################

suppressWarnings(rm("order_book.bug",pos=.strategy))
suppressWarnings(rm("account.colony","portfolio.bug",pos=.blotter))
suppressWarnings(rm("sym","port","acct","initEq","initDate","fast",'slow','sd'))



source('bee.GLD.r')

TradeStats.GLD= 'Testing consistent trade stats'

test(TradeStats.GLD) = function() {

  stats = tradeStats(port)

  checkEquals( stats$Num.Txns, 67)
  checkEquals( stats$Num.Trades, 33)
  checkEquals( stats$Net.Trading.PL, 2142)
  checkEquals( stats$Largest.Winner, 1860)
  checkEquals( stats$Largest.Loser, -1773)
  checkEquals( stats$Gross.Profits, 9543)
  checkEquals( stats$maxDrawdown, -4431)
  checkEquals( stats$Max.Equity, 4970)
  checkEquals( stats$Min.Equity, -2052)
}

runTest(TradeStats.GLD)
Log()
#summary(Log())
