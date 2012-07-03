###### Place this at the bottom of your trade demo ##################

# replace 'port' arg in stats = tradeStats(port) with the name of your portfolio
# insert correct values for the 9 trade stats

###############################################################

require(svUnit)

TradeStats = 'Testing consistent trade stats'

test(TradeStats) = function() {

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
runTest(TradeStats)
Log()
summary(Log())
