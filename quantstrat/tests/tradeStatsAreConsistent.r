require(testthat)

################## bee.r  #########################

source('beeTEST.r')

stats.one = tradeStats(port)

txns1       = stats.one$Num.Txns
trades1     = stats.one$Num.Trades
maxeq1      = stats.one$Max.Equity
maxdraw1    = stats.one$maxDrawdown
largwin1    = stats.one$Largest.Winner
pfactor1    = stats.one$Profit.Factor

suppressWarnings(rm(list=ls(all=TRUE))

################## bbands.R #########################

source('bbandsTEST.r')

stats.two = tradeStats(portfolio.st)

txns2       = stats.two$Num.Txns
trades2     = stats.two$Num.Trades
maxeq2      = stats.two$Max.Equity
maxdraw2    = stats.two$maxDrawdown
largwin2    = stats.two$Largest.Winner
pfactor2    = stats.two$Profit.Factor

suppressWarnings(rm(list=ls(all=TRUE))

################## faber.R #########################

source('faberTEST.r')

stats.three  = tradeStats('faber')

txns3       = last(stats.three$Num.Txns)
trades3     = last(stats.three$Num.Trades)
maxeq3      = last(stats.three$Max.Equity)
maxdraw3    = last(stats.three$maxDrawdown)
largwin3    = last(stats.three$Largest.Winner)
pfactor3    = last(stats.three$Profit.Factor)

suppressWarnings(rm(list=ls(all=TRUE))

################## faberMC.R #########################

source('faberMCTEST.r')

stats.four  = tradeStats(symbols)

txns4       = last(stats.four$Num.Txns)
trades4     = last(stats.four$Num.Trades)
maxeq4      = last(stats.four$Max.Equity)
maxdraw4    = last(stats.four$maxDrawdown)
largwin4    = last(stats.four$Largest.Winner)
pfactor4    = last(stats.four$Profit.Factor)

suppressWarnings(rm(list=ls(all=TRUE))

################## luxor.R #########################

source('luxorTEST.r')

stats.five  = tradeStats(p)

txns5       = stats.five$Num.Txns
trades5     = stats.five$Num.Trades
maxeq5      = stats.five$Max.Equity
maxdraw5    = stats.five$maxDrawdown
largwin5    = stats.five$Largest.Winner
pfactor5    = stats.five$Profit.Factor

suppressWarnings(rm(list=ls(all=TRUE))

################## maCros.R #########################

source('maCrossTEST.r')

stats.six  = tradeStats(portfolio.st)

txns6       = stats.six$Num.Txns
trades6     = stats.six$Num.Trades
maxeq6      = stats.six$Max.Equity
maxdraw6    = stats.six$maxDrawdown
largwin6    = stats.six$Largest.Winner
pfactor6    = stats.six$Profit.Factor

suppressWarnings(rm(list=ls(all=TRUE))

################## macd.R #########################

source('macdTEST.r')

stats.seven  = tradeStats(port)

txns7       = stats.seven$Num.Txns
trades7     = stats.seven$Num.Trades
maxeq7      = stats.seven$Max.Equity
maxdraw7    = stats.seven$maxDrawdown
largwin7    = stats.seven$Largest.Winner
pfactor7    = stats.seven$Profit.Factor

suppressWarnings(rm(list=ls(all=TRUE))

################## pair_trade.R #########################

source('pair_tradeTEST.r')

stats.eight  = tradeStats(portfolio.st)

txns8       = last(stats.eight$Num.Txns)
trades8     = last(stats.eight$Num.Trades)
maxeq8      = last(stats.eight$Max.Equity)
maxdraw8    = last(stats.eight$maxDrawdown)
largwin8    = last(stats.eight$Largest.Winner)
pfactor8    = last(stats.eight$Profit.Factor)

suppressWarnings(rm(list=ls(all=TRUE))

################## rocema.R #########################
#
#source('rocemaTEST.r')
#
#stats.nine  = tradeStats(port)
#
#txns9       = stats.nine$Num.Txns
#trades9     = stats.nine$Num.Trades
#maxeq9      = stats.nine$Max.Equity
#maxdraw9    = stats.nine$maxDrawdown
#largwin9    = stats.nine$Largest.Winner
#pfactor9    = stats.nine$Profit.Factor
#
#suppressWarnings(rm(list=ls(all=TRUE))
#
################### rsi.R#########################
#
#source('rsiTEST.r')
#
#stats.ten  = tradeStats(portfolio.st)
#
#txns10       = stats.ten$Num.Txns
#trades10     = stats.ten$Num.Trades
#maxeq10      = stats.ten$Max.Equity
#maxdraw10    = stats.ten$maxDrawdown
#largwin10    = stats.ten$Largest.Winner
#pfactor10    = stats.ten$Profit.Factor
#
#suppressWarnings(rm(list=ls(all=TRUE))
#
######################## RUN TEST SUITE #######################

context('Consistency across trade statistics')

test_that('Number of transactions is consistent', {

  expect_that(txns1, equals(67))
  expect_that(txns2, equals(151))
  expect_that(txns3, equals(10))
  expect_that(txns4, equals(15))
  expect_that(txns5, equals(15))
  expect_that(txns6, equals(11))
  expect_that(txns7, equals(27))
  expect_that(txns8, equals(92))
#  expect_that(txns9, equals(161))
#  expect_that(txns10, equals(161))
})

test_that('Number of the number of trades is consistent', {

  expect_that(trades1, equals(33))
  expect_that(trades2, equals(68))
  expect_that(trades3, equals(5))
  expect_that(trades4, equals(15))
  expect_that(trades5, equals(7))
  expect_that(trades6, equals(5))
  expect_that(trades7, equals(13))
  expect_that(trades8, equals(31))
#  expect_that(trades9, equals(80))
#  expect_that(trades10, equals(80))
})

test_that('Max equity is consistent', {

  expect_that(maxeq1, equals(4970))
  expect_that(maxeq2, equals(8964))
  expect_that(maxeq3, equals(6021980))
  expect_that(maxeq4, equals(21705))
  expect_that(maxeq5, equals(730))
  expect_that(maxeq6, equals(54739))
  expect_that(maxeq7, equals(48918.75))
  expect_that(maxeq8, equals(3877.741))
#  expect_that(maxeq9, equals(5044))
#  expect_that(maxeq10, equals(5044))
})

test_that('Max drawdown is consistent', {

  expect_that(maxdraw1, equals(-4431))
  expect_that(maxdraw2, equals(-5269))
  expect_that(maxdraw3, equals(-2321310))
  expect_that(maxdraw4, equals(-6800))
  expect_that(maxdraw5, equals(-1340))
  expect_that(maxdraw6, equals(-13434))
  expect_that(maxdraw7, equals(-8374))
  expect_that(maxdraw8, equals(-30979.74))
#  expect_that(maxdraw9, equals(-56019))
#  expect_that(maxdraw10, equals(-56019))
})

test_that('Largest winner is consistent', {

  expect_that(largwin1, equals(1860))
  expect_that(largwin2, equals(1681))
  expect_that(largwin3, equals(2891500))
  expect_that(largwin4, equals(10835))
  expect_that(largwin5, equals(340))
  expect_that(largwin6, equals(5018))
  expect_that(largwin7, equals(16265))
  expect_that(largwin8, equals(7185.448))
#  expect_that(largwin9, equals(34054))
#  expect_that(largwin10, equals(34054))
})

test_that('Profit Factor is consistent', {

  expect_that(pfactor1, equals(1.337491))
  expect_that(pfactor2, equals(1.935963))
  expect_that(pfactor3, equals(4.904525))
  expect_that(pfactor4, equals(4.9181818))
  expect_that(pfactor5, equals(0.9404762))
  expect_that(pfactor6, equals(1.561851))
  expect_that(pfactor7, equals(8.801109))
  expect_that(pfactor8, equals(0.6433525))
#  expect_that(pfactor9, equals(0.8401456))
#  expect_that(pfactor10, equals(0.8401456))
})



