require(testthat)

################## bee.r  #########################

source('beeTEST.r')

stats.first = tradeStats(port)

txns1       = stats.first$Num.Txns
trades1     = stats.first$Num.Trades
maxeq1      = stats.first$Max.Equity
maxdraw1    = stats.first$maxDrawdown
largwin1    = stats.first$Largest.Winner
pfactor1    = stats.first$Profit.Factor

suppressWarnings(rm("order_book.bug",pos=.strategy))
suppressWarnings(rm("account.colony","portfolio.bug",pos=.blotter))
suppressWarnings(rm("sym","port","acct","initEq","initDate","fast",'slow','sd'))

################## bbands.R #########################

source('bbandsTEST.r')

stats.second = tradeStats(port)

txns2       = stats.second$Num.Txns
trades2     = stats.second$Num.Trades
maxeq2      = stats.second$Max.Equity
maxdraw2    = stats.second$maxDrawdown
largwin2    = stats.second$Largest.Winner
pfactor2    = stats.second$Profit.Factor

suppressWarnings(rm("order_book.bug",pos=.strategy))
suppressWarnings(rm("account.colony","portfolio.bug",pos=.blotter))
suppressWarnings(rm("sym","port","acct","initEq","initDate","fast",'slow','sd'))

################## faber.R #########################

source('faberTEST.r')

stats.third  = tradeStats(port)

txns3       = stats.third$Num.Txns
trades3     = stats.third$Num.Trades
maxeq3      = stats.third$Max.Equity
maxdraw3    = stats.third$maxDrawdown
largwin3    = stats.third$Largest.Winner
pfactor3    = stats.third$Profit.Factor

suppressWarnings(rm("order_book.bug",pos=.strategy))
suppressWarnings(rm("account.colony","portfolio.bug",pos=.blotter))
suppressWarnings(rm("sym","port","acct","initEq","initDate","fast",'slow','sd'))

################## faberMC.R #########################

source('faberMCTEST.r')

stats.third  = tradeStats(port)

txns3       = stats.third$Num.Txns
trades3     = stats.third$Num.Trades
maxeq3      = stats.third$Max.Equity
maxdraw3    = stats.third$maxDrawdown
largwin3    = stats.third$Largest.Winner
pfactor3    = stats.third$Profit.Factor

suppressWarnings(rm("order_book.bug",pos=.strategy))
suppressWarnings(rm("account.colony","portfolio.bug",pos=.blotter))
suppressWarnings(rm("sym","port","acct","initEq","initDate","fast",'slow','sd'))

################## luxor.R #########################

source('luxorTEST.r')

stats.third  = tradeStats(port)

txns3       = stats.third$Num.Txns
trades3     = stats.third$Num.Trades
maxeq3      = stats.third$Max.Equity
maxdraw3    = stats.third$maxDrawdown
largwin3    = stats.third$Largest.Winner
pfactor3    = stats.third$Profit.Factor

suppressWarnings(rm("order_book.bug",pos=.strategy))
suppressWarnings(rm("account.colony","portfolio.bug",pos=.blotter))
suppressWarnings(rm("sym","port","acct","initEq","initDate","fast",'slow','sd'))

################## maCros.R #########################

source('maCrossTEST.r')

stats.third  = tradeStats(port)

txns3       = stats.third$Num.Txns
trades3     = stats.third$Num.Trades
maxeq3      = stats.third$Max.Equity
maxdraw3    = stats.third$maxDrawdown
largwin3    = stats.third$Largest.Winner
pfactor3    = stats.third$Profit.Factor

suppressWarnings(rm("order_book.bug",pos=.strategy))
suppressWarnings(rm("account.colony","portfolio.bug",pos=.blotter))
suppressWarnings(rm("sym","port","acct","initEq","initDate","fast",'slow','sd'))

################## macd.R #########################

source('macdTEST.r')

stats.third  = tradeStats(port)

txns3       = stats.third$Num.Txns
trades3     = stats.third$Num.Trades
maxeq3      = stats.third$Max.Equity
maxdraw3    = stats.third$maxDrawdown
largwin3    = stats.third$Largest.Winner
pfactor3    = stats.third$Profit.Factor

suppressWarnings(rm("order_book.bug",pos=.strategy))
suppressWarnings(rm("account.colony","portfolio.bug",pos=.blotter))
suppressWarnings(rm("sym","port","acct","initEq","initDate","fast",'slow','sd'))

################## pair_trade.R #########################

source('pair_tradeTEST.r')

stats.third  = tradeStats(port)

txns3       = stats.third$Num.Txns
trades3     = stats.third$Num.Trades
maxeq3      = stats.third$Max.Equity
maxdraw3    = stats.third$maxDrawdown
largwin3    = stats.third$Largest.Winner
pfactor3    = stats.third$Profit.Factor

suppressWarnings(rm("order_book.bug",pos=.strategy))
suppressWarnings(rm("account.colony","portfolio.bug",pos=.blotter))
suppressWarnings(rm("sym","port","acct","initEq","initDate","fast",'slow','sd'))

################## rocema.R #########################

source('rocemaTEST.r')

stats.third  = tradeStats(port)

txns3       = stats.third$Num.Txns
trades3     = stats.third$Num.Trades
maxeq3      = stats.third$Max.Equity
maxdraw3    = stats.third$maxDrawdown
largwin3    = stats.third$Largest.Winner
pfactor3    = stats.third$Profit.Factor

suppressWarnings(rm("order_book.bug",pos=.strategy))
suppressWarnings(rm("account.colony","portfolio.bug",pos=.blotter))
suppressWarnings(rm("sym","port","acct","initEq","initDate","fast",'slow','sd'))

################## rsi.R#########################

source('rsiTEST.r')

stats.third  = tradeStats(port)

txns3       = stats.third$Num.Txns
trades3     = stats.third$Num.Trades
maxeq3      = stats.third$Max.Equity
maxdraw3    = stats.third$maxDrawdown
largwin3    = stats.third$Largest.Winner
pfactor3    = stats.third$Profit.Factor

suppressWarnings(rm("order_book.bug",pos=.strategy))
suppressWarnings(rm("account.colony","portfolio.bug",pos=.blotter))
suppressWarnings(rm("sym","port","acct","initEq","initDate","fast",'slow','sd'))

######################## RUN TEST SUITE #######################

context('Consistency across trade statistics')

test_that('Number of transactions is consistent', {

  expect_that(txns1, equals(161))
  expect_that(txns2, equals(161))
  expect_that(txns3, equals(161))
  expect_that(txns3, equals(161))
  expect_that(txns3, equals(161))
  expect_that(txns3, equals(161))
  expect_that(txns3, equals(161))
  expect_that(txns3, equals(161))
  expect_that(txns3, equals(161))
  expect_that(txns3, equals(161))
})

test_that('Number of the number of trades is consistent', {

  expect_that(trades1, equals(80))
  expect_that(trades2, equals(80))
  expect_that(trades3, equals(80))
  expect_that(trades3, equals(80))
  expect_that(trades3, equals(80))
  expect_that(trades3, equals(80))
  expect_that(trades3, equals(80))
  expect_that(trades3, equals(80))
  expect_that(trades3, equals(80))
  expect_that(trades3, equals(80))
})

test_that('Max equity is consistent', {

  expect_that(maxeq1, equals(5044))
  expect_that(maxeq2, equals(5044))
  expect_that(maxeq2, equals(5044))
  expect_that(maxeq2, equals(5044))
  expect_that(maxeq2, equals(5044))
  expect_that(maxeq2, equals(5044))
  expect_that(maxeq2, equals(5044))
  expect_that(maxeq2, equals(5044))
  expect_that(maxeq2, equals(5044))
  expect_that(maxeq2, equals(5044))
})

test_that('Max equity is consistent', {

  expect_that(maxeq1, equals(5044))
  expect_that(maxeq2, equals(5044))
  expect_that(maxeq3, equals(5044))
  expect_that(maxeq3, equals(5044))
  expect_that(maxeq3, equals(5044))
  expect_that(maxeq3, equals(5044))
  expect_that(maxeq3, equals(5044))
  expect_that(maxeq3, equals(5044))
  expect_that(maxeq3, equals(5044))
  expect_that(maxeq3, equals(5044))
})

test_that('Max drawdown is consistent', {

  expect_that(maxdraw1, equals(-56019))
  expect_that(maxdraw2, equals(-56019))
  expect_that(maxdraw3, equals(-56019))
  expect_that(maxdraw3, equals(-56019))
  expect_that(maxdraw3, equals(-56019))
  expect_that(maxdraw3, equals(-56019))
  expect_that(maxdraw3, equals(-56019))
  expect_that(maxdraw3, equals(-56019))
  expect_that(maxdraw3, equals(-56019))
  expect_that(maxdraw3, equals(-56019))
})

test_that('Largest winner is consistent', {

  expect_that(largwin1, equals(34054))
  expect_that(largwin2, equals(34054))
  expect_that(largwin3, equals(34054))
  expect_that(largwin3, equals(34054))
  expect_that(largwin3, equals(34054))
  expect_that(largwin3, equals(34054))
  expect_that(largwin3, equals(34054))
  expect_that(largwin3, equals(34054))
  expect_that(largwin3, equals(34054))
  expect_that(largwin3, equals(34054))
})

test_that('Profit Factor is consistent', {

  expect_that(pfactor1, equals(0.8401456))
  expect_that(pfactor2, equals(0.8401456))
  expect_that(pfactor3, equals(0.8401456))
  expect_that(pfactor3, equals(0.8401456))
  expect_that(pfactor3, equals(0.8401456))
  expect_that(pfactor3, equals(0.8401456))
  expect_that(pfactor3, equals(0.8401456))
  expect_that(pfactor3, equals(0.8401456))
  expect_that(pfactor3, equals(0.8401456))
  expect_that(pfactor3, equals(0.8401456))
})



