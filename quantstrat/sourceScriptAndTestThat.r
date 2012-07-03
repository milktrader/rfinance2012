require(testthat)
source('bee.SPY.r')
stats.SPY = tradeStats(port)
t1        = stats.SPY$Num.Txns

suppressWarnings(rm("order_book.bug",pos=.strategy))
suppressWarnings(rm("account.colony","portfolio.bug",pos=.blotter))
suppressWarnings(rm("sym","port","acct","initEq","initDate","fast",'slow','sd'))

source('bee.GLD.r')
stats.GLD = tradeStats(port)
t2        = stats.GLD$Num.Txns

test_that('Number of transactions is consistent', {

  expect_that(t1, equals(75))
  expect_that(t2, equals(67))
})
