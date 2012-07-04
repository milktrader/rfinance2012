require(testthat)

################## bee.r  #########################

source('beeTEST.r')

stats.one = tradeStats(port)

txns1       = stats.one$Num.Txns
trades1     = stats.one$Num.Trades
maxeq1      = stats.one$Max.Equity
maxdraw1    = stats.one$maxDrawdown
largwin1    = stats.one$Largest.Winner

suppressWarnings(rm("order_book.bug",pos=.strategy))
suppressWarnings(rm("account.colony","portfolio.bug",pos=.blotter))
suppressWarnings(rm("sym","port","acct","initEq","initDate","fast",'slow','sd'))

######################## RUN TEST SUITE #######################

context('Consistency across trade statistics')

test_that('Number of transactions is consistent', {

  expect_that(txns1, equals(67))
})

test_that('Number of the number of trades is consistent', {

  expect_that(trades1, equals(33))
})

test_that('Max equity is consistent', {

  expect_that(maxeq1, equals(4970))
})

test_that('Max drawdown is consistent', {

  expect_that(maxdraw1, equals(-4431))
})

test_that('Largest winner is consistent', {

  expect_that(largwin1, equals(1860))
})




