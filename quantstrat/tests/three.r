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

################## bbands.R #########################

source('bbandsTEST.r')

stats.two = tradeStats(portfolio.st)

txns2       = stats.two$Num.Txns
trades2     = stats.two$Num.Trades
maxeq2      = stats.two$Max.Equity
maxdraw2    = stats.two$maxDrawdown
largwin2    = stats.two$Largest.Winner

suppressWarnings(rm("order_book.bbands",pos=.strategy))
suppressWarnings(rm("account.bbands","portfolio.bbands",pos=.blotter))
suppressWarnings(rm("account.st","portfolio.st","stock.str","stratBBands","initDate","initEq",'start_t','end_t'))

################## faber.R #########################

source('faberTEST.r')

stats.three  = tradeStats('faber')

txns3       = last(stats.three$Num.Txns)
trades3     = last(stats.three$Num.Trades)
maxeq3      = last(stats.three$Max.Equity)
maxdraw3    = last(stats.three$maxDrawdown)
largwin3    = last(stats.three$Largest.Winner)

suppressWarnings(rm("account.faber","portfolio.faber",pos=.blotter))
suppressWarnings(rm("ltaccount", "ltportfolio", "ClosePrice", "CurrentDate", "equity", 
            "GSPC", "stratFaber", "initDate", "initEq", "Posn", "UnitSize", "verbose"))
suppressWarnings(rm("order_book.faber",pos=.strategy))

######################## RUN TEST SUITE #######################

context('Consistency across trade statistics')

test_that('Number of transactions is consistent', {

  expect_that(txns1, equals(67))
  expect_that(txns2, equals(151))
  expect_that(txns3, equals(15))
})

test_that('Number of the number of trades is consistent', {

  expect_that(trades1, equals(33))
  expect_that(trades2, equals(68))
  expect_that(trades3, equals(15))
})

test_that('Max equity is consistent', {

  expect_that(maxeq1, equals(4970))
  expect_that(maxeq2, equals(8964))
  expect_that(maxeq3, equals(21705))
})

test_that('Max drawdown is consistent', {

  expect_that(maxdraw1, equals(-4431))
  expect_that(maxdraw2, equals(-5269))
  expect_that(maxdraw3, equals(-6800))
})

test_that('Largest winner is consistent', {

  expect_that(largwin1, equals(1860))
  expect_that(largwin2, equals(1681))
  expect_that(largwin3, equals(10835))
})




