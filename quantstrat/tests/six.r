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

################## faberMC.R #########################

source('faberMCTEST.r')

stats.four  = tradeStats(symbols)

txns4       = last(stats.four$Num.Txns)
trades4     = last(stats.four$Num.Trades)
maxeq4      = last(stats.four$Max.Equity)
maxdraw4    = last(stats.four$maxDrawdown)
largwin4    = last(stats.four$Largest.Winner)

suppressWarnings(rm("account.faber","account.faberMC","portfolio.faber","portfolio.combMC", 
                        "portfolio.GDAXI", "portfolio.GSPC", "portfolio.N225",pos=.blotter))
suppressWarnings(rm("ltaccount","ltportfolio","ClosePrice","CurrentDate","equity","stratFaber","initDate","initEq","Posn","UnitSize","verbose"))
suppressWarnings(rm("order_book.faber","order_book.combMC", "order_book.GDAXI", "order_book.GSPC", "order_book.N225", pos=.strategy))

################## macd.R #########################

source('macdTEST.r')

stats.five  = tradeStats(portfolio.st)

txns5       = stats.five$Num.Txns
trades5     = stats.five$Num.Trades
maxeq5      = stats.five$Max.Equity
maxdraw5    = stats.five$maxDrawdown
largwin5    = stats.five$Largest.Winner

suppressWarnings(rm("order_book.macd",pos=.strategy))
suppressWarnings(rm("account.macd","portfolio.macd",pos=.blotter))
suppressWarnings(rm("account.st","portfolio.st","stock.str","stratMACD","initDate","initEq",'start_t','end_t'))

################## maCross.R #########################

source('maCrossTEST.r')

stats.six  = tradeStats(portfolio.st)

txns6       = stats.six$Num.Txns
trades6     = stats.six$Num.Trades
maxeq6      = stats.six$Max.Equity
maxdraw6    = stats.six$maxDrawdown
largwin6    = stats.six$Largest.Winner

suppressWarnings(rm("order_book.macross",pos=.strategy))
suppressWarnings(rm("account.macross","portfolio.macross",pos=.blotter))
suppressWarnings(rm("account.st","portfolio.st","stock.str","stratMACROSS","initDate","initEq",'start_t','end_t'))

######################## RUN TEST SUITE #######################


context('Consistency across trade statistics')

test_that('Number of transactions is consistent', {

  expect_that(txns1, equals(67))
  expect_that(txns2, equals(151))
  expect_that(txns3, equals(15))
  expect_that(txns4, equals(10))
  expect_that(txns5, equals(27))
  expect_that(txns6, equals(11))
})

test_that('Number of the number of trades is consistent', {

  expect_that(trades1, equals(33))
  expect_that(trades2, equals(68))
  expect_that(trades3, equals(15))
  expect_that(trades4, equals(5))
  expect_that(trades5, equals(13))
  expect_that(trades6, equals(5))
})

test_that('Max equity is consistent', {

  expect_that(maxeq1, equals(4970))
  expect_that(maxeq2, equals(8964))
  expect_that(maxeq3, equals(21705))
  expect_that(maxeq4, equals(6021980))
  expect_that(maxeq5, equals(48918.75))
  expect_that(maxeq6, equals(54739))
})

test_that('Max drawdown is consistent', {

  expect_that(maxdraw1, equals(-4431))
  expect_that(maxdraw2, equals(-5269))
  expect_that(maxdraw3, equals(-6800))
  expect_that(maxdraw4, equals(-2321310))
  expect_that(maxdraw5, equals(-8374))
  expect_that(maxdraw6, equals(-13434))
})

test_that('Largest winner is consistent', {

  expect_that(largwin1, equals(1860))
  expect_that(largwin2, equals(1681))
  expect_that(largwin3, equals(10835))
  expect_that(largwin4, equals(2891500))
  expect_that(largwin5, equals(16265))
  expect_that(largwin6, equals(5018))
})



