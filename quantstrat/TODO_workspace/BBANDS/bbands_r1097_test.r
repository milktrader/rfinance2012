require(testthat)

################## bee.r  #########################

source('bbands.R')
#source('bbands_r1097_refactored_for_testing.r')

stratstat   = tradeStats(portfolio.st)

Txns      = stratstat$Num.Txns
Trades    = stratstat$Num.Trades
NetPL     = stratstat$Net.Trading.PL
LWinner   = stratstat$Largest.Winner
LLoser    = stratstat$Largest.Loser
MaxDD     = stratstat$maxDrawdown

suppressWarnings(rm("order_book.bug",pos=.strategy))
suppressWarnings(rm("account.colony","portfolio.bug",pos=.blotter))
suppressWarnings(rm("sym","port","acct","initEq","initDate","fast",'slow','sd'))

######################## RUN TEST SUITE #######################

context('Consistency across trade statistics')

test_that('Number of transactions is consistent', {

  expect_that(Txns, equals(151))
})

test_that('Number of the number of trades is consistent', {

  expect_that(Trades, equals(68))
})

test_that('Net Trading PL is consistent', {

  expect_that(NetPL, equals(8945))
})

test_that('Largest Winner is consistent', {

  expect_that(LWinner, equals(1681))
})

test_that('Largest Loser is consistent', {

  expect_that(LLoser, equals(-2576))
})

test_that('Max Drawdown is consistent', {

  expect_that(MaxDD, equals(-5269))
})



