#!/usr/bin/Rscript --no-save

initDate = '2002-10-21'
.from='2010-01-01'
.to='2010-12-31'

initEq = 100000

####

green = 'green'
red   = 'red'
a = 'IB1'

###
require(quantstrat)

getSymbols('AAPL', from=.from, to=.to, verbose=FALSE)

###

initPortf(green, symbols='AAPL', initDate=initDate, currency='USD')
initPortf(red  , symbols='AAPL', initDate=initDate, currency='USD')
initAcct(a, portfolios=c(green, red), initDate=initDate, currency='USD')

###

initOrders(green, initDate=initDate)
initOrders(red  , initDate=initDate)
green.bee <- strategy(green)
red.bee   <- strategy(red)

# indicators

green.bee <- add.indicator(green.bee, name="SMA", arguments = list(x = quote(Cl(mktdata)), n=10), label="SmaFAST")
green.bee <- add.indicator(green.bee, name="SMA", arguments = list(x = quote(Cl(mktdata)), n=30), label="SmaSLOW")
red.bee   <- add.indicator(red.bee, name="SMA", arguments = list(x = quote(Cl(mktdata)), n=10), label="SmaFAST")
red.bee   <- add.indicator(red.bee, name="SMA", arguments = list(x = quote(Cl(mktdata)), n=30), label="SmaSLOW")

# signals

green.bee <- add.signal(green.bee, 'sigCrossover', arguments = list(columns=c("SmaFAST","SmaSLOW"), relationship="gte"), label='fast.gt.up')
green.bee <- add.signal(green.bee, 'sigCrossover', arguments = list(columns=c("SmaFAST","SmaSLOW"), relationship="lt"), label='fast.lt.dn')

red.bee <- add.signal(red.bee, 'sigCrossover', arguments = list(columns=c("SmaFAST","SmaSLOW"), relationship="gte"), label='fast.gt.up')
red.bee <- add.signal(red.bee, 'sigCrossover', arguments = list(columns=c("SmaFAST","SmaSLOW"), relationship="lt"), label='fast.lt.dn')
# rules

green.bee <- add.rule(
                         strategy = green.bee,
                         name='ruleSignal',
                         arguments = list(sigcol="fast.gt.up",
                                          sigval=TRUE,
                                          orderqty=100,
                                          ordertype='market',
                                          orderside='long'),

                         type='enter',
                         label='EnterLONG')
green.bee <- add.rule(
                         strategy = green.bee,
                         name='ruleSignal',
                         arguments = list(sigcol="fast.lt.dn",
                                          sigval=TRUE,
                                          orderqty='all',
                                          ordertype='market',
                                          orderside='long'),
                         type='exit',
                         label='ExitLONG')

red.bee <- add.rule(
                         strategy = red.bee,
                         name='ruleSignal',
                         arguments = list(sigcol="fast.lt.dn",
                                          sigval=TRUE,
                                          orderqty=-100,
                                          ordertype='market',
                                          orderside='short'),
                         type='enter',
                         label='EnterSHORT')
red.bee  <- add.rule(
                         strategy = red.bee,
                         name='ruleSignal',
                         arguments = list(sigcol="fast.gt.up",
                                          sigval=TRUE,
                                          orderqty='all',
                                          ordertype='market',
                                          orderside='short'),
                         type='exit',
                         label='ExitSHORT')

#

applyStrategy(green.bee, green, prefer='Open', verbose = FALSE)
applyStrategy(red.bee, red, prefer='Open', verbose = FALSE)

print(getOrderBook(green))
print(getOrderBook(red))

green.txns <- getTxns(green, 'AAPL')
red.txns <- getTxns(red, 'AAPL')

green.txns
red.txns


cat('Net profit from long side:', sum(green.txns$Net.Txn.Realized.PL), '\n')
cat('Net profit from short side:', sum(red.txns$Net.Txn.Realized.PL), '\n')


