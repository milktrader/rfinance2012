
#!/usr/bin/Rscript --no-save

initDate = '2002-10-21'
.from='2010-01-01'
.to='2010-12-31'

initEq = 100000

####

p = 'bug'
a = 'IB1'

###
require(quantstrat)

getSymbols('AAPL', from=.from, to=.to, verbose=FALSE)

###

initPortf(p, symbols='AAPL', initDate=initDate, currency='USD')
initAcct(a, portfolios=p, initDate=initDate, currency='USD')

###

initOrders(p, initDate=initDate)
bee <- strategy(p)

# indicators

bee <- add.indicator(bee, name="SMA", arguments = list(x = quote(Cl(mktdata)), n=10), label="SmaFAST")
bee <- add.indicator(bee, name="SMA", arguments = list(x = quote(Cl(mktdata)), n=30), label="SmaSLOW")

# signals

bee <- add.signal(bee, 'sigCrossover', arguments = list(columns=c("SmaFAST","SmaSLOW"), relationship="gte"), label='fast.gt.up')
bee <- add.signal(bee, 'sigCrossover', arguments = list(columns=c("SmaFAST","SmaSLOW"), relationship="lt"), label='fast.lt.dn')

# rules

bee <- add.rule(
                         strategy = bee,
                         name='ruleSignal',
                         arguments = list(sigcol="fast.lt.dn",
                                          sigval=TRUE,
                                          orderqty='all',
                                          ordertype='market',
                                          orderside='long'),
                         type='exit',
                         label='ExitLONG')

bee <- add.rule(
                         strategy = bee,
                         name='ruleSignal',
                         arguments = list(sigcol="fast.gt.up",
                                          sigval=TRUE,
                                          orderqty='all',
                                          ordertype='market',
                                          orderside='short'),
                         type='exit',
                         label='ExitSHORT')
bee <- add.rule(
                         strategy = bee,
                         name='ruleSignal',
                         arguments = list(sigcol="fast.lt.dn",
                                          sigval=TRUE,
                                          orderqty=-100,
                                          ordertype='market',
                                          orderside='short'),
                         type='enter',
                         label='EnterSHORT')

bee <- add.rule(
                         strategy = bee,
                         name='ruleSignal',
                         arguments = list(sigcol="fast.gt.up",
                                          sigval=TRUE,
                                          orderqty=100,
                                          ordertype='market',
                                          orderside='long'),

                         type='enter',
                         label='EnterLONG')
#

applyStrategy(bee, p, prefer='Open', verbose = FALSE)

print(getOrderBook(p))

txns <- getTxns(p, 'AAPL')
txns
cat('Net profit:', sum(txns$Net.Txn.Realized.PL), '\n')

