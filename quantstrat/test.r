require(quantstrat)

getSymbols('SPY', from='2000-01-01')

currency('USD')
stock('SPY', currency='USD')

initPortf('bug', 'SPY')
initAcct('bug', portfolios='bug', initEq=1000000, initDate='1999-12-31')
initOrders(portfolio='bug', initDate='1999-12-31')
 
ant = strategy('bug')
ant = add.indicator(
                    strategy  =  ant,
                    name      = 'SMA', 
                    arguments = list(
                                     x = quote(Cl(mktdata)), 
                                     n = 100), 
                    label     ='sma100')
ant = add.indicator(
                    strategy  =  ant,
                    name      = 'SMA', 
                    arguments = list(
                                     x = quote(Cl(mktdata)), 
                                     n = 200 ), 
                    label     ='sma200')
ant = add.signal(
                    strategy  = ant, 
                    name      = 'sigCrossover', 
                    arguments = list(
                                     column       = c('sma100', 'sma200'), 
                                     relationship = 'gt'), 
                    label     ='sma100.gt.sma200')
ant = add.signal(
                    strategy  = ant, 
                    name      = 'sigCrossover',
                    arguments = list(
                                     column       = c('sma100', 'sma200'),
                                     relationship = 'lt'),
                    label='sma100.lt.sma200')
ant = add.rule(
                    strategy  = ant, 
                    name      = 'ruleSignal', 
                    arguments = list(
                                     sigcol       = 'sma100.gt.sma200', 
                                     sigval       = TRUE,
                                     orderqty     = 100,
                                     orderside    = 'long', 
                                     ordertype    = 'market' ), 
                    type      = 'enter')
ant = add.rule(
                    strategy  = ant, 
                    name      = 'ruleSignal',
                    arguments = list( 
                                      sigcol      = 'sma100.lt.sma200 ', 
                                      sigval      = TRUE, 
                                      orderqty    = 'all', 
                                      orderside   = 'long' ,
                                      ordertype   = 'market' ),
                    type      = 'exit')
out = applyStrategy(ant, portfolios='bug')
