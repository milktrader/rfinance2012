############################# DEFINE VARIABLES ##############################

sym      = 'SPY'
port     = 'bug'
acct     = 'spray'
currency = 'USD'
initDate = '1949-12-31'
initEq   = 100000
fast     = 10
slow     = 30
sd       = 0.5

############################### GET DATA ####################################

require(quantstrat)
getSymbols(sym)

############################ INTIALIZE #####################################

initPortf(port, symbols=sym, initDate=initDate, currency=currency)
initAcct(acct, portfolios=port, initDate=initDate, currency=currency)
initOrders(port, initDate=initDate)
bee     = strategy(port)

############################ INDICATORS ####################################

bee <- add.indicator( 
                     strategy  = bee, 
                     name      = 'BBands', 
                     arguments = list(HLC=quote(HLC(mktdata)), 
                                      n=slow, 
                                      sd=sd))

bee <- add.indicator(
                     strategy  = bee, 
                     name      = 'SMA', 
                     arguments = list(x=quote(Cl(mktdata)), 
                                      n=fast),
                     label     = 'fast' )

########################### SIGNALS ######################################

bee <- add.signal(
                  strategy  = bee,
                  name      = 'sigCrossover',
                  arguments = list(columns=c('fast','dn'), 
                                   relationship='lt'),
                  label     = 'fast.lt.dn')

bee <- add.signal(
                  strategy  = bee,
                  name      = 'sigCrossover',
                  arguments = list(columns=c('fast','up'),
                                   relationship='gt'),
                  label     = 'fast.gt.up')

########################## RULES #########################################

bee <- add.rule(
                strategy  = bee,
                name      = 'ruleSignal',
                arguments = list(sigcol    = 'fast.gt.up',
                                 sigval    = TRUE,
                                 orderqty  = 100,
                                 ordertype = 'market',
                                 orderside = 'long'),

                type      = 'enter',
                label     = 'EnterLONG')

bee <- add.rule(
                strategy  = bee,
                name      = 'ruleSignal',
                arguments = list(sigcol    = 'fast.lt.dn',
                                 sigval    = TRUE,
                                 orderqty  = -100,
                                 ordertype = 'market',
                                 orderside = 'long'),
                type      = 'exit',
                label     = 'ExitLONG')


bee <- add.rule(
                strategy  = bee,
                name      = 'ruleSignal',
                arguments = list(sigcol     = 'fast.lt.dn',
                                  sigval    = TRUE,
                                  orderqty  =  -100,
                                  ordertype = 'market',
                                  orderside = 'short'),
                type      = 'enter',
                label     = 'EnterSHORT')

bee <- add.rule(
                strategy  = bee,
                name      = 'ruleSignal',
                arguments = list(sigcol     = 'fast.gt.up',
                                 sigval     = TRUE,
                                 orderqty   = 100,
                                 ordertype  = 'market',
                                 orderside  = 'short'),
                type      = 'exit',
                label     = 'ExitSHORT')

#################################### APPLY STRATEGY #######################

applyStrategy(bee, port )

#################################### TABLES ###############################
print(getOrderBook(port))

cat('Net profit:', sum(txns$Net.Txn.Realized.PL), '\n')
cat('Sharpe Ratio:', sum(txns$Net.Txn.Realized.PL), '\n')


################################## PLOTS ###################################

# themelist            = chart_theme()
# themelist$col$up.col = 'lightblue'
# themelist$col$dn.col = 'lightpink'
# 
# chart.Posn(Portfolio=port, Symbol=sym, theme=themelist)
