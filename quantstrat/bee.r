#!/usr/bin/Rscript --no-save

########################## OPTIONAL COMMANDLINE ARG #####################

## uncomment this and comment out sym = line in the DEFINE VARIABLES section ####

# sym     = commandArgs(TRUE)

############################# DEFINE VARIABLES ##############################

sym           = 'SPY'
port          = 'bug'
acct          = 'colony'
initEq        = 100000
initDate      = '1950-01-01'
trade.percent = .1
fast          = 10
slow          = 30
sd            = 0.5

############################### GET DATA ####################################

suppressMessages(require(quantstrat))
getSymbols(sym, index.class=c("POSIXt","POSIXct"))

############################ INITIALIZE #####################################

currency('USD')
stock(sym ,currency='USD', multiplier=1)
initPortf(port, sym, initDate=initDate)
initAcct(acct, port, initEq=initEq, initDate=initDate)
initOrders(port, initDate=initDate )
bee     = strategy(port)

############################### MAX POSITION LOGIC ########################

addPosLimit(
            portfolio=port,
            symbol=sym, 
            timestamp=initDate,  
            maxpos=100)

############################### SIZING FUNCTION ########################


#osEquityCurve <- function (timestamp, orderqty, portfolio, symbol, ruletype, ...)
#{
#  tempPortfolio = getPortfolio(port)
#  dummy         = updatePortf(Portfolio=port, Dates=paste( '::' ,as.Date(timestamp),sep='' ))
#  trading.pl    =  sum(getPortfolio(port)$summary$Net.Trading.PL) 
#
#  assign(paste("portfolio.", port, sep=""), tempPortfolio, pos=.blotter) 
#
#  total.equity  = initEq+trading.pl 
#  tradeSize     = total.equity * trade.percent
#  ClosePrice    = as.numeric(Cl(mktdata[timestamp,])) 
#  orderqty      =  sign(orderqty)*round(tradeSize/ClosePrice) 
#
#  return(orderqty)
#}

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
                                 orderside = 'long',
                                 osFUN     = 'osMaxPos'),

                type      = 'enter',
                label     = 'EnterLONG')

bee <- add.rule(
                strategy  = bee,
                name      = 'ruleSignal',
                arguments = list(sigcol    = 'fast.lt.dn',
                                 sigval    = TRUE,
                                 orderqty  = 'all',
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
                                  orderside = 'short',
                                  osFUN     = 'osMaxPos'),
                type      = 'enter',
                label     = 'EnterSHORT')

bee <- add.rule(
                strategy  = bee,
                name      = 'ruleSignal',
                arguments = list(sigcol     = 'fast.gt.up',
                                 sigval     = TRUE,
                                 orderqty   = 'all',
                                 ordertype  = 'market',
                                 orderside  = 'short'),
                type      = 'exit',
                label     = 'ExitSHORT')

#################################### APPLY STRATEGY #######################

applyStrategy(bee, port, prefer='Open')

#################################### UPDATE ###############################

updatePortf(port, sym, Date=paste('::',as.Date(Sys.time()),sep=''))
updateAcct(acct)

################################## ORDER BOOK ###########################

print(getOrderBook(port))

###################### UTILIZE blotter ##################################

# cat('From the blotter package ...', '\n' )
# 
# stats = tradeStats(port)
# cat('Profit Factor: ', stats$Profit.Factor, '\n')
# 
# txns  = getTxns(port, sym)
# cat('Net profit:', sum(txns$Net.Txn.Realized.PL), '\n')

######################### UTILIZE PerformanceAnalytics ####################

suppressMessages(require(PerformanceAnalytics))
#
returns = PortfReturns(acct)
#
#cat('From the PerformanceAnalytics package...', '\n' )
#
#cat('A histogram is being plotted now ...', '\n')
chart.Histogram(returns)
#
#cat('The Annualized Sharpe Ratio is: ',  
#     SharpeRatio.annualized(returns), 
#     '\n')
#
#cat('The Annualized Return is: ',  
#     100 * Return.annualized(returns), 
#     'percent', 
#     '\n')

################################## EXPERIMENTAL #########################

#cat('Applying some TA to the equity curve..', '\n' )
#
#ifelse(last(SMA(returns, n=10)) > last(SMA(returns, n=30)), 
#       print('Your system is in an uptrend: Hurray!'), 
#       print('Your system is in a downtrend: Caution!'))

################################## PLOTS ###################################

#themelist            = chart_theme()
#themelist$col$up.col = 'lightblue'
#themelist$col$dn.col = 'lightpink'
# 
#chart.Posn(port, sym, theme=themelist)
#

################################# TESTING ###############################


# head(.strategy$order_book.bug$bug$SPY[,c(1,4,11)])


