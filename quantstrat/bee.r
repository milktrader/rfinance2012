#!/usr/bin/Rscript --no-save

########################## OPTIONAL COMMANDLINE ARG #####################

## uncomment this and comment out 'SPY' in the DEFINE VARIABLES section ####

#args    = commandArgs(TRUE)

#'SPY'  =  args[1]    
#fast =  args[2]    
#slow =  args[3]    
#sd   =  args[4]    

############################# DEFINE VARIABLES ##############################

sym      = 'AAPL'
port     = 'bug'
acct     = 'spray'
initEq   = 100000
initDate = '1950-01-01'
fast     = 10
slow     = 30
sd       = 0.5

############################### GET DATA ####################################

suppressMessages(require(quantstrat))
getSymbols(sym, index.class=c("POSIXt","POSIXct"))

############################ INITIALIZE #####################################

currency('USD')
stock(sym ,currency='USD', multiplier=1)
initPortf(port, sym, initDate=initDate)
initAcct(acct, port, initDate=initDate)
initOrders(port, initDate=initDate )
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
                                  orderside = 'short'),
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

applyStrategy(bee, port, verbose=FALSE)

#################################### TABLES ###############################

# print(getOrderBook(port))

############################ STAT OUTPUT #################################

txns = getTxns(port, sym)
cat('Net profit:', sum(txns$Net.Txn.Realized.PL), '\n')

################################## PLOTS ###################################

# themelist            = chart_theme()
# themelist$col$up.col = 'lightblue'
# themelist$col$dn.col = 'lightpink'
# 
# chart.Posn(Portfolio=port, 'SPY'bol='SPY', theme=themelist)


################################# TESTING ###############################


# head(.strategy$order_book.bug$bug$SPY[,c(1,4,11)])


