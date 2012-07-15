#
# Bumblebee trading system
# copyright (c) 2009-2012, Algorithm Alpha, LLC
# Licensed GPL-2
#
############################# LOOP ##########################################

for (i in seq(7,21,7)) 
{  
  for (j in seq(30,50,10))
  { 
############################# SET TIMEZONE  ##############################
Sys.setenv(TZ="UTC")
############################# DEFINE VARIABLES ##############################

sym           = 'GLD'
port          = 'bug'
acct          = 'colony'
initEq        = 100000
initDate      = '1999-12-31'
fast          = i 
slow          = j 
sd            = 0.5

############################# GET DATA ######################################

suppressMessages(require(quantstrat))
getSymbols(sym, from='2000-01-01', index.class=c("POSIXt","POSIXct"))

############################# INITIALIZE ####################################

currency('USD')
stock(sym ,currency='USD', multiplier=1)
initPortf(port, sym, initDate=initDate)
initAcct(acct, port, initEq=initEq, initDate=initDate)
initOrders(port, initDate=initDate )
bee = strategy(port)

############################# MAX POSITION LOGIC ############################

addPosLimit(
            portfolio=port,
            symbol=sym, 
            timestamp=initDate,  
            maxpos=100)


############################# INDICATORS ####################################

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

############################# SIGNALS #######################################

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

############################# RULES #########################################

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

############################# APPLY STRATEGY ################################

applyStrategy(bee, port, prefer='Open', verbose=FALSE)

############################# UPDATE ########################################

updatePortf(port, sym, Date=paste('::',as.Date(Sys.time()),sep=''))
updateAcct(acct)

########################### USEFUL CONTAINERS ###############################

stratStats   = tradeStats(port)
stratReturns = PortfReturns(acct)

############################# STAT OF INTEREST ##############################

cat('Profit Factor for fast ==', i, 'and slow ==',j, 'is: ', stratStats$Profit.Factor, '\n')

############################# CLEAN UP ######################################

suppressWarnings(rm("order_book.bug",pos=.strategy))
suppressWarnings(rm("account.colony","portfolio.bug",pos=.blotter))
suppressWarnings(rm("sym","port","acct","initEq","initDate","fast",'slow','sd'))
}}
