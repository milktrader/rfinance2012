beefun  <-  function() {

 
################## LOAD PACKAGES ######################################

suppressMessages(require(quantstrat))

############################# DEFINE VARIABLES ##############################

sym           = 'GSPC'
port          = 'bug'
acct          = 'colony'
initEq        = 100000
initDate      = '1950-01-01'
fast          = 10
slow          = 30
sd            = 0.5

############################### LOAD DATA ####################################

load('~/clones/blotter/pkg/quantstrat/sandbox/GSPC.rda')

############################ INITIALIZE #####################################

currency('USD')
stock(sym ,currency='USD', multiplier=1)
initPortf(port, sym, initDate=initDate)
initAcct(acct, port, initEq=initEq, initDate=initDate)
initOrders(port, initDate=initDate )
bee = strategy(port)

############################### MAX POSITION LOGIC ########################

addPosLimit(
            portfolio=port,
            symbol=sym, 
            timestamp=initDate,  
            maxpos=100)

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

applyStrategy(bee, port, prefer='Open', verbose=FALSE)

#################################### UPDATE ###############################

updatePortf(port, sym, Date=paste('::',as.Date(Sys.time()),sep=''))
updateAcct(acct)

########################### RETURN PROFIT FACTOR ###############################

stats = tradeStats(port)
PF = stats$Profit.Factor

return(PF)
}
