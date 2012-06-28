################## TEST SCRIPT FOR BEE.R ######################################
 
################## LOAD PACKAGES ######################################

suppressMessages(require(quantstrat))
suppressMessages(require(PerformanceAnalytics)) #for other testing later

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

######################### TEST STATISTICS ####################

bookStatus = .strategy$order_book.bug$bug$GSPC$Order.Status 
bookQty    = .strategy$order_book.bug$bug$GSPC$Order.Qty

t1 = nrow(bookStatus[bookStatus$Order.Status == 'rejected'])    # 34
t2 = nrow(bookStatus[bookStatus$Order.Status == 'closed'])      # 162
t3 = nrow(bookStatus[bookStatus$Order.Status == 'open'])        # 2 

t4 = nrow(mktdata[mktdata$fast.gt.up == 1])   # 61
t5 = nrow(mktdata[mktdata$fast.lt.dn == 1])   # 54 

t6 = suppressWarnings(max(as.numeric(bookQty), na.rm=TRUE)) # 100
t7 = suppressWarnings(min(as.numeric(bookQty), na.rm=TRUE)) # -100

print('Test Results:')
print('')

myUnit  <- function(t1, t2, t3, t4, t5, t6, t7) {

    ifelse(t1 == 34,   print("PASS"), print("FAIL - rejected total wrong"))
    ifelse(t2 == 162,  print("PASS"), print("FAIL - closed total wrong"))
    ifelse(t3 == 2,    print("PASS"), print("FAIL - open total wrong"))
    ifelse(t4 == 61,   print("PASS"), print("FAIL - long signals wrong"))
    ifelse(t5 == 54,   print("PASS"), print("FAIL - short signals wrong"))
    ifelse(t6 == 100,  print("PASS"), print("FAIL - max position wrong"))
    ifelse(t7 == -100, print("PASS"), print("FAIL - min position wrong"))
}

myUnit(t1,t2,t3,t4,t5,t6,t7)



