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


######################### ISOLATE RETURNS ####################

returns = PortfReturns(acct)

######################### svUnit TESTS ####################

suppressMessages(require(svUnit))

######### DEFINE OUTSIDE TEST FUNCTION ############

statusCheck = 'should have correct number of each status'
signalCheck = 'should have the correct number of signals'
qtyCheck    = 'should not exceed 100'

############ DEFINE INSIDE TEST FUNCTION ################

test(statusCheck) = function() {

  Status = .strategy$order_book.bug$bug$GSPC$Order.Status 

  t1     = nrow(Status[Status$Order.Status == 'rejected'])    # 34
  t2     = nrow(Status[Status$Order.Status == 'closed'])      # 162
  t3     = nrow(Status[Status$Order.Status == 'open'])        # 2 

  checkEquals(t1, 34)
  checkEquals(t2, 162)
  checkEquals(t3, 2)

}

test(signalCheck) = function() {
    
  t4 = nrow(mktdata[mktdata$fast.gt.up == 1])   # 61
  t5 = nrow(mktdata[mktdata$fast.lt.dn == 1])   # 54 
  
  checkEquals(t4, 61)
  checkEquals(t5, 54)
}

test(qtyCheck) = function() {
    
  Qty    = .strategy$order_book.bug$bug$GSPC$Order.Qty

  t6     = suppressWarnings(max(as.numeric(Qty), na.rm=TRUE)) # 100
  t7     = suppressWarnings(min(as.numeric(Qty), na.rm=TRUE)) # -100
  
  checkEquals(t6, 100)
  checkEquals(t7, -100)
}
 
################ RUN THE TESTS ###############################

clearLog()
runTest(statusCheck)
runTest(signalCheck)
runTest(qtyCheck)
Log()
summary(Log())
