 
require(quantstrat)
getSymbols('^GSPC') 


initDate='2011-09-30'
initEq= 1e6

SD = .5
SLOW = 30
FAST = 10

currency('USD')
stock('GSPC',currency='USD',multiplier=1)
initPortf('bug',symbols='GSPC', initDate=initDate)
initAcct('bug',portfolios='bug', initDate=initDate)
initOrders(portfolio='bug',initDate=initDate)


bee <- strategy('bug')

################################ INDICATORS ##########################

bee <- add.indicator(
                              strategy = bee, 
                              name = "BBands", 
                              arguments = list(HLC=quote(HLC(mktdata)), 
                                               n=SLOW, 
                                               sd=SD))

bee <- add.indicator(
                              strategy = bee, 
                              name = "SMA", 
                              arguments = list(x=quote(Cl(mktdata)), 
                                               n=FAST),
                              label= "fast" )

################################### SIGNALS ############################

bee <- add.signal(
                           strategy = bee,
                           name="sigCrossover",
                           arguments = list( columns=c('fast',"dn"), 
                                             relationship="lt"),
                           label="fast.lt.dn")
bee <- add.signal(
                           strategy = bee,
                           name="sigCrossover",
                           arguments = list( columns=c("fast","up"),
                                             relationship="gt"),
                           label="fast.gt.up")

################################### RULES ################################

bee <- add.rule(
                         strategy = bee,
                         name='ruleSignal', 
                         arguments = list(sigcol="fast.gt.up",
                                          sigval=TRUE, 
                                          orderqty=100, 
                                          ordertype='market', 
                                          orderside='long'),
                         label='EnterLONG',
                         type='enter')
bee <- add.rule(
                         strategy = bee,
                         name='ruleSignal', 
                         arguments = list(sigcol="fast.lt.dn",
                                          sigval=TRUE, 
                                          orderqty='all', 
                                          ordertype='market', 
                                          orderside='long'),
                         label='ExitLONG',
                         type='exit')

bee <- add.rule(
                         strategy = bee,
                         name='ruleSignal', 
                         arguments = list(sigcol="fast.lt.dn",
                                          sigval=TRUE, 
                                          orderqty=-100, 
                                          ordertype='market', 
                                          orderside='short'),
                         label='EnterSHORT',
                         type='enter')
bee <- add.rule(
                         strategy = bee,
                         name='ruleSignal', 
                         arguments = list(sigcol="fast.gt.up",
                                          sigval=TRUE, 
                                          orderqty='all', 
                                          ordertype='market', 
                                          orderside='short'),
                         label='ExitSHORT',
                         type='exit')
 
####################################### AFTER RULES ###############################

applyStrategy(bee, 'bug', prefer='Open', verbose = FALSE)

print(getOrderBook('bug'))

cat('Net profit:', sum(txns$Net.Txn.Realized.PL), '\n')
