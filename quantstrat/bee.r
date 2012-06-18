#!/usr/bin/Rscript --no-save
 
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

#print(getOrderBook('bug'))

txns <- getTxns('bug', 'GSPC')
txns
cat('Net profit:', sum(txns$Net.Txn.Realized.PL), '\n')
# getSymbols('^GSPC',from=initDate)
# for(i in 'GSPC')
#   assign(i, adjustOHLC(get(i),use.Adjusted=TRUE))
# 
# start_t<-Sys.time()
# out<-try(applyStrategy(strategy=bee , portfolios='bug', verbose=T))
# end_t<-Sys.time()
# print(end_t-start_t)
# 
# start_t<-Sys.time()
# updatePortf(Portfolio='bug',Dates=paste('::',as.Date(Sys.time()),sep=''))
# end_t<-Sys.time()
# print("trade blotter portfolio update:")
# print(end_t-start_t)


# Process the indicators and generate trades
# out <- try(applyStrategy(strategy=stratFaber, portfolios="faber"))
# updatePortf(Portfolio = "faber",
# Dates=paste('::',as.Date("2012-01-13"),sep=''))


# # Evaluate results
# portRet <- PortfReturns("bug")
# portRet$Total <- rowSums(portRet, na.rm=TRUE)
# charts.PerformanceSummary(portRet$Total)
# tradeStats("bug")[,c("Symbol","Num.Trades","Net.Trading.PL","maxDrawdown")]



# themelist = chart_theme()
# themelist$col$up.col = 'lightblue'
# themelist$col$dn.col = 'lightpink'
# 
# 
# chart.Posn(Portfolio='bug',Symbol='GSPC', theme=themelist)

