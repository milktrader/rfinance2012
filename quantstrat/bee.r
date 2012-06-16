# ok, here's the basics: use addPosLimit to set your max position, with a date = your initDate (before the first trade)
# [07:36am] braverock: and then, in your entry rules, add osFUN='osMaxPos' to your arguments=list(...)
# [07:36am] braverock: now, no matter how many 'entry' signals you get, you'll never exceed your max pos
# 
require(quantstrat)

suppressWarnings(rm("order_book.bug",pos=.strategy))
suppressWarnings(rm("account.bug","portfolio.bug",pos=.blotter))
suppressWarnings(rm("'bug'","'bug'","'GSPC'","bee","initDate","initEq",'start_t','end_t'))

currency('USD')
stock('GSPC',currency='USD',multiplier=1)

initDate='2011-09-30'
million = 1e6
initEq= million

SD = .5
SLOW = 30
FAST = 10

initPortf('bug',symbols='GSPC', initDate=initDate)
initAcct('bug',portfolios='bug', initDate=initDate)
initOrders(portfolio='bug',initDate=initDate)

addPosLimit(
            portfolio='bug',
            symbol='GSPC', 
            timestamp = initDate,  
            maxpos=100)


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
                           arguments = list( columns=c("fast","dn"),
                                            relationship="gt"),
                           label="fast.gt.up")

################################### RULES ################################

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
                         arguments = list(sigcol="fast.gt.up",
                                          sigval=TRUE, 
                                          orderqty=100, 
                                          ordertype='market', 
                                          orderside='long',
                                          osFUN='osMaxPos'),
                         label='EnterLONG',
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
bee <- add.rule(
                         strategy = bee,
                         name='ruleSignal', 
                         arguments = list(sigcol="fast.lt.dn",
                                          sigval=TRUE, 
                                          orderqty=-100, 
                                          ordertype='market', 
                                          orderside='short',
                                          osFUN='osMaxPos'),
                         label='EnterSHORT',
                         type='enter')
 
####################################### AFTER RULES ###############################

getSymbols('^GSPC',from=initDate)
for(i in 'GSPC')
  assign(i, adjustOHLC(get(i),use.Adjusted=TRUE))

start_t<-Sys.time()
out<-try(applyStrategy(strategy=bee , portfolios='bug', verbose=T))
end_t<-Sys.time()
print(end_t-start_t)

start_t<-Sys.time()
updatePortf(Portfolio='bug',Dates=paste('::',as.Date(Sys.time()),sep=''))
end_t<-Sys.time()
print("trade blotter portfolio update:")
print(end_t-start_t)




# Process the indicators and generate trades
# out <- try(applyStrategy(strategy=stratFaber, portfolios="faber"))
# updatePortf(Portfolio = "faber",
# Dates=paste('::',as.Date("2012-01-13"),sep=''))





# # Evaluate results
# portRet <- PortfReturns("bug")
# portRet$Total <- rowSums(portRet, na.rm=TRUE)
# charts.PerformanceSummary(portRet$Total)
# tradeStats("bug")[,c("Symbol","Num.Trades","Net.Trading.PL","maxDrawdown")]



themelist = chart_theme()
themelist$col$up.col = 'lightblue'
themelist$col$dn.col = 'lightpink'


chart.Posn(Portfolio='bug',Symbol='GSPC', theme=themelist)

