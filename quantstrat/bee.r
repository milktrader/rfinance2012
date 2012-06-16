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

initDate='2011-12-31'

foo = as.Date(initDate)
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
            timestamp = as.character(foo - 1),  
            maxpos=200)


bee <- strategy('bug')

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

bee <- add.rule(
                         strategy = bee,
                         name='ruleSignal', 
                         arguments = list(sigcol="fast.gt.up",
                                          sigval=TRUE, 
                                          orderqty=100, 
                                          ordertype='market', 
                                          orderside='long',
                                          osFUN='osMaxPos'),

                         type='enter')
bee <- add.rule(
                         strategy = bee,
                         name='ruleSignal', 
                         arguments = list(sigcol="fast.lt.dn",
                                          sigval=TRUE, 
                                          orderqty=-100, 
                                          ordertype='market', 
                                          orderside='short',
                                          osFUN='osMaxPos'),
                         type='enter')
bee <- add.rule(
                         strategy = bee,
                         name='ruleSignal', 
                         arguments = list(sigcol="fast.lt.dn",
                                          sigval=TRUE, 
                                          orderqty='all', 
                                          ordertype='market', 
                                          orderside='long'),
                         type='exit')

bee <- add.rule(
                         strategy = bee,
                         name='ruleSignal', 
                         arguments = list(sigcol="fast.gt.up",
                                          sigval=TRUE, 
                                          orderqty='all', 
                                          ordertype='market', 
                                          orderside='short'),
                         type='exit')
 
getSymbols('^GSPC',from=initDate)
for(i in 'GSPC')
  assign(i, adjustOHLC(get(i),use.Adjusted=TRUE))

start_t<-Sys.time()
out<-try(applyStrategy(strategy=bee , portfolios='bug'))
end_t<-Sys.time()
print(end_t-start_t)

start_t<-Sys.time()
updatePortf(Portfolio='bug',Dates=paste('::',as.Date(Sys.time()),sep=''))
end_t<-Sys.time()
print("trade blotter portfolio update:")
print(end_t-start_t)


chart_theme <- mytheme <- function() {
  theme <-list(col=list(bg='ivory',
                        label.bg='ivory',
                        grid='ivory',
                        grid2='ivory',
                        ticks=1,
                        labels=4,
                        line.col='ivory',
                        dn.col='lightcyan',
                        up.col='lightcyan',
                        dn.border='lightblue',
                        up.border='lightblue'),
               shading=1,
               format.labels=TRUE,
               coarse.time=FALSE,
               rylab=FALSE,
               lylab=TRUE,
               grid.ticks.lwd=1)
  theme
}

chart.Posn(Portfolio='bug',Symbol='GSPC', theme=chart_theme(), clev=0)

