library(quantstrat)
#ticker="T"
total_hist.start = as.Date("2006-06-22")
total_hist.end   = as.Date("2008-06-20")
total_hist = total_hist.end - total_hist.start

currency("USD")
#stock(ticker,currency="USD",multiplier=1)

stock('ATT', currency='USD')
ticker<-'ATT'
ATT<-getSymbols('T',from=total_hist.start,to=total_hist.end,auto.assign=FALSE)

#getSymbols(ticker,from=total_hist.start,to=total_hist.end,to.assign=TRUE)
#ticker = 'ATT'
#ATT = T

#ifelse(ticker='T', ticker='ATT' && ATT =T, ticker=ticker)
init.date = initDate=total_hist.start-1
strat.name<- "MyStrat"
port.name <- "MyPort"
acct.name <- "MyAcct"

TradeSize = 1000
initEq=as.numeric( TradeSize*max(Ad(get(ticker)) ) )

port <- initPortf(port.name,ticker,initDate=init.date)
acct <- initAcct(acct.name,portfolios=port.name, initDate=init.date, initEq=initEq)
ords <- initOrders(portfolio=port.name,initDate=init.date)
strat<- strategy(strat.name)

strat<- add.indicator(strategy = strat, name = "SMA", arguments = list(x=quote(Ad(mktdata)), n=20),label= "ma20" )
strat<- add.indicator(strategy = strat, name = "SMA", arguments = list(x=quote(Ad(mktdata)), n=50),label= "ma50")

strat<- add.signal(strat,name="sigCrossover",arguments = 
list(columns=c("ma20","ma50"),relationship="gte"),label="ma20.gt.ma50")
strat<- add.signal(strat,name="sigCrossover",arguments =   
list(column=c("ma20","ma50"),relationship="lt"),label="ma20.lt.ma50")

strat<- add.rule(strategy = strat,name='ruleSignal', arguments = list(sigcol="ma20.gt.ma50",sigval=TRUE,  
orderqty=TradeSize, ordertype='market', orderside='long', pricemethod='market'),type='enter', path.dep=TRUE)
strat<- add.rule(strategy = strat,name='ruleSignal', arguments = list(sigcol="ma20.lt.ma50",sigval=TRUE, orderqty='all', 
ordertype='market', orderside='long', pricemethod='market'),type='exit', path.dep=TRUE)

out<-try(applyStrategy(strategy=strat, portfolios=port.name))
