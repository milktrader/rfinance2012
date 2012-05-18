currency('USD')

symbols = c("XLF", "XLP", "XLE", "XLY", "XLV", "XLI", "XLB", "XLK", "XLU")

for(symbol in symbols){stock(symbol, currency="USD",multiplier=1) } 

getSymbols(symbols, src='yahoo', index.class=c("POSIXt","POSIXct"), from='1990-01-01')
    
## convert daily to monthly for all the stocks ###############
    
    
for(symbol in symbols) {
   x <- get(symbol) 
   x <- to.monthly(x,indexAt='lastof',drop.time=TRUE) 
   colnames(x)<­gsub("x",symbol,colnames(x)) 
   assign(symbol,x)}
         
## initialize a bunch of stuff ###############################
         
         
initPortf('faber', symbols=symbols, initDate='1997­12­31') 
initAcct('faber', portfolios='faber', initDate='1997­12­31') 
initOrders(portfolio='faber', initDate='1997­12­31') 

## define a strategy ##########################################


s <- strategy("faber") 
s <- add.indicator(strategy = s, name = "SMA", arguments = list(x = quote(Cl(mktdata)), n=10), label="SMA10")

s <- add.signal(s, name="sigCrossover", arguments = list(data=quote(mktdata), columns=c("Close","SMA"), relationship="gt"), label="Cl.gt.SMA") 
s <- add.signal(s, name="sigCrossover", arguments = list(data=quote(mktdata), columns=c("Close","SMA"), relationship="lt"),label="Cl.lt.SMA")

s <- add.rule(s, name='ruleSignal', arguments = list(data=quote(mktdata), sigcol="Cl.gt.SMA", sigval=TRUE, orderqty=100, ordertype='market', orderside=NULL, threshold=NULL), type='enter')
s <- add.rule(s, name='ruleSignal', arguments = list(data=quote(mktdata), sigcol="Cl.lt.SMA", sigval=TRUE, orderqty='all', ordertype='market', orderside=NULL, threshold=NULL), type='exit')

## do something mysterious #########################################


out <- try(applyStrategy(strategy='s' , portfolios='faber')) 

## do something else that's mysterious #############################

updatePortf(Portfolio='faber')