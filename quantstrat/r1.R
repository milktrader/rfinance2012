#!/usr/local/bin/R -f

.ema = 19
.roc = 45
.trend = 118
.tplong = 3.0
.tpshort = -3.0
.sllong = -5.0
.slshort = 5.0
#.timespan='T08:00:00/T12:59:00'
.timespan=NULL
.TxnFees=-1.9

#.subset='2011-12-01::2012-01-31'
.subset='2011-12'
.subset='2011-12-01::2011-12-10'
.subset='2011::2012'
.subset='2011-01'
.subset='2011'
.subset='2012-03'
#.subset='2012-03-01::2012-03-07'
#.subset='2012'

initDate = '2011-01-01'
initEq = 10000

####

p = 'rocema'
a = 'rocema'

####

RocSys = function(x, nEMA, nROC, nTREND)
{
  rocema = ROC(EMA(x, nEMA), nROC)
  trend = EMA(x, nTREND)

  signal = 
    ifelse(rocema>0 & lag(rocema)<=0, ifelse(x>trend,  1, 0), 
    ifelse(rocema<0 & lag(rocema)>=0, ifelse(x<trend, -1, 0), 
    NA)
  )
  
  signal <- na.locf(signal)
  
  return(signal)
}

###
require(FinancialInstrument)

if(!exists(".instrument")) .instrument <<- new.env()

getSymbols('FGBL', split_method='common' )

#FGBL = align.time(to.period(FGBL, 'minutes', 5), 300)
FGBL = align.time(to.period(FGBL, 'minutes', 15), 900)
#FGBL = align.time(to.period(FGBL, 'minutes', 30), 1800)
#FGBL = align.time(to.period(FGBL, 'minutes', 60), 3600)
FGBL <- FGBL[.subset]
FGBL$ROCSYS <- RocSys(Cl(FGBL), .ema, .roc, .trend)

###
require("blotter")

if(!exists(".blotter")) .blotter <<- new.env()

initPortf(p, symbols='FGBL', initDate=initDate, currency="EUR")
initAcct(a, portfolios=p, initDate=initDate, currency="EUR")

#addPosLimit(p, timestamp=initDate, symbol='FGBL', maxpos=1)

###
require("quantstrat")

if(!exists(".strategy")) .strategy <<- new.env()

initOrders(p, initDate=initDate)

s <- strategy(p)

#

s <- add.indicator(s, 'RocSys', arguments=list(x=quote(Cl(mktdata))), label='myrocsys')

#

s <- add.signal(s, 'sigThreshold', arguments = list(column="myrocsys", relationship="eq", threshold=0, cross=TRUE), label='cash')
s <- add.signal(s, 'sigThreshold', arguments = list(column="myrocsys", relationship="gt", threshold=0, cross=TRUE), label='long')
s <- add.signal(s, 'sigThreshold', arguments = list(column="myrocsys", relationship="lt", threshold=0, cross=TRUE), label='short')

#

s <- add.rule(s, 'ruleSignal', arguments=list(sigcol='short', sigval=TRUE, replace=FALSE, TxnFees=.TxnFees, orderside='long' , orderqty='all', ordertype='market', orderset='ocolong'),  type='exit', label='ExitLONG2SHORT')
s <- add.rule(s, 'ruleSignal', arguments=list(sigcol='cash' , sigval=TRUE, replace=FALSE, TxnFees=.TxnFees, orderside='long' , orderqty='all', ordertype='market', orderset='ocolong'),  type='exit', label='ExitLONG2CASH')

s <- add.rule(s, 'ruleSignal', arguments=list(sigcol='long' , sigval=TRUE, replace=FALSE, TxnFees=.TxnFees, orderside='short', orderqty='all', ordertype='market', orderset='ocoshort'),  type='exit', label='ExitSHORT2LONG')
s <- add.rule(s, 'ruleSignal', arguments=list(sigcol='cash' , sigval=TRUE, replace=FALSE, TxnFees=.TxnFees, orderside='short', orderqty='all', ordertype='market', orderset='ocoshort'),  type='exit', label='ExitSHORT2CASH')

s <- add.rule(s, 'ruleSignal', arguments=list(sigcol='long' , sigval=TRUE, replace=FALSE, TxnFees=.TxnFees, orderside='long' , orderqty='all', ordertype='limit', orderset='ocolong', threshold=.tplong), type='exit', label='TakeProfitLONG')
s <- add.rule(s, 'ruleSignal', arguments=list(sigcol='long' , sigval=TRUE, replace=FALSE, TxnFees=.TxnFees, orderside='long' , orderqty='all', ordertype='stoplimit', orderset='ocolong', threshold=.sllong),  type='exit', label='StopLossLONG')
s <- add.rule(s, 'ruleSignal', arguments=list(sigcol='long' , sigval=TRUE, replace=FALSE, TxnFees=.TxnFees, orderside='long',  orderqty=1    , ordertype='market'), timespan=.timespan, type='enter', label='EnterLONG')

s <- add.rule(s, 'ruleSignal', arguments=list(sigcol='short', sigval=TRUE, replace=FALSE, TxnFees=.TxnFees, orderside='short', orderqty='all', ordertype='limit', orderset='ocoshort', threshold=.tpshort), type='exit', label='TakeProfitSHORT')
s <- add.rule(s, 'ruleSignal', arguments=list(sigcol='short', sigval=TRUE, replace=FALSE, TxnFees=.TxnFees, orderside='short', orderqty='all', ordertype='stoplimit', orderset='ocoshort', threshold=.slshort),  type='exit', label='StopLossSHORT')
s <- add.rule(s, 'ruleSignal', arguments=list(sigcol='short', sigval=TRUE, replace=FALSE, TxnFees=.TxnFees, orderside='short', orderqty=-1   , ordertype='market'), timespan=.timespan, type='enter', label='EnterSHORT')

applyStrategy(s, p, parameters=list(nEMA=.ema,nROC=.roc,nTREND=.trend), verbose = FALSE, prefer='Open')

chart.Posn(p, "FGBL")

print(getOrderBook(p))

# example info
txns <- getTxns(p, 'FGBL')

txns
#txns$Net

cat('Net profit:', sum(txns$Net.Txn.Realized.PL), '\n')

#warnings()

