rm(list = ls())

#load libraries
library(quantmod)
library(PerformanceAnalytics)
library(blotter)
library(FinancialInstrument)
try(library(quantstrat))



#clear portfolio and acct not needed due to the clearing workspace but here incase you don't use it.
suppressWarnings(rm("account.stocky","portfolio.stocky",pos=.blotter))
suppressWarnings(rm("order_book.stocky",pos=.strategy))
suppressWarnings(rm(stocky))

#if your stock is different you need to change (initdate,initportf,addposlimit, chart.posn)
#define stock change spy to your stock of choice
symbol = "SPY"
currency('USD')
stock(symbol, currency="USD", multiplier = 1)



####################################################### Get Data #################################################
 getSymbols(symbol,src="yahoo")

#Set Initial Date and Equity, note change SPY to your stock of choice.
initDate = start(SPY)
initEq = 10000

#Set up currencies

currency("USD")





########################## Set up portfolio orders and Acct #######################################
#change SPY to your stock choice

initPortf(name="stocky","SPY",initPosQty=0,initDate=initDate,currency="USD")
initAcct("stocky",portfolios="stocky",initDate=initDate,initEq=initEq)
initOrders("stocky",symbols=symbol,initDate=initDate)

#position limits
#change "SPY" to your stock choice
addPosLimit("stocky","SPY",timestamp=initDate,maxpos=100, minpos=-100)

#Set up Strategy
stratstocky<-strategy("stocky")
###################################################Indicators and Functions########################################


##############################FUNCTIONS#################################

# PUT YOUR CUSTOM Indicator here
movingavgret<-function(x,n){
  step1<-ROC(x)
  step2<-SMA(step1,n)
  return(step2)
  }





########################indicators#############################


stratstocky<-add.indicator(
  strategy  =  stratstocky, 
  name    =  "movingavgret", 
  arguments  =  list(
    x    =  quote(Cl(mktdata)),
    n      =  20),
  label    =  "movavg")



################################################ Signals ###################################################33333

stratstocky<-add.signal(
  strategy      = stratstocky,
  name        = "sigThreshold",
  arguments      = list(
    threshold    = 0.001,
     column      = "movavg",
    relationship  = "gte",
    cross      = TRUE),
  label        = "movavgPOS")

stratstocky<-add.signal(
  strategy      = stratstocky,
  name        = "sigThreshold",
  arguments      = list(
    threshold    = -0.001,
     column      = "movavg",
    relationship  = "lt",
    cross      = TRUE),
  label        = "movavgNEG")






############################################# Rules ##########################################################################################

#Entry Rule Long
stratstocky<- add.rule(stratstocky,
  name        =  "ruleSignal",
  arguments      =  list(
    sigcol      =  "movavgPOS",
    sigval      =  TRUE,
    orderqty    =  100,
    ordertype    =  "market",
    orderside    =  "long",
    pricemethod    =  "market",
    replace      =  FALSE,
  TxnFees        =  -1,
  osFUN        =  osMaxPos), 
  type        =  "enter",
  path.dep      =  TRUE,
  label        =  "Entry")

#Entry Rule Short
stratstocky<- add.rule(stratstocky,
  name        =  "ruleSignal",
  arguments      =  list(
    sigcol      =  "movavgNEG",
    sigval      =  TRUE,
    orderqty    =  -100,
    ordertype    =  "market",
    orderside    =  "short",
    pricemethod    =  "market",
    replace      =  FALSE,
  TxnFees        =  -1,
  osFUN        =  osMaxPos), 
  type        =  "enter",
  path.dep      =  TRUE,
  label        =  "Entry")




#Exit Rules

stratstocky <- add.rule(stratstocky,
   name        =  "ruleSignal",
  arguments      =  list(
  sigcol        =  "movavgNEG", 
    sigval      =  TRUE, 
    orderqty    =  "all", 
    ordertype    =  "market",
    orderside    =  "short", 
    pricemethod    =  "market",
    replace      =  FALSE,
    TxnFees      =  -1),
    type      =  "exit",
  path.dep      =  TRUE,
  label        =  "Exit")


stratstocky <- add.rule(stratstocky,
   name        =  "ruleSignal",
  arguments      =  list(
  sigcol        =  "movavgPOS", 
    sigval      =  FALSE, 
    orderqty    =  "all", 
    ordertype    =  "market",
    orderside    =  "long", 
    pricemethod    =  "market",
    replace      =  FALSE,
    TxnFees      =  -1),
    type      =  "exit",
  path.dep      =  TRUE,
  label        =  "Exit")





##############################    Apply Strategy ##############################################

out <- applyStrategy(strategy=stratstocky, portfolios="stocky")
updatePortf("stocky")

############################# Portfolio Return Characterics ################################
#get portfolio data
portRet <- PortfReturns("stocky")
#portRet$Total <- rowSums(portRet, na.rm=TRUE)
#charts.PerformanceSummary(portRet$Total)
charts.PerformanceSummary(portRet)
tradeStats("stocky")[,c("Symbol","Num.Trades","Net.Trading.PL","maxDrawdown")]
#change SPY to your stock choice
chart.Posn("stocky","SPY")
results1<-getTxns("stocky","SPY")
plot(results1$Net.Txn.Realized.PL)
