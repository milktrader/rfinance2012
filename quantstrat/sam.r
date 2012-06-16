require(quantstrat)
require(PerformanceAnalytics)



# Set initial values
initDate <- "2010-04-30"
endDate <- "2012-01-13"
initEq <- 1000000

# Pull Yahoo Finance data
symbols <- c("IEF", "SPY")
getSymbols(symbols, from=initDate, to=endDate, index.class=c("POSIXt","POSIXct"), src='yahoo')

# Set up instruments with FinancialInstruments package
currency("USD")
for(symbol in symbols) {
  stock(symbol, currency="USD", multiplier=1)
 }

# Delete portfolio, account, and order book if they already exist
suppressWarnings(rm("account.faber","portfolio.faber",pos=.blotter))
suppressWarnings(rm("order_book.faber",pos=.strategy))

# Initialize portfolio and account
initPortf("faber", symbols=symbols, initDate=initDate)
initAcct("faber", portfolios="faber", initDate=initDate, initEq=initEq)
initOrders(portfolio="faber", initDate=initDate)

# Initialize a strategy object
stratFaber <- strategy("faber")

# Add the 40-day Donchian indicator
stratFaber <- add.indicator(strategy=stratFaber,
                            name="DonchianChannel",
                            arguments=list(HL=quote(cbind(Hi(mktdata),Lo(mktdata))), n=40),
                            label="Donchian40")

# There are two signals:
# The first is when monthly price crosses over the 40-days Donchian
stratFaber <- add.signal(stratFaber,
                         name="sigComparison",
                         arguments=list(columns=c("High","Donchian40.high"),
                         relationship="gte"),
                         label="Hi.gt.Donchian40")
# The second is when the monthly price crosses under the 40-days Donchian
stratFaber <- add.signal(stratFaber,
                         name="sigComparison",
                         arguments=list(columns=c("Low","Donchian40.low"),
                                        relationship="lte"),
                         label="Lo.lt.Donchian40")

# There are two rules:
# The first is to buy when the price crosses above the Donchian
stratFaber <- add.rule(stratFaber,
                                             name="ruleSignal",
                                             arguments=list(sigcol="Hi.gt.Donchian40",
                                                                                                        sigval=TRUE,
                                                                                                        orderqty=1000,
                                                                                                        ordertype="market",
                                                                                                        orderside="long",
                                                                                                        pricemethod="market",
                                                                                                        TxnFees=-5,
                                                                                                        osFUN=osMaxPos),
                                              type="enter",
                                              path.dep=TRUE)
# The second is to sell when the price crosses below the Donchian
stratFaber <- add.rule(stratFaber,
                                              name="ruleSignal",
                                              arguments=list(sigcol="Lo.lt.Donchian40",
                                                                                                        sigval=TRUE,
                                                                                                        orderqty=-1000,
                                                                                                        ordertype="market",
                                                                                                        orderside="short",
                                                                                                        pricemethod="market",
                                                                                                        TxnFees=-5,
                                                                                                        osFUN=osMaxPos),
                                              type="exit",
                                              path.dep=TRUE)

stratFaber <- add.rule(stratFaber,
                                              name="ruleSignal",
                                              arguments=list(sigcol="Lo.lt.Donchian40",
                                                                                                        sigval=TRUE,
                                                                                                        orderqty=-1000,
                                                                                                        ordertype="market",
                                                                                                        orderside="short",
                                                                                                        pricemethod="market",
                                                                                                        TxnFees=-5,
                                                                                                        osFUN=osMaxPos),
                                              type="enter",
                                              path.dep=TRUE)

stratFaber <- add.rule(stratFaber,
                                              name="ruleSignal",
                                              arguments=list(sigcol="Hi.gt.Donchian40",
                                                                                                        sigval=TRUE,
                                                                                                        orderqty=1000,
                                                                                                        ordertype="market",
                                                                                                        orderside="long",
                                                                                                        pricemethod="market",
                                                                                                        TxnFees=-5,
                                                                                                        osFUN=osMaxPos),
                                              type="exit",
                                              path.dep=TRUE)



# Set position limits so we don't add to the position every month Close > Donchian40
addPosLimit("faber", "SPY", timestamp=initDate, maxpos=1000, minpos=-1000)
addPosLimit("faber", "IEF", timestamp=initDate, maxpos=1000, minpos=-1000)

# Process the indicators and generate trades
out <- try(applyStrategy(strategy=stratFaber, portfolios="faber"))
updatePortf(Portfolio = "faber", Dates=paste('::',as.Date("2012-01-13"),sep=''))

# Evaluate results
portRet <- PortfReturns("faber")
portRet$Total <- rowSums(portRet, na.rm=TRUE)
charts.PerformanceSummary(portRet$Total)
tradeStats("faber")[,c("Symbol","Num.Trades","Net.Trading.PL","maxDrawdown")]


themelist<-chart <- theme()
themelist$col$up.col<-'green'
themelist$col$dn.col<-'pink'
chart.Posn(Portfolio="faber",Symbol="SPY",theme=themelist,log=TRUE, TA = "Donchian40")
