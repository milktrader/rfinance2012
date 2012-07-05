#!/usr/bin/Rscript

library(quantmod)

i = 1
j = 44


#load data
symbol_str = 'FX'
#fn = '/opt/data/temp/gbpusd.csv'
fn = 'GBPUSD_M30_20021021-20120106.csv'
data.csv = read.csv(fn, sep=',', header=TRUE)
data.xts = as.xts(data.csv[,2:6], as.POSIXct(strptime(data.csv[,1], '%Y-%m-%d %H:%M:%S')))
colnames(data.xts) <- c('Open','High','Low','Close','Volume')
assign(symbol_str, data.xts)

FX <- FX['2002-10-21::2008-07-04']

#add indicator
FX.fast <- SMA(Cl(FX), n=i)
FX.slow <- SMA(Cl(FX), n=j)

golden_cross <- Lag(ifelse(FX.fast > FX.slow, 1, -1))
golden_cross <- na.locf(golden_cross, na.rm=TRUE)

#calc performance
#each coin represents p/l in pips for each bar
#momentum is positive and signal = 1 means a profit
#and a negative momentum with signal = -1 is also a profit
#so profits are always positive coins
coin         <- momentum(Cl(FX))*golden_cross
best_coin    <- max(coin)
worst_coin   <- min(coin)

#end of period p/l
total_coins <- sum(coin)

#annual_coin   <- round((last_coin-1)*100, digits=2)/(NROW(coin)/252)


cat('fast sma, slow sma, total_coins, best_coin, worst_coin:\n')
cat(i,j, total_coins, best_coin, worst_coin, "\n")

#vs buy and hold
cat('\nvs buy and hold:\n')
buy_n_hold = as.double(last(Cl(FX))) - as.double(first(Cl(FX)))
cat (buy_n_hold, '\n')
