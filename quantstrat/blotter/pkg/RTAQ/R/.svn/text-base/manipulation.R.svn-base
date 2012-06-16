#MANIPULATION FUNCTIONS:
TAQLoad = function(tickers,from,to,trades=TRUE,quotes=FALSE,datasource=NULL,variables=NULL){ 
  if( is.null(datasource)){print("Please provide the argument 'datasource' to indicate in which folder your data is stored")}

  if(!(trades&quotes)){#not both trades and quotes demanded
  for( ticker in tickers ){
      out = uniTAQload( ticker = ticker , from = from, to = to , trades=trades,quotes=quotes,datasource = datasource,variables=variables);
      if( ticker == tickers[1] ){ totalout = out 
	}else{ 
	totalout = merge(totalout,out) }
   }
   }

   if((trades&quotes)){#in case both trades and quotes
	totalout=list();
	totalout[[1]] = TAQload( ticker = tickers , from = from, to = to , trades=TRUE,quotes=FALSE,datasource = datasource,variables=variables);
	totalout[[2]] = TAQload( ticker = tickers , from = from, to = to , trades=FALSE,quotes=TRUE,datasource = datasource,variables=variables);
				}
return(totalout);
}

uniTAQload = function(ticker,from,to,trades=TRUE,quotes=FALSE,datasource=NULL,variables=NULL){
##Function to load the taq data from a certain stock 
#From&to (both included) should be in the format "%Y-%m-%d" e.g."2008-11-30"
  dates = timeSequence(as.character(from),as.character(to), format = "%Y-%m-%d", FinCenter = "GMT")
  dates = dates[isBizday(dates, holidays = holidayNYSE(2004:2010))];

  if(trades){ tdata=NULL;
  for(i in 1:length(dates)){
  datasourcex = paste(datasource,"/",dates[i],sep="");
  filename = paste(datasourcex,"/",ticker,"_trades.RData",sep="");

  ifmissingname = paste(datasourcex,"/missing_",ticker,".RData",sep="");  

  if(file.exists(ifmissingname)){stop(paste("No trades available on ",dates[i],sep=""))}
  if(!file.exists(filename)){stop(paste("The file ",filename," does not exist. Please read the documentation.",sep=""))}
  if(file.exists(ifmissingname)==FALSE){
  load(filename);
  if(i==1)	{
	  if( is.null(variables)){totaldata=tdata;
		}else{
		allnames=as.vector(colnames(tdata));
		selection = allnames%in%variables;
		qq=(1:length(selection))[selection];
		totaldata=tdata[,qq];
		}	  
		};
  if(i>1){
  if( is.null(variables)){totaldata=rbind(totaldata,tdata);
		}else{
	totaldata=rbind(totaldata,tdata[,qq])};
		}
  rm(tdata);
				}
				}
				}

  if(quotes){ qdata=NULL;
  for(i in 1:length(dates)){
  datasourcex = paste(datasource,"/",dates[i],sep="");
  filename = paste(datasourcex,"/",ticker,"_quotes.RData",sep="");
  ifmissingname = paste(datasourcex,"/missingquotes_",ticker,".RData",sep="");
  
  if(file.exists(ifmissingname)){stop(paste("no quotes available on ",dates[i],sep=""))}
  if(!file.exists(filename)){stop(paste("The file ",filename," does not exist. Please read the documentation.",sep=""))}
  if(file.exists(ifmissingname)==FALSE){
  load(filename);

  if(i==1)	{
	  if( is.null(variables)){totaldataq=qdata;
		}else{
		allnames=as.vector(colnames(qdata));
		selection = allnames%in%variables;
		qq=(1:length(selection))[selection];
		totaldataq=qdata[,qq];
		}	  
		}
  if(i>1){
  if( is.null(variables)){totaldataq=rbind(totaldataq,qdata);
		}else{
	totaldataq=rbind(totaldataq,qdata[,qq])};
		}
  rm(qdata);
				}
				}
				}

  if(trades&quotes){return(list(trades = totaldata,quotes=totaldataq))}
  if(trades==TRUE & quotes==FALSE){return(totaldata)}
  if(trades==FALSE & quotes==TRUE){return(totaldataq)}
  }


matchTradesQuotes = function(tdata,qdata,adjustment=2){ ##FAST VERSION
tdata = dataformatc(tdata);
qdata = dataformatc(qdata);
qdatacheck(qdata);
tdatacheck(tdata);

  tt = dim(tdata)[2];  
  index(qdata) = index(qdata) + adjustment;

  #merge:
  merged = merge(tdata,qdata);

  ##fill NA's:
  merged[,((tt+1):dim(merged)[2])] = na.locf(as.zoo(merged[,((tt+1):dim(merged)[2])]), na.rm=FALSE);

  #Select trades:
  index(tdata)=  as.POSIXct(index(tdata));
  index(merged)= as.POSIXct(index(merged));  
  merged = merged[index(tdata)];

  #return useful parts:
	#remove duplicated SYMBOL & EX (new)
	eff =  colnames(merged);
	realnames = c("SYMBOL","EX","PRICE","SIZE","COND","CORR","G127","BID","BIDSIZ","OFR","OFRSIZ","MODE");
	condition = (1:length(eff))[eff%in%realnames];
	merged = merged[,condition];

  ##a bit rough but otherwise opening price disappears...
  merged = as.xts(na.locf(as.zoo(merged),fromLast=TRUE));

  index(merged) = as.timeDate(index(merged));
  return(merged)
}

#matchtq_old = function(tdata,qdata,adjustment=2){ ##FAST VERSION
#qdata = dataformatc(qdata);
#tdata = dataformatc(tdata);
#
#  tt = dim(tdata)[2];  
#  index(qdata) = index(qdata) + adjustment;
#  
#  #merge:
#  counter = xts(as.character(1:dim(qdata)[1]),order.by=index(qdata))#an integer for every quote
#  merged = cbind(qdata,counter);
#  merged = merge(tdata,merged);
#  
#  ##fill NA's:
#  merged[,((tt+1):dim(merged)[2])] = na.locf(as.zoo(merged[,((tt+1):dim(merged)[2])]), na.rm=FALSE);
#  
#  #Select trades:
#  merged = merged[index(tdata)];
#  
#  #Remove duplicated quotes:
#  merged = merged[!duplicated(merged[,dim(merged)[2]])];
#
#  #return usefull parts:
#  merged = merged[,c((1:tt),((tt+3):(dim(merged)[2]-1)))];
#
#  return(merged)
#}



getTradeDirection = function(tqdata,...){
  if(hasArg(data)){ tqdata = data; rm(data) }
  tqdata = dataformatc(tqdata);
  tqdatacheck(tqdata); 

##Function returns a vector with the inferred trade direction:
##NOTE: the value of the first (and second) observation should be ignored if price=midpoint for the first (second) observation.
  bid = as.numeric(tqdata$BID);
  offer = as.numeric(tqdata$OFR);
  midpoints = (bid + offer)/2;
  price = as.numeric(tqdata$PRICE);
 
  buy1 = price > midpoints; #definitely a buy
  equal = price == midpoints;
  dif1 = c(TRUE,0 < price[2:length(price)]-price[1:(length(price)-1)]);#for trades=midpoints: if uptick=>buy
  equal1 = c(TRUE,0 == price[2:length(price)]-price[1:(length(price)-1)]);#for trades=midpoints: zero-uptick=>buy
  dif2 = c(TRUE,TRUE,0 < price[3:length(price)]-price[1:(length(price)-2)]);

  buy = buy1 | (dif1 & equal) | (equal1 & dif2 & equal);

  buy[buy==TRUE]=1;
  buy[buy==FALSE]=-1;
  
  return(buy);
}

es = function(data){
data = dataformatc(data);
#returns the effective spread as xts object
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  midpoints = (bid + offer)/2;
  price = as.numeric(data$PRICE);
  d = gettradedir(data);
 
  es=xts(2*d*(price-midpoints),order.by=index(data));
  return(es);
}

rs = function(data,tdata,qdata){
data = dataformatc(data);
qdata = dataformatc(qdata);
tdata = dataformatc(tdata);

###Function returns the realized spread as an xts object
#Please note that the returned object can contain less observations that the original "data"
#because of the need to find quotes that match the trades 5 min ahead

#arguments
#data=> xts object containing matched trades and quotes
#tdata and qdata, the xts object containing the trades and quotes respectively

  ##First part solves the problem that unequal number of obs (in data and data2) is possible when computing the RS
  data2 = matchtq(tdata,qdata,adjustment =300);
  if(dim(data2)[1]>dim(data)[1]){
  condition = as.vector(as.character(index(data2)))%in%as.vector(as.character(index(data)));
  data2 = subset(data2,condition,select=1:(dim(data)[2]));
  data = subset(data,as.vector(as.character(index(data)))%in%as.vector(as.character(index(data2))),select=1:(dim(data2)[2]));
  }

  if(dim(data2)[1]<dim(data)[1]){
  condition = as.vector(as.character(index(data)))%in%as.vector(as.character(index(data2)));
  data = subset(data,condition,select=1:(dim(data2)[2]));
  data2 = subset(data2,as.vector(as.character(index(data2)))%in%as.vector(as.character(index(data))),select=1:(dim(data)[2]));
  }


  bid = as.numeric(data2$BID);
  offer = as.numeric(data2$OFR);
  midpoints = (bid + offer)/2;
  price = as.numeric(data$PRICE);
  d = gettradedir(data);
  rs = 2*d*(price-midpoints);

  rs_xts = xts(rs,order.by=index(data));
  return(rs_xts);
}

value_trade = function(data){
data = dataformatc(data);
#returns the trade value as xts object
  price = as.numeric(data$PRICE);
  size = as.numeric(data$SIZE);
  
  value = xts(price*size,order.by=index(data));
  return(value);
}

signed_value_trade = function(data){
data = dataformatc(data);
#returns the signed trade value as xts object
  price = as.numeric(data$PRICE);
  size = as.numeric(data$SIZE);
  d = gettradedir(data);

  value = xts(d*price*size,order.by=index(data));
  return(value);
}


signed_trade_size = function(data){
data = dataformatc(data);
#returns the signed size of the trade as xts object
  size = as.numeric(data$SIZE);
  d = gettradedir(data);

  value = xts(d*size,order.by=index(data));
  return(value);
}

di_diff = function(data){
data = dataformatc(data);
#returns the depth imbalance (as a difference) as xts object
  bidsize = as.numeric(data$BIDSIZ);
  offersize = as.numeric(data$OFRSIZ);

  d = gettradedir(data);
  di = (d*(offersize-bidsize))/(offersize+bidsize);
  di_xts = xts(di,order.by=index(data));
  return(di_xts);
}

di_div = function(data){
data = dataformatc(data);
#returns the depth imbalance (as a ratio) as xts object
  bidsize = as.numeric(data$BIDSIZ);
  offersize = as.numeric(data$OFRSIZ);
  d = gettradedir(data);

  di = (offersize/bidsize)^d;
  di_xts = xts(di,order.by=index(data));
  return(di_xts);
}

pes = function(data){
data = dataformatc(data);
#returns the Proportional Effective Spread as xts object
  es = es(data);
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  midpoints = (bid + offer)/2;

  pes = es/midpoints
  pes_xts = xts(pes,order.by=index(data));
  return(pes_xts);
}

prs = function(data,tdata,qdata){
data = dataformatc(data);
#returns the Proportional Realized Spread as xts object
  rs = rs(data,tdata,qdata);
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  midpoints = (bid + offer)/2;
  prs = rs/midpoints
  prs_xts = xts(prs,order.by=index(data));
  return(prs_xts);
}

price_impact = function(data,tdata,qdata){
data = dataformatc(data);
#returns the Price impact as xts object
  rs = rs(data,tdata,qdata);
  es = es(data);

  pi = (es-rs)/2;
  pi_xts = xts(pi,order.by=index(data));
  return(pi_xts);
}

prop_price_impact = function(data,tdata,qdata){
data = dataformatc(data);
#returns the Proportional Price impact as xts object
  rs = rs(data,tdata,qdata);
  es = es(data);
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  midpoints = (bid + offer)/2;

  prop_pi = ((es-rs)/2)/midpoints;
  prop_pi_xts = xts(prop_pi,order.by=index(data));
  return(prop_pi_xts);
}

tspread = function(data){
data = dataformatc(data);
#returns the half traded spread as xts object
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  midpoints = (bid + offer)/2;
  price = as.numeric(data$PRICE);
  d = gettradedir(data);

  ts = xts(d*(price-midpoints),order.by=index(data));
  return(ts);
}

pts = function(data){
data = dataformatc(data);
#returns the proportional half traded spread as xts object
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  midpoints = (bid + offer)/2;
  price = as.numeric(data$PRICE);
  d = gettradedir(data);
  pts = (d*(price-midpoints))/midpoints;

  pts_xts = xts(pts,order.by=index(data));
  return(pts_xts);
}

p_return_sqr = function(data){
data = dataformatc(data);
#returns the squared log return on Trade prices as xts object
  price = as.numeric(data$PRICE);
  return = c(0,log(price[2:length(price)])-log(price[1:length(price)-1]));
  sqr_return = return^2;

  sqr_return_xts = xts(sqr_return,order.by=index(data));
  return(sqr_return_xts);
}

qs = function(data){
data = dataformatc(data);
#returns the quoted spread as xts object
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  qs = offer-bid;

  qs_xts = xts(qs,order.by=index(data));
  return(qs_xts);
}

pqs = function(data){
data = dataformatc(data);
#returns the proportional quoted spread as xts object
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  midpoints = (bid + offer)/2;
  qs = offer-bid;
  pqs = qs/midpoints;

  pqs_xts = xts(pqs,order.by=index(data));
  return(pqs_xts);
}

logqs = function(data){
data = dataformatc(data);
#returns the logarithm of the quoted spread as xts object
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  logqs = log(offer/bid);

  logqs_xts = xts(logqs,order.by=index(data));
  return(logqs_xts);
}

logsize = function(data){
data = dataformatc(data);
#returns the log quoted size as xts object
  bidsize = as.numeric(data$BIDSIZ);
  offersize = as.numeric(data$OFRSIZ);
  logsize = log(bidsize)+log(offersize);

  logsize_xts = xts(logsize,order.by=index(data));
  return(logsize_xts);
}

qslope = function(data){
data = dataformatc(data);
#returns the quoted slope as xts object
  logsize = logsize(data);
  qs = qs(data);

  qslope = qs/logsize;

  qslope_xts = xts(qslope,order.by=index(data));
  return(qslope_xts);
}

logqslope = function(data){
data = dataformatc(data);
#returns the log quoted slope as xts object
  logqs = logqs(data);
  logsize = logsize(data);
  
  logqslope = logqs/logsize;

  logqslope_xts = xts(logqslope,order.by=index(data));
  return(logqslope_xts);
}

mq_return_sqr = function(data){
data = dataformatc(data);
#returns midquote squared returns slope as xts object
  mq_return = mq_return(data);
  
  mq_return_sqr = mq_return^2;

  mq_return_sqr_xts = xts(mq_return_sqr,order.by=index(data));
  return(mq_return_sqr_xts);
}

mq_return_abs = function(data){
data = dataformatc(data);
#returns absolute midquote returns slope as xts object
  mq_return = mq_return(data);
  
  mq_return_abs = abs(mq_return);

  mq_return_abs_xts = xts(mq_return_abs,order.by=index(data));
  return(mq_return_abs_xts);
}

tqLiquidity <- function(tqdata=NULL,tdata=NULL,qdata=NULL,type,...) {
  if(hasArg(data)){ tqdata = data }
  if(!is.null(tqdata)){tqdatacheck(tqdata)}
  if(!is.null(qdata)){qdatacheck(qdata)}
  if(!is.null(tdata)){tdatacheck(tdata)}
  
  result=switch(type,
  es = es(tqdata),
  rs = rs(tqdata,tdata,qdata),
  value_trade = value_trade(tqdata),
  signed_value_trade = signed_value_trade(tqdata),
  di_diff = di_diff(tqdata),
  pes = pes(tqdata),
  prs = prs(tqdata,tdata,qdata),
  price_impact = price_impact(tqdata,tdata,qdata),
  prop_price_impact = prop_price_impact(tqdata,tdata,qdata),
  tspread =tspread(tqdata),
  pts = pts(tqdata),
  p_return_sqr = p_return_sqr(tqdata),
  p_return_abs = p_return_abs(tqdata),
  qs = qs(tqdata),
  pqs = pqs(tqdata),
  logqs = logqs(tqdata),
  logsize = logsize(tqdata),
  qslope = qslope(tqdata),
  logqslope = logqslope(tqdata),
  mq_return_sqr = mq_return_sqr(tqdata),
  mq_return_abs = mq_return_abs(tqdata),
  signed_trade_size = signed_trade_size(tqdata)
  )

  colnames(result)=type;
  return(result);
}

##help_function:
mq_return = function(data){
data = dataformatc(data);
#function returns the midquote logreturns as xts object
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  midpoints = (bid + offer)/2;
  mq_return = c(0,log(midpoints[2:length(midpoints)])-log(midpoints[1:length(midpoints)-1]));

  mq_return_xts = xts(mq_return,order.by=index(data));
  return(mq_return_xts);
}


###Zivot:
makeReturns = function(ts){
  l = dim(ts)[1];
  x = matrix(as.numeric(ts),nrow=l);
  x[(2:l),] = log(x[(2:l),]) - log(x[(1:(l-1)),])
  x[1,] = rep(0,dim(ts)[2]);
  x = xts(x,order.by=index(ts));
  return(x);
}

#p_return <- function (data)
#{
#    price = as.numeric(data$PRICE)
#    log.return = c(0, log(price[2:length(price)]) - log(price[1:length(price) -
#        1]))
#    return_xts = xts(log.return, order.by = index(data))
#    return(return_xts)
#}

p_return_abs <- function (data)
{
    price = as.numeric(data$PRICE)
    return = c(0, log(price[2:length(price)]) - log(price[1:length(price) -
        1]))
    abs_return = abs(return)
    abs_return_xts = xts(abs_return, order.by = index(data))
    return(abs_return_xts)
}


