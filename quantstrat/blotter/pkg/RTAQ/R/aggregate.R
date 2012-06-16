previoustick = function(a){
a = as.vector(a);
b = a[length(a)];
return(b)
}

weightedaverage = function(a){
aa = as.vector(as.numeric(a[,1]));
bb = as.vector(as.numeric(a[,2]));
c = weighted.mean(aa,bb);
return(c)
}

period.apply2 = function (x, INDEX, FUN2, ...) 
{
    x <- try.xts(x, error = FALSE)
    FUN <- match.fun(FUN2)
    xx <- sapply(1:(length(INDEX) - 1), function(y) {
        FUN(x[(INDEX[y] + 1):INDEX[y + 1]], ...)
    })
    reclass(xx, x[INDEX])
}

## AGGREGATION;
aggregatets = function (ts, FUN = "previoustick", on = "minutes", k = 1, weights = NULL,dropna=FALSE) 
{
    makethispartbetter = ((!is.null(weights))| on=="days"|on=="weeks"| (FUN!="previoustick")|dropna);
    if(makethispartbetter)  {

    FUN = match.fun(FUN);
    
    if (is.null(weights)) {
        ep = endpoints(ts, on, k)
        if(dim(ts)[2]==1){ ts2 = period.apply(ts, ep, FUN) }
        if(dim(ts)[2]>1){  ts2 = xts(apply(ts,2,FUN=period.apply2,FUN2=FUN,INDEX=ep),order.by=index(ts)[ep],)}
    }
    if (!is.null(weights)) {
        tsb = cbind(ts, weights)
        ep = endpoints(tsb, on, k)
        ts2 = period.apply(tsb, ep, FUN = match.fun(weightedaverage) )
    }
    if (on == "minutes" | on == "mins" | on == "secs" | on == 
        "seconds") {
        if (on == "minutes" | on == "mins") {
            secs = k * 60
        }
        if (on == "secs" | on == "seconds") {
            secs = k
        }
        a = .index(ts2) + (secs - .index(ts2)%%secs)
        ts3 = .xts(ts2, a,tzone="GMT")
    }
    if (on == "hours") {
        secs = 3600
        a = .index(ts2) + (secs - .index(ts2)%%secs)
        ts3 = .xts(ts2, a,tzone="GMT")
    }
    if (on == "days") {
        secs = 24 * 3600
        a = .index(ts2) + (secs - .index(ts2)%%secs) - (24 * 
            3600)
        ts3 = .xts(ts2, a,tzone="GMT")
    }
    if (on == "weeks") {
        secs = 24 * 3600 * 7
        a = (.index(ts2) + (secs - (.index(ts2) + (3L * 86400L))%%secs)) - 
            (24 * 3600)
        ts3 = .xts(ts2, a,tzone="GMT")
    }

    if (!dropna) {
        if (on != "weeks" | on != "days") {
            if (on == "secs" | on == "seconds") {
                tby = "s"
            }
            if (on == "mins" | on == "minutes") {
                tby = "min"
            }
            if (on == "hours") {
                tby = "h"
            }
            by = paste(k, tby, sep = " ")
            allindex = as.timeDate(base:::seq(start(ts3), end(ts3), 
                by = by))
            xx = xts(rep("1", length(allindex)), order.by = allindex)
            ts3 = merge(ts3, xx)[, (1:dim(ts)[2])]
        }
    }
    
    index(ts3) = as.timeDate(index(ts3));
    return(ts3);
    }
    
    if(!makethispartbetter){
     if (on == "secs" | on == "seconds") { secs = k; tby = paste(k,"sec",sep=" ")}
     if (on == "mins" | on == "minutes") { secs = 60*k; tby = paste(60*k,"sec",sep=" ")}
     if (on == "hours") {secs = 3600*k; tby = paste(3600*k,"sec",sep=" ")}
    
    FUN = match.fun(FUN);
    
    g = base:::seq(start(ts), end(ts), by = tby);
    rawg = as.numeric(as.POSIXct(g,tz="GMT"));
    newg = rawg + (secs - rawg%%secs);
    g = as.timeDate(as.POSIXct(newg,origin="1970-01-01",tz="GMT"));
    ts3 = na.locf(merge(ts, zoo(, g)))[as.POSIXct(g,tz="GMT")]; 
    return(ts3) 
    }
}

#PRICE (specificity: opening price and previoustick)

aggregatePrice = function (ts, FUN = "previoustick", on = "minutes", k = 1,marketopen="09:30:00",marketclose = "16:00:00") 
{

    ts = dataformatc(ts)
    ts2 = aggregatets(ts, FUN = FUN, on, k)
    date = strsplit(as.character(index(ts)), " ")[[1]][1]

	#open
    a = as.timeDate(paste(date, marketopen))
    b = as.xts(matrix(as.numeric(ts[1]),nrow=1), a)
    ts3 = c(b, ts2)

	#close
    aa = as.timeDate(paste(date, marketclose))
    condition = index(ts3) < aa
    ts3 = ts3[condition]
    bb = as.xts(matrix(as.numeric(last(ts)),nrow=1), aa)
    ts3 = c(ts3, bb)

    return(ts3)
}

#VOLUME: (specificity: always sum)
agg_volume= function(ts, FUN = "sumN", on = "minutes", k = 5, includeopen = FALSE,marketopen="09:30:00",marketclose="16:00:00") 
{

    ts = dataformatc(ts)
    if (!includeopen) {
        ts3 = aggregatets(ts, FUN = FUN, on, k)
    }
    if (includeopen) {
        ts2 = aggregatets(ts, FUN = FUN, on, k)
        date = strsplit(as.character(index(ts)), " ")[[1]][1]
        a = as.timeDate(paste(date, marketopen))
	  b = as.xts(matrix(as.numeric(ts[1]),nrow=1), a)
        ts3 = c(b, ts2)
    }

    aa = as.timeDate(paste(date, marketclose))
    condition = index(ts3) < aa
    ts4 = ts3[condition]

    lastinterval = matrix(colSums(matrix(ts3[!condition],ncol=dim(ts3)[2])),ncol=dim(ts3)[2])
    bb = xts(lastinterval, aa)
    ts4 = c(ts4, bb)

    return(ts4)
}

aggregateTrades =  function (tdata, on = "minutes", k = 5,marketopen="09:30:00",marketclose="16:00:00") 
{
	tdata = dataformatc(tdata)
	tdatacheck(tdata)
  ## Aggregates an entire trades xts object (tdata) over a "k"-minute interval.
  ## Returned xts-object contains: SYMBOL,EX,PRICE,SIZE.
  ## Variables COND, CORR, G127 are dropped because aggregating them makes no sense.
  ## NOTE: first observation (opening price) always included.

	selection = colnames(tdata)%in%c("PRICE","EX","SYMBOL");
	tdata1 = tdata[,selection];
	PRICE = aggregatePrice(tdata$PRICE,on=on,k=k,marketopen=marketopen,marketclose=marketclose);
	SIZE = agg_volume(tdata$SIZE, on = on, k = k, includeopen = TRUE,marketopen=marketopen,marketclose=marketclose)

	EX = rep(tdata$EX[1], length(PRICE));
	SYMBOL = rep(tdata$SYMBOL[1], length(PRICE));

	all = data.frame(SYMBOL, EX, PRICE, SIZE);
	colnames(all) = c("SYMBOL", "EX", "PRICE", "SIZE");
	ts = xts(all, index(SIZE));
	return(ts);
}

###QUOTES AGGREGATION:
aggregateQuotes = function(qdata,on="minutes",k=5,marketopen="09:30:00",marketclose="16:00:00"){
  qdata = dataformatc(qdata);
  qdatacheck(qdata);

  ## Aggregates an entire quotes xts object (qdata) object over a "k"-minute interval.
  ## Returned xts-object contains: SYMBOL,EX,BID,BIDSIZ,OFR,OFRSIZ.
  ## Variable MODE is dropped because aggregation makes no sense.
  ## "includeopen" determines whether to include the exact opening quotes.

  BIDOFR = aggregatePrice(cbind(qdata$BID,qdata$OFR),on=on,k=k,marketopen=marketopen,marketclose=marketclose);
  BIDOFRSIZ = agg_volume(cbind(qdata$BIDSIZ,qdata$OFRSIZ),on=on,k=k,includeopen=TRUE,marketopen=marketopen,marketclose=marketclose);

  EX = rep(qdata$EX[1],dim(BIDOFR)[1]);
  SYMBOL = rep(qdata$SYMBOL[1],dim(BIDOFR)[1]);

  all = data.frame(SYMBOL,EX,BIDOFR[,1],BIDOFRSIZ[,1],BIDOFR[,2],BIDOFRSIZ[,2]);
  colnames(all) =c("SYMBOL","EX","BID","BIDSIZ","OFR","OFRSIZ");

  ts = xts(all,index(BIDOFR));
  return(ts);
}

##LIQUIDITY AGGREGATION:
##Just combine aggregation functions and spot liquidity functions!


###### refresh time ########
refreshTime = function(pdata){
dim = length(pdata);
lengths = rep(0,dim+1);
  for(i in 1:dim){
    lengths[i+1] = length(pdata[[i]]);
  }
  minl = min(lengths[(2:(dim+1))]); #number of obs for stock with least observations
  lengths = cumsum(lengths);        
  alltimes = rep(0,lengths[dim+1]); #all timestamps in 1 vector
  for(i in 1:dim){
    alltimes[(lengths[i]+1):lengths[i+1]] = as.numeric(as.POSIXct(index(pdata[[i]]),tz="GMT"));
  }
  
  #get refresh points via C:
  x = .C("refreshpoints",as.integer(alltimes),as.integer(lengths),
  as.integer(rep(0,minl)),as.integer(dim),
  as.integer(0),as.integer(rep(0,minl*dim)),as.integer(minl));

  #get matrix with "refresh prices"
  newlength=x[[5]];
  pmatrix = matrix(ncol=dim,nrow=newlength);
  
  for(i in 1:dim){
  selection = x[[6]][((i-1)*minl+1):(i*minl)];
  pmatrix[,i] = pdata[[i]][ selection[1:newlength] ];
  }

  time = as.timeDate(as.POSIXct(x[[3]][1:newlength],origin="1970-01-01",tz="GMT"));

  resmatrix = xts(pmatrix,order.by=time)
  return(resmatrix);
}

