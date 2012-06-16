dataformatc = function(a){
if(any(colnames(a)=="CR")){colnames(a)[colnames(a)=="CR"]="CORR"}
if(any(colnames(a)=="OFFER")){colnames(a)[colnames(a)=="OFFER"]="OFR"}
if(any(colnames(a)=="BIDSIZE")){colnames(a)[colnames(a)=="BIDSIZE"]="BIDSIZ"}
if(any(colnames(a)=="OFFERSIZE")){colnames(a)[colnames(a)=="OFFERSIZE"]="OFRSIZ"}
return(a)
}

qdatacheck = function(qdata){
if(!is.xts(qdata)){stop("The argument qdata should be an xts object")}
if(!any(colnames(qdata)=="BID")){stop("The argument qdata should have a BID column")}
if(!any(colnames(qdata)=="OFR")){stop("The argument qdata should have a OFR column")}
}

tdatacheck = function(tdata){
if(!is.xts(tdata)){stop("The argument tdata should be an xts object")}
if(!any(colnames(tdata)=="PRICE")){stop("The argument tdata should have a PRICE column")}
}

tqdatacheck = function(tqdata){
if(!is.xts(tqdata)){stop("The argument tqdata should be an xts object")}
if(!any(colnames(tqdata)=="PRICE")){stop("The argument tqdata should have a PRICE column")}
if(!any(colnames(tqdata)=="BID")){stop("The argument tqdata should have a BID column")}
if(!any(colnames(tqdata)=="OFR")){stop("The argument tqdata should have a OFR column")}
}

rdatacheck = function(rdata,multi=FALSE){
#if(!is.xts(rdata)){stop("The argument rdata should be an xts object")} CAN PERFECTLY BE A MATRIX FOR ALL FUNCTIONS SO FAR...
if((dim(rdata)[2] < 2) & (multi)){stop("Your rdata object should have at least 2 columns")}
}


tradesCleanup = function(from,to,datasource,datadestination,ticker,exchanges,tdataraw=NULL,report=TRUE,selection="median",...){
       
  nresult = rep(0,5);
  if(is.null(tdataraw)){
  dates = timeSequence(from,to, format = "%Y-%m-%d", FinCenter = "GMT");
  dates = dates[isBizday(dates, holidays = holidayNYSE(2004:2010))];

  for(j in 1:length(dates)){
  datasourcex = paste(datasource,"/",dates[j],sep="");
  datadestinationx = paste(datadestination,"/",dates[j],sep="");

  for(i in 1:length(ticker)){
  dataname = paste(ticker[i],"_trades.RData",sep="");
  load(paste(datasourcex,"/",dataname,sep=""));

  if(class(tdata)[1]!="try-error"){
  exchange = exchanges[i];  
  
  tdata = dataformatc(tdata);  nresult[1]= nresult[1]+dim(tdata)[1];

  ##actual clean-up: 
  ##general:
  tdata = try(nozeroprices(tdata));  nresult[2]= nresult[2]+dim(tdata)[1];
  tdata = try(selectexchange(tdata,exch=exchange));  nresult[3]= nresult[3]+dim(tdata)[1];

  ##trade specific:
  tdata = try(salescond(tdata));   nresult[4] = nresult[4] + dim(tdata)[1];
  tdata = try(mergeTradesSameTimestamp(tdata,selection=selection));   nresult[5] = nresult[5] + dim(tdata)[1];

  save(tdata, file = paste(datadestinationx,"/",dataname,sep=""));
  }

  if(class(tdata)=="try-error")	{
  abc=1;
  save(abc, file = paste(datadestinationx,"/missing_",ticker[i],".RData",sep=""));
						}
  }
  }
  if(report==TRUE){
  names(nresult) = c("initial number","no zero prices","select exchange",
  "sales condition","merge same timestamp");
  return(nresult)
  }
  }

  if(!is.null(tdataraw)){
  if(class(tdataraw)[1]!="try-error"){
  if(length(exchanges)>1){print("The argument exchanges contains more than 1 element. Please select a single exchange, in case you provide tdataraw.")}
  tdata=tdataraw; rm(tdataraw);  
  tdata = dataformatc(tdata);  nresult[1]= nresult[1]+dim(tdata)[1];

  ##actual clean-up: 
  ##general:
  tdata = try(nozeroprices(tdata));  nresult[2]= nresult[2]+dim(tdata)[1];
  tdata = try(selectexchange(tdata,exch=exchanges));  nresult[3]= nresult[3]+dim(tdata)[1];

  ##trade specific:
  tdata = try(salescond(tdata));   nresult[4] = nresult[4] + dim(tdata)[1];
  tdata = try(mergeTradesSameTimestamp(tdata,selection=selection));   nresult[5] = nresult[5] + dim(tdata)[1];
  
  if(report==TRUE){
  names(nresult) = c("initial number","no zero prices","select exchange",
  "sales condition","merge same timestamp");
  return(list(tdata=tdata,report=nresult))
  }
  if(report!=TRUE){return(tdata)}
  }
  }

}

quotesCleanup = function(from,to,datasource,datadestination,ticker,exchanges, qdataraw=NULL,report=TRUE,selection="median",maxi=50,window=50,type="advanced",rmoutliersmaxi=10,...){
  nresult = rep(0,7);
  if(is.null(qdataraw)){
  dates = timeSequence(from,to, format = "%Y-%m-%d", FinCenter = "GMT");
  dates = dates[isBizday(dates, holidays = holidayNYSE(2004:2010))];

  for(j in 1:length(dates)){
  datasourcex = paste(datasource,"/",dates[j],sep="");
  datadestinationx = paste(datadestination,"/",dates[j],sep="");

  for(i in 1:length(ticker)){
  dataname = paste(ticker[i],"_quotes.RData",sep="");
  load(paste(datasourcex,"/",dataname,sep=""));

  if(class(qdata)[1]!="try-error"){
  exchange = exchanges[i];  
  if(exchange=="Q"){exchange="T"}

  qdata = dataformatc(qdata); nresult[1] = nresult[1]+dim(qdata)[1];
  ##actual clean-up:
  ##general:
  qdata = try(nozeroquotes(qdata)); nresult[2] = nresult[2]+dim(qdata)[1];
  qdata = try(selectexchange(qdata,exch=exchange)); nresult[3] = nresult[3]+dim(qdata)[1];

  ##quote specific:
  qdata = try(rmnegspread(qdata)); nresult[4] = nresult[4]+dim(qdata)[1];
  qdata = try(rmlargespread(qdata,maxi=maxi)); nresult[5] = nresult[5]+dim(qdata)[1];

  qdata = try(mergequotessametimestamp(qdata,selection=selection)); nresult[6] = nresult[6]+dim(qdata)[1];
  qdata = try(rmoutliers(qdata,maxi=rmoutliersmaxi,window=window,type=type)); nresult[7] = nresult[7]+dim(qdata)[1];

  save(qdata, file = paste(datadestinationx,"/",dataname,sep=""));
  }

  if(class(qdata)=="try-error"){
  abc=1;
  save(abc, file = paste(datadestinationx,"/missingquotes_",ticker[i],".RData",sep=""));
  }
  }
  }
  }

    if(!is.null(qdataraw)){
    if(class(qdataraw)[1]!="try-error"){
    if(length(exchanges)>1){print("The argument exchanges contains more than 1 element. Please select a single exchange, in case you provide qdataraw.")}
    if(class(qdataraw)[1]!="try-error"){
    exchange=exchanges;
    qdata = qdataraw; rm(qdataraw) 
    
    qdata = dataformatc(qdata); nresult[1] = nresult[1]+dim(qdata)[1];
    ##actual clean-up:
    ##general:
    qdata = try(nozeroquotes(qdata));                                           nresult[2] = nresult[2]+dim(qdata)[1];
    qdata = try(selectexchange(qdata,exch=exchange));                           nresult[3] = nresult[3]+dim(qdata)[1];

    ##quote specific:
    qdata = try(rmnegspread(qdata));                                            nresult[4] = nresult[4]+dim(qdata)[1];
    qdata = try(rmlargespread(qdata,maxi=maxi));                                nresult[5] = nresult[5]+dim(qdata)[1];

    qdata = try(mergequotessametimestamp(qdata,selection=selection));           nresult[6] = nresult[6]+dim(qdata)[1];
    qdata = try(rmoutliers(qdata,maxi=rmoutliersmaxi,window=window,type=type)); nresult[7] = nresult[7]+dim(qdata)[1];

     if(report==TRUE){
     names(nresult) = c("initial number","no zero quotes","select exchange",
     "remove negative spread","remove large spread","merge same timestamp","remove outliers");
     return(list(qdata=qdata,report=nresult))
     }
     if(report!=TRUE){return(qdata)}
    }
    }
    }
}


tradesCleanupFinal = function(from,to,datasource,datadestination,ticker,tdata=NULL,qdata=NULL,...){
  if(is.null(tdata)&is.null(qdata)){
  dates = timeSequence(from,to, format = "%Y-%m-%d", FinCenter = "GMT");
  dates = dates[isBizday(dates, holidays = holidayNYSE(2004:2010))];

  for(j in 1:length(dates)){
  datasourcex = paste(datasource,"/",dates[j],sep="");
  datadestinationx = paste(datadestination,"/",dates[j],sep="");

  for(i in 1:length(ticker)){
  dataname = paste(ticker[i],"_trades.RData",sep="");
  dataname2 = paste(ticker[i],"_quotes.RData",sep="");

  #Missing file??
  m1 = paste(datasourcex,"/missing_",ticker[i],".RData",sep="");
  m2 = paste(datasourcex,"/missingquotes_",ticker[i],".RData",sep="");
  miscondition = file.exists(m1)|file.exists(m1);
  a=FALSE;#check whether tried to clean

  if(!miscondition){
  #load trades and quotes
  load(paste(datasourcex,"/",dataname,sep=""));
  load(paste(datasourcex,"/",dataname2,sep=""));

  tdata = dataformatc(tdata);
  qdata = dataformatc(qdata);

  #1 cleaning procedure that needs cleaned trades and quotes
  tdata = try(rmtradeoutliers(tdata,qdata));

  #save
  save(tdata, file = paste(datadestinationx,"/",dataname,sep=""));
  a=TRUE;
					}

  if(a==TRUE){a=(class(tdata)=="try-error")}

  if(miscondition|a)	{
  abc=1;
  save(abc, file = paste(datadestinationx,"/missing_",ticker[i],".RData",sep=""));
						}
  }
  }
  }
  
  if((!is.null(tdata))&(!is.null(qdata))){
  tdata = dataformatc(tdata);
  qdata = dataformatc(qdata);

  #1 cleaning procedure that needs cleaned trades and quotes
  tdata = try(rmtradeoutliers(tdata,qdata));
  return(tdata);
  }
}

