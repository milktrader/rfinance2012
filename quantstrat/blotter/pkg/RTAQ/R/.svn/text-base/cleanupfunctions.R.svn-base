##### Help functions
## help function to make all time notation consistent
adjtime = function(z){ 
  zz = unlist(strsplit(z,":")); 
  if(nchar(zz[1])!=2){
  return(paste(paste(0,zz[1],sep=""),zz[2],zz[3],sep=":"))}
  return(z);
  }
  
  period.apply3 = function (x, INDEX, FUN, ...) 
{
#small adaptation of the xts - function which experiences some troubles in multidimensional setting
    x <- try.xts(x, error = FALSE)
    FUN <- match.fun(FUN)
    xx <- sapply(1:(length(INDEX) - 1), function(y) {
        FUN(x[(INDEX[y] + 1):INDEX[y + 1]], ...)
    })
    if (is.vector(xx)) 
        xx <- t(xx)
    xx <- t(xx)
    reclass(xx, x[INDEX])
}

########## DATA CLEAN-UP: FOR ALL DATA #####################

####FUNCTION TO FILTER EXCHANGE HOURS ONLY: ExchangeHoursOnly
exchangeHoursOnly = function(data, daybegin = "09:30:00",dayend="16:00:00")
{
data = dataformatc(data);
    # a function to excerpt data within exchange trading hours
    # daybegin and dayend: two characters in the format of "HH:MM:SS",
    #                specifying the starting hour and minute and sec of an exhange
    #               trading day and the closing hour and minute and sec
    #                   of the trading day repectively
        
    if(!is(data, "xts"))
        stop("data must be an xts object")

  gettime = function(z){unlist(strsplit(as.character(z)," "))[2]};
  times1 = as.matrix(as.vector(as.character(index(data))));
  times = apply(times1,1,gettime); 
  tdtimes = timeDate:::timeDate(times,format = "%H:%M:%S",FinCenter = "GMT",zone="GMT");

  #create timeDate begin and end
  tddaybegin = timeDate:::timeDate(daybegin,format = "%H:%M:%S",FinCenter = "GMT",zone="GMT");
  tddayend = timeDate:::timeDate(dayend,format = "%H:%M:%S",FinCenter = "GMT",zone="GMT");

  #select correct observations
  filteredts = data[tdtimes>=tddaybegin & tdtimes<=tddayend];
  return(filteredts);
}


noZeroPrices = function(tdata){
tdata = dataformatc(tdata);
tdatacheck(tdata);
####FUNCTION TO DELETE ZERO PRICES: nozeroprices
filteredts = tdata[as.numeric(tdata$PRICE)!= 0];
return(filteredts);
}


selectExchange = function(data,exch="N"){ 
data = dataformatc(data);
###FUNCTION TO SELECT THE OBSERVATIONS OF A SINGLE EXCHANGE: selectexchange
filteredts = data[data$EX==exch];
return(filteredts);
}

autoSelectExchangeTrades = function(tdata){
tdata = dataformatc(tdata);
tdatacheck(tdata);
## AUTOSELECT EXCHANGE WITH HIGHEST NUMBER OF SHARES TRADED (for trades) ON:
#function returns ts with obs of only 1 exchange
#searches exchange with a maximum on the variable "SIZE"
  nobs=c();

  exchanges = c("Q","A","P","B","C","N","D","X","I","M","W","Z");
  exchangenames = c("NASDAQ","AMEX","ARCA","Boston","NSX","NYSE","NASD ADF and TRF","Philadelphia","ISE","Chicago","CBOE","BATS");
  

  z1 = sum(as.numeric(selectexchange(tdata,"Q")$SIZE));
  z2 = sum(as.numeric(selectexchange(tdata,"T")$SIZE));
  z = max(z1,z2);
  watchout = z == z2;
  nobs = cbind(nobs,z);

  for(i in 2:length(exchanges)) {
  z = sum(as.numeric(selectexchange(tdata,exchanges[i])$SIZE));
  nobs = cbind(nobs,z); 
                        }

  exch = exchanges[max(nobs)==nobs];

  as.character(tdata$EX[1]) == exchanges;
  namechosen = exchangenames[exch==exchanges];
  print(paste("The information of the",namechosen,"exchange was collected"));
  
  if(exch=="Q"&watchout){exch="T"}
  filteredtdata = tdata[tdata$EX==exch];
}


##### TRADE DATA SPECIFIC FUNCTIONS: ###################################
#salescond = function(tdata){ 
#tdata = dataformatc(tdata);
###DELETE ENTRIES WITH AN ABONORMAL SALES CONDITION
#filteredts = tdata[tdata$COND == "0"|tdata$COND == "E"|tdata$COND == "F"];
#return(filteredts);
#}

#zivot
salesCondition <- function (tdata)
{
tdatacheck(tdata);
    filteredts = tdata[tdata$COND == "0" | tdata$COND == "E" |
        tdata$COND == "F" | tdata$COND == "" | tdata$COND == "@F"]
    return(filteredts)
}


##Merge same timestamp:
sumN = function(a){
  a = sum(as.numeric(a));
  return(a)
}

medianN = function(a){
  a = median(as.numeric(a));
  return(a)
}

maxvol = function(a){
  p = as.numeric(a[,1]);
  s = as.numeric(a[,2]);

  b = median(p[s == max(s)]);
  return(b);
}

waverage = function(a){
  p = as.numeric(a[,1]);
  s = as.numeric(a[,2]);

  b = sum(p*s/sum(s));
  return(b);
}

mergeTradesSameTimestamp = function (tdata, selection = "median") 
{
    tdata = dataformatc(tdata)
    tdatacheck(tdata)
    ep = endpoints(tdata, "secs")
    size = period.apply(tdata$SIZE, ep, sumN)
    if (selection == "median") {
        price = period.apply3(tdata$PRICE, ep, medianN)
    }
    if (selection == "maxvolume") {
        price = period.apply3(cbind(tdata$PRICE, tdata$SIZE), 
            ep, maxvol)
    }
    if (selection == "weightedaverage") {
        price = period.apply3(cbind(tdata$PRICE, tdata$SIZE), 
            ep, waverage)
    }
    selection = ep[2:length(ep)]
    tdata2 = tdata[selection]
    tdata2$PRICE = price
    tdata2$SIZE = size
    return(tdata2)
}

rmTradeOutliers = function(tdata,qdata){
tdata = dataformatc(tdata);
qdata = dataformatc(qdata);
qdatacheck(qdata);
tdatacheck(tdata);

##Function to delete entries with prices that are above the ask plus the bid-ask
##spread. Similar for entries with prices below the bid minus the bid-ask
##spread.
  data = matchtq(tdata,qdata);
  price = as.numeric(data$PRICE);
  bid = as.numeric(data$BID);
  offer = as.numeric(data$OFR);
  spread = offer - bid;

  upper = offer+spread;
  lower = bid-spread;

  tdata = tdata[(price<upper) & (price>lower)];
  return(tdata);
}


#################       QUOTE SPECIFIC FUNCTIONS:       #################

noZeroQuotes = function(qdata){
qdata = dataformatc(qdata);
qdatacheck(qdata);
####FUNCTION TO DELETE ZERO QUOTES: nozeroquotes
filteredts = qdata[as.numeric(qdata$BID)!= 0& as.numeric(qdata$OFR)!= 0];
return(filteredts);
}


autoSelectExchangeQuotes = function(qdata){
qdata = dataformatc(qdata);
qdatacheck(qdata);
####Autoselect exchange with highest value for (bidsize+offersize)
  nobs=c();
  exchanges = c("Q","A","P","B","C","N","D","X","I","M","W","Z");
  exchangenames = c("NASDAQ","AMEX","ARCA","Boston","NSX","NYSE","NASD ADF and TRF","Philadelphia","ISE","Chicago","CBOE","BATS");

  selected1 = selectexchange(qdata,"Q");
  selected2 = selectexchange(qdata,"T");
  z1 = sum(as.numeric(selected1$BIDSIZ)+as.numeric(selected1$OFRSIZ));
  z2 = sum(as.numeric(selected2$BIDSIZ)+as.numeric(selected2$OFRSIZ));
  z = max(z1,z2);
  watchout = z == z2;
  nobs = cbind(nobs,z);

  for(i in 2:length(exchanges)) {
  selected = selectexchange(qdata,exchanges[i]);
  z = sum(as.numeric(selected$BIDSIZ)+as.numeric(selected$OFRSIZ));
  nobs = cbind(nobs,z); 
                        }

  exch=exchanges[max(nobs)==nobs];

  namechosen = exchangenames[exch==exchanges];  
  print(paste("The information of the",namechosen,"exchange was collected"));

  if(exch=="Q"&watchout){exch="T"}

  filteredts = qdata[qdata$EX==exch];
  return(filteredts);
}


mergeQuotesSameTimestamp = function (qdata, selection = "median") 
{
    qdata = dataformatc(qdata)
    qdatacheck(qdata)
    condition = selection == "median" | selection == "maxvolume" | 
        selection == "weightedaverage"
    if (!condition) {
        print(paste("WARNING:The result will be corrupted. Check whether", 
            selection, "is an existing option for the attribute selection."))
    }
    ep = endpoints(qdata, "secs")
    bidsize = period.apply(qdata$BIDSIZ, ep, sumN)
    offersize = period.apply(qdata$OFRSIZ, ep, sumN)
    if (selection == "median") {
        bid = period.apply(qdata$BID, ep, medianN)
        offer = period.apply(qdata$OFR, ep, medianN)
    }
    if (selection == "maxvolume") {
        bid = period.apply3(cbind(qdata$BID, qdata$BIDSIZ), ep, 
            maxvol)
        offer = period.apply3(cbind(qdata$OFR, qdata$OFRSIZ), 
            ep, maxvol)
    }
    if (selection == "weightedaverage") {
        bid = period.apply3(cbind(qdata$BID, qdata$BIDSIZ), ep, 
            waverage)
        offer = period.apply3(cbind(qdata$OFR, qdata$OFRSIZ), 
            ep, waverage)
    }
    selection = ep[2:length(ep)]
    ts2 = qdata[selection]
    ts2$BID = bid
    ts2$OFR = offer
    ts2$BIDSIZ = bidsize
    ts2$OFRSIZ = offersize
    return(ts2)
}


rmNegativeSpread = function(qdata){
qdata = dataformatc(qdata);
qdatacheck(qdata);
##function to remove observations with negative spread
  condition = as.numeric(qdata$OFR)>as.numeric(qdata$BID);
  qdata[condition];
}


rmLargeSpread = function(qdata,maxi=50){
qdatacheck(qdata);
##function to remove observations with a spread larger than 50 times the median spread that day
###WATCH OUT: works only correct if supplied input data consists of 1 day...
  spread = as.numeric(qdata$OFR)-as.numeric(qdata$BID);
  condition = ((maxi*median(spread))>spread);
  return(qdata[condition])
}

rmOutliers = function (qdata, maxi = 10, window = 50, type = "advanced")
{
qdata = dataformatc(qdata);
qdatacheck(qdata);
##function to remove entries for which the mid-quote deviated by more than 10 median absolute deviations 
##from a rolling centered median (excluding the observation under consideration) of 50 observations if type = "standard".

##if type="advanced":
##function removes entries for which the mid-quote deviates by more than 10 median absolute deviations
##from the variable "mediani".
##mediani is defined as the value closest to the midquote of these three options:
##1. Rolling centered median (excluding the observation under consideration)
##2. Rolling median of the following "window" observations
##3. Rolling median of the previous "window" observations

##NOTE: Median Absolute deviation chosen contrary to Barndorff-Nielsen et al.
    window = floor(window/2) * 2
    condition = c();
    halfwindow = window/2;
    midquote = as.vector(as.numeric(qdata$BID) + as.numeric(qdata$OFR))/2;
    mad_all = mad(midquote);

    midquote = xts(midquote,order.by = index(qdata))

    if (mad_all == 0) {
        m = as.vector(as.numeric(midquote))
        s = c(TRUE, (m[2:length(m)] - m[1:(length(m) - 1)] != 
            0))
        mad_all = mad(as.numeric(midquote[s]))
    }

    medianw = function(midquote, n = window) {
        m = floor(n/2) + 1
        q = median(c(midquote[1:(m - 1)], midquote[(m + 1):(n + 
            1)]))
        return(q)
    }

    if (type == "standard") {
        meds = as.numeric(rollapply(midquote, width = (window + 
            1), FUN = medianw, align = "center"))
    }
    if (type == "advanced") {
        advancedperrow = function(qq) {
            diff = abs(qq[1:3] - qq[4])
            select = min(diff) == diff
            value = qq[select]
            if (length(value) > 1) {
                value = median(value)
            }
            return(value)
        }
        n = length(midquote)
        allmatrix = matrix(rep(0, 4 * n), ncol = 4)
        median2 = function(a) {
            median(a)
        }
        standardmed = as.numeric(rollapply(midquote, width = (window), 
            FUN = median2, align = "center"))
        allmatrix[(halfwindow + 1):(n - halfwindow), 1] = as.numeric(rollapply(midquote, 
            width = (window + 1), FUN = medianw, align = "center"))
        allmatrix[(1:(n - window)), 2] = standardmed[2:length(standardmed)]
        allmatrix[(window + 1):(n), 3] = standardmed[1:(length(standardmed) - 
            1)]
        allmatrix[, 4] = midquote
        meds = apply(allmatrix, 1, advancedperrow)[(halfwindow + 
            1):(n - halfwindow)]
    }

    midquote = as.numeric(midquote);
    maxcriterion = meds + maxi * mad_all
    mincriterion = meds - maxi * mad_all

    condition = mincriterion < midquote[(halfwindow + 1):(length(midquote) - 
        halfwindow)] & midquote[(halfwindow + 1):(length(midquote) - 
        halfwindow)] < maxcriterion
    condition = c(rep(TRUE, halfwindow), condition, rep(TRUE, 
        halfwindow))
    qdata[condition];
}

###zivot
correctedTrades <- function (tdata)
{
tdatacheck(tdata);
    filteredts = tdata[tdata$CR == " 0"]
    return(filteredts)
}

##########################  JUNK  #############################################################
#conv =function(z){ 
#  zz = unlist(strsplit(z,",")); 
#  return(as.numeric(paste(zz[1],zz[2],sep=".")))
#}
### make prices numeric ###
#x = as.matrix(as.vector(test2$PRICE))
#xx = apply(x,1,conv)
#test2$PRICE=xx

##appropriate days selection:
#create list of all trading days
#start = unlist(strsplit(as.character(start(myxts))," "))[1];
#end = unlist(strsplit(as.character(end(myxts))," "))[1];
#alldays = timeSequence(from = start, to = end, by = "day");
#alldays = alldays[isWeekday(alldays)];