########## HELPFUNCTION ####
readdata = function(path=NULL, extention="txt",header=FALSE,dims=0){
#extention should either be "txt" or "csv"
if(!(extention=="txt"|extention=="csv")){print("Please select a supported extention")}
colnames = rep("x",dims);
#load txt
if(extention == "txt"){
fullpath = paste(path,".txt",sep="");
data = try(read.delim(fullpath,sep="",header=header,dec=",",col.names=colnames),silent=TRUE);

  if(is.null(dim(data))){
  data = try(read.delim(fullpath,sep="",header=header,dec=",",col.names=c(colnames,"EXTRA")),silent=TRUE);
  if(is.null(dim(data))){data=matrix(nrow=0,ncol=9);
  }else{data=data[,(-dim(data)[2])]}
  }
}

if(extention == "csv"){
fullpath = paste(path,".csv",sep="");
data = try(read.delim(fullpath,sep=",",header=header,dec=".",col.names=colnames),silent=TRUE);

  if(is.null(dim(data))){
  data = try(read.delim(fullpath,sep=",",header=header,dec=".",col.names=c(colnames,"EXTRA")),silent=TRUE);
  if(is.null(dim(data))){data=matrix(nrow=0,ncol=9);
  }else{data=data[,(-dim(data)[2])]}
  }
}
return(data);
}


convert_trades = function (datasource, datadestination, ticker, extention = "txt", 
    header = FALSE, tradecolnames = NULL, format = "%Y%M%D %H:%M:%S") 
{  
    missingt=matrix(ncol=2,nrow=0);
   
    suppressWarnings(dir.create(datadestination));
    suppressWarnings(dir.create(datasource));

    setwd(datasource)
    adjtime = function(z) {
        zz = unlist(strsplit(z, ":"))
        if (nchar(zz[1]) != 2) {
            return(paste(paste(0, zz[1], sep = ""), zz[2], zz[3], 
                sep = ":"))
        }
        return(z)
    }
    for (i in 1:length(ticker)) {
        tfile_name = paste(datasource, "/", ticker[i], "_trades", 
            sep = "")
        tdata = try(RTAQ:::readdata(path = tfile_name, extention = extention, 
            header = header, dims = 9), silent = TRUE)

        error = dim(tdata)[1] == 0
        if (error) {
            print(paste("no trades for stock", ticker[i]))
            missingt = rbind(missingt, c(datasource, ticker[i]))
        }
        if (error == FALSE) {
                if (is.null(tradecolnames)) {
                  tradecolnames = c("SYMBOL", "DATE", "EX", "TIME", 
                    "PRICE", "SIZE", "COND", "CORR", "G127")
                  colnames(tdata) = tradecolnames
                }else {
                  colnames(tdata) = tradecolnames
                }
            cond = tdata$COND[is.na(tdata$G127)];
            cr = tdata$CORR[is.na(tdata$G127)];

            tdata$COND[is.na(tdata$G127)] = 0
            tdata$CORR[is.na(tdata$G127)] = as.character(cond)
            tdata$G127[is.na(tdata$G127)] = as.character(cr)
            rm(cond, cr)
            oldtime = as.matrix(as.vector(tdata$TIME))
            newtime = apply(oldtime, 1, adjtime)
            tdata$TIME = newtime
            rm(oldtime, newtime);

            tdobject = timeDate:::timeDate(paste(as.vector(tdata$DATE), 
                as.vector(tdata$TIME)), format = format, FinCenter = "GMT", 
                zone = "GMT")
            tdata = xts(tdata, order.by = tdobject)
            tdata = tdata[, c("SYMBOL", "EX", "PRICE", "SIZE", 
                "COND", "CORR", "G127")]
            rm(tdobject)
        }
        xts_name = paste(ticker[i], "_trades.RData", sep = "")
        setwd(datadestination)
        save(tdata, file = xts_name)
    }
}


convert_quotes = function (datasource, datadestination, ticker, extention = "txt", 
    header = FALSE, quotecolnames = NULL, format = "%Y%M%D %H:%M:%S") 
{
    missingq=matrix(ncol=2,nrow=0);
    
    suppressWarnings(dir.create(datadestination));
    suppressWarnings(dir.create(datasource));
    
    setwd(datasource)
    adjtime = function(z) {
        zz = unlist(strsplit(z, ":"))
        if (nchar(zz[1]) != 2) {
            return(paste(paste(0, zz[1], sep = ""), zz[2], zz[3], 
                sep = ":"))
        }
        return(z)
    }
    for (i in 1:length(ticker)) {
        qfile_name = paste(datasource, "/", ticker[i], "_quotes", 
            sep = "")
        qdata = try(readdata(path = qfile_name, extention = extention, 
            header = header, dims = 9), silent = TRUE)
        error = dim(qdata)[1] == 0
        if (error) {
            print(paste("no quotes for stock", ticker[i]))
            missingq = rbind(missingq, c(datasource, ticker[i]))
        }
        if (error == FALSE) {
                if (is.null(quotecolnames)) {
                  quotecolnames = c("SYMBOL", "DATE", "EX", "TIME", 
                    "BID", "BIDSIZ", "OFR", "OFRSIZ", "MODE")
                  colnames(qdata) = quotecolnames
                }
                else {
                  colnames(qdata) = quotecolnames
                }
            qdata = qdata[qdata$SYMBOL == ticker[i], ]
            oldtime = as.matrix(as.vector(qdata$TIME))
            newtime = apply(oldtime, 1, adjtime)
            qdata$TIME = newtime
            rm(oldtime, newtime)

#            if(year == 1993){qdata = qdata[sort(qdata[,4], index.return = TRUE)$ix,]}

            test = paste(as.vector(qdata$DATE), as.vector(qdata$TIME))
            tdobject = timeDate:::timeDate(test, format = format, FinCenter = "GMT", 
                zone = "GMT")
            tdobject = timeDate:::timeDate(test, format = format, FinCenter = "GMT", 
                zone = "GMT")
            qdata = xts(qdata, order.by = tdobject)
            qdata = qdata[, c("SYMBOL", "EX", "BID", "BIDSIZ", 
                "OFR", "OFRSIZ", "MODE")]
        }
        xts_name = paste(ticker[i], "_quotes.RData", sep = "")
        setwd(datadestination)
        save(qdata, file = xts_name)
    }
}

############## NEW CODE GSoC 2012 ###############################################
makeXtsTrades = function(tdata,format=format){
  adjtime = function(z) {
    zz = unlist(strsplit(z, ":"))
    if (nchar(zz[1]) != 2) {
      return(paste(paste(0, zz[1], sep = ""), zz[2], zz[3], 
                   sep = ":"))  } 
    return(z) }
  tradecolnames = colnames(tdata)
  if (is.null(tradecolnames)){
    tradecolnames = c("SYMBOL", "DATE", "EX", "TIME", 
                      "PRICE", "SIZE", "COND", "CORR", "G127");
    colnames(tdata) = tradecolnames; }  
  
  cond = tdata$COND[is.na(tdata$G127)];
  cr = tdata$CORR[is.na(tdata$G127)];
  tdata$COND[is.na(tdata$G127)] = 0;
  tdata$CORR[is.na(tdata$G127)] = as.character(cond);
  tdata$G127[is.na(tdata$G127)] = as.character(cr);
  rm(cond, cr);
  oldtime = as.matrix(as.vector(tdata$TIME));
  newtime = apply(oldtime, 1, adjtime);
  tdata$TIME = newtime;
  rm(oldtime, newtime);
  tdobject = timeDate:::timeDate(paste(as.vector(tdata$DATE),as.vector(tdata$TIME)), format = format, FinCenter = "GMT", zone = "GMT");
  tdata  = xts(tdata, order.by = tdobject);
  tdata  = tdata[, c("SYMBOL", "EX", "PRICE", "SIZE","COND", "CORR", "G127")];
  rm(tdobject)
  return(tdata)  
}  

####
makeXtsQuotes = function( qdata, format = format){ 
  adjtime = function(z) {    zz = unlist(strsplit(z, ":")); if (nchar(zz[1]) != 2) {return(paste(paste(0, zz[1], sep = ""), zz[2], zz[3], sep = ":"))}; return(z) }
  quotecolnames = colnames(qdata);
  
  if (is.null(quotecolnames)) {
    quotecolnames = c("SYMBOL", "DATE", "EX", "TIME", "BID", "BIDSIZ", "OFR", "OFRSIZ", "MODE")
    colnames(qdata) = quotecolnames;
  }else{ colnames(qdata) = quotecolnames }
  
  oldtime = as.matrix(as.vector(qdata$TIME));
  newtime = apply(oldtime, 1, adjtime);
  qdata$TIME = newtime;
  rm(oldtime, newtime);
  test = paste(as.vector(qdata$DATE), as.vector(qdata$TIME))
  tdobject = timeDate:::timeDate(test, format = format, 
                                 FinCenter = "GMT", zone = "GMT")
  tdobject = timeDate:::timeDate(test, format = format, FinCenter = "GMT", zone = "GMT")
  qdata = xts(qdata, order.by = tdobject)
  qdata = qdata[, c("SYMBOL", "EX", "BID", "BIDSIZ","OFR", "OFRSIZ", "MODE")];
  rm(tdobject);
  return(qdata);
}

################ the real conversion starts here ;)

convert = function(from, to, datasource, datadestination, trades = TRUE, 
                   quotes = TRUE, ticker, dir = FALSE, extention = "txt", header = FALSE, 
                   tradecolnames = NULL, quotecolnames = NULL, format = "%Y%m%d %H:%M:%S", onefile=FALSE){  
  
  #############  1.A the data is in the "RTAQ folder" sturcture ##############
  if( onefile == FALSE ){
    
    # Create trading dates:
    dates = timeSequence(from, to, format = "%Y-%m-%d", FinCenter = "GMT")
    dates = dates[isBizday(dates, holidays = holidayNYSE(1950:2030))];
    
    # Create folder structure for saving:
    if (dir) { dir.create(datadestination); for (i in 1:length(dates)) {dirname = paste(datadestination, "/", as.character(dates[i]), sep = ""); dir.create(dirname)    } }
    for (i in 1:length(dates)){ #Loop over days  
      #Get the day-specific path
      datasourcex = paste(datasource, "/", dates[i], sep = "")
      datadestinationx = paste(datadestination, "/", dates[i], sep = "")
      
      if(trades == TRUE){ 
        if(extention=="txt"|extention=="csv"){ convert_trades(datasourcex, datadestinationx, ticker, extention = extention, header = header, tradecolnames = tradecolnames, format = format) }
      }
      
      if (quotes == TRUE) { 
        if(extention=="txt"|extention=="csv"){ convert_quotes(datasourcex, datadestinationx, ticker, extention = extention, header = header, quotecolnames = quotecolnames,format = format)}
      } 
    }#End loop over days
  }#End "not oneday" if
  
  #############  1.B The data is in one file: ###########
  if( onefile == TRUE ){
    # Load the data: ############################ This depends on the data provider
    if(trades == TRUE){ 
      if( extention=="txt"){ dataname = paste(datasource,"/",ticker,"_trades",sep=""); RTAQ:::readdata(path = datasource, extention = "txt", header = FALSE, dims = 0); } 
      if( extention=="csv"){ dataname = paste(datasource,"/",ticker,"_trades.csv",sep=""); data = read.csv(dataname);}
      if( extention=="tickdatacom"){ 
        dataname   = paste(datasource,"/",ticker,"_trades.asc",sep="");
        colnames   = c("DATE","TIME","PRICE","SIZE","EX","COND","CORR","SEQN","SOURCE","TSTOP","G127","EXCL","FPRICE");
        alldata    = read.delim(dataname, header=F, sep=",",dec=".",col.names=colnames); 
        taqnames   = c("DATE","EX","TIME","PRICE","SIZE","COND","CORR","G127"); 
        data = alldata[,taqnames]; 
        data = cbind(rep(ticker,dim(data)[1]),data); colnames(data)[1] = "SYMBOL"; 
        format = "%d/%m/%Y %H:%M:%S"; #tickdata always has this format
      }
      alldata = suppressWarnings(makeXtsTrades(tdata=data,format=format)); 
    }
    if (quotes == TRUE){ 
      if( extention=="txt"){ dataname = paste(datasource,"/",ticker,"_quotes",sep=""); RTAQ:::readdata(path = datasource, extention = "txt", header = FALSE, dims = 0); } 
      if( extention=="csv"){ dataname = paste(datasource,"/",ticker,"_quotes.csv",sep=""); data = read.csv(dataname);}
      if( extention=="tickdatacom"){ 
        dataname   = paste(datasource,"/",ticker,"_quotes.asc",sep=""); 
        colnames   = c("DATE","TIME","EX","BID","OFR","BIDSIZ","OFRSIZ","MODE","MMID","SEQN","EXB", "EXO","NBBOID","NBBOID","CORR","QSO"); 
        alldata    = read.delim(dataname, header=F, sep=",",dec=".",col.names=colnames); 
        taqnames   = c("DATE","TIME","EX","BID","BIDSIZ","OFR","OFRSIZ","MODE"); 
        data = alldata[,taqnames]; 
        data = cbind(rep(ticker,dim(data)[1]),data); colnames(data)[1] = "SYMBOL"; 
        format = "%d/%m/%Y %H:%M:%S"; # Tickdata always has this format
      } 
      alldata = suppressWarnings( makeXtsQuotes( qdata=data, format=format) );
    }
    
    # Save the data: ############################ This is the same irrespective of the data provider
    # Create trading dates: 
    
    dates = unique(as.Date(index(alldata)));
    
    # Create folder structure for saving : 
    suppressWarnings( if (dir){ dir.create(datadestination); for (i in 1:length(dates)) {dirname = paste(datadestination, "/", as.character(dates[i]), sep = ""); dir.create(dirname) } })
    
    for(i in 1:length(dates) ){ # Loop over days
      datadestinationx = paste(datadestination, "/", dates[i], sep = ""); 
      
      if( trades == TRUE ){ 
        tdata        = alldata[as.character(dates[i])];
        xts_name     = paste(ticker, "_trades.RData", sep = "")
        destfullname = paste(datadestinationx,"/",xts_name,sep="");          
        save(tdata, file = destfullname); # Save daily in right folder:
      } 
      
      if( quotes == TRUE ){ 
        qdata        = alldata[as.character(dates[i])]; 
        xts_name     = paste(ticker, "_quotes.RData", sep = ""); 
        destfullname = paste(datadestinationx,"/",xts_name,sep=""); 
        save(qdata, file = destfullname); # Save daily in right folder: 
      }#End quotes if
    } #End save loop over days
  } #End oneday   
} #End convert function




