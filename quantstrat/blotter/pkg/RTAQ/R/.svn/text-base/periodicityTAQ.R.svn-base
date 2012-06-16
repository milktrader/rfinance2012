# Documented function:

spotVol =  function(pdata, dailyvol = "bipower", periodicvol = "TML", on = "minutes", 
    k = 5, dummies = FALSE, P1 = 4, P2 = 2,  marketopen = "09:30:00", 
    marketclose = "16:00:00") 
{
    require(chron);
    dates = unique(format(time(pdata), "%Y-%m-%d"))
    cDays = length(dates)
    rdata = mR = c()
    if(on=="minutes"){
       intraday = seq(from=times(marketopen), to=times(marketclose), by=times(paste("00:0",k,":00",sep=""))) 
    }
    if(tail(intraday,1)!=marketclose){intraday=c(intraday,marketclose)}
    intraday = intraday[2:length(intraday)];
    for (d in 1:cDays) {
        pdatad = pdata[as.character(dates[d])]
        pdatad = aggregatePrice(pdatad, on = on, k = k , marketopen = marketopen, marketclose = marketclose)
        z = xts( rep(1,length(intraday)) , order.by = timeDate( paste(dates[d],as.character(intraday),sep="") , format = "%Y-%m-%d %H:%M:%S"))
        pdatad = merge.xts( z , pdatad )$pdatad
        pdatad = na.locf(pdatad)
        rdatad = makeReturns(pdatad)
        rdatad = rdatad[time(rdatad) > min(time(rdatad))]
        rdata = rbind(rdata, rdatad)
        mR = rbind(mR, as.numeric(rdatad))
    }
    mR[is.na(mR)]=0
    M = ncol(mR)
    if (cDays == 1) {
        mR = as.numeric(rdata)
        estimdailyvol = switch(dailyvol, bipower = RBPCov(mR), 
            medrv = MedRV(mR), rv = RV(mR))
    }else {
        estimdailyvol = switch(dailyvol, bipower = apply(mR, 
            1, "RBPCov"), medrv = apply(mR, 1, "MedRV"), rv = apply(mR, 
            1, "RV"))
    }
    if (cDays <= 50) {
        print("Periodicity estimation requires at least 50 observations. Periodic component set to unity")
        estimperiodicvol = rep(1, M)
    }
    else {
        mstdR = mR/sqrt(estimdailyvol * (1/M))
        selection = c(1:M)[ (nrow(mR)-apply(mR,2,'countzeroes')) >=20] 
        # preferably no na is between
        selection = c( min(selection) : max(selection) )
        mstdR = mstdR[,selection]
        estimperiodicvol_temp = diurnal(stddata = mstdR, method = periodicvol, 
            dummies = dummies, P1 = P1, P2 = P2)[[1]]
        estimperiodicvol = rep(1,M)
        estimperiodicvol[selection] = estimperiodicvol_temp
        mfilteredR = mR/matrix(rep(estimperiodicvol, cDays), 
            byrow = T, nrow = cDays)
        estimdailyvol = switch(dailyvol, bipower = apply(mfilteredR, 
            1, "RBPCov"), medrv = apply(mfilteredR, 1, "MedRV"), 
            rv = apply(mfilteredR, 1, "RV"))
    }
    out = cbind(rdata, rep(sqrt(estimdailyvol * (1/M)), each = M) * 
        rep(estimperiodicvol, cDays), rep(sqrt(estimdailyvol * 
        (1/M)), each = M), rep(estimperiodicvol, cDays))
    out = xts(out, order.by = time(rdata))
    names(out) = c("returns", "vol", "dailyvol", "periodicvol")
    return(out)
}


# internal non documented functions: 
HRweight = function( d,k){
# Hard rejection weight function
   w = 1*(d<=k); return(w)
}

shorthscale = function( data )
{
   sorteddata = sort(data);
   n = length(data);
   h = floor(n/2)+1;
   M = matrix( rep(0,2*(n-h+1) ) , nrow= 2 );
   for( i in 1:(n-h+1) ){
       M[,i] = c( sorteddata[ i ], sorteddata[ i+h-1 ] )
   }
   return( 0.7413*min( M[2,]-M[1,] ) );
}

diurnal = 
function (stddata, method = "TML", dummies = F, P1 = 6, P2 = 4) 
{
    cDays = dim(stddata)[1]
    intraT = dim(stddata)[2]
    meannozero = function(series) {
        return(mean(series[series != 0]))
    }
    shorthscalenozero = function(series) {
        return(shorthscale(series[series != 0]))
    }
    WSDnozero = function(weights, series) {
        out = sum((weights * series^2)[series != 0])/sum(weights[series != 
            0])
        return(sqrt(1.081 * out))
    }
    if (method == "SD" | method == "OLS") {
        seas = sqrt(apply(stddata^2, 2, "meannozero"))
    }
    if (method == "WSD" | method == "TML") {
        seas = apply(stddata, 2, "shorthscalenozero")
        shorthseas = seas/sqrt(mean(seas^2))
        shorthseas[shorthseas == 0] = 1
        weights = matrix(HRweight(as.vector(t(stddata^2)/rep(shorthseas, 
            cDays)^2), qchisq(0.99, df = 1)), ncol = dim(stddata)[2], 
            byrow = T)
        for (c in 1:intraT) {
            seas[c] = WSDnozero(weights[, c], stddata[, c])
        }
    }
    seas = na.locf(seas,na.rm=F) #do not remove leading NA
    seas = na.locf(seas,fromLast=T)
    seas = seas/sqrt(mean(seas^2))
    if (method == "OLS" | method == "TML") {
        c = center()
        vstddata = as.vector(stddata)
        nobs = length(vstddata)
        vi = rep(c(1:intraT), each = cDays)
        if (method == "TML") {
            if( length(vstddata)!= length(seas)*cDays ){ print(length(vstddata)); print(length(seas)); print(cDays)}
            firststepresids = log(abs(vstddata)) - c - log(rep(seas, 
                each = cDays))
        }
        X = c()
        if (!dummies) {
            if (P1 > 0) {
                for (j in 1:P1) {
                  X = cbind(X, cos(2 * pi * j * vi/intraT))
                }
            }
            M1 = (intraT + 1)/2
            M2 = (2 * intraT^2 + 3 * intraT + 1)/6
            ADD = (vi/M1)
            X = cbind(X, ADD)
            ADD = (vi^2/M2)
            X = cbind(X, ADD)
            if (P2 > 0) {
                ADD = c()
                for (j in 1:P2) {
                  ADD = cbind(ADD, sin(2 * pi * j * vi/intraT))
                }
            }
            X = cbind(X, ADD)
            opening = vi - 0
            stdopening = (vi - 0)/80
            almond1_opening = (1 - (stdopening)^3)
            almond2_opening = (1 - (stdopening)^2) * (opening)
            almond3_opening = (1 - (stdopening)) * (opening^2)
            X = cbind(X, almond1_opening, almond2_opening, almond3_opening)
            closing = max(vi) - vi
            stdclosing = (max(vi) - vi)/max(vi)
            almond1_closing = (1 - (stdclosing)^3)
            almond2_closing = (1 - (stdclosing)^2) * (closing)
            almond3_closing = (1 - (stdclosing)) * (closing^2)
            X = cbind(X, almond1_closing, almond2_closing, almond3_closing)
        }
        else {
            for (d in 1:intraT) {
                dummy = rep(0, intraT)
                dummy[d] = 1
                dummy = rep(dummy, each = cDays)
                X = cbind(X, dummy)
            }
        }
        selection = c(1:nobs)[vstddata != 0]
        vstddata = vstddata[selection]
        X = X[selection, ]
        if (method == "TML") {
            firststepresids = firststepresids[selection]
        }
        vy = matrix(log(abs(vstddata)), ncol = 1) - c
        if (method == "OLS") {
            Z = try(solve(t(X) %*% X), silent = T)
            if (inherits(Z, "try-error")) {
                print("X'X is not invertible. Switch to TML")
            }
            else {
                theta = solve(t(X) %*% X) %*% t(X) %*% vy
                rm(X)
                rm(vy)
            }
        }
        if (method == "TML") {
            inittheta = rep(0, dim(X)[2])
            l = -2.272
            u = 1.6675
            nonoutliers = c(1:length(vy))[(firststepresids > 
                l) & (firststepresids < u)]
            truncvy = vy[nonoutliers]
            rm(vy)
            truncX = X[nonoutliers, ]
            rm(X)
            negtruncLLH = function(theta) {
                res = truncvy - truncX %*% matrix(theta, ncol = 1)
                return(mean(-res - c + exp(2 * (res + c))/2))
            }
            grnegtruncLLH = function(theta) {
                res = truncvy - truncX %*% matrix(theta, ncol = 1)
                dres = -truncX
                return(apply(-dres + as.vector(exp(2 * (res + 
                  c))) * dres, 2, "mean"))
            }
            est = optim(par = inittheta, fn = negtruncLLH, gr = grnegtruncLLH, 
                method = "BFGS")
            theta = est$par
            rm(truncX)
            rm(truncvy)
        }
        plot(seas, main = "Non-parametric (dashed line) and parametric (full line) periodicity", 
            xlab = "intraday period", type = "l", lty = 3)
        seas = RTAQ:::diurnalfit(theta = theta, P1 = P1, P2 = P2, intraT = intraT, 
            dummies = dummies)
        lines(seas, lty = 1)
        return(list(seas, theta))
    }
    else {
        return(list(seas))
    }
}

diurnalfit = function( theta , P1 , P2 , intraT , dummies=F )
{
     vi = c(1:intraT) ;  
     M1 = (intraT+1)/2 ; M2 = (2*intraT^2 + 3*intraT + 1)/6;

     # Regressors that do not depend on Day of Week:
     X = c()
     if(!dummies){
        if ( P1 > 0 ){ for( j in 1:P1 ){ X = cbind( X , cos(2*pi*j*vi/intraT) )   }  } 

        ADD = (vi/M1 ) ; X = cbind(X,ADD);
        ADD = (vi^2/M2); X = cbind(X,ADD);
        if ( P2 > 0 ){ ADD= c(); for( j in 1:P2 ){  ADD = cbind( ADD , sin(2*pi*j*vi/intraT)  ) }}; X = cbind( X , ADD ) ; 

        #openingeffect
        opening = vi-0 ; stdopening = (vi-0)/80 ;
        almond1_opening   = ( 1 - (stdopening)^3 );
        almond2_opening   = ( 1 - (stdopening)^2 )*( opening);
        almond3_opening   = ( 1 - (stdopening)   )*( opening^2);   
        X = cbind(  X, almond1_opening , almond2_opening , almond3_opening   )  ;

        #closing effect
        closing = max(vi)-vi ; stdclosing = (max(vi)-vi)/max(vi) ;
        almond1_closing   = ( 1 - (stdclosing)^3 );
        almond2_closing   = ( 1 - (stdclosing)^2 )*( closing);
        almond3_closing   = ( 1 - (stdclosing)   )*( closing^2);   
        X = cbind(  X, almond1_closing , almond2_closing , almond3_closing   )  ;

     }else{
        for( d in 1:intraT){
          dummy = rep(0,intraT); dummy[d]=1; 
          X = cbind(X,dummy); 
        }
     }
     # Compute fit
     seas = exp( X%*%matrix(theta,ncol=1) );
     seas = seas/sqrt(mean( seas^2) )    
     return( seas )          
}

LeeMyklandCV = function( beta = 0.999 , M = 78 )
{
    # Critical value for Lee-Mykland jump test statistic
    # Based on distribution of Maximum of M absolute normal random variables
    a = function(n){ a1=sqrt(2*log(n)) ; a2= (log(pi)+log(log(n))  )/( 2*sqrt(2*log(n))   ); return(a1-a2)             };
    b = function(n){ return( 1/sqrt(2*log(n) )  ) ; return(b)} ;
    return( -log(-log(beta))*b(M) + a(M)     )
}

center = function()
{
    g=function(y){ return( sqrt(2/pi)*exp(y-exp(2*y)/2)  )}
    f=function(y){ return( y*g(y)    )  }
    return( integrate(f,-Inf,Inf)$value )
}


