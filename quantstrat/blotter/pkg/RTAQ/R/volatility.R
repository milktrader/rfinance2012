# UNIVARIATE: 
# Realized Volatility (RV)
RV = function(rdata,...){
  if(hasArg(data)){ rdata = data }
returns=as.numeric(rdata);
RV = sum(returns*returns);
return(RV);
}

#Realized Outlyingness Weighted Variance (ROWVar):
univariateoutlyingness = function(rdata,...){
require('robustbase');
  if(hasArg(data)){ rdata = data }
#computes outlyingness of each obs compared to row location and scale
	location = 0;
	scale = mad(rdata);
		if(scale==0){
		scale = mean(rdata);
		}
	d = ((rdata - location)/scale)^2;
}


ROWVar =
function(rdata, seasadjR = NULL, wfunction = "HR" , alphaMCD = 0.75, alpha = 0.001,...) 
{
require('robustbase');
  if(hasArg(data)){ rdata = data }
    require(robustbase)
    if (is.null(seasadjR)) {
        seasadjR = rdata;
    }

    rdata = as.vector(rdata); seasadjR = as.vector(seasadjR);
    intraT = length(rdata); N=1;
    MCDcov = as.vector(covMcd( rdata , use.correction = FALSE )$raw.cov)
    outlyingness = seasadjR^2/MCDcov    
    k = qchisq(p = 1 - alpha, df = N)
    outlierindic = outlyingness > k
    weights = rep(1, intraT)
    if( wfunction == "HR" ){
       weights[outlierindic] = 0
       wR = sqrt(weights) * rdata
       return((conHR(di = N, alpha = alpha) * sum(wR^2))/mean(weights))
    }
    if( wfunction == "SR" ){
       weights[outlierindic] = k/outlyingness[outlierindic]
       wR = sqrt(weights) * rdata
       return((conhuber(di = N, alpha = alpha) * sum(wR^2))/mean(weights))
    }

}



#Realized BiPower Variation (RBPVar) (RBPVar)
RBPVar = function(rdata,...){
  if(hasArg(data)){ rdata = data }

  returns = as.vector(as.numeric(rdata));
  n = length(returns);
  rbpvar = (pi/2)*sum(abs(returns[1:(n-1)])*abs(returns[2:n]));
  return(rbpvar);
}

#MinRV:
MinRV = function(rdata,...){
  if(hasArg(data)){ rdata = data }

  q = as.zoo(abs(as.numeric(rdata))); #absolute value
  q = as.numeric(rollapply(q, width=2, FUN=min,by = 1, align="left"));
  N = length(q)+1; #number of obs
  minrv = (pi/(pi-2))*(N/(N-1))*sum(q^2);
return(minrv)
}

#MedRV
MedRV = function(rdata,...){
  if(hasArg(data)){ rdata = data }

  q = abs(as.numeric(rdata)); #absolute value
  q = as.numeric(rollmedian(q, k=3, align="center"));
  N = length(q) + 2;
  medrv = (pi/(6-4*sqrt(3)+pi))*(N/(N-2))*sum(q^2);
return(medrv)
}


##Multivariate measures:
#Realized Covariation (RCov):
RCov = function (rdata, cor = FALSE, makeReturns = FALSE, ...) 
{
    if (hasArg(data)) {
        rdata = data
    }
    if (makeReturns) {
        rdata = makeReturns(rdata)
    }
    if (is.null(dim(rdata))) {
        n = 1
    }
    else {
        n = dim(rdata)[2]
    }
    if (n == 1) {
        return(RV(rdata))
    }
    if (n > 1) {
#        rdata = na.locf(rdata, na.rm = FALSE)

        rdata = as.matrix(rdata)
        covariance = t(rdata) %*% rdata
        if (cor == FALSE) {
            return(covariance)
        }
        if (cor == TRUE) {
            sdmatrix = sqrt(diag(diag(covariance)))
            rcor = solve(sdmatrix) %*% covariance %*% solve(sdmatrix)
            return(rcor)
        }
    }
}

#Realized Outlyingness Weighted Quadratic Covariation (ROWQCov)
conhuber = function(di,alpha=0.05)
{# consistency factor ROWQCov based on Huber weight function
   c = qchisq(p=1-alpha,df=di)
   fw2 = function(t){
      z=t^2; return(  huberweight(z,c)*( t^(di-1) )*exp(-z/2)    ) }
   fw1 = function(t){
      z=t^2; return(  huberweight(z,c)*( t^(di+1) )*exp(-z/2)   )}
   c2 = integrate(fw2,0,Inf)$value;  c1 = integrate(fw1,0,Inf)$value;
   return( di*c2/c1 )
}

conHR = function(di,alpha=0.05)
{
# consistency factor ROWQCov based on hard rejection weight function
   return( (1-alpha)/pchisq(qchisq(1-alpha,df=di),df=di+2)  )
}

huberweight = function(d,k){
# Huber or soft rejection weight function
   w = apply( cbind( rep(1,length(d) ) , (k/d) ),1,'min'); return(w);
}

countzeroes = function( series )
{
    return( sum( 1*(series==0) ) )
}


ROWCov =
function (rdata, cor=FALSE, makeReturns = FALSE, seasadjR = NULL, wfunction = "HR" , alphaMCD = 0.75, alpha = 0.001,...) {
  if(hasArg(data)){ rdata = data }
  
  if(makeReturns){ rdata = makeReturns(rdata); 
  if(!is.null(seasadjR)){ seasadjR = makeReturns(seasadjR)} }
  
  if(is.null(seasadjR)) { seasadjR = rdata }


  if(is.null(dim(rdata))){ n=1 }else{ n = dim(rdata)[2]}        
  
  if( n == 1 ){ return( ROWVar( rdata , seasadjR = seasadjR , wfunction = wfunction , alphaMCD = alphaMCD , alpha = alpha ))}
  
  if( n > 1 ){ 
  rdatacheck(rdata,multi=TRUE);

    require(robustbase)
	 rdata = as.matrix(rdata); seasadjR = as.matrix(seasadjR);
       intraT = nrow(rdata)
       N = ncol(rdata)
       perczeroes = apply(seasadjR, 2, countzeroes)/intraT
       select = c(1:N)[perczeroes < 0.5]
       seasadjRselect = seasadjR[, select]
       N = ncol(seasadjRselect)
       MCDobject = try(covMcd(x = seasadjRselect, alpha = alphaMCD))
       if (length(MCDobject$raw.mah) > 1) {
           betaMCD = 1-alphaMCD; asycor = betaMCD/pchisq( qchisq(betaMCD,df=N),df=N+2 )
           MCDcov = (asycor*t(seasadjRselect[MCDobject$best,])%*%seasadjRselect[MCDobject$best,])/length(MCDobject$best);  
           invMCDcov = solve(MCDcov) ; outlyingness = rep(0,intraT);
           for( i in 1:intraT ){ 
                    outlyingness[i] = matrix(seasadjRselect[i,],ncol=N)%*%invMCDcov%*%matrix(seasadjRselect[i,],nrow=N)    }
       }
       else {
          print(c("MCD cannot be calculated")); stop();
       }
       k = qchisq(p = 1 - alpha, df = N)
       outlierindic = outlyingness > k
       weights = rep(1, intraT)
       if( wfunction == "HR" ){
          weights[outlierindic] = 0
          wR = sqrt(weights) * rdata
          covariance = (conHR(di = N, alpha = alpha) * t(wR) %*% wR)/mean(weights);
      if(cor==FALSE){return(covariance)}
      if(cor==TRUE){
      sdmatrix = sqrt(diag(diag(covariance)));
      rcor = solve(sdmatrix)%*%covariance%*%solve(sdmatrix);
      return(rcor)
      }
      }
       if( wfunction == "SR" ){
          weights[outlierindic] = k/outlyingness[outlierindic]
          wR = sqrt(weights) * rdata
          covariance = (conhuber(di = N, alpha = alpha) * t(wR) %*% wR)/mean(weights);
      if(cor==FALSE){return(covariance)}
      if(cor==TRUE){
      sdmatrix = sqrt(diag(diag(covariance)));
      rcor = solve(sdmatrix)%*%covariance%*%solve(sdmatrix);
      return(rcor)
      }
      }   
   }
}

#Realized BiPower Covariation (RBPCov)
RBPCov_bi = function(ts1,ts2){
  n = length(ts1);
  a = abs(ts1+ts2);
  b = abs(ts1-ts2);  
  first = as.numeric(a[1:(n-1)])*as.numeric(a[2:n]);
  last = as.numeric(b[1:(n-1)])*as.numeric(b[2:n]);
  result =  (pi/8)*sum(first-last);
  return(result);
}

RBPCov = function (rdata,cor=FALSE,makeReturns=FALSE,makePsd=FALSE,...) 
{
  if(hasArg(data)){ rdata = data }
  if(makeReturns){ rdata = makeReturns(rdata)}

  if(is.null(dim(rdata))){ n=1 }else{ n = dim(rdata)[2]}
          
  if( n == 1 ){ return( RBPVar( rdata ))}
  
  if( n > 1 ){ 
  
    rdatacheck(rdata,multi=TRUE);
    
	 rdata  = as.matrix(rdata);
       n = dim(rdata)[2]
       cov = matrix(rep(0, n * n), ncol = n)
       diagonal = c()
       for (i in 1:n) {
          diagonal[i] = RBPVar(rdata[, i])
       }
       diag(cov) = diagonal
       for (i in 2:n) {
           for (j in 1:(i - 1)) {
               cov[i, j] = cov[j, i] = RBPCov_bi(rdata[, i], rdata[, j])
           }
       }

    if(cor==FALSE){
    if(makePsd==TRUE){cov = makePsd(cov);}
    return(cov)
    }
    if(cor==TRUE){
    sdmatrix = sqrt(diag(diag(cov)));
    rcor = solve(sdmatrix)%*%cov%*%solve(sdmatrix);
    if(makePsd==TRUE){rcor = makePsd(rcor);}
    return(rcor)}
   }
}

thresholdCov = function(rdata, cor=FALSE, makeReturns=FALSE,...)	{
  if(hasArg(data)){ rdata = data }
  if(makeReturns){  rdata = makeReturns(rdata)}
 
  rdatacheck(rdata,multi=TRUE);
    
  rdata=as.matrix(rdata);
  n=dim(rdata)[1];						                  #number of observations
  delta = 1/n;
  rbpvars = apply(rdata,2,FUN=RBPVar);		      #bipower variation per stock
  tresholds = 3*sqrt(rbpvars)*(delta^(0.49));	  #treshold per stock
  tresmatrix = matrix(rep(tresholds,n),ncol=length(tresholds),nrow=n,byrow=TRUE);
  condition = abs(rdata) > tresmatrix;
  rdata[condition] = 0;
  covariance = RCov(rdata);
  
  if(cor==FALSE){ return(covariance) }
  if(cor==TRUE){
  sdmatrix = sqrt(diag(diag(covariance)));
  rcor = solve(sdmatrix)%*%covariance%*%solve(sdmatrix);
  return(rcor)}
				}

#Realized Correlation (RCor)
#RCor = function(rdata,...){
#  if(hasArg(data)){ rdata = data }
#    rdatacheck(rdata,multi=TRUE);
#    
#  rdata = na.locf(rdata,na.rm=FALSE);
#  rdata = as.matrix(rdata);
#  covariance = t(rdata)%*%rdata;
#  sdmatrix = sqrt(diag(diag(covariance)));
#  rcor = solve(sdmatrix)%*%covariance%*%solve(sdmatrix);
#  return(rcor);
# }

RTSRV = function (pdata, startIV = NULL, noisevar = NULL, K = 300, J = 1, 
    eta = 9){
    logprices = log(as.numeric(pdata))
    n = length(logprices)
    nbarK = (n - K + 1)/(K)
    nbarJ = (n - J + 1)/(J)
    adj = (1 - (nbarK/nbarJ))^-1
    zeta = 1/pchisq(eta, 3)
    seconds = as.numeric(as.POSIXct(index(pdata)))
    secday = last(seconds) - first(seconds)
    logreturns_K = vdelta_K = logreturns_J = vdelta_J = c()
    for (k in 1:K) {
        sel = seq(k, n, K)
        logreturns_K = c(logreturns_K, diff(logprices[sel]))
        vdelta_K = c(vdelta_K, diff(seconds[sel])/secday)
    }
    for (j in 1:J) {
        sel = seq(j, n, J)
        logreturns_J = c(logreturns_J, diff(logprices[sel]))
        vdelta_J = c(vdelta_J, diff(seconds[sel])/secday)
    }
    if (is.null(noisevar)) {
        noisevar = max(0,1/(2 * nbarJ) * (sum(logreturns_J^2)/J - TSRV(pdata=pdata,K=K,J=J)))        
    }
    if (!is.null(startIV)) {
        RTSRV = startIV
    }
    if (is.null(startIV)) {
        sel = seq(1, n, K)
        RTSRV = MedRV(diff(logprices[sel]))
    }
    iter = 1
    while (iter <= 20) {
        I_K = 1 * (logreturns_K^2 <= eta * (RTSRV * vdelta_K + 
            2 * noisevar))
        I_J = 1 * (logreturns_J^2 <= eta * (RTSRV * vdelta_J + 
            2 * noisevar))
        if (sum(I_J) == 0) {
            I_J = rep(1, length(logreturns_J))
        }
        if (sum(I_K) == 0) {
            I_K = rep(1, length(logreturns_K))
        }
        RTSRV = adj * (zeta * (1/K) * sum(logreturns_K^2 * I_K)/mean(I_K) - 
            ((nbarK/nbarJ) * zeta * (1/J) * sum(logreturns_J^2 * 
                I_J)/mean(I_J)))
        iter = iter + 1
    }
    return(RTSRV)
}


TSRV = function ( pdata , K=300 , J=1 ) 
{
    # based on rv.timescale
    logprices = log(as.numeric(pdata))
    n = length(logprices) ;
    nbarK = (n - K + 1)/(K) # average number of obs in 1 K-grid
    nbarJ = (n - J + 1)/(J)
    adj = (1 - (nbarK/nbarJ))^-1 
    logreturns_K = logreturns_J = c();
    for( k in 1:K){
       sel =  seq(k,n,K)  
       logreturns_K = c( logreturns_K , diff( logprices[sel] ) )
    }
    for( j in 1:J){
       sel =  seq(j,n,J)  
       logreturns_J = c( logreturns_J , diff( logprices[sel] ) )
    }
    TSRV = adj * ( (1/K)*sum(logreturns_K^2) - ((nbarK/nbarJ) *(1/J)*sum(logreturns_J^2)))
    return(TSRV)
}

 


RTSCov = function (pdata, cor = FALSE, startIV = NULL, noisevar = NULL, 
    K = 300, J = 1, 
    K_cov = NULL , J_cov = NULL,
    K_var = NULL , J_var = NULL , 
    eta = 9, makePsd = FALSE){
    if (!is.list(pdata)) {
        n = 1
    }
    else {
        n = length(pdata)
        if (n == 1) {
            pdata = pdata[[1]]
        }
    }
    if (n == 1) {
        return(RTSRV(pdata, startIV = startIV, noisevar = noisevar, 
            K = K, J = J, eta = eta))
    }
    if (n > 1) {
        cov = matrix(rep(0, n * n), ncol = n)
        diagonal = c()
        if( is.null(K_cov)){ K_cov = K }
        if( is.null(J_cov)){ J_cov = J }  
        if( is.null(K_var)){ K_var = rep(K,n) }
        if( is.null(J_var)){ J_var = rep(J,n) }        
        for (i in 1:n) {
            diagonal[i] = RTSRV(pdata[[i]], startIV = startIV[i], 
                noisevar = noisevar[i], K = K_var[i], J = J_var[i], 
                 eta = eta)
        }
        diag(cov) = diagonal
        if( is.null(K_cov)){ K_cov = K }
        if( is.null(J_cov)){ J_cov = J }                        
        for (i in 2:n) {
            for (j in 1:(i - 1)) {
                cov[i, j] = cov[j, i] = RTSCov_bi(pdata[[i]], 
                  pdata[[j]], startIV1 = diagonal[i], startIV2 = diagonal[j], 
                  noisevar1 = noisevar[i], noisevar2 = noisevar[j], 
                  K = K_cov, J = J_cov, eta = eta)
            }
        }
        if (cor == FALSE) {
            if (makePsd == TRUE) {
                cov = makePsd(cov)
            }
            return(cov)
        }
        if (cor == TRUE) {
            invsdmatrix = try(solve(sqrt(diag(diag(cov)))), silent = F)
            if (!inherits(invsdmatrix, "try-error")) {
                rcor = invsdmatrix %*% cov %*% invsdmatrix
                if (makePsd == TRUE) {
                  rcor = makePsd(rcor)
                }
                return(rcor)
            }
        }
    }
}

RTSCov_bi = 
function (pdata1, pdata2, startIV1 = NULL, startIV2 = NULL, noisevar1 = NULL, 
    noisevar2 = NULL, K = 300, J = 1, 
    K_cov = NULL , J_cov = NULL , 
    K_var1 = NULL , K_var2 = NULL , 
    J_var1 = NULL , J_var2 = NULL ,       
          eta = 9) 
{
    if( is.null(K_cov)){ K_cov = K }   ;   if( is.null(J_cov)){ J_cov = J } 
    if( is.null(K_var1)){ K_var1 = K } ;   if( is.null(K_var2)){ K_var2 = K }   
    if( is.null(J_var1)){ J_var1 = J } ;   if( is.null(J_var2)){ J_var2 = J }

    # Calculation of the noise variance and TSRV for the truncation
 

    
    if (   is.null(noisevar1)   ) {
        logprices1 = log(as.numeric(pdata1))     
        n_var1 = length(logprices1)
        nbarK_var1 = (n_var1 - K_var1 + 1)/(K_var1) ;
        nbarJ_var1 = (n_var1 - J_var1 + 1)/(J_var1)
        adj_var1 = n_var1/((K_var1 - J_var1) * nbarK_var1) 
        
        logreturns_K1 = logreturns_J1 = c()
        for (k in 1:K_var1) {
           sel.avg = seq(k, n_var1, K_var1)
           logreturns_K1 = c(logreturns_K1, diff(logprices1[sel.avg]))
        }
        for (j in 1:J_var1) {
           sel.avg = seq(j, n_var1, J_var1)
           logreturns_J1 = c(logreturns_J1, diff(logprices1[sel.avg]))
        }   
        if(  is.null(noisevar1)  ){
           noisevar1 = max(0,1/(2 * nbarJ_var1) * (sum(logreturns_J1^2)/J_var1 - TSRV(pdata1,K=K_var1,J=J_var1)))
        }
    }
    if (is.null(noisevar2)) {
        logprices2 = log(as.numeric(pdata2))
        n_var2 = length(logprices2)
        nbarK_var2 = (n_var2 - K_var2 + 1)/(K_var2) ;
        nbarJ_var2 = (n_var2 - J_var2 + 1)/(J_var2)
        adj_var2 = n_var2/((K_var2 - J_var2) * nbarK_var2)    
        
        logreturns_K2 = logreturns_J2 = c()
        for (k in 1:K_var2) {
           sel.avg = seq(k, n_var2, K_var2)
           logreturns_K2 = c(logreturns_K2, diff(logprices2[sel.avg]))
        }
        for (j in 1:J_var2) {
           sel.avg = seq(j, n_var2, J_var2)
           logreturns_J2 = c(logreturns_J2, diff(logprices2[sel.avg]))
        }        
        noisevar2 = max(0,1/(2 * nbarJ_var2) * (sum(logreturns_J2^2)/J_var2 - TSRV(pdata2,K=K_var2,J=J_var2)))
    }    

    if (!is.null(startIV1)) {
        RTSRV1 = startIV1
    }else{
        RTSRV1 = RTSRV(pdata=pdata1, noisevar = noisevar1, K = K_var1, J = J_var1, eta = eta)      
    }
    if (!is.null(startIV2)) {
        RTSRV2 = startIV2
    }else{
        RTSRV2 = RTSRV(pdata=pdata2, noisevar = noisevar2, K = K_var2, J = J_var2, eta = eta)      
    }
        
    # Refresh time is for the covariance calculation
        
    x = refreshTime(list(pdata1, pdata2))
    newprice1 = x[, 1]
    newprice2 = x[, 2]
    logprices1 = log(as.numeric(newprice1))
    logprices2 = log(as.numeric(newprice2))
    seconds = as.numeric(as.POSIXct(index(newprice1)))
    secday = last(seconds) - first(seconds)        
    K = K_cov ; J = J_cov ;    
    
    n = length(logprices1)
    nbarK_cov = (n - K_cov + 1)/(K_cov)
    nbarJ_cov = (n - J_cov + 1)/(J_cov)
    adj_cov = n/((K_cov - J_cov) * nbarK_cov)    

    logreturns_K1 = logreturns_K2 = vdelta_K = c()
    for (k in 1:K_cov) {
         sel.avg = seq(k, n, K_cov)
         logreturns_K1 = c(logreturns_K1, diff(logprices1[sel.avg]))
         logreturns_K2 = c(logreturns_K2, diff(logprices2[sel.avg]))
         vdelta_K = c(vdelta_K, diff(seconds[sel.avg])/secday)
    }
         
    logreturns_J1 = logreturns_J2 = vdelta_J = c()      
    for (j in 1:J_cov) {
         sel.avg = seq(j, n, J_cov)
         logreturns_J1 = c(logreturns_J1, diff(logprices1[sel.avg]))
         logreturns_J2 = c(logreturns_J2, diff(logprices2[sel.avg]))
         vdelta_J = c(vdelta_J, diff(seconds[sel.avg])/secday)
    }


    I_K1 = 1 * (logreturns_K1^2 <= eta * (RTSRV1 * vdelta_K + 2 * noisevar1))
    I_K2 = 1 * (logreturns_K2^2 <= eta * (RTSRV2 * vdelta_K + 2 * noisevar2))
    I_J1 = 1 * (logreturns_J1^2 <= eta * (RTSRV1 * vdelta_J + 2 * noisevar1))
    I_J2 = 1 * (logreturns_J2^2 <= eta * (RTSRV2 * vdelta_J + 2 * noisevar2))
    if (eta == 9) {
        ccc = 1.0415
    } else {
        ccc = cfactor_RTSCV(eta = eta)
    }
    RTSCV = adj_cov * (ccc * (1/K_cov) * sum(logreturns_K1 * I_K1 * 
        logreturns_K2 * I_K2)/mean(I_K1 * I_K2) - ((nbarK_cov/nbarJ_cov) * 
        ccc * (1/J_cov) * sum(logreturns_J1 * logreturns_J2 * I_J1 * 
        I_J2)/mean(I_J1 * I_J2)))
    return(RTSCV)
}


 

TSCov = function (pdata, cor = FALSE, K = 300, J = 1, K_cov = NULL, J_cov = NULL, 
                  K_var = NULL, J_var = NULL, makePsd = FALSE) 
{
    if (!is.list(pdata)) {
        n = 1
    }
    else {
        n = length(pdata)
        if (n == 1) {
            pdata = pdata[[1]]
        }
    }
    if (n == 1) {
        return(TSRV(pdata, K = K, J = J))
    }
    if (n > 1) {
        cov = matrix(rep(0, n * n), ncol = n)
        if( is.null(K_cov)){ K_cov = K }
        if( is.null(J_cov)){ J_cov = J }
        if( is.null(K_var)){ K_var = rep(K,n) }
        if( is.null(J_var)){ J_var = rep(J,n) }
        
        diagonal = c()
        for (i in 1:n) {
            diagonal[i] = TSRV(pdata[[i]], K = K_var[i], J = var[i])
        }
        diag(cov) = diagonal
      
        for (i in 2:n) {
            for (j in 1:(i - 1)) {
                cov[i, j] = cov[j, i] = TSCov_bi(pdata[[i]], 
                  pdata[[j]], K = K_cov, J = J_cov)
            }
        }
        if (cor == FALSE) {
            if (makePsd == TRUE) {
                cov = makePsd(cov)
            }
            return(cov)
        }
        if (cor == TRUE) {
            invsdmatrix = try(solve(sqrt(diag(diag(cov)))), silent = F)
            if (!inherits(invsdmatrix, "try-error")) {
                rcor = invsdmatrix %*% cov %*% invsdmatrix
                if (makePsd == TRUE) {
                  rcor = makePsd(rcor)
                }
                return(rcor)
            }
        }
    }
}

TSCov_bi = function (pdata1, pdata2, K = 300, J = 1) 
{
    x = refreshTime(list(pdata1, pdata2))
    newprice1 = x[, 1]
    newprice2 = x[, 2]
    logprices1 = log(as.numeric(newprice1))
    logprices2 = log(as.numeric(newprice2))
    seconds = as.numeric(as.POSIXct(index(newprice1)))
    secday = last(seconds) - first(seconds)
    n = length(logprices1)
    nbarK = (n - K + 1)/(K)
    nbarJ = (n - J + 1)/(J)
    adj = n/((K - J) * nbarK)

    logreturns_K1 = logreturns_K2 = logreturns_J1 = logreturns_J2 = c()
    vdelta_K =  vdelta_J = c();

    for (k in 1:K) {
        sel.avg = seq(k, n, K)
        logreturns_K1 = c(logreturns_K1, diff(logprices1[sel.avg]))
        logreturns_K2 = c(logreturns_K2, diff(logprices2[sel.avg]))
        vdelta_K = c(vdelta_K, diff(seconds[sel.avg]) / secday)
    }

    for (j in 1:J) {
        sel.avg = seq(j, n, J)
        logreturns_J1 = c(logreturns_J1, diff(logprices1[sel.avg]))
        logreturns_J2 = c(logreturns_J2, diff(logprices2[sel.avg]))
        vdelta_J = c(vdelta_J, diff(seconds[sel.avg])/secday)
    }

    TSCOV = adj * ((1/K) * sum(logreturns_K1 * logreturns_K2) - 
        ((nbarK/nbarJ) * (1/J) * sum(logreturns_J1 * logreturns_J2)))
    return(TSCOV)
}

cfactor_RTSCV = function(eta=9){
   require('cubature'); require('mvtnorm')
   # rho = 1
   c1 = pchisq(eta,df=1)/pchisq(eta,df=3) 
   # 
   rho = 0.001
   R = matrix( c(1,rho,rho,1) , ncol = 2 ) 
   int1 <- function(x) {    dmvnorm(x,sigma=R) }
   num = adaptIntegrate(int1, c(-3,-3), c(3,3), tol=1e-4)$integral
   int2 <- function(x) {  x[1]*x[2]*dmvnorm(x,sigma=R) }
   denom = adaptIntegrate(int2, c(-3,-3), c(3,3), tol=1e-4)$integral
   c2 = rho*num/denom   
   return( (c1+c2)/2 )
}

makePsd = function(S,method="covariance"){
   if(method=="correlation" & !any(diag(S)<=0) ){
     # Fan, J., Y. Li, and K. Yu (2010). Vast volatility matrix estimation using high frequency data for portfolio selection.
     D = matrix(diag(S)^(1/2),ncol=1)
     R = S/(D%*%t(D))
     out = eigen( x=R , symmetric = TRUE )
     mGamma = t(out$vectors)
     vLambda = out$values
     vLambda[vLambda<0] = 0
     Apsd = t(mGamma)%*%diag(vLambda)%*%mGamma
     dApsd = matrix(diag(Apsd)^(1/2),ncol=1)
     Apsd = Apsd/(dApsd%*%t(dApsd))
     D = diag( as.numeric(D)  , ncol = length(D) )
     Spos = D%*%Apsd%*%D
     return(Spos)
     #check:  eigen(Apsd)$values
  }else{
     # Rousseeuw, P. and G. Molenberghs (1993). Transformation of non positive semidefinite correlation matrices. Communications in Statistics - Theory and Methods 22, 965-984.
     out = eigen( x=S , symmetric = TRUE )
     mGamma = t(out$vectors)
     vLambda = out$values
     vLambda[vLambda<0] = 0
     Apsd = t(mGamma)%*%diag(vLambda)%*%mGamma
  }
}

