

filtered_variance = function(rdata,flag=NULL,L=25){
  # Based on original code from Fulvio Corsi, Davide Pirino and Roberto Reno'
  # "Threshold bipower variation and the impact of jumps on volatility forecasting" (J Ects, 2010)
  # Auxiliary estimation as described in the appendix
  r2 = rdata^2; 
  T = length(r2);
  
  if( is.null(flag)){ flag = rep(0,T)}
  hL = rep(0,T)
  for( k in 1:T ){
    h=s=0;
    for( i in c( (-L:-2),(2:L))  ){
        if ( ((k+i)>0) & ( (k+i)<(T+1))  ){
           if( flag[k+i]==0){
                h = h + dnorm(i/L)*r2[k+i]; 
                s = s + dnorm(i/L);             
           }
        }     
    }
    if (s!=0){ hL[k]=h/s }else{ hL[k]=0 }
  }
  return(hL);
}

CPRlocalvol <- function(rdata,niter=3,L=25,eta=9){
  # Based on original code from Fulvio Corsi, Davide Pirino and Roberto Reno'
  # "Threshold bipower variation and the impact of jumps on volatility forecasting" (J Ects, 2010)
  # Auxiliary estimation as described in the appendix
  hL = Inf;
  for( iterations in 1:niter ){
    flag_jumps = rep(0,length(rdata))
    flag_jumps[ rdata^2 > (eta*hL)] = 1    
    hL = filtered_variance(rdata=rdata,flag=flag_jumps,L=L);   
  }
  return(hL)
}

thresholdBPCov = function (rdata, startIV = NULL, eta = 9, cor=FALSE, makeReturns=FALSE,...){
  # Based on original code from Fulvio Corsi, Davide Pirino and Roberto Reno'
  # "Threshold bipower variation and the impact of jumps on volatility forecasting" (J Ects, 2010)
  # Auxiliary estimation as described in the appendix
  if(hasArg(data)){ rdata = data }
  if(makeReturns){  rdata = makeReturns(rdata)}
 
  rdatacheck(rdata,multi=TRUE);
    
  rdata=as.matrix(rdata);
  n=dim(rdata)[1];  					                  #number of observations
  delta = 1/n;
  if (!is.null(startIV)) {
        localvars = startIV*delta;
  }
  if (is.null(startIV)) {
        localvars = apply(rdata,2,FUN="CPRlocalvol");  #Robust local vol estimator of Corsi, Pireno and Reno
  }  
  tresmatrix = eta*localvars;	  #treshold per stock
  condition = (rdata^2) > tresmatrix;
  rdata[condition] = 0;
  covariance = RBPCov(rdata);
  if(cor==FALSE){ return(covariance) }
  if(cor==TRUE){
  sdmatrix = sqrt(diag(diag(covariance)));
  rcor = solve(sdmatrix)%*%covariance%*%solve(sdmatrix);
  return(rcor)}
}  
  
