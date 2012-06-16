 #  START implementation of paper:
 #  ROUGHING IT UP: INCLUDING JUMP COMPONENTS IN THE MEASUREMENT, MODELING, AND FORECASTING OF RETURN VOLATILITY
 #  Torben G. Andersen, Tim Bollerslev, and Francis X. Diebold
 #  data: a xts object with the intraday data
 #  periods: a vector with time periods to aggregate over, expressed in days
 #  RVest: estimator for daily realized volatility, 
 #  in case a vector is supplied, the first estimator is the unrobust estimator, the second is the robust estimator 
 #  type: string defining the type of model
 #  "HARRV" from "roughing paper"
 #  "HARRVJ" from "roughing paper"
 #  "HARRVCJ" from "roughing paper"
 #  jumptest: function to calculate the jump test statistic which determines whether the daily jump contribution is significant
 #  alpha: a value between zero and one to indicate what
 #  h: integer, determining over how many periods the depend variable should be aggregated. The default is 1, i.e. no aggregation is done, just one day. 
 #  TODO ADD extra argument: jump-periods??? for aggregated jumps in the model...
 
 # Helpfunctions: 
 TQfun = function(rdata){ #Calculate the realized tripower quarticity
   returns = as.vector(as.numeric(rdata));
   n = length(returns);
   mu43 = 0.8308609; #    2^(2/3)*gamma(7/6) *gamma(1/2)^(-1)   
   tq = n * ((mu43)^(-3)) *  sum( abs(returns[1:(n - 2)])^(4/3) *abs(returns[2:(n-1)])^(4/3) *abs(returns[3:n])^(4/3) );
   return(tq);
 } 
 
 ABDJumptest = function(RV, BPV, TQ){ # Comput jump detection stat mentioned in roughing paper
   mu1  = sqrt(2/pi);
   n = length(RV);
   zstat = ((1/n)^(-1/2))*((RV-BPV)/RV)*(  (mu1^(-4) + 2*(mu1^(-2))-5) * pmax( 1,TQ*(BPV^(-2)) )   )^(-1/2); 
  return(zstat);
 }

 harModel = function(data, periods = c(1,5,22), periodsJ = c(1,5,22), leverage=NULL, RVest = c("RCov","RBPCov"), type="HARRV", 
                     jumptest="ABDJumptest",alpha=0.05,h=1,transform=NULL, ...){  
  nperiods = length(periods); # Number of periods to aggregate over
  nest = length(RVest);       # Number of RV estimators
  if( !is.null(transform) ){ Ftransform = match.fun(transform); }
  if( !(type %in% c("HARRV","HARRVJ","HARRVCJ"))){ warning("Please provide a valid argument for type, see documentation.")  }    
  
  if( sum(data<0) != 0 ){ #If it are returns as input
   # Get the daily RMs (in a non-robust and robust way)
   RV1 = match.fun(  RVest[1]);
   RM1 = apply.daily( data, RV1 );
   # save dates:
   alldates = index(RM1)
   if( nest == 2 ){ 
    RV2 = match.fun( RVest[2]); 
    RM2 = apply.daily( data, RV2 ); }
  } 
    
  if( sum(data<0) == 0 ){ #The input is most likely already realized measures
      dimdata = dim(data)[2]; 
      alldates = index(data);
      RM1 = data[,1];
      if( dimdata > 1 ){ RM2 = data[,2]; } 
      if( type != "HARRV" ){ warning("Please provide returns as input for the type of model you want to estimate. All your returns are positive which is quite unlikely honestly. Only for the HAR-RV model you can input realized measures.") }
     }
 
    # Get the matrix for estimation of linear model
    maxp      = max(periods,periodsJ); #max number of aggregation levels
    if(!is.null(leverage)){ maxp = max(maxp,leverage) }
    n         = length(RM1);  #Number of Days
  
    # Aggregate RV: 
    RVmatrix1 = aggRV(RM1,periods);
    if( nest==2 ){ RVmatrix2 = aggRV(RM2,periods); }  # In case a jumprobust estimator is supplied
  
    # Aggregate and subselect y:
    y = aggY(RM1,h,maxp);
  
   # Only keep useful parts: 
   x1 = RVmatrix1[(maxp:(n-h)),]; 
   if( nest==2 ){ x2 = RVmatrix2[(maxp:(n-h)),]; } # In case a jumprobust estimator is supplied 
  
   # Jumps:
   if(type!="HARRV"){ # If model type is as such that you need jump component 
     J = pmax( RM1 - RM2,0 ); # Jump contributions should be positive
     J = aggJ(J,periodsJ);         
   }
  
  if( !is.null(leverage) ){ 
    if( sum(data<0) == 0 ){ warning("You cannot use leverage variables in the model in case your input consists of Realized Measures") }
      # Get close-to-close returns
      e = apply.daily(data,sum); #Sum logreturns daily     
      # Get the rmins:
      rmintemp = pmin(e,0);    
      # Aggregate everything:
      rmin = aggRV(rmintemp,periods=leverage,type="Rmin"); 
      # Select:
      rmin = rmin[(maxp:(n-h)),];
       }else{ rmin = matrix(ncol=0,nrow=dim(x1)[1]) }
        
   ###############################
   # Estimate the model parameters, according to type of model : 
   # First model type: traditional HAR-RV: 
   if( type == "HARRV" ){ 
    if(!is.null(transform)){ y = Ftransform(y); x1 = Ftransform(x1) }
      x1 = cbind(x1,rmin);
      model     = estimhar(y=y,x=x1); 
      model$transform = transform; model$h = h; model$type = "HARRV"; model$dates = alldates[(maxp+h):n];
      class(model) = c("harModel","lm"); 
      return( model )
  } #End HAR-RV if cond

  if( type == "HARRVJ" ){    
      J = J[(maxp:(n-h)),]; 
      x = cbind(x1,J);              # bind jumps to RV data 
      if(!is.null(transform)){ y = Ftransform(y); x = Ftransform(x); }       
      x = cbind(x,rmin);
      model = estimhar(y=y,x=x); 
      model$transform = transform; model$h = h; model$type = "HARRVJ"; model$dates = alldates[(maxp+h):n];
      class(model) = c("harModel","lm"); 
      return( model )    
  }#End HAR-RV-J if cond
  
  if( type == "HARRVCJ" ){ 
      # Are the jumps significant? if not set to zero:
      if( jumptest=="ABDJumptest" ){ 
      TQ = apply.daily(data, TQfun); 
      J = J[,1];
      teststats    = ABDJumptest(RV=RM1,BPV=RM2,TQ=TQ ); 
      }else{ jtest = match.fun(jumptest); teststats = jtest(data,...) }  
      Jindicators  = teststats > qnorm(1-alpha); 
      J[!Jindicators] = 0; 
      # Get continuus components if necessary RV measures if necessary: 
      Cmatrix = matrix( nrow = dim(RVmatrix1)[1], ncol = 1 );
      Cmatrix[Jindicators,]    = RVmatrix2[Jindicators,1];      #Fill with robust one in case of jump
      Cmatrix[(!Jindicators)]  = RVmatrix1[(!Jindicators),1];   #Fill with non-robust one in case of no-jump  
      # Aggregate again:
      Cmatrix <- aggRV(Cmatrix,periods,type="C");
      Jmatrix <- aggJ(J,periodsJ);
      # subset again:
      Cmatrix <- Cmatrix[(maxp:(n-h)),];
      Jmatrix <- Jmatrix[(maxp:(n-h)),];            
      x = cbind(Cmatrix,Jmatrix);               # bind jumps to RV data      
      if(!is.null(transform)){ y = Ftransform(y); x = Ftransform(x); }  
      x = cbind(x,rmin);
      model = estimhar( y=y, x=x ); 
      model$transform = transform; model$h = h; model$type = "HARRVCJ"; model$dates = alldates[(maxp+h):n];      
      class(model) = c("harModel","lm");
      return(model)
      } 

} #End function harModel
 #################################################################
 estimhar = function(y, x){ #Potentially add stuff here
   colnames(y)="y";
   output = lm( formula(y~x), data=cbind(y,x));
 }
 
 # Help function to get nicely formatted formula's for print/summary methods..
 getHarmodelformula = function(x){
  modelnames = colnames(x$model$x);
  if(!is.null(x$transform)){ 
    
   modelnames = paste(x$transform,"(",modelnames,")",sep=""); } #Added visual tingie for plotting transformed RV
   betas      = paste("beta",(1:length(modelnames)),"",sep="")
   betas2     = paste(" + ",betas,"*")
   rightside  = paste(betas2, modelnames,collapse="");
   h = x$h;
   left = paste("RV",h,sep="");
   if(!is.null(x$transform)){  left = paste(x$transform,"(",left,")",sep="" ) }
   modeldescription = paste(left,"= beta0",rightside);
  return(list(modeldescription,betas))  
 }
 
 aggRV <- function(RM1,periods,type="RV"){
   n = length(RM1);
   nperiods = length(periods);
   RVmatrix1 = matrix(nrow=n,ncol=nperiods);
   for(i in 1:nperiods){ 
     if(periods[i]==1){ RVmatrix1[,i] = RM1; 
     }else{ RVmatrix1[(periods[i]:n),i] = rollmean(x=RM1,k=periods[i],align="left")  }
   } #end loop over periods for standard RV estimator
   colnames(RVmatrix1) = paste(type,periods,sep="");
   return(RVmatrix1);
 }

 aggJ <- function( J, periodsJ ){
   n = length(J);
   nperiods = length(periodsJ);
   JM = matrix(nrow=n,ncol=nperiods);
   for(i in 1:nperiods){ 
     if(periodsJ[i]==1){ JM[,i] = J; 
     }else{ JM[(periodsJ[i]:n),i] = rollmean( x=J, k=periodsJ[i], align="left")  }
   } # End loop over periods for standard RV estimator
   colnames(JM) = paste("J",periodsJ,sep="");
   return(JM)
 }
 
 aggY = function(RM1,h,maxp){
   n         = length(RM1);
   if( h == 1 ){  y  = RM1[(maxp+1):n]; }
   if( h != 1 ){ 
     y = matrix( nrow=length(RM1), ncol=1 ); colnames(y) = "y";
     y[(h:n),] = rollmean(x=RM1,k=h,align="left");
     y = matrix(y[((maxp+h):n),],ncol=1); y=as.data.frame(y) }  
   return(y);
 }
 
 
######################################################################### 
 # Print method for harmodel:  
 print.harModel = function(x, digits = max(3, getOption("digits") - 3), ...){ 
   formula = getHarmodelformula(x); modeldescription = formula[[1]]; betas = formula[[2]];
   
   cat("\nModel:\n", paste(modeldescription, sep = "\n", collapse = "\n"), 
       "\n\n", sep = "")
   
   coefs = coef(x);
   names(coefs)  = c("beta0",betas)
   
   if (length(coef(x))){
     cat("Coefficients:\n")
     print.default(format(coefs, digits = digits), print.gap = 2,quote = FALSE);
     cat("\n\n");
     Rs = summary(x)[c("r.squared", "adj.r.squared")]
     zz = c(Rs$r.squared,Rs$adj.r.squared);
     names(zz) = c("r.squared","adj.r.squared")
     print.default((format(zz,digits=digits) ),print.gap = 2,quote=FALSE)
   }
   else cat("No coefficients\n")
   cat("\n")
   invisible(x)
 } 
  
 summary.harModel = function(object, correlation = FALSE, symbolic.cor = FALSE,...){
   x=object; 
   dd = summary.lm(x);
   formula = getHarmodelformula(x); modeldescription = formula[[1]]; betas = formula[[2]];
   dd$call = modeldescription;
   rownames(dd$coefficients) = c("beta0",betas);
   return(dd)
 }

 plot.harModel = function(x, which = c(1L:3L, 5L), caption = list("Residuals vs Fitted", 
                                                                  "Normal Q-Q", "Scale-Location", "Cook's distance", "Residuals vs Leverage", 
                                                                  expression("Cook's dist vs Leverage  " * h[ii]/(1 - h[ii]))), 
                          panel = if (add.smooth) panel.smooth else points, sub.caption = NULL, 
                          main = "", ask = prod(par("mfcol")) < length(which) && dev.interactive(), 
                          ..., id.n = 3, labels.id = names(residuals(x)), cex.id = 0.75, 
                          qqline = TRUE, cook.levels = c(0.5, 1), add.smooth = getOption("add.smooth"), 
                          label.pos = c(4, 2), cex.caption = 1){ 
  observed = x$model$y;
  fitted   = x$fitted.values;
  dates    = x$dates;
  dates    = as.POSIXct(dates);
  observed = xts(observed, order.by=dates);
  fitted   = xts(fitted, order.by=dates);
  type     = x$type;
  
  g_range = range(fitted,observed)
  g_range[1] = 0.95*g_range[1]; g_range[2]= 1.05 * g_range[2]; 
  #ind = seq(1,length(fitted),length.out=5);
  title = paste("Observed and forecasted RV based on HAR Model:",type);
  plot.zoo(observed,col="red",lwd=2,main=title, ylim=g_range,xlab="Time",ylab="Realized Volatility"); 
  #  axis(1,time(b)[ind], format(time(b)[ind],), las=2, cex.axis=0.8); not used anymore
  #  axis(2);
  lines(fitted,col="blue",lwd=2);
  legend("topleft", c("Observed RV","Forecasted RV"), cex=1.1, col=c("red","blue"),lty=1, lwd=2, bty="n"); 
}
 