\name{MedRV}
\Rdversion{1.1}
\alias{MedRV}
\title{
MedRV
}
\description{
Function returns the MedRV, defined in Andersen et al. (2009).

Let \eqn{r_{t,i}} be a return (with \eqn{i=1,\ldots,M}) in period \eqn{t}.

Then, the MedRV is given by
\deqn{
\mbox{MedRV}_{t}=\frac{\pi}{6-4\sqrt{3}+\pi}\left(\frac{M}{M-2}\right) \sum_{i=2}^{M-1} \mbox{med}(|r_{t,i-1}|,|r_{t,i}|, |r_{t,i+1}|)^2
}

}

\usage{
MedRV(rdata,...)
}

\arguments{
  \item{rdata}{ a zoo/xts object containing all returns in period t for one asset. }
\item{...}{additional arguments.}

}

\section{Details}{

The MedRV belongs to the class of realized volatility measures in RTAQ 
that use the series of high-frequency returns \eqn{r_{t,i}} of a day \eqn{t} 
to produce an ex post estimate of the realized volatility of that day \eqn{t}. 
MedRV is designed to be robust to price jumps. 
The difference between RV and MedRV is an estimate of the realized jump 
variability. Disentangling the continuous and jump components in RV 
can lead to more precise volatility forecasts, 
as shown in Andersen et al. (2007) and Corsi et al. (2010).
}

\value{
numeric
}

\references{
Andersen, T. G., D. Dobrev, and E. Schaumburg (2009). Jump-robust volatility 
estimation using nearest neighbor truncation. NBER Working Paper No.
15533.

Andersen, T.G., T. Bollerslev, and F. Diebold (2007). Roughing it up: including
jump components in the measurement, modelling and forecasting of return
volatility. The Review of Economics and Statistics 89 (4), 701-720.

Corsi, F., D. Pirino, and R. Reno (2010). Threshold Bipower Variation and the Impact of Jumps on Volatility Forecasting. 
Journal of Econometrics 159 (2), 276-288.
}

\author{ Jonathan Cornelissen and Kris Boudt}

\keyword{ volatility}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%R CMD Rdconv --type=html --output=sample_5minprices.htm sample_5minprices.Rd