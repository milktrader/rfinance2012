\name{MinRV}
\Rdversion{1.1}
\alias{MinRV}
\title{
MinRV
}
\description{
Function returns the MinRV, defined in Andersen et al. (2009).

Let \eqn{r_{t,i}} be a return (with \eqn{i=1,\ldots,M}) in period \eqn{t}.

Then, the MinRV is given by
\deqn{
\mbox{MinRV}_{t}=\frac{\pi}{\pi - 2}\left(\frac{M}{M-1}\right) \sum_{i=1}^{M-1} \mbox{min}(|r_{t,i}| ,|r_{t,i+1}|)^2
}

}

\usage{
MinRV(rdata,...)
}

\arguments{
  \item{rdata}{ a zoo/xts object containing all returns in period t for one asset. }
\item{...}{additional arguments.}
}


\value{
numeric
}

\references{
Andersen, T. G., D. Dobrev, and E. Schaumburg (2009). Jump-robust volatility 
estimation using nearest neighbor truncation. NBER Working Paper No.
15533.}

\author{ Jonathan Cornelissen and Kris Boudt}

\keyword{ volatility}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%R CMD Rdconv --type=html --output=sample_5minprices.htm sample_5minprices.Rd