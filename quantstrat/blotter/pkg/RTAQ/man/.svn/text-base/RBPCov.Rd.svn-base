\name{RBPCov}
\Rdversion{1.1}
\alias{RBPCov}
\title{
Realized BiPower Covariance}

\description{
Function returns the Realized BiPower Covariance (RBPCov), 
defined in Barndorff-Nielsen and Shephard (2004).

Let \eqn{r_{t,i}} be an intraday \eqn{N x 1} return vector and \eqn{i=1,...,M} 
the number of intraday returns.


The RBPCov is defined as the process whose value at time \eqn{t}
is the \eqn{N}-dimensional square matrix with \eqn{k,q}-th element equal to
\deqn{
\mbox{RBPCov}[k,q]_t = \frac{\pi}{8} \bigg( \sum_{i=2}^{M} 
\left|
r_{(k)t,i} + r_{(q)t,i} \right| \ \left| r_{(k)t,i-1} + r_{(q)t,i-1} \right|   \\
- \left| r_{(k)t,i}  - r_{(q)t,i} \right| \ \left|
r_{(k)t,i-1} - r_{(q)t,i-1} \right|  \bigg),
}
where \eqn{r_{(k)t,i}} is the
\eqn{k}-th component of the return vector \eqn{r_{i,t}}.
}

\usage{
RBPCov(rdata,cor=FALSE,makeReturns=FALSE,makePsd=FALSE,...)
}

\arguments{
  \item{rdata}{ a \eqn{(M x N)} matrix/zoo/xts object containing the \eqn{N}
return series over period \eqn{t}, with \eqn{M} observations during \eqn{t}.}
\item{cor}{ boolean, in case it is TRUE, the correlation is returned. FALSE by default.}
\item{makeReturns}{boolean, should be TRUE when rdata contains prices instead of returns. FALSE by default.}
\item{makePsd}{ boolean, in case it is TRUE, the positive definite version of RBPCov is returned. FALSE by default.}
\item{...}{additional arguments.}
}

\value{
an \eqn{N x N} matrix
}

\references{
Barndorff-Nielsen, O. and N. Shephard (2004). Measuring the impact of
jumps in multivariate price processes using bipower covariation. Discussion
paper, Nuffield College, Oxford University.}

\author{ Jonathan Cornelissen and Kris Boudt}

\keyword{ volatility}

%cd C:\package\TradeAnalytics\pkg\RTAQ\man
%R CMD Rdconv --type=html --output=sample_5minprices.htm sample_5minprices.Rd