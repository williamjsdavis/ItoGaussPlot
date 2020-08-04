\name{OUanalytic}
\alias{OUanalytic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Ornstein-Uhlenbeck analytic expression
}
\description{
\loadmathjax{}
The closed form expression of slope standard deviation, for an Ornstein-Uhlenbeck process.

\mjeqn{\sigma_B(w) = \sqrt{24\gamma D f(\gamma w)},}{}

where \eqn{\gamma} is the slope of the drift term, \eqn{D} is the amplitude of the noise term, and \eqn{f(w)} is

\mjeqn{f(x) = x^{-3}-3x^{-4}+12x^{-6}-e^{-x}\Big(3x^{-4}+12x^{-5}+12x^{-6}\Big).}{}
}
\usage{
OUanalytic(windowLength, gamma, D, approx="none")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{windowLength}{
Length of window, scalar or vector, float
}
  \item{gamma}{
Slope of drift term, scalar, float
}
  \item{D}{
Amplitude of noise term, scalar, float
}
  \item{approx}{
Approximation type (optional, defaults to "none"), string: none/smallW
}
}
\details{
If 'windowLength' is a vector, the function will broadcast over it.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Broadcasting over a vector of windows
> OUanalytic(10^seq(-1,2,len=5), 1, 1, "none")
[1] 4.798460391 1.843358930 0.495709635 0.059640405 0.004824965
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
