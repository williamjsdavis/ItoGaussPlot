\name{stdWindowSlopeCast}
\alias{stdWindowSlopeCast}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Broadcast calculation for variance of slope of windows
}
\description{
\loadmathjax{}
Calculates the variability of trends as a function of windowing length. The time-series data is split up into chunks of size \eqn{w}, and each chunk is fit to the equation

\mjeqn{x(t) = A + Bt.}{}

The function returns the standard deviation of the distribution of all the chunks \eqn{B}s.
}
\usage{
stdWindowSlopeCast(time, amplitude, windows)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{time}{
Times (spacings can be uniform or non-uniform), vector, float
}
  \item{amplitude}{
Amplitudes corresponding to times, vector, float
}
  \item{windows}{
Length of window(s) to calculate for, scalar or vector, float
}
}
\details{
If 'windows' is a vector, the function will broadcast over it. Different calculation algorithms will be used depending on whether 'time' is uniformly or non-uniformly spaced.
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
> time <- seq(0, 10, len=1000)
> amplitude <- sin(time)
stdWindowSlopeCast(time,amplitude,seq(0.1,0.8,len=5))
[1] 0.7243351 0.7299902 0.7294216 0.7379881 0.7297922
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
