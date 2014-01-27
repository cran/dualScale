\name{plot.ds}
\alias{plot.ds}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plotting 2D graphs for ds objects%%  ~~function to do ... ~~
}
\description{
Produces a two-dimensional graph of dual scaling results.%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{

\method{plot}{ds}(x, dim1=1, dim2=2, type="Asy1", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Dual scaling object from dsMC or dsFC
%%     ~~Describe \code{x} here~~
}
  \item{dim1}{Component for the horizontal axis 
%%     ~~Describe \code{dim1} here~~
}
  \item{dim2}{Component for the vertical axis
%%     ~~Describe \code{dim2} here~~
}
  \item{type}{Graph types: "Sym" for symmetric graph; "Asy" for assymetric graph (default); "Sub" for only subjects; "Ite" for only item options.} 
\item{...}{Arguments to be passed to methods}  
}

%%     ~~Describe \code{type} here~~


\details{
	The function plot.ds makes a two-dimensional representation of the object ds created by dsFC or dsMC. }

\seealso{
\code{\link{dsMC}}, \code{\link{dsFC}}, \code{\link{summary.ds}}}
\examples{
	data(singapore) 
  bea<-dsFC(singapore,1,6)
	plot(bea)
	plot(bea,1,2,'Asy1')	
}
\keyword{Forced Classification}
\keyword{Dual Scaling}
