\name{print.ds}
\alias{print.ds}

\title{Print Method for Class 'ds'}
\description{
Printing dual scaling objects
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{print}{ds}(x, type, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{dual scaling object from dsFC or dsMC
%%     ~~Describe \code{x} here~~
}
  \item{type}{Specific dsFC outputs: 'B'=results from ignoring the criterion item; 'C'=results from the subspace of the criterion item and from its complementary subspace. 
%%     ~~Describe \code{type} here~~
}
\item{...}{Arguments to be passed to methods}  

}
\details{
	The function print.ds provides basic statistics of the ds object. The function summary.ca offers all availabe information from analysis.}
\note{
Regarding all available information, please refer to Clavel and Nishisato (2014).}

\seealso{
\code{\link{dsMC}}, \code{\link{dsFC}}, \code{\link{summary.ds}}, \code{\link{plot.ds}}}
\examples{
data(singapore)
oscar<-dsFC(singapore,2,6)
print(oscar)
print(oscar,'B')
print(oscar,'C')
bea<-dsMC(singapore)
print(bea)
}
\keyword{Forced Classification}
\keyword{Dual Scaling}% __ONLY ONE__ keyword per line
