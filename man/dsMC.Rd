\name{dsMC}
\alias{dsMC}
\title{
Dual Scaling for Multiple Choice Data
}
\description{
The traditional analysis}
\usage{
dsMC(X, dim = NA)  
}
\arguments{
  \item{X}{The data
}
  \item{dim}{is the number of components one wishes to extract.  If no value is provided, the entire components will be extracted.
}
}
\value{
\item{Tot.Op}{Total Number of Options}
\item{Proj.Op_O}{Projected options weights}
\item{Proj.Su_O}{Projected subject scores}
\item{Inf_O}{Distribution of information over components}
\item{ItemStat_O}{Item statistics}
\item{Out_O}{Results}
\item{Rij_O}{Inter-item correlation}
\item{Norm.Op_O}{Normed options weights}
\item{Norm.Su_O}{Normed subject scores}

}
\references{
Nishisato, S. (1980). \emph{Analysis of categorical data: Dual scaling and its applications.
Toronto: University of Toronto Press.}
}

\seealso{
\code{\link{dsFC}}, \code{\link{summary.ds}}, \code{\link{plot.ds}}}
\examples{
data(singapore)
dsMC(singapore)
pato<-dsMC(singapore)
pato$Out_O
}
\keyword{Dual Scaling}
