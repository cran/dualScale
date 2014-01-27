\name{dsCHECK}
\alias{dsCHECK}
\title{Transforming data approprite for dual scaling}
\description{Initial data-polishing and handling of missing responses
}
\usage{
dsCHECK(X, mode='rad')
}
\arguments{
  \item{X}{The input data, a multiple choice table.}
  \item{mode}{There are two options: "rad" (default) for radical imputation and "act" for active imputation.}
}
\details{With option "rad," those subjects with NA (no answer) responses are discarded from analysis. With option "act," the NA responses were placed into newly created options so that missing responses are also subjected to analysis.
}
\value{
\item{InitialData}{The Initial Data.}
\item{TData}{The transformed data, ready for dsMC or dsFC}
}
\references{
Nishisato and Clavel (2014 in print). Dual scaling og multiple-choice data in R.
\emph{Journal of Statistical Software}.
}
\seealso{
\code{\link{dsMC}}, \code{\link{dsFC}}}
\examples{	
data(badCoded)
dsCHECK(badCoded, mode='act')
}
