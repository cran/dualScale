\name{dualScale-package}
\alias{dualScale-package}
\alias{dualScale}
\docType{package}
\title{Dual Scaling for Multiple Choice Data
}
\description{
This package includes \code{dsMC} and \code{dsFC} for several versions of dual scaling of multiple-choice data.
}
\details{
\tabular{ll}{
Package: \tab ds\cr
Type: \tab Package\cr
Version: \tab 0.9\cr
Date: \tab 2014-01-24\cr
License: \tab GPL2\cr
}
With \code{dsMC} and \code{dsFC} a \code{ds} class object is created. It can be analyzed later with print.ds, plot.ds or summary.ds
}
\author{
Jose G. Clavel, Shizuhiko Nishisato and Antonio Pita

Maintainer: Jose G. Clavel <dualScale@gmail.com>
}
\references{
Nishisato and Clavel (2014).
Nishisato (2007)
}
\keyword{ package }
\keyword{Forced Classification}
\keyword{Dual Scaling}% __ONLY ONE__ keyword per line

\seealso{
\code{\link{dsMC}}, \code{\link{dsFC}}, \code{\link{dsCHECK}}, \code{\link{plot.ds}}, \code{\link{print.ds}}, \code{\link{summary.ds}} 
}
\examples{
data(singapore)
ciuca<-dsFC(singapore,2,6)
plot(ciuca)
bea<-dsMC(singapore)
print(bea)
}
