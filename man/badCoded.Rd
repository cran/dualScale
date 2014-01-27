\name{badCoded}
\alias{badCoded}
\docType{data}
\title{Nishisato and Clavel, artificial set of data}
\description{
10 observation and 3 variables erroneously coded.
}
\usage{data(badCoded)}
\format{
  A data frame with 10 observations on the following 3 variables.
  \describe{
    \item{\code{V2}}{Option 1 is omited}
    \item{\code{V3}}{Options go from 1 to 8}
    \item{\code{V4}}{Option 1 omited, other are not consecutive and there is NA}
  }
}
\source{
\describe{
\item{}{
Nishisato S, Baba Y (1999). On contingency, projection and forced classification of dual scaling. \emph{Behaviormetrika}, 26, 207--219.}
}
}

\references{
	\describe{
 \item{}{Nishisato S (2007). \emph{Multidimensional Nonlinear Descriptive Analysis}. Chapman & Hall/CRC.}
}
}
\examples{
data(badCoded)
dsCHECK(badCoded,'rad')
}
\keyword{datasets}
