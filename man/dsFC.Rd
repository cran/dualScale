\name{dsFC}
\alias{dsFC}
\title{Forced Classification Analysis
	}
\description{This program is for forced classification of dual scaling.
	}
\usage{
dsFC(X, Crit, dim)
}
\arguments{
  \item{X}{The Initial Data. }
  \item{Crit}{The criterion item for forced classification.
}
\item{dim}{The maximun number of components to be extracted.
}
}
\details{There are three types of outputs: Forced classification of the criterion item (type A); dual scaling of 
non-criterion items by ignoring the criterion item (type B); dual scaling of non-criterion items after eliminating 
the influence of the criterion item (type C).  These three types correspond to, respectively, dual scaling of data projected onto the subspace of the criterion item, dual scaling of non-criterion items, and dual scaling of
data in the complementary space of the criterion item.}
\value{
\item{Match}{Match-mismatch tables}
\item{Predict}{Correct prediction percentages}
\item{Proj.Op_A}{Projected options weights}
\item{Proj.Su_A}{Projected subject scores}
\item{Inf_A}{Distribution of information over components}
\item{ItemStat_A}{Item statistics}
\item{Out_A}{Results obtained by forced classification}
\item{Rij_A}{Inter-item correlation}
\item{Norm.Op_A}{Normed options weights}
\item{Norm.Su_A}{Normed subject scores}
}
\references{
Nishisato (1984). Forced classification: A simple application of a quantification technique.
\emph{Psychometrika, 49, 25-36}.
}
\seealso{
\code{\link{dsMC}}, \code{\link{dsCHECK}}, \code{\link{summary.ds}}, \code{\link{plot.ds}}}

\examples{
  data(singapore)
  dsFC(singapore,2,6)
}
\keyword{Forced Classification}
\keyword{Dual Scaling}% __ONLY ONE__ keyword per line
