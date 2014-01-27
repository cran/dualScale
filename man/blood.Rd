\name{blood}
\alias{blood}
\docType{data}
\title{Blood Preassure}
\description{
Nishisato's widely used artificial data on 15 subjects answering 6 questions on their health.}
\usage{data(blood)}

\format{
  A frame 15x6 multiple-choice table of chosen option numbers.
  \describe{
    \item{\code{V1}}{How would you rate your blood preassure? (1=low, 2=medium, 3=high)}
    \item{\code{V2}}{Do you get migraines? (1=rarely, 2=sometimes, 3=often)}
    \item{\code{V3}}{What is your age group? (1=20-34, 2=35-49, 3=50-65)}
    \item{\code{V4}}{How would you rate your daily level of anxiety? (1=low, 2=medium, 3=high)}
    \item{\code{V5}}{How would you rate your weight? (1=light, 2= medium, 3=heavy)}
    \item{\code{V6}}{What about your height? (1= short, 2=medium, 3=tall)}
  }
}
\details{
The data were used for the first time in Nishisato (1999), and have been used in a number of his papers.}
\source{
\describe{
\item{}{Nishisato S (1999). Data types and information: beyond the current practice of
  data analysis. In \emph{Classification and Information Processing at the Turn of the
  Millennium}, pp. 40--51. Springer-Verlag. }
}
}
\references{
\describe{
\item{}{Nishisato S (1999), cited above.}

 \item{}{Nishisato S (2007). \emph{Multidimensional Nonlinear Descriptive Analysis}. Chapman & Hall/CRC.}
}
}
\examples{
data(blood)
}
\keyword{datasets}
