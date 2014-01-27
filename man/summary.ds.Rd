\name{summary.ds}
\alias{summary.ds}

\title{Summarizing Dual Scaling Analysis}

\description{This generic function is used to produce results of several aaplications
of dsFC and dsMC.
}
\usage{
\method{summary}{ds}(object,...)
}
\arguments{
  \item{object}{Dual scaling object from dsMC or dsFC.
}
\item{...}{Arguments to be passed to methods}  

}

\details{Available results available from different applications.
}
\value{
For Ordinary Dual Scaling (dsMC)
\item{IniDat}{Initial Data}

\item{ItONa}{Item options labels}  
\item{N.Comp}{Total number of Components}
\item{N.Item}{Total number of items} 
\item{N.Op}{Number of options of each item} 
\item{N.Ss}{Total number of subjects}             
\item{SubNa}{Subject labels} 
\item{Tot.Op}{Total number of options}    


\item{Inf_O}{Distribution of information over components}  
\item{ItemStat_O}{Item statistics} 
\item{Out_O}{Results obtained}                  
\item{Rij_O}{Inter item correlation}                 
\item{Norm.Op_O}{Normed option weights}       
\item{Norm.Su_O}{Normed subjects scores}               
\item{Proj.Op_O}{Projected option weights} 
\item{Proj.Su_O}{Projected subjects scores}  

For Force Classification Dual Scaling (dsFC). (NOTE: '_B' and '_C' values also available).

\item{IniDat}{Initial data}

\item{CramerV}{Cramer's coefficient V}
\item{CritItem}{The criterion item for forced classification}
\item{ItONa}{Item options labels}  
\item{Match}{Match-missmatch tables}
\item{N.Comp}{Total number of components} 
\item{N.Item}{Total number of items} 
\item{NSs}{Total number of subjects}
\item{NOpt}{Number of options of each item} 
\item{Predict}{Petcentage of correct classification}
\item{SubNa}{Subject labels}    
\item{Tot.Op}{Total number of options}                    
\item{Inf_A}{Distribution of information over components}   
\item{ItemStat_A}{Item statistics}  
\item{Out_A}{Results obtained by forced classification in the criterion subspace}                  
\item{Rij_A}{Inter item correlation}                 
\item{Norm.Op_A}{Normed option weights}       
\item{Norm.Su_A}{Normed subject scores}               
\item{Proj.Op_A}{Projected option weights} 
\item{Proj.Su_A}{Projected subject scores} 
  
\item{Out_B}{Results obtained by ignoring the criterion item}                  
\item{Out_C}{Results obtained in subspace complimentary to the criterion item}

}


\references{
\describe{
 \item{}{Nishisato S (2007). \emph{Multidimensional Nonlinear Descriptive Analysis}. Chapman & Hall/CRC.}
}}
\author{
	Jose G. Clavel, Shizuhiko Nishisato and Antonio Pita
}

\seealso{
\code{\link{dsFC}}, \code{\link{dsMC}}, \code{\link{print.ds}}, \code{\link{plot.ds}}
}
\examples{
  data(singapore)
	ole<-dsMC(singapore)
	summary(ole)
	ole$IniDa
}
\keyword{Forced Classification}
\keyword{Dual Scaling}% __ONLY ONE__ keyword per line
