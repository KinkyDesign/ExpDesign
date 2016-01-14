\name{r2.adj.funct}
\alias{r2.adj.funct}

\title{ Adjusted R2 function }

\description{Calculates the adjusted R2 value}

\usage{
r2.adj.funct(y, y.new, num.pred)
}

\arguments{
  \item{y}{ observed y values }
  \item{y.new}{ predicted y values }
  \item{num.pred}{ number of parameters in the predicted model} 
}

\details{No details required }

\value{ A numeric value for the adjusted coefficient of determination R2 }

\references{ Dobson An introduction to linear modelling }

\author{Georgia Tsiliki }

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

r2.adj.funct(1:10,1:10,2)

}

\keyword{ r2adj }

