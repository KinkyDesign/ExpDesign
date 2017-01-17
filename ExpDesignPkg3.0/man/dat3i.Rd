\name{dat3i}
\alias{dat3i}
\docType{data}

\title{Information for experimental design function suggest.trials.dr
}

\description{A list with information for experimental design function suggest.trials.dr
}

\usage{data("dat3i")}

\format{
  A list of 2 objects
 \describe{
  \item{predictedFeatures}{Names for the features calculated by  suggest.trials.dr function}
  \item{parameters}{A list of 7 objects, namely 'p.vec','dMax' , 'choice','designCrit','optimizer','Delta','n'.}
}
}

\details{
For more details please refer to manual page for exp.design.dr()
}

\source{
Internal eNM data
}

\references{
https://cran.r-project.org/web/packages/DoseFinding/index.html}

\examples{
data(dat3i)
## maybe str(dat3i) ; plot(dat3i) ...
}

\keyword{datasets}
