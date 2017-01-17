\name{dat4}
\alias{dat4}
\docType{data}

\title{
A sample data object for dose-response
}

\description{
Dose-response data as included in eNM database
}

\usage{data("dat4")}

\format{
A list of 3 objects
\describe{
    \item{dataset}{A list of 8 objects, i.e. 'meta','visible', 'dataEntry','features','totalRows','totalColumns','descriptors',  '_id'}
    \item{predictionFeature}{A list of Ambit data set uris for predicted features- here empty}
    \item{parameters}{A list of 7 objects needed for exp.design.dr() and suggest.trials.dr() functions: 'p.vec', 'dMAx','choice','designCrit','optimizer','Delta','n'}	
}
}

\details{Example dataset for Doptimal experimental design and Dose-Response data 
}

\source{The source of this function is in the
}

\references{
There are no references
}

\examples{
data(dat4)
## maybe str(dat4) ; plot(dat4) ...
}

\keyword{datasets}
