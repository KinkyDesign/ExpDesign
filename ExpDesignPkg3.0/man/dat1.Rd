\name{dat1}
\alias{dat1}
\docType{data}

\title{
A sample data object
}

\description{
The dataset for this test is a data frame
}

\usage{data("dat1")}

\format{
  A list of two objects
  \describe{
    \item{datasetURI}{a character vector- ambit data set uri}
    \item{dataEntry}{a data frame containing two columns: compound and values. Compound is a character vector with all compound ambit uris, and values is a data frame with all numberic values of the protein corona data set (compounds by features). One of the columns is the dependent variable (Net.cell.association) which has some null values- the experimental design algorithm will select some/all of them for next suggested trials.}
  }
}

\details{
There are no more details
}

\source{
The source of this function is in the
}

\references{
There are no references
}

\examples{
data(dat1)
## maybe str(dat1) ; plot(dat1) ...
}

\keyword{datasets}
