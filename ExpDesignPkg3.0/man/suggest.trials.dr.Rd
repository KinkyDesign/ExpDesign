\name{suggest.trials.dr}
\alias{suggest.trials.dr}
\title{
Returns suggested weights and/or sample sizes for Dose-Response data}
\description{Suggested weights/sample size are returned as a list with one object, a numeric matrix.
}
\usage{
suggest.trials.dr(dataset, rawModel, additionalInfo)
}
\arguments{
  \item{dataset}{ Data for prediction. A list of two objects: datasetURI (a character string ), dataEntry (a data frame).}
  \item{rawModel}{ R model serialized (suggested sample sizes for the data matrix supplied in exp.design.dr). 
}
  \item{additionalInfo}{Any additional information needed for rawModel. Here the list generated by exp.design.dr. The list should contain a field named 'predictedFeatures' which should be exactly the same as that returned by exp.design.dr function. 
}
}
\details{No further details required
}
\value{A list of one objected called 'predictions' which is also a list of data-frames each containing doses and suggested weights or sample sizes for the data tested. This object is a matrix for the suggested sample sizes per dose.
}
\references{https://cran.r-project.org/web/packages/DoseFinding/index.html
}
\author{Georgia Tsiliki
}
\note{No notes for this function
}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

data(dat4)
data(dat3m)
data(dat3i)

new.ss<- suggest.trials.dr(dat4$dataset,dat3m,dat3i)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ suggestDR }
