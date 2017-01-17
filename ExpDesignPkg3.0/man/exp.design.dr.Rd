\name{exp.design.dr}
\alias{exp.design.dr}

\title{
Experimental design function for Dose-Response data
}

\description{
Given a set of nine models (with full parameter values and model probabilities) this function calculates the optimal design for estimating the dose-response model parameters (D-optimal) or
the design for estimating the target dose (TD-optimal design) (see Dette, Bretz, Pepelyshev and Pinheiro (2008)), or a mixture of these two criteria, using the optDesign() function from DoseFinding cran package. 
}

\usage{
exp.design.dr(dataset, predictionFeature, parameters)
}

\arguments{
  \item{dataset}{A list of 8 objects, i.e. 'meta','visible', 'dataEntry','features','totalRows','totalColumns','descriptors',  '_id'}
  \item{predictionFeature}{A list of Ambit data set uris for predicted features- here empty}
  \item{parameters}{A list of 7 objects needed for exp.design.dr() and suggest.trials.dr() functions. Particularly 'p.vec' is a vector of three values corresponding to the expected percentages of the maximum effect achieved at the first,second, and last doses, 'dMAx' is the dose at which maximum effect occurs in 0-1 scale (only needed for the beta model),'choice' is a character string awaiting one of three strings ('weight' for weights on doses, 'sampleSize' for sample size per dose, 'tsampleSize' for tailored sample size n given the weights, 'designCrit' one of c("Dopt", "TD", "Dopt&TD", "userCrit") to determine which type of design to calculate, 'optimizer' one of c("solnp", "Nelder-Mead", "nlminb", "exact") which corresponds to the algorithm used for calculating the optimal design, 'Delta'  the target effect needed for calculating "TD" and "TD&Dopt" type designs, 'n' the sample size of next cohort (if $choice is 'tSampleSize' for specific sample sie calculations, or 'SampleSize' to specify the upper bound for target sample size).}	
}

\details{No details required.
}

\value{A List 
  \item{rawModel}{ A serialized numeric matrix indicating the experimental design for the various doses.}
  \item{pmmlModel}{ A pmml object - now empty}
  \item{independentFeatures}{ A list with the names of the variables as given by the user - now empty.}
  \item{predictedFeatures}{A character vector with names for the doses and suggested weights/samples.}
  \item{additionalInfo}{ A list including predictedFeatures and parameters arguments.}
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

exp.exampleDR<- exp.design.dr(dat4$dataset,dat4$predictionFeature,dat4$parameters) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ expDesignDR }

