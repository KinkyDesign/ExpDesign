\name{exp.design.xy}
\alias{exp.design.xy}

\title{Experimental design function with X and/or y values }

\description{Calculates an exact or approximate algorithmic design for one of three criteria, using Federov's exchange algorithm from AlgDesign package
}

\usage{
exp.design.xy(dataset, predictionFeature, parameters)
}

\arguments{
  \item{dataset}{list of 2 objects, datasetURI:= character sring, code name of dataset, dataEntry:= data frame with 2 columns 
}
  \item{predictionFeature}{ character string specifying which is the prediction feature in dataEntry 
}
  \item{parameters}{list with parameter values for experimental design. 5 objects should be included, i.e. nTrials a numeric value indicating number of trials suggested, if 0 then an estimated number is suggested, criterion a character value to indicate which optimal deisgn to apply (possible values are  'D', 'A', 'I'), form a string indicating the formula of the deisgn (possible formulas are 'linear','quad','cubic','cubicS'), r2.threshold a numeric value indicating the r2 threshold value (If the data supplied provides
  r2 value greater than the threshold value, a warning message is returned.). The 5th object is only needed in case the dependent variable is not available yet, then a new parameter should be added, called 'newY' which is a character string with the name of the new dependent feature.
}
}
\details{No details required }

\value{A List 
  \item{rawModel}{ A serialized numeric vector indicating the experimental design memberships of nanoparticles in the data.}
  \item{pmmlModel}{ A pmml object - now empty}
  \item{independentFeatures}{ A list with Ambit names for all genes/ proteins features included in the model }
  \item{predictedFeatures}{A character vector with names for the suggested trials}
  \item{additionalInfo}{ A list including the following: design (The design suggested), selected.rows (The rows (nanoparticles) suggested for new trials), norm.values (The minimax normalized variance over X, expressed as an efficiency with respect to the optimal approximate theory design.), confounding.effect (The diagonality of the design, excluding the constant, if any.), r.squared (when y is given), adj.r.squared (when y is given), verbal.notes (Verbal notes to comment on norm.values and confounding.effect), predictedFeatures (The character string 'suggestedTrials' and 'newY' to indicate the new vectors created including all experimental design memberships).}
  Note: When the number of trials (nTrials) is higher than the number of y empty values, all values are suggested by the design algorithm.
}

\references{The help file of blockcluster package }

\author{Georgia Tsiliki}

\note{ No notes for this function }

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--  or do  help(data=index)  for the standard data sets.

data("dat1")

predF<- "https://apps.ideaconsult.net/enmtest/property/TOX/UNKNOWN_TOXICITY_SECTION/Net+cell+association/8058CA554E48268ECBA8C98A55356854F413673B/3ed642f9-1b42-387a-9966-dea5b91e5f8a"

required.param<- list(nTrials=c(11),criterion='D',form='linear',r2.threshold=0.9)

exp.example<- exp.design.xy(dat1,predF,required.param) 

}

\keyword{ expDesignXY }

