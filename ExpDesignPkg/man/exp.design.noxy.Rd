\name{exp.design.noxy}
\alias{exp.design.noxy}

\title{Experimental design function for (full) factorial designs
}
\description{Calculates an exact or approximate algorithmic design for one of three criteria, using Federov's exchange algorithm from AlgDesign package. The user needs to specify the number of variables and their levels, then a design matrix with suggested trials is produced. Please note in this case, no X or Y values are provided.
}
\usage{
exp.design.noxy(dataset, predictionFeature, parameters)
}

\arguments{
  \item{dataset}{list of 2 objects, datasetURI and dataEntry - NOT required 
}
  \item{predictionFeature}{ character string specifying which is the prediction feature in dataEntry - NOT required
}
  \item{parameters}{A list with 7 objects: levels, nVars (number of variables), factors (which are the factor variables), varNames (variables' names), nTrials (the number of suggested trials, if 0 then an estimated number is suggested), criterion ('A', 'I', 'D'), form of the design ('linear','quad','cubic','cubicS')
}
}

\details{No details required}

\value{A List 
  \item{rawModel}{ A serialized numeric matrix indicating the experimental design for the various variables and their levels.}
  \item{pmmlModel}{ A pmml object - now empty}
  \item{independentFeatures}{ A list with the names of the variables as given by the user.}
  \item{predictedFeatures}{A character vector with names for the suggested trials}
  \item{additionalInfo}{ A list including the following: design (The design suggested), selected.rows (The rows (nanoparticles) suggested for new trials), norm.values (The minimax normalized variance over X, expressed as an efficiency with respect to the optimal approximate theory design.), confounding.effect (The diagonality of the design, excluding the constant, if any.), verbal.notes (Verbal notes to comment on norm.values and confounding.effect), predictedFeatures (The character string 'suggestedTrials' to indicate the new vector created including all experimental design memberships).}
}

\references{The help file of blockcluster package }

\author{Georgia Tsiliki}

\note{ No notes for this function }


\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.


required.param<-  list(levels=3, nVars=3, factors='null', varNames=c('a','b','c'),nTrials=10,criterion='D',form='linear')


exp.example<- exp.design.noxy(null,null,required.param) 
}

\keyword{expDesignNoXY }
