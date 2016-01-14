\name{ExpDesignPkg-package}
\alias{ExpDesignPkg-package}
\alias{ExpDesignPkg}
\docType{package}
\title{
\packageTitle{ExpDesignPkg}
}

\description{Calculates an exact or approximate algorithmic design for one of three criteria, using Federov's exchange algorithm from AlgDesign package
}

\details{

The DESCRIPTION file:
\packageDESCRIPTION{ExpDesignPkg}
\packageIndices{ExpDesignPkg}
The most important functions of the package are exp.design.funct.xy and exp.design.funct.noxy. They produce a a suggested trials vector/ matrix respectively serialized for suggested.trials.xy and suggested.trials.noxy to handle. 
}

\author{
\packageAuthor{ExpDesignPkg}

Maintainer: \packageMaintainer{ExpDesignPkg}
}

\references{Help files of AlgDesign}

\keyword{ package }

\examples{

data("dat1")

predF<- "https://apps.ideaconsult.net/enmtest/property/TOX/UNKNOWN_TOXICITY_SECTION/Net+cell+association/8058CA554E48268ECBA8C98A55356854F413673B/3ed642f9-1b42-387a-9966-dea5b91e5f8a"

required.param<- list(nTrials=c(11),criterion='D',form='linear',r2.threshold=0.9)

exp.example<- exp.design.xy(dat1,predF,required.param) 

}
