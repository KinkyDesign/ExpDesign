\name{ExpDesignPkg3.0-package}
\alias{ExpDesignPkg3.0-package}
\alias{ExpDesignPkg3.0}
\docType{package}
\title{
\packageTitle{ExpDesignPkg3.0}
}
\description{
\packageDescription{ExpDesignPkg3.0}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{ExpDesignPkg3.0}
\packageIndices{ExpDesignPkg3.0}
The most important functions of the package are exp.design.xy, exp.design.noxy, exp.design.dr. They produce a suggested trials vector/ matrix respectively serialized for suggested.trials.xy, suggested.trials.noxy, suggested.trials.dr to handle. 
}
\author{
\packageAuthor{ExpDesignPkg3.0}

Maintainer: \packageMaintainer{ExpDesignPkg3.0}
}
\references{Help files of AlgDesign and DoseFinding
}
\keyword{ package }
\examples{
data("dat1")

predF<- "https://apps.ideaconsult.net/enmtest/property/TOX/UNKNOWN_TOXICITY_SECTION/Net+cell+association/8058CA554E48268ECBA8C98A55356854F413673B/3ed642f9-1b42-387a-9966-dea5b91e5f8a"

required.param<- list(nTrials=c(11),criterion='D',form='linear',r2.threshold=0.9)

exp.example<- exp.design.xy(dat1,predF,required.param) 

}
