\name{dat1i}
\alias{dat1i}
\docType{data}

\title{Information for experimental design function suggest.trials.xy}

\description{A list with information for experimental design function suggest.trials.xy}

\usage{data("dat1i")}

\format{
  A list with 8 objects:
  \describe{
  \item{\code{design}}{a numeric matrix with the suggested design for the data}
    \item{\code{selected.rows}}{the selected rows for all the available combinations of the parameters.}
  \item{\code{norm.var}}{the normalized variance. This is the Ge value from optFederov(): The minimax normalized variance over X, expressed as an efficiency with respect to the optimal approximate theory design. Optimal value is 1.}
  \item{\code{confounding.effect}}{the diagonality of the design, excluding the constant if any, from eval.design() function. The optimal number is 1.}
  \item{\code{r.squared}}{the r2 value for the data supplied}
  \item{\code{adj.r.squared}}{the adjusted r2 value for the data supplied.}
  \item{\code{verbal.notes}}{remarks commenting on Ge and diagonality.}
}
}

\details{Example dataset to suggest trials together with dat1p, dat1m}

\source{The source of this function is in the}

\references{There are no references}

\examples{
data(dat1i)
## maybe str(dat1i) ; plot(dat1i) ...
}

\keyword{datasetsi}
