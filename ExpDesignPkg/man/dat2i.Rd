\name{dat2i}
\alias{dat2i}
\docType{data}

\title{Information for experimental design function suggest.trials.noxy}

\description{A list with information for experimental design function suggest.trials.noxy}

\usage{data("dat2i")}

\format{
  A list with 6 objects:
  \describe{
  \item{\code{design}}{a numeric matrix with the suggested design for the data}
    \item{\code{selected.rows}}{the selected rows for all the available combinations of the parameters.}
  \item{\code{norm.var}}{the normalized variance. This is the Ge value from optFederov(): The minimax normalized variance over X, expressed as an efficiency with respect to the optimal approximate theory design. Optimal value is 1.}
  \item{\code{confounding.effect}}{the diagonality of the design, excluding the constant if any, from eval.design() function. The optimal number is 1.}
  \item{\code{verbal.notes}}{remarks commenting on Ge and diagonality.}
}
}

\details{Example dataset to suggest trials together with dat2m}

\source{The source of this function is in the}

\references{There are no references}

\examples{
data(dat2i)
## maybe str(dat2i) ; plot(dat2i) ...
}

\keyword{datasets2i}
