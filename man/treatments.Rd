\name{treatments}
\alias{treatments}
\docType{data}
\title{
  
	Data for the Cross Validated question
	\href{https://stats.stackexchange.com/questions/575666/lme4-glmer-warning-messages-with-count-data-mixed-effects-model-and-how-to-pr}{lme4:
	glmer() warning messages with count data mixed-effects model and
	how to proceed with model fit}

}
\description{
  A continuous variable to be used as outcome (\code{total_no}), another
  to be used as predictor (\code{week}), two factor variables to be used
  as predictors (\code{treatment} and \code{fzone}) and another factor
  to be used as cluster for the random effects (\code{plot}) of a
  Poisson model failing to converge, and an extra variable.
}

\usage{data("treatments")}
\format{
  A data frame with 142 observations on the following 7 variables.
  \describe{
    \item{\code{plot}}{a numeric vector}
    \item{\code{date}}{a character vector}
    \item{\code{total_no}}{a numeric vector}
    \item{\code{zone}}{a character vector}
    \item{\code{treatment}}{a character vector}
    \item{\code{week}}{a numeric vector}
    \item{\code{fzone}}{a character vector}
  }
}
\source{
  
\href{https://stats.stackexchange.com/questions/575666/lme4-glmer-warning-messages-with-count-data-mixed-effects-model-and-how-to-pr}{lme4:
	glmer() warning messages with count data mixed-effects model and
	how to proceed with model fit}

}
\examples{
data(treatments)
str(treatments)
\donttest{
library(lme4)
glmm.1 <- glmer(total_no ~ week*treatment*fzone + (1|plot), data = treatments,
              family = poisson)
summary(glmm.1)
glmm.11 <- dwmw(glmm.1, verbose = TRUE)
summary(glmm.11)
}
}
\keyword{datasets}
