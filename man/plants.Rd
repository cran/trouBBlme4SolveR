\name{plants}
\alias{plants}
\docType{data}
\title{
  Data for the SO question \href{https://stackoverflow.com/questions/60028673/lme4-error-boundary-singular-fit-see-issingular}{lme4 error: boundary (singular) fit: see ?isSingular}.
}
\description{
  Five variables, being one continuous to use as outcome
  (\code{Weight}), and four factors, of which two (\code{Rep} and
  \code{PLANT}) are used as clusters for the random effects of a
  singular linear mixed model (\code{Weight ~ 1 + (1|Rep:PLANT)}).
}

\usage{data("plants")}
\format{
  A data frame with 536 observations on the following 5 variables.
  \describe{
    \item{\code{Line}}{a factor with levels \code{Line1} \code{Line10} \code{Line11} \code{Line12} \code{Line13} \code{Line14} \code{Line15} \code{Line16} \code{Line17} \code{Line18} \code{Line19} \code{Line2} \code{Line20} \code{Line21} \code{Line22} \code{Line23} \code{Line24} \code{Line25} \code{Line26} \code{Line27} \code{Line28} \code{Line29} \code{Line3} \code{Line30} \code{Line31} \code{Line32} \code{Line33} \code{Line34} \code{Line35} \code{Line36} \code{Line37} \code{Line38} \code{Line39} \code{Line4} \code{Line40} \code{Line41} \code{Line42} \code{Line43} \code{Line44} \code{Line45} \code{Line46} \code{Line47} \code{Line48} \code{Line49} \code{Line5} \code{Line50} \code{Line51} \code{Line52} \code{Line53} \code{Line54} \code{Line55} \code{Line56} \code{Line57} \code{Line58} \code{Line59} \code{Line6} \code{Line60} \code{Line61} \code{Line62} \code{Line63} \code{Line64} \code{Line65} \code{Line66} \code{Line67} \code{Line7} \code{Line8} \code{Line9}}
    \item{\code{Rep}}{a factor with levels \code{1} \code{2}}
    \item{\code{Weight}}{a numeric vector}
    \item{\code{PLANT}}{a factor with levels \code{1} \code{2} \code{3} \code{4}}
    \item{\code{X}}{a factor with levels \code{24} \code{12} \code{21} \code{11} \code{13} \code{14} \code{22} \code{23}}
  }
}
\source{
	\href{https://stackoverflow.com/questions/60028673}{lme4 error: boundary (singular) fit: see ?isSingular}
}

\examples{
data(plants)
str(plants)
\donttest{
library(lme4)
fit <- lmer(Weight ~ 1 + (1|PLANT:Rep), data = plants)
summary(fit)
fit2 <- dwmw(fit)
summary(fit2)
}
}

\keyword{datasets}
