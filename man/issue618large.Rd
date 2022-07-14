\name{issue618large}
\alias{issue618large}
\docType{data}
\title{
	Data example github issue 618 on the lme4 repository
}

\description{
	Three variables, being one dichotomous, other continuous and the last,
	categorical. Data for the first reproducible example for issue 618 on
	the lme4 Github-repository, making \code{glmer} to produce a
	\preformatted{Model is nearly unidentifiable: very large eigenvalue -
		Rescale variables?} warning for the model \code{outcome_dead ~ AGE +
		(1|ZIP)}.
}

\usage{data("issue618")}

\format{
	A data frame with 1000 observations on the following 3 variables.
	\describe{
		\item{\code{outcome_dead}}{an integer vector}
		\item{\code{AGE}}{an integer vector}
		\item{\code{ZIP}}{an integer vector}
	}
}



\source{
	\href{https://github.com/lme4/lme4/issues/618}{convergence issues with continuous variables in model}
}

\examples{
data(issue618)
str(issue618)
\donttest{
library(lme4)
m1<- glmer(outcome_dead ~ AGE + (1|ZIP), family = binomial, data = issue618large)
summary(m1)
m2 <- dwmw(m1)
summary(m2)
m3 <- glmer(outcome_dead ~ scale(AGE) + (1|ZIP), family = binomial, data = issue618large, 
	    nAGQ=20)
aa <- allFit(m3)
ss <- summary(aa)
ss$msgs  ## all NULL
}
}

\keyword{datasets}
