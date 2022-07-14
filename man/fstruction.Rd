\name{fstruction}

\alias{fstruction}

\title{Formula removal of singular random effects}

\description{
	Removes those random effects from a model formula making the model to
	be singular.
}
\usage{
fstruction(model, tol = 1e-4)
}
\arguments{
	\item{model}{The model of interest, output of either
		\code{\link[lme4]{lmer}} or \code{\link[lme4]{glmer}}, that is, an
		object of class \code{\link[=merMod-class]{merMod}} and
		\emph{subclass} either \code{lmerMod} or \code{glmerMod}.}
	\item{tol}{A numeric value (default is \code{1e-4}), tolerance under
		which to determine if the random effects make the model to be
		singular.}
}

\value{

	A list with a string component \code{dstring} which is the formula as
	character updated (to be used by \code{update}) after removing the
	singular random effects and, when no all the random effects are
	removed two other string components:

	\itemize{
		\item{\code{betchar}: a character vector with the random effect
			terms removed from the formula.}
		\item{\code{inchar}: a character vector having the same length as
			\code{betchar} with the corresponding groups (aggregate levels) for
			which the random effects are removed.}
	}

}


\seealso{
	\code{\link{formula}}, \code{\link{update}}
}

\examples{
if(requireNamespace("nlme")){
	library(lme4)
	data(Orthodont, package = "nlme")
	Orthodont$nsex <- as.numeric(Orthodont$Sex == "Male")
	Orthodont$nsexage <- with(Orthodont, nsex*age)
	## The next model is singular
	fmo <- lmer(distance ~ age + (age|Subject) + (0+nsex|Subject) +
		    (0 + nsexage|Subject), data = Orthodont)
	summary(fmo)

	## Let's see the formula updated (as a string)
	fstruction(fmo)
}
}

\author{
	Iago Giné-Vázquez, \email{iago.gin-vaz@protonmail.com}
}


