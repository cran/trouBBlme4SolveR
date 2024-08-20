\name{dwmw}

\alias{dwmw}

\alias{trouBBlme4SolveR}

\title{Solver for (generalized) linear MM warnings}

\description{  
	Updates a (possibly generalized) linear mixed model computed with the
	library \pkg{lme4} functions \code{lmer} or \code{glmer}, in order to
	elude some singularity or convergence problems informed by distinct
	warnings or messages.
}

\usage{
dwmw(lmmodel,
     boundary_check = TRUE, scale = FALSE, scale_info = TRUE,
     tol = 1e-4, max_message_iter = 7, pri_nAGQ = FALSE, max_nAGQ = 6,
     next_optimizer = "bobyqa", next_optCtrl = list(maxfun = 2e5),
     when_next = max_message_iter - 1, verbose = FALSE)
}

\arguments{  
	\item{lmmodel}{The model of interest, output of either
		\code{\link[lme4]{lmer}} or \code{\link[lme4]{glmer}}, that is, an
		object of class \code{\link[lme4]{merMod}} and
		\emph{subclass} either \code{lmerMod} or \code{glmerMod}.}
	\item{boundary_check}{A length 1 boolean object (\code{TRUE} [default]
		or \code{FALSE}), determining if the function removes those random
		effects which make the model singular (in the simplest situation,
		those with standard deviation almost 0.}
	\item{scale}{A length 1 boolean object (\code{TRUE} or \code{FALSE}
		[default]), determining if the function must rescale the numeric
		predictors when this action is asked in a \emph{warning} or
		\emph{message}.}
	\item{scale_info}{A length 1 boolean object (\code{TRUE} [default] or
		\code{FALSE}), determining if the function should cat a message when
		some predictor variables are on very different scales, but are not
		rescaled.}
	\item{tol}{A numeric value (default is \code{1e-4}), tolerance under
		which to consider if a model is singular (see \code{boundary_check}
		argument).}
	\item{max_message_iter}{A numeric value (default is \code{7}),
		generally the maximum number of iterations allowed to update the
		model. For the exception, see the Details section.}
	\item{pri_nAGQ}{A length 1 boolean object (\code{TRUE} or \code{FALSE} 
		[default]), determining if the function prioritizes incrising
		\code{nAGQ} over updating model start parameters when both are 
		possible solutions to warned issues.}
	\item{max_nAGQ}{A numeric value (default is \code{6}), maximum to
		which increase the \code{nAGQ} argument fitting
		\code{\link[lme4]{glmer}} models.}
	\item{next_optimizer}{An optimizer (default is \code{"bobyqa"}), next
		to be used when model convergence is failing.}
	\item{next_optCtrl}{A \code{\link{list}} (default is
		\code{list(maxfun=2e5)}) of additional arguments to be passed to the
		nonlinear optimizer (see \code{\link[lme4]{Nelder_Mead}},
		\code{\link[minqa]{bobyqa}}). In particular, both \code{Nelder_Mead}
		and \code{bobyqa} use \code{maxfun} to specify the maximum number of
		function evaluations they will try before giving up - in contrast to
		\code{\link{optim}} and \code{optimx}-wrapped optimizers, which use
		\code{maxit}. (Also see \code{\link[lme4]{convergence}} for details
		of stopping tolerances for different optimizers.}
	\item{when_next}{A numeric value not greater than
		\code{max_message_iter - 1} (which is the default), specifying from
		which iteration a model convergence failing is approached with a new
		optimizer (\code{next_optimizer}) and with a new set of arguments to
		it (\code{next_optCtrl}).}
	\item{verbose}{A length 1 boolean object (\code{TRUE} or \code{FALSE} 
		[default])} specifying if function iterations and model updating 
	should be verbose.
}

\value{
	Generally, an object of class
	\code{\link[lme4]{merMod}}, being either an object of
	\emph{subclass} \code{glmerMod} or an object of subclass
	\code{lmerMod}, corresponding to the input \code{lmmodel}
	argument. When all random effects are removed, an object of
	class \code{glm} or \code{lm} depending also on the input
	\code{lmmodel}.
}

\details{

	If the model does not generate any warning or message, \code{dwmw}
	returns itself. The function rescales numeric predictors if some
	warning or message suggests it and the \code{scale} argument is
	\code{TRUE}. If the model fails to converge, it only updates the starting
	values for the parameters in the model through the first
	\code{when_next - 1} iterations, while it also updates the optimizer
	and their arguments through the next iterations.

	If the model is singular (there is a
	\emph{"boundary (singular)"}-alike message) and \code{boundary_check}
	is \code{TRUE}, the formula is updated removing the random effects
	which cause the model to be singular. If all the random effects are
	removed, a linear or generalized linear model is computed according to
	the \code{lmmodel} object.

	If the model has subclass \code{glmerMod}, a single scalar random
	effect and converges but it is nearly unidentifiable with a very large
	eigenvalue, then \code{nAGQ} is increased one unity while \code{nAGQ
		<= max_nAGQ} (and only in this case, allowing the iterations to exceed
	\code{max_message_iter} until \code{nAGQ == max_nAGQ}).

}

\seealso{

	\code{\link[lme4]{lmer}}, \code{\link[lme4]{glmer}},
	\code{\link[lme4]{merMod}} \code{\link[lme4]{lmerControl}},
	\code{\link[lme4]{convergence}}, \code{\link[lme4]{troubleshooting}},
	\code{\link[lme4]{isSingular}}, \code{\link{lm}} and
	\code{\link{glm}}.

}

\examples{

if(requireNamespace("nlme")){
	library(lme4)
	data(Orthodont, package = "nlme")
	Orthodont$nsex <- as.numeric(Orthodont$Sex == "Male")
	Orthodont$nsexage <- with(Orthodont, nsex*age)
	## Without using dwmw, the next model is singular
	fmo <- lmer(distance ~ age + (age|Subject) + (0+nsex|Subject) +
		    (0 + nsexage|Subject), data = Orthodont)
	summary(fmo)

	## Using dwmw, we remove the singularity of the model
	fmo1 <- dwmw(lmer(distance ~ age + (age|Subject) + (0+nsex|Subject) +
			  (0 + nsexage|Subject), data = Orthodont))
	summary(fmo1)

	## It also can be used on the output object

	fmo2 <- dwmw(fmo)
	summary(fmo2)
}

}

\author{
	Iago Giné-Vázquez, \email{iago.gin-vaz@protonmail.com}
}

\keyword{models}
