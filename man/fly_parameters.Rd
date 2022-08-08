\name{fly_parameters}
\alias{fly_parameters}
\docType{data}
\title{  
	Data for the SO question
	\href{https://stackoverflow.com/questions/23478792/warning-messages-when-trying-to-run-glmer-in-r}{warning
	  messages when trying to run glmer in r}.
}
    
\description{  
	Two integer variables (\code{ValidDetections} and
	\code{FalseDetections}) to build an outcome, two factor
	variables to be used as clusters for the random effects
	(\code{SUR.ID} and \code{Day}), three factor variables to be
	used as fixed effects (\code{tm}, \code{Area} and
	\code{replicate}), five numeric variables to be used as fixed
	effects (\code{c.distance}, \code{c.tm.depth},
	\code{c.receiver.depth}, \code{c.temp} and \code{c.wind}) and a
	pair of variables extra, allowing to build a model which fails
	to converge.
}

\usage{data("fly_parameters")}

\format{
  A data frame with 220 observations on the following 14 variables.
  \describe{
    \item{\code{SUR.ID}}{a factor with levels \code{10185} \code{10186} \code{10250}}
    \item{\code{tm}}{a factor with levels \code{CT} \code{PT-04}}
    \item{\code{ValidDetections}}{a numeric vector}
    \item{\code{CountDetections}}{a numeric vector}
    \item{\code{FalseDetections}}{a numeric vector}
    \item{\code{replicate}}{a factor with levels \code{1} \code{2}}
    \item{\code{Area}}{a factor with levels \code{Drug Channel} \code{Finger}}
    \item{\code{Day}}{a factor with levels \code{03/06/13} \code{2/22/13} \code{2/26/13} \code{2/27/13} \code{3/14/13}}
    \item{\code{R.det}}{a numeric vector}
    \item{\code{c.receiver.depth}}{a numeric vector}
    \item{\code{c.tm.depth}}{a numeric vector}
    \item{\code{c.temp}}{a numeric vector}
    \item{\code{c.wind}}{a numeric vector}
    \item{\code{c.distance}}{a numeric vector}
  }
}

\source{
  \href{https://stackoverflow.com/questions/23478792/warning-messages-when-trying-to-run-glmer-in-r}{warning
	messages when trying to run glmer in r}
}

\examples{
data(fly_parameters)
str(fly_parameters)
df <- fly_parameters
df$SUR.ID <- factor(df$SUR.ID)
df$replicate <- factor(df$replicate)
Rdet <- cbind(df$ValidDetections,df$FalseDetections)
Unit <- factor(1:length(df$ValidDetections))
\donttest{
library(lme4)
m1 <- glmer(Rdet ~ tm:Area + tm:c.distance +
            c.distance:Area + c.tm.depth:Area +
            c.receiver.depth:Area + c.temp:Area +
            c.wind:Area +
            c.tm.depth + c.receiver.depth +
            c.temp +c.wind + tm + c.distance + Area +
            replicate +
            (1|SUR.ID) + (1|Day) + (1|Unit) ,
            data = df, family = binomial(link="logit"))
summary(m1)
m1_new <- dwmw(m1, scale = TRUE, max_message_iter = 3)
summary(m1_new)
}
}

\keyword{datasets}

