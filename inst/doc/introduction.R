### R code from vignette source 'introduction.Rnw'

###################################################
### code chunk number 1: <
###################################################
library(lme4)
data("fly_parameters", package = "trouBBlme4SolveR")
df <- fly_parameters
df$SUR.ID <- factor(df$SUR.ID)
df$replicate <- factor(df$replicate)
Rdet <- cbind(df$ValidDetections,df$FalseDetections)
Unit <- factor(1:length(df$ValidDetections))

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
numcols <- grep("^c\\.",names(df))
dfs <- df
dfs[,numcols] <- scale(dfs[,numcols])
m1_sc <- update(m1,data=dfs)
ss <- getME(m1_sc,c("theta","fixef"))
m3 <- update(m1_sc,start=ss,control=glmerControl(optimizer="bobyqa",
						 optCtrl=list(maxfun=2e5)))
summary(m3)

library(trouBBlme4SolveR)
m1_new <- dwmw(m1, scale = TRUE, max_message_iter = 3)
summary(m1_new)


###################################################
### code chunk number 2: introduction.Rnw:61-70
###################################################
if(requireNamespace("nlme")){
	data(Orthodont,package="nlme")
	Orthodont$nsex <- as.numeric(Orthodont$Sex=="Male")
	Orthodont$nsexage <- with(Orthodont, nsex*age)
	fmo <- lmer(distance ~ age + (age|Subject) + (0 + nsex|Subject) +
		    (0 + nsexage|Subject), data = Orthodont)
	# without warnings
	fmo_new <- dwmw(fmo)
}


###################################################
### code chunk number 3: introduction.Rnw:73-74 (eval = FALSE)
###################################################
## summary(fmo)


###################################################
### code chunk number 4: introduction.Rnw:77-78
###################################################
tryCatch(summary(fmo), error = function(e) "fmo object does not exist. Package 'nlme' should be installed first in your system.")


###################################################
### code chunk number 5: introduction.Rnw:81-82 (eval = FALSE)
###################################################
## summary(fmo_new)


###################################################
### code chunk number 6: introduction.Rnw:85-86
###################################################
tryCatch(summary(fmo_new), error = function(e) "fmo_new object does not exist. Package 'nlme' should be installed first in your system.")


###################################################
### code chunk number 7: introduction.Rnw:97-102
###################################################
data("plants", package = "trouBBlme4SolveR")
fit <- lmer(Weight ~ 1 + (1|Rep:PLANT), data = plants)
summary(fit)
fit_new <- dwmw(fit)
summary(fit_new)


###################################################
### code chunk number 8: introduction.Rnw:114-119
###################################################
data("issue618", package = "trouBBlme4SolveR")
fit <- glmer(outcome_dead ~ AGE + (1|ZIP), family = binomial, data = issue618)
summary(fit)
fit_new <- dwmw(fit, scale = TRUE)
summary(fit_new)


###################################################
### code chunk number 9: introduction.Rnw:124-129
###################################################
data("issue618large", package = "trouBBlme4SolveR")
fit <- glmer(outcome_dead ~ AGE + (1|ZIP), family = binomial, data = issue618large)
summary(fit)
fit_new <- dwmw(fit, scale = TRUE)
summary(fit_new)


###################################################
### code chunk number 10: introduction.Rnw:139-144
###################################################
data("treatments", package = "trouBBlme4SolveR")
glmm.1 <- glmer(total_no ~ week * treatment * fzone + (1|plot), data = treatments, family = poisson)
summary(glmm.1)
glmm.11 <- dwmw(glmm.1, verbose = TRUE)
summary(glmm.11)


###################################################
### code chunk number 11: introduction.Rnw:156-168
###################################################
if(requireNamespace("ggplot2")){
	data("diamonds", package = "ggplot2")

	# Grab the priciest diamonds
	diamonds_subset <- diamonds[(nrow(diamonds)-10000):nrow(diamonds),]
	# Fit the model
	fit_1 <- lmer(carat ~ depth + table + price + x + y + z + (1 + price | cut), data = diamonds_subset)
	# Let's try dividing price by 1000
	fit_2 <- lmer(carat ~ depth + table + I(price/1000) + x + y + z + (1 + I(price/1000) | cut), data = diamonds_subset)

	fit_new <- dwmw(fit_1, scale = TRUE, verbose = TRUE)
}


###################################################
### code chunk number 12: introduction.Rnw:172-173 (eval = FALSE)
###################################################
## summary(fit_1)


###################################################
### code chunk number 13: introduction.Rnw:176-177
###################################################
tryCatch(summary(fit_1), error = function(e) "fit_1 object does not exist. Package 'ggplot2' should be installed first in your system.")


###################################################
### code chunk number 14: introduction.Rnw:181-182 (eval = FALSE)
###################################################
## summary(fit_2)


###################################################
### code chunk number 15: introduction.Rnw:185-186
###################################################
tryCatch(summary(fit_2), error = function(e) "fit_2 object does not exist. Package 'ggplot2' should be installed first in your system.")


###################################################
### code chunk number 16: introduction.Rnw:190-191 (eval = FALSE)
###################################################
## summary(fit_new)


###################################################
### code chunk number 17: introduction.Rnw:194-195
###################################################
tryCatch(summary(fit_new), error = function(e) "fit_new object does not exist. Package 'ggplot2' should be installed first in your system.")


###################################################
### code chunk number 18: <
###################################################
sessionInfo()


